
require(abodOutlier)
require(HighDimOut)


## lof------------
AlgLof <- function( dat, k.nn ){
  require(dbscan)
  score <- lof( dat, k.nn  )
  score[is.infinite(score)] <- 1
  return(score)
}


## wknn-------------
AlgWknn <- function( dat, k.nn ){
  knn.dst <- as.data.frame(kNN(dat, k.nn)$dist)
  score <- rowMeans( knn.dst )
  return(score)
}


## iforest-------------
AlgIforest <- function( dat, ntree = 100 ){
  require(IsolationForest)
  iTrees <- IsolationTrees(dat, ntree)
  
  score <- AnomalyScore(dat,iTrees)$outF
  return(score)
}


## also------------
AlgAlso <- function(da, method="m5p"){
  
  ## some special character in names may cause errors
  #old.names <- colnames(da)
  colnames(da) <- paste0("V", 1:ncol(da))
  
  if( !method %in% c("m5p", "cart")){
    print( "method in ALSO is  wrong, choose from m5p or CART.")
    stop()
  }
  
  da <- MyScale(da)
  if( (nrow(da)>2000) && (ncol(da) >150) ) para <- T else para <- F
  # use total core - 1 for parallel computation
  if(para) registerDoParallel(detectCores() - 1)
  `%myinfix%` <- ifelse(para, `%dorng%`, `%do%`)
  # only parallel computing at inner loop
  flds <- caret::createFolds(1:nrow(da), k = 10, list = TRUE, returnTrain = FALSE)
  
  
  dev.all <- foreach( i = 1:10, .combine = rbind, .inorder = T) %do% {
    # i=2
    tst <- as.data.frame( da[flds[[i]],] )
    trn <- as.data.frame (da[!(1:nrow(da)) %in% flds[[i]],])
    
    dev.fld <- foreach( j = 1:ncol(da), .combine = cbind, .inorder = T ) %myinfix% {
      # j=2
      # fit regression model
      if (method == "m5p"){
        library(Cubist)
        fit <- cubist(x=trn[,-j],
                      y=trn[,j],
                      committees=1)
      }
      
      if (method == "cart"){
        exp <- parse(text=sprintf("%s~%s",
                                  colnames(da)[j],
                                  paste0(colnames(da)[-j], collapse = "+")))
        
        require(rpart)
        fit <- rpart(eval(exp),
                     data = trn,
                     method = "anova",  ## "anova" "poisson"
                     control = rpart.control(minsplit = 20,
                                             minbucket = 7,
                                             maxdepth = 30,
                                             xval = 10,
                                             cp = 0.003 ))
      }
      expect <- predict(fit, tst)
      (tst[,j] - expect)^2
    }
    dev.fld
  }
  dev.all[unlist(flds),] <- dev.all
  dev.all <- as.data.frame(dev.all)
  # del.c <- which( vapply(dev.all, function(x) length(unique(x)), numeric(1) ) == T )
  # dev.all <- dev.all[,-del.c]
  
  # i = 21
  wk <- foreach ( i = 1:ncol(dev.all), .combine = c, .inorder = T ) %do% {
    1 - min( 1, sqrt( sum(dev.all[,i]) / sum((da[,i]-mean(da[,i]))^2)) )
  }
  wk[is.na(wk)] <- 0
  
  score <- foreach ( i = 1:nrow(dev.all), .combine = 'c', .inorder = T) %do% {
    sqrt ( sum(dev.all[i,] * wk) / sum(wk) )
  }
  
  if(para) stopImplicitCluster()
  return(score)
}






## combn------------

GetExpectValueCol <- function(bn, ds, column){
  #get parents
  p <- bn[[column]]$parents
  if(length(p) == 0) {
    exp <- rep( mean(ds[,column]), nrow(ds)) 
  } else{
    coe <- bn[[column]]$coefficients
    n <- nrow(ds)
    exp <- vapply( 2:length(coe), 
                   function(i) ds[,p[i-1]] * coe[i],
                   numeric(n))
    exp <- rowSums( cbind( rep(coe[[1]],n), exp) )
  }
  return(exp)
}

# get the dependency factor of a specific colunm of ds based on bn
GetPcDevationSingle <- function(bn, ds, column){
  if( is.numeric(column) & (column > length(bn))) {
    print(sprintf("column index (%d) exceed the length of BN (%d)",column, length(bn)))
    return(NULL)
  }
  if( is.character(column) & !(column %in% names(bn))) {
    print(sprintf("column name (%s) is not in BN",column))
    return(NULL)
  }
  if(ncol(ds) != length(bn)){
    print(sprintf("column number of ds(%d) is different with BN (%d)",ncol(ds), length(bn)))
    return(NULL)
  }
  if( is.matrix(ds)) ds <- data.frame(ds)
  
  exp <- GetExpectValueCol(bn, ds, column)
  colnames(ds) <- names(bn)
  
  #compute the deviation
  dev <- abs((ds[, column] - exp) / bn[[column]]$sd)
  dev[is.na(dev)] <- 0  
  return(dev)
}

AlgCombn <- function( dat, sample.info ){
  if (is.matrix(dat)) dat <- data.frame(dat)
  bn <- LearnBn(dat, sample.info)
  
  child <- lapply(names(bn), function(x) children(bn,x))
  child <- unique(unlist(child))
  
  if(length(child) == 0 ){
    print("There is no child in bn")
    return(NULL)
  }
  
  n <- nrow(dat)
  dev <- vapply( iter(child),
                 function(x) GetPcDevationSingle(bn,dat,x),
                 numeric(n)  )
  return(rowSums(dev))
}




## mbom------------
AlgMbom <- function(dat, sample.info, k.nn){
  
  get.mb <- F
  mb <- LoadAlgInfo( folder="rel_var", sample.info, alg="mbom")
  if( is.null(mb)) get.mb <- T else {
    ## load the saved time for learning MB
    rt.mb <- LoadAlgInfo("time_rel", sample.info, alg = "mbom" )
    if( is.null(rt.mb) ) get.mb <- T else {
      idx <- which(rt.mb$computer == Sys.info()["nodename"])
      if( length(idx) > 0 ) rt.mb <- rt.mb[idx[1], "runtime"] else get.mb <- T
    }
  } 
  
  if (get.mb == T) {
    rt.mb <- Sys.time()
    mb <- GetRelVar( sample.info, dat, "mb" )
    rt.mb <- difftime(Sys.time(), rt.mb, units = "secs")[[1]]
    SaveAlgInfo(rt.mb, folder="time_rel", sample.info = sample.info, alg = "mbom")
  }
 
  rt.test <- Sys.time()
  dat <- as.data.frame(dat)
  
  n <- nrow(dat)
  lf <- vapply(iter(mb), 
               function(x) AlgLof(dat[,unlist(x)], k.nn),
               numeric(n))
  
  #save test time
  rt.test <- difftime(Sys.time(), rt.test, units = "secs")[[1]] + rt.mb
  SaveAlgInfo( rt.test, "time_test", sample.info, alg)
  
  return(apply(lf, 1, max))
  
}


## abod ---------------
AlgAbod <- function( dat, k = 10 ){
  score <- abod(dat, method = "knn", k=k)
  score <- max(score) - score
  return(score)
}


## Sod ---------------
AlgSod <- function( dat, k = 10){
  
  if( (nrow(dat)>2000) && (ncol(dat) >150) ) para <- T
  if(para) registerDoParallel(detectCores() - 1)
  
  score <- func.SOD(dat, k + 10, k, alpha = 0.8)
  if(para) stopImplicitCluster()
  return(score)
}

func.SOD <- function(data.scaled, kNN.value, kNN.final, alpha) {
  # Generate the matrix containing the row.no of obs to be considered as
  # nearest neighbors
  candidate.mat <- func.SNN(data.scaled = data.scaled, kNN.value = kNN.value, 
                            kNN.final = kNN.final)
  res.SOD <- foreach(i = 1:dim(candidate.mat)[1], .combine = rbind) %dopar% 
    {
      index.tc <- as.numeric(candidate.mat[i, ])
      df.temp <- data.scaled[index.tc, ]
      col.mean <- colMeans(df.temp)
      # Get the total variance
      VAR.s <- sum(diag(var(df.temp)), na.rm = T)
      # Get the individual variance for each variable
      var.s <- foreach(j = 1:dim(df.temp)[2], .combine = c) %dopar% {
        sum((df.temp[, j] - colMeans(df.temp)[j])^2, na.rm = T)/(dim(df.temp)[1] - 
                                                                   1)
      }
      # Get a vector judging whether the variable should be used
      limit.var <- alpha * VAR.s/dim(df.temp)[2]
      vec.judge <- foreach(m = 1:length(var.s), .combine = c) %dopar% 
        {
          judge <- ifelse(var.s[m] < limit.var, yes = 1, no = 0)
        }
      # Calculate the distance to the reference hyperplane
      obs <- data.scaled[i, ]
      dist.final <- sqrt(sum((obs - col.mean)^2 * vec.judge, na.rm = T))/sum(vec.judge)
      return(dist.final)
    }
  return(res.SOD)
}

func.SNN <- function(data.scaled, kNN.value, kNN.final) {
  require(Rlof)  #Since function distmc is attached in this package
  # Calculate the distance among observations
  dist.mat <- as.matrix(dist(x = data.scaled, method = "euclidean", diag = F))
  # Create a matrix to contain the index of kNN observations
  res.dist.order <- foreach(i = 1:dim(dist.mat)[2], .combine = rbind) %dopar% 
    {
      dist.order <- order(dist.mat[, i], decreasing = F)[1:(kNN.value + 
                                                              1)]
      dist.order.upd <- dist.order[-1]
    }
  # Create a matrix to store the number of common nearest neighbors
  nn.mat <- matrix(data = NA, nrow = dim(data.scaled)[1], ncol = dim(data.scaled)[1])
  for (j in 1:dim(data.scaled)[1]) {
    index.to.compare <- c(1:dim(data.scaled)[1])[-j]
    for (m in 1:length(index.to.compare)) {
      nn.mat[j, index.to.compare[m]] <- length(intersect(res.dist.order[j, 
                                                                        ], res.dist.order[index.to.compare[m], ]))
    }
  }
  diag(nn.mat) <- 0
  # Create a matrix to contain the candidate index to consider (using ordered
  # SNN)
  cand.mat <- foreach(p = 1:dim(nn.mat)[1], .combine = rbind) %dopar% {
    vec <- as.numeric(nn.mat[p, ])
    vec.order <- order(vec, decreasing = T)
    vect.output <- vec.order[1:kNN.final]
  }
  return(cand.mat)
}


## One Class SVM ---------------
AlgOcsvm <- function(train, sample.info, ga){
  require(e1071)
  
  ## train ocsvm
  fit <- svm( train, 
              type='one-classification',
              kernel = "radial",
              gamma= ga,
              nu= sample.info$ratio)
  
  score <- max(fit$decision.values) - fit$decision.values
  
  return(score)
}
  
