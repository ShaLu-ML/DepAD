

require(doRNG)
require(bnlearn)
require(ipred)
require(rpart)
require(FNN)
require(FSinR)


## (1) get relevant variables; (2)save time_rel;  (3)save rel.set
### input: sample.info, train, rel.type
### output: rel.set, a list of each element:
###          list( target = the name target variable, 
###                rel.var = a vector of name of variable, relevant variables of the target)
GetRelVar <- function( sample.info, train, rel.type ){

  ## added for tango version
  return(None)
  
  
  rel.type <- str_split(rel.type, "_", simplify = T)
  rel.name <- rel.type[1]
  if(length(rel.type) > 1) rel.para <- rel.type[2:length(rel.type)]
  
  if( rel.name %in% c("mb", "pc") ){
    cat("\n ERROR: Shouldn't go to GetRelVar !! \n")
    return(NULL)
  }
  
  # integer could not be accepted by bnlearn
  # TBD: why does the type of data is changed to integer from double after sampling??
  train <- as.data.frame(apply( train, 2, as.double))
  
  ## determine parallel computation
  if( (ncol(train)> 150) ) para <- T else para <- F
  if( rel.name == "i.effects") para <- T
  cat("start to get relevant variables. rel.type=", rel.type, " para=", para, "\n")
  if(para) registerDoParallel( detectCores() )
  `%myinfix%` <- ifelse(para, `%dorng%`, `%do%`)
  
  rt.rel <- Sys.time()
  
  ## ___ mi ----
  if( rel.name == "mi"){
    rel.set <- foreach( i=1:ncol(train) ) %myinfix% {
      # i = 1
      target <- colnames(train)[i]
      rel <- selectSlope(train, target, mutualInformation, 0.8)
      rel <- rel$featuresSelected
      if( length(rel) == 0 ){
        return(NULL)
      }  else {
        return( list( target = target, rel.var = rel) )
      }
    }
  }
  
  ## ___ iepc ----
  if( rel.name == "iepc"){
    rel.set <- foreach( i=1:ncol(train) ) %myinfix% {
      # i = 1
      target <- colnames(train)[i]
      rel <- selectSlope(train, target, IEPConsistency, 0.8)
      rel <- rel$featuresSelected
      if( length(rel) == 0 ){
        return(NULL)
      }  else {
        return( list( target = target, rel.var = rel) )
      }
    }
  }
  
  ## ___ relief ----
  if( rel.name == "relief"){
    rel.set <- foreach( i=1:ncol(train) ) %myinfix% {
      # i = 1
      target <- colnames(train)[i]
      rel <- selectSlope(train, target, relief, 0.8)
      rel <- rel$featuresSelected
      if( length(rel) == 0 ){
        return(NULL)
      }  else {
        return( list( target = target, rel.var = rel) )
      }
    }
  }
  
  ## ___ dc ----
  if( rel.name == "dc"){
    rel.set <- foreach( i=1:ncol(train) ) %myinfix% {
      # i = 1
      target <- colnames(train)[i]
      rel <- selectSlope(train, target, determinationCoefficient, 0.8)
      rel <- rel$featuresSelected
      if( length(rel) == 0 ){
        return(NULL)
      }  else {
        return( list( target = target, rel.var = rel) )
      }
    }
  }
  
  
  ## ___ mb ----
  if( rel.name == "mb"){
    alpha <- as.numeric(rel.para)
    ## get relevant variables
    rel.set <- foreach( i=1:ncol(train) ) %myinfix% {
      ## availabe mb methods:  "iamb", "fast.iamb", "inter.iamb", "gs"
      rel <- bnlearn::learn.mb( x = train, 
                       node = colnames(train)[i], 
                       method ="fast.iamb",
                       alpha = alpha)
      if( length(rel) == 0 ) return(NULL) else {
        return( list( target = colnames(train)[i], rel.var = rel) )
      }
    }
  }
  
  ## ___ pc ----
  if( rel.name == "pc"){
    alpha <- as.numeric(rel.para)
    # if( sample.info$id %in% c(36,39)){
    #   ## 36: backdoor
    #   ## subsample because PC can't work 
    #   train.smp <- train[ sample(1:nrow(train), 2000, replace = F ) , ]
    #   cat("\n subsample the dataset")
    # } else train.smp <- train
    train.smp <- train
    
    rel.set <- foreach( i=1:ncol(train.smp) ) %myinfix% {
      ## availabe mb methods:  "pc.stable", "mmpc", "si.hiton.pc", "hpc"
      rel <- bnlearn::learn.nbr( x = train.smp, 
                        node = colnames(train.smp)[i], 
                        method="si.hiton.pc", 
                        alpha = alpha,
                        debug = T,
                        max.sx = 50)
      if( length(rel) == 0 ) return(NULL) else {
        return( list( target = colnames(train.smp)[i], rel.var = rel) )
      }
    }
  }
  
  ## zero-order correlation
  if( rel.name == "cor"){
    threshold <- as.numeric(rel.para)
    rel.set <- foreach (i = 1:ncol(train)) %myinfix% {
      ## i = 1
      i.cor <-  abs( cor(train[,i], train) )
      i.cor[i] <- 0 ## filter correlation of the variable with itself
      i.cor[i.cor < threshold ] <- 0
      if( sum(i.cor, na.rm = T) == 0) return(NULL) else{
        list( target = colnames(train)[i], 
              rel.var = colnames(train)[which(i.cor >= threshold)] )
      }
    }
  }
  
  
  ## zero-order first, then independence effect
  if( rel.name == "i.effects"){
    require(hier.part)
    threshold <- as.numeric(rel.para)
    
    ## use parallel computation
    rel.set <- foreach (i = 1:ncol(train)) %myinfix% {
      ## i = 40
      # print(i)
      ## use zero-order correlation to filter variable with near zero correlation (0.05)
      i.cor <-  abs( cor(train[,i], train) )
      i.cor[i] <- 0 ## filter correlation of the variable with itself
      i.cor[i.cor < 0.05 ] <- 0
      
      
      ## use independence effect to get the variable importance
      idx.x <- which( i.cor > 0 )
      # length(idx.x)
      
      ## only support 12 predictors
      if( length(idx.x) > 12 ) idx.x <- which( rank(-i.cor, ties.method = "first") <= 12 )
      # i.cor[idx.x]
      # idx.x
      
      if( length(idx.x) <= 1 ) return(NULL)
      ie <- hier.part::hier.part( y = train[,i], 
                       xcan = train[, idx.x], 
                       fam = "gaussian", 
                       gof = "Rsqu",
                       exact = F,
                       barplot = F)
      ## get the percentage of I
      ie <- ie$I.perc
      rel <- rownames(ie)[ which( ie > threshold) ]
      if( length(rel) == 0 ) return(NULL) else {
        list( target = colnames(train)[i],
              rel.var = rel)
      }
    }
  }
  
  rt.rel <- difftime(Sys.time(), rt.rel, units = "secs")[[1]]
  if(para) stopImplicitCluster()
  ## remove NULL element
  rel.set[sapply(rel.set, is.null)] <- NULL
  
  
  ## save time_rel and rel.set
  SaveAlgInfo( rt.rel, "time_rel", sample.info, alg)
  SaveAlgInfo( rel.set, 
               folder="rel_var", 
               sample.info = sample.info, 
               alg=alg)
  
  return( rel.set )
}


## (1) train prediction model to get dev; (2) save time_pred; (3) save time_dev; (4) save dev
### input: sample.info, train, rel.set, pred.type
### output: dev, observed - expected, the deviation matrix of train data
## NOTE: the save of prediction model is suspended because of the following two reasons
### 1. saving model and loading model take long time for large dataset. 
###    For minst data, training cart model takes around 150s, and loading the saved one is also around 150s.
### 2. An error of out of memory occurs when this function returns the fitted model in parallel scenario.
FitDepadPredModel <- function( sample.info, train, rel.set, pred.type){
  
  # TO BE DELETE ---------
   # return(NULL)
  
  pred.name <- str_split(pred.type, "_", simplify = T)
  pred.type <- pred.name[1]
  if(length(pred.name) > 1) pred.para <- pred.name[2:length(pred.name)]
  
  ## set parallel computing
  para <- F
  if( pred.type %in% c("bcart", "banotree", "subdev", "lassocv", "ridgecv", "elasticcv")){
    ## parallel computation is only for cart
    if( (length(rel.set) > 10)  && (nrow(train) > 5000) ) para <- T
    if( (length(rel.set) > 50) ) para <- T
    # if( (length(rel.set) > 50)  && (nrow(train) > 2000) ) para <- T
    # if( (length(rel.set) > 100) && (nrow(train) > 500 ) ) para <- T
    # para <- T
    
    if(para) registerDoParallel( detectCores() )
    `%myinfix%` <- ifelse(para, `%dorng%`, `%do%`)
  } else {
    `%myinfix%` <- `%do%`
  }
  
  cat("\n start to train prediction model. pred.type=", pred.type," para=", para, "\n")
  
  rt.dev <- Sys.time()
  fit.lst <- foreach( i = 1:length(rel.set) ) %myinfix% {
    ## i = 2
    rel.var <- rel.set[[i]]$rel.var
    
    if(is.null(rel.var)) return()
    
    ##___ knn ----
    if( pred.type == "knn"){
      nn <- FNN::get.knn(scale(train[,rel.var]), k = 20)
      expect <- sapply( 1:nrow(train), 
                        function(x) mean(train[ nn$nn.index[x,], rel.set[[i]]$target ]) )
    }
    
    ##___ anoknn ----
    if( pred.type == "anoknn"){
      nn <- FNN::get.knn(scale(train[,rel.var]), k = 20)
      expect <- sapply( 1:nrow(train), 
                        function(x) median(train[ nn$nn.index[x,], rel.set[[i]]$target ]) )
    }
    
    ##___ subdev ----
    if( pred.type == "subdev"){
      x.train <- train
      nn <- FNN::get.knn(scale(x.train[, c( rel.var, rel.set[[i]]$target) ]), k = k.nn)
      expbase <- sapply( 1:nrow(x.train),
                      function(x) median(x.train[ nn$nn.index[x,], rel.set[[i]]$target] ) )
      da.dep <- foreach(i.dep=1:nrow(x.train), .combine = "c") %do%{
        ## i.dep = 1
        idx.knn <- nn$nn.index[i.dep,]
        tgt.knn <- x.train[idx.knn,rel.set[[i]]$target ]
        return(idx.knn[which(rank(tgt.knn) == round(length(tgt.knn)/2))])
      }
      devbase <- x.train[, rel.set[[i]]$target ]  - expbase

      # approximate to the center
      depth <- 15
      adj <- foreach( i.adj = 1:nrow(x.train)) %do%{
        ## i.adj = 1
        adj.ind <- i.adj
        curr <- i.adj
        for( j.d in 1:depth ){
          ## j.d = 1
          if( ! is.na(da.dep[curr])){
            adj.ind <- c(adj.ind, da.dep[curr])
            curr <- da.dep[curr]
          }

        }
        return(unique(adj.ind))
      }

      if(depth > 1) dev <- sapply( 1:nrow(x.train),
                                   function(x) abs(sum(devbase[adj[[x]]])))
    }
    
    ##___ anotree ----
    if( pred.type == "anotree"){
      x.train <- train
      
      ## if the name of variable include -, the parse produces error
      exp <- parse(text = sprintf("%s~%s", 
                                  rel.set[[i]]$target, 
                                  paste0(rel.var, collapse = "+")))
      cart.method <- "anova"  #available: "anova" "poisson"
      cart.minsplit <- 20
      cart.minbucket <- 7
      cart.xval <- 0 
      cart.maxdepth <- 30
      cart.cp <- 0.003
      
      fit <- rpart( eval(exp),
                    data = x.train, 
                    control = rpart::rpart.control( 
                      minsplit = cart.minsplit,
                      minbucket = cart.minbucket,
                      maxdepth = cart.maxdepth,
                      xval = cart.xval,
                      cp = cart.cp
                    )
      )
      cls <- unique(fit$where)
      cls.median <- sapply(cls, function(x) median( 
        train[ which(fit$where == x), rel.set[[i]]$target], na.rm = T) )
      
      expect <- rep(0,nrow(x.train))
      for( i.exp in 1:length(cls)){
        expect[which(fit$where == cls[i.exp])] <- cls.median[i.exp]
      }
    }
    
    ##___ banotree ----
    if( pred.type == "banotree"){
      n.bagg <- as.numeric(pred.para)
     
      cart.method <- "anova"  #available: "anova" "poisson"
      cart.minsplit <- 20
      cart.minbucket <- 7
      cart.xval <- 0 
      cart.maxdepth <- 30
      cart.cp <- 0.003
      
      bag.y <- foreach( i.bag = 1:n.bagg, .combine = rbind ) %do%{
        ## i.bag = 1
        set.seed(i.bag)
        idx.bag <- sample( 1:nrow(train), nrow(train), replace = T )
        x.train <- train[idx.bag,]
        rownames(x.train) <- 1:nrow(x.train)
        
        ## if the name of variable include -, the parse produces error
        exp <- parse(text = sprintf("%s~%s", 
                                    rel.set[[i]]$target, 
                                    paste0(rel.var, collapse = "+")))
        
        fit <- rpart( eval(exp),
                      data = x.train, 
                      control = rpart::rpart.control( 
                        minsplit = cart.minsplit,
                        minbucket = cart.minbucket,
                        maxdepth = cart.maxdepth,
                        xval = 0,
                        cp = cart.cp
                      )
        )
        cls <- unique(fit$where)
        cls.median <- sapply(cls, function(x) median(
          x.train[ which(fit$where == x), rel.set[[i]]$target], na.rm = T) )
        fit$frame[cls, "yval"] <- cls.median
        
        predict(fit, train)
      }
      set.seed( dget("seed_list.txt")[sample.info$index] )
      expect <- apply(bag.y, 2, mean,  na.rm=T)
    }
    
    
    ##___ cart ----
    if( pred.type == "cart"){
      x.train <- train
      
      ## if the name of variable include -, the parse produces error
      exp <- parse(text = sprintf("%s~%s", 
                                  rel.set[[i]]$target, 
                                  paste0(rel.var, collapse = "+")))
      cart.method <- "anova"  #available: "anova" "poisson"
      cart.minsplit <- 20
      cart.minbucket <- 7
      cart.xval <- 0 
      cart.maxdepth <- 30
      cart.cp <- 0.003
      
      fit <- rpart( eval(exp),
                    data = x.train, 
                    control = rpart::rpart.control( 
                      minsplit = cart.minsplit,
                      minbucket = cart.minbucket,
                      maxdepth = cart.maxdepth,
                      xval = cart.xval,
                      cp = cart.cp
                    )
      )
      expect <- predict(fit, x.train)
    }
    
    
    ##___ bcart ----
    if( pred.type == "bcart"){
      x.train <- train
      
      ## if the name of variable include -, the parse produces error
      exp <- parse(text = sprintf("%s~%s", 
                                  rel.set[[i]]$target, 
                                  paste0(rel.var, collapse = "+")))
      cart.method <- "anova"  #available: "anova" "poisson"
      cart.minsplit <- 20
      cart.minbucket <- 7
      cart.xval <- 0 
      cart.maxdepth <- 30
      cart.cp <- 0.003
      n.bagg <- 25
      
      fit <- ipred::bagging( eval(exp),
                             data = x.train, 
                             coob = TRUE, 
                             nbagg = n.bagg, 
                             control = rpart::rpart.control( 
                               minsplit = cart.minsplit,
                               minbucket = cart.minbucket,
                               maxdepth = cart.maxdepth,
                               xval = 0,
                               cp = cart.cp
                             )
      )
      expect <- predict( fit, x.train)
    }
    
    ##___ linear ----
    if( pred.type == "linear" ){
      x.train <- train
      exp <- parse(text = sprintf("%s~%s", 
                                  rel.set[[i]]$target,
                                  paste0(rel.var, collapse = "+")))
      fit <- lm( eval(exp), data = x.train, method = "qr" )
      expect <- predict( fit, x.train)
    }
    
    ##___ ridge ----
    if( pred.type == "ridge" ){
      ## x should be a matrix with 2 or more columns
      if( length(rel.var) < 2 ) return(NULL)
      
      require(glmnet)
      x.train <- as.matrix( train[, rel.var] )
      fit <- glmnet(  y = train[,rel.set[[i]]$target],
                      x = x.train,
                      family = "gaussian",
                      alpha = 0,  #1:lasso penalty; 0:ridge penalty; 0.5: elastic net
                      nlambda = 10
      )
      expect <- predict( fit, x.train)
      if( ncol(expect)>1 ) expect <- expect[,ncol(expect)]
    }
    
    ##___ ridgecv ----
    if( pred.type == "ridgecv" ){
      
      return(NULL)
      
      ## x should be a matrix with 2 or more columns
      if( length(rel.var) < 2 ) return(NULL)
      
      require(glmnet)
      x.train <- as.matrix( train[, rel.var] )
      
      ## find optimal lambda
      fit.cv <- cv.glmnet(x = x.train, 
                              y = train[,rel.set[[i]]$target], 
                              family = "gaussian",
                              alpha = 0 )
      optimal_lambda <- fit.cv$lambda.min
      
      fit <- glmnet(  y = train[,rel.set[[i]]$target],
                      x = x.train,
                      family = "gaussian",
                      alpha = 0,  #1:lasso penalty; 0:ridge penalty; 0.5: elastic net
                      lambda = optimal_lambda )
      expect <- predict( fit, x.train)
    }
    
    ##___ lasso ----
    if( pred.type == "lasso" ){
      ## x should be a matrix with 2 or more columns
      if( length(rel.var) < 2 ) return(NULL)
      
      require(glmnet)
      x.train <- as.matrix( train[, rel.var] )
      fit <- glmnet(  y = train[,rel.set[[i]]$target],
                      x = x.train,
                      family = "gaussian",
                       alpha = 1,  #1:lasso penalty; 0:ridge penalty; 0.5: elastic net
                       nlambda = 10
      )
      expect <- predict( fit, x.train)
      if( ncol(expect)>1 ) expect <- expect[,ncol(expect)]
    }
    
    ##___ lassocv ----
    if( pred.type == "lassocv" ){
      return(NULL)
      
      ## x should be a matrix with 2 or more columns
      if( length(rel.var) < 2 ) return(NULL)
      
      require(glmnet)
      x.train <- as.matrix( train[, rel.var] )
      
      # lambdas <- 10^seq(2, -3, by = -.1)
      fit.cv <- cv.glmnet(x = x.train, 
                          y = train[,rel.set[[i]]$target], 
                          family = "gaussian",
                          alpha = 1)
                          # nfolds = length(lambdas),
                          # lambda = lambdas)
      
      optimal_lambda <- fit.cv$lambda.min
      fit <- glmnet(  y = train[,rel.set[[i]]$target],
                      x = x.train,
                      family = "gaussian",
                      alpha = 1,  #1:lasso penalty; 0:ridge penalty; 0.5: elastic net
                      lambda = optimal_lambda )
      expect <- predict( fit, x.train)
    }
    
    ##___ elastic net ----
    if( pred.type == "elastic" ){
      ## x should be a matrix with 2 or more columns
      if( length(rel.var) < 2 ) return(NULL)
      
      require(glmnet)
      x.train <- as.matrix( train[, rel.var] )
      fit <- glmnet(  y = train[,rel.set[[i]]$target],
                      x = x.train,
                      family = "gaussian",
                      alpha = 0.5,  #1:lasso penalty; 0:ridge penalty; 0.5: elastic net
                      nlambda = 10
      )
      expect <- predict( fit, x.train)
      if( ncol(expect)>1 ) expect <- expect[,ncol(expect)]
    }
    
    ##___ elasticcv ----
    if( pred.type == "elasticcv" ){
      ## x should be a matrix with 2 or more columns
      if( length(rel.var) < 2 ) return(NULL)
      
      require(glmnet)
      x.train <- as.matrix( train[, rel.var] )
      fit.cv <- cv.glmnet(x = x.train, 
                          y = train[,rel.set[[i]]$target], 
                          family = "gaussian",
                          alpha = 0.5)
      optimal_lambda <- fit.cv$lambda.min
      
      fit <- glmnet(  y = train[,rel.set[[i]]$target],
                      x = x.train,
                      family = "gaussian",
                      alpha = 0.5,  #1:lasso penalty; 0:ridge penalty; 0.5: elastic net
                      lambda = optimal_lambda )
      expect <- predict( fit, x.train)
    }
    
    ## calculate dev
    if( pred.type != "subdev") dev <- train[,rel.set[[i]]$target] - expect

    ## return of each prediction model
    list( target = rel.set[[i]]$target,
          dev = dev )
  }
  rt.dev <- difftime(Sys.time(), rt.dev, units = "secs")[[1]]
  
  if(para) stopImplicitCluster()
  gc()
  
  ## remove null element
  fit.lst[ sapply(fit.lst, is.null) ] <- NULL
  if(length(fit.lst) == 0 ) return(NULL)
  
  ## extract each elements from fit.lst
  target <- unlist(lapply(fit.lst, `[[`, "target"))
  dev <- as.data.frame(lapply(fit.lst, `[[`, "dev"))
  colnames(dev) <- target
  
  SaveAlgInfo( rt.dev, "time_dev", sample.info, alg)
  SaveAlgInfo(dev, "deviation", sample.info,  alg)
  
  return(dev)
}



## overall process of depad methods
## input: sample.info, train, alg
## output: score
## TBD: couldn't save model due to lack of memory
AlgDepad <- function( sample.info, train, alg ){
  
  ## if ther is saved model, load it
  ## TBD: load saved model takes long time, so comment it 
  # dep.fit <- LoadAlgInfo( "model", sample.info, alg ) )
  # if( !is.null(dep.fit) ) return(dep.fit)
  
  ## if there is saved dev, return it directly
  dev <-  LoadAlgInfo ( folder = "deviation", sample.info, alg)
  if( !is.null(dev) ){
    score <- GenDepadScore( dev, sample.info, alg )
    return( score)
  }
  
  ## otherwise, get rel_var
  rel.set <- LoadAlgInfo( "rel_var", sample.info, alg)
  if( is.null(rel.set) ){
    rel.set <- GetRelVar( sample.info = sample.info,
                          train = train,
                          rel.type = GetDepadComponent(alg, "rel") )
  }
 
  ## Fit prediction model
  pred.type = GetDepadComponent(alg, "pred")
  dev <- FitDepadPredModel( sample.info = sample.info, 
                            train = train, 
                            rel.set = rel.set, 
                            pred.type = pred.type)
  if( is.null(dev) ){
    cat("\n Error: Deviation is empty! \n")
    return(NULL)
  } 
  
    
  
  # ## TBD: load saved model takes long time, so comment it
  # # save model
  # SaveAlgInfo( dep.fit,
  #              folder="model",
  #              sample.info = sample.info,
  #              alg=alg)
  
  
  ## get score
  score <- GenDepadScore( dev, sample.info, alg )
  
  ## clear memory
  rm(dev)
  gc()
  
  return(score)
}



## generate score for depad methods from dev, save time_score
## input: dev, sample.info, alg
## output: score
GenDepadScore <- function(dev, sample.info, alg){
  
  require(stringr)
  comb <- GetDepadComponent(alg, "comb")
  comb.info <- GetCombInfo()
  
  if(! comb %in% comb.info$name  ){
    cat(" Error: couldn't find inputed combination method:", comb, "\n")
    cat(" Allowed combination method:", 
        paste0( comb.all$name, collapse = ", ") , "\n")
    return(NULL)
  }
  
  ## get configuration of each component of combination
  regu <- comb.info[ comb.info$name == comb, "regularization" ]
  norm <- comb.info[ comb.info$name == comb, "normalization" ]
  aggr <- comb.info[ comb.info$name == comb, "aggregation" ]
  
  ## start timer
  rt.score <- Sys.time()
  
  ## remove columns with all the same value
  ## if all objects deviate the same degree on one variable
  ## these deviations do not contribute to distinguish anomalies
  idx <- which( apply(dev, 2, function(x) length(unique(x))) != 1)
  dev <- dev[,idx]
  
  ## regularization
  if( regu ) dev <- abs(dev)
  
  ## normalization
  if( norm == "gaussian scaling" ) dev <- apply(dev, 2, GaussianScaling)
  if( norm == "simply zscore") dev <- apply(dev, 2, SimplyZscore)
  if( norm == "zscore") dev <- apply(dev, 2, scale)
  if( norm == "robust zscore") dev <- apply(dev, 2, RobustZscore)
  if( norm == "abs robust zscore") dev <- abs( apply(dev, 2, RobustZscore) )
  if( norm == "robust simply zscore") dev <- apply(dev, 2, RobustSimplyZscore) 
  if( norm == "abs robust simply zscore") dev <- abs( apply(dev, 2, RobustSimplyZscore) )
  if( norm == "zscore 90") dev <- apply(dev, 2, Zscore90)
  if( norm == "zscore median") dev <- apply(dev, 2, ZscoreMedian)
  if( norm == "rsd") dev <- apply(dev, 2, ReverseStandardDeviation)
  if( norm == "adp_sd") dev <- apply(dev, 2, AdpSd)
  if( norm == "adp_rsd") dev <- apply(dev, 2, AdpRsd)
  if( norm == "adp_azm") dev <- apply(dev, 2, AdpAzm)
  
    
  ## aggregation
  aggr <- str_split(aggr, "_", simplify = T)
  aggr.name <- aggr[1]
  if( length(aggr) > 1 ) aggr.para <- aggr[2:length(aggr)]
  
  if( aggr.name == "average") score <- apply(dev, 1, function(x) mean( x, na.rm = T))
  if( aggr.name == "sum") score <- apply( dev, 1, function(x) sum( x, na.rm = T))
  if( aggr.name == "max") score <- apply( dev, 1, max, na.rm=T)
  if( aggr.name == "squared sum") score <- apply( dev, 1, function(x) sum(x^2))
  
  if( aggr.name == "pruned sum"){
    if( aggr.para == "0" ) thd <- 0
    if( aggr.para == "mean" ) thd <- apply(dev, 2, mean)
    if( aggr.para == "median" ) thd <- apply(dev, 2, median)
    score <- PrunedSum(dev, threshold = thd)
  }
  
  if( aggr.name == "aom" ) score <- AverageOverMaximum(dev,aggr.para)
  
  if(aggr.name == "euclidean") {
    dev.center <- apply(dev, 2, median)
    score <-  apply( dev, 1, function(x) {
      q <- x - dev.center
      d <- sqrt( t(q) %*% q )
      d
    } ) 
  }
  
  if(aggr.name == "mahalanobis") {
    dev.center <- apply(dev, 2, median)
    score <-  mahalanobis(dev, 
                          center = dev.center, 
                          cov= cov(dev),
                          tol=1e-40) 
  
  }
  
  if(aggr.name == "manhattan") {
    dev.center <- apply(dev, 2, median)
    print(dev.center)
    score <-  apply( dev, 1, function(x) {
      q <- abs(x - dev.center)
      sum(q)
    } ) 
  }
  
  
  rt.score <- difftime(Sys.time(), rt.score, units = "secs")[[1]]
  SaveAlgInfo( rt.score, "time_score", sample.info, alg)
  return(score)
}


