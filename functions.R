

library(foreach)
library(doParallel)
library(doRNG)
library(PRROC)
library(data.table)
library(farff) #used to handl arff file
library(dbscan)  #lof
library(bnlearn)
library(ipred)
library(rpart)
library(stringr)
library(dplyr)

## source files
source("depad.R")
source("gen_data.R")
source("comp_algorithms.R")
source("real_data.R")


options(stringsAsFactors = FALSE)


## General funtions =======================================
MyScale <- function(da){
  s.da <- scale(da)
  s.da[is.na(s.da)] <- 0
  return(s.da)
}


GetRstSummaryPath <- function(computer = "galaxy-ubuntu"){
  require(stringr)
  
  path.paper3 <- str_sub( getwd(), 
                          1, 
                          str_locate(getwd(), "Paper 3/")[1,"end"] )
  
  return( paste0(path.paper3, 
                 "Experiments/summary/", 
                 computer) )
}

## Data Related  =======================================
GenRealDataInfoTable <- function( ids ){
  max.n <- 100
  if( missing(ids)) ids.all <- 1:max.n
  
  ## load data info
  info <- foreach( i=ids.all, .combine = rbind, .inorder = T ) %do%{
    ## i = 43
    dat <- LoadRealData( i, load.data = T )
    if( is.null(dat) ) return(NULL)
    
    ratio.ano <- round( sum(dat$data.org$class %in% dat$class.ano) / 
                          sum(dat$data.org$class %in% c( dat$class.ano, dat$class.nor)),
                        digits = 2 )
    info <- 
      data.frame( id = i,
                  name = dat$name,
                  n.row = nrow(dat$data.org), 
                  n.col = ncol(dat$data.org), 
                  ratio.ano = ratio.ano,
                  class.ano = paste( ShortenLabels(dat$class.ano), collapse = " "),
                  class.nor = paste( ShortenLabels(dat$class.nor), collapse = " ")
      )
  }
  
  tbl.name <- "real_data.csv"
  
  ## if no input ids, then save the whole table
  if( missing(ids) |(! file.exists(tbl.name)) ){
    write.csv( info, tbl.name, row.names = F)
    return(info)
  }
  
  ## with inputed ids, update if exists
  tbl <- read.csv( tbl.name ) 
  upd.id <- tbl$id[tbl$id %in% ids]
  for( i in upd.id ){
    tbl[ which(tbl$id == i), ] <- info[ which(info$id == i), ]
  }
  
  ## with inputed ids, appdend if not exists
  app.id <- ids[ ! ids %in% tbl$id ]
  tbl <- rbind( tbl, info[ info$id %in% app.id ,  ]  )
  
  ## save updated table
  tbl <- tbl[ order(tbl$id),]  # sort as the ascending order of id
  write.csv( tbl, tbl.name, row.names = F)
  
  return(tbl)
}


GetRealDataInfo <- function( ids ){
  if( ! file.exists("real_data.csv") ){
    print("Couldn't find the data table: real_data.csv")
    return()
  }
  
  
  tbl <- read.csv( "real_data.csv") 
  ## no specified ids, return the whole table
  if ( missing(ids) ) return(tbl)
  
  ## only return the specified ids
  return( tbl[ tbl$id %in% ids,  ] )
}


GenSampleInfo <- function( id, index, ratio = 0.01, dft.n.it = 20){
  data.info <- GetRealDataInfo(id)
  sample.info <- as.list(data.info)
  sample.info$ratio <- ratio
  sample.info$n.smp <- ifelse( data.info$ratio.ano <= ratio, 1, dft.n.it )
  sample.info$index <- index
  sample.info[["ratio.ano"]] <- NULL
  
  return(sample.info)
}

GetDataId <- function(name){
  data.info <- GetRealDataInfo() 
  return( data.info[which( data.info$name == name )  ,"id"] )
}

GetDataName <- function(id){
  data.info <- GetRealDataInfo() 
  return( data.info[which( data.info$id == id )  ,"name"] )
}

## algorithm related ------------------------------
LearnBn <- function(train, sample.info){
  train <- as.data.frame(train)
  train <- data.frame(vapply ( train, as.numeric, numeric(nrow(train))))
  
  if( (ncol(train)> 150) ) para <- T else para <- F
  
  if(para){
    cl = makeCluster( detectCores()-1 )
    ## set seed
    clusterSetRNGStream(cl, dget("data/seed_list.txt")[sample.info$index])
    #bn.str <- bnlearn::hc(train, cluster = cl)  #hill climb
    bn.str <- bnlearn::pc.stable(train, cluster = cl)
    bn.str$arcs <- directed.arcs(bn.str) # get rid of undirected arcs
    bn <- bn.fit(bn.str,train, cluster = cl)
    stopCluster(cl)
  } else {
    bn.str <- bnlearn::si.hiton.pc(train)
    bn.str$arcs <- directed.arcs(bn.str) # get rid of undirected arcs
    bn <- bn.fit(bn.str,train)
  }
  
  return(bn)
  
}


EvaluateScore <- function(score, label){
  
  metrics <- c("roc.auc", "pr.auc", "r.precision", "ap")
  
  ## get truth lables
  truth <- rep(0, length(label))
  truth[label == "anomaly"] <- 1
  
  score[which(is.na(score))] <- 0
  
  ## evaluate
  eva <- foreach( m = metrics, .combine = cbind, .inorder = T) %do% {
    ## m = metrics[1]
    switch (m,
            "roc.auc" = { rst <-  roc.curve(scores.class0 = score,
                                            weights.class0 = truth)$auc },
            
            "pr.auc" = { rst <- pr.curve(scores.class0 = score,
                                         weights.class0 = truth)$auc.integral },
            
            "r.precision" = { rst <- PrecisionAtN(score, sum(truth), which(truth == 1) ) },
            
            "ap" = { 
              abr.rank <- rank(-score)[which(truth == 1)];
              rst <- foreach( r=abr.rank, .combine = c) %do% { PrecisionAtN(score, r, which(truth == 1) )};
              rst <-  mean(rst)  },
            
            {NULL}
    )
    data.frame( matrix( rst, nrow = 1, ncol = 1, dimnames = list(NULL, m) ))
  }
  
  return(eva)
}

UpdateEvaluation <- function( id ){
  ## id=1
  ## load score
  score.df <- GetSavedFileName(folders="score", ids = id, full.name = T)
  score.df <- ConvertFullNameToDF(score.files)
  
  ## Evaluation
  for( i in 1:nrow(score.df) ){
    ## i=1
    ## get saved score and label
    score <- LoadAlgInfo( full.name = score.files[i])
    sample.info <- list( id = score.df[i,"id"],
                         name = score.df[i,"name"],
                         ratio = score.df[i,"ratio"],
                         index = score.df[i,"index"]
    )
    label <- LoadAlgInfo( "label", sample.info)
    
    eva <- EvaluateScore( score, label)
    
    ## save new evaluation
    SaveAlgInfo( eva, "evaluation", sample.info, alg = score.df[i,"algorithm"]  )
  }
}

GenDepadAlgName <- function(depad.type){
  depad <- expand.grid( depad.type[[1]], 
                        depad.type[[2]], 
                        depad.type[[3]])
  depad <- (apply( depad, 1, function(x) paste0(x,collapse = "-")))
  return( paste0("dep-",depad) )
}

PrecisionAtN <- function(score, N, abr.idx){
  sum(order(-score)[1:N] %in% abr.idx) / N
}

## Combination Related -----------------------

GetCombInfo <- function(){
  options(stringsAsFactors = FALSE)
  comb.info <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  comb.info <- rbind( comb.info, c("Sum", T, "zscore", "sum") )
  comb.info <- rbind( comb.info, c("Max", T, "zscore", "max") )
  comb.info <- rbind( comb.info, c("GS", T, "gaussian scaling", "average") )
  comb.info <- rbind( comb.info, c("HeDES", T, "simply zscore", "sum") )
  comb.info <- rbind( comb.info, c("Thresh", T, "zscore", "pruned sum_0") )
  comb.info <- rbind( comb.info, c("AOM", T, "zscore", "aom_5_1") )
  comb.info <- rbind( comb.info, c("R.Thresh", F, "abs robust zscore", "pruned sum_mean") )
  comb.info <- rbind( comb.info, c("R.HeDES", F, "abs robust simply zscore", "pruned sum_mean") )
  comb.info <- rbind( comb.info, c("R.AOM", F, "abs robust zscore", "aom_5_100") )
  comb.info <- rbind( comb.info, c("E.Euclidean", F, "robust zscore", "euclidean") )
  comb.info <- rbind( comb.info, c("E.Mahalanobis", F, "robust zscore", "mahalanobis") )
  comb.info <- rbind( comb.info, c("E.Manhattan", F, "robust simply zscore", "manhattan") )
  comb.info <- rbind( comb.info, c("dss", F, "robust zscore", "squared sum") )
  comb.info <- rbind( comb.info, c("groundtrue", T, "zscore 90", "pruned sum_0") )
  comb.info <- rbind( comb.info, c("azm_ps", T, "zscore median", "pruned sum_0") )
  comb.info <- rbind( comb.info, c("azm_sum", T, "zscore median", "sum") )
  comb.info <- rbind( comb.info, c("azm_ss", T, "zscore median", "squared sum") )
  comb.info <- rbind( comb.info, c("rsd_ps", T, "rsd", "pruned sum_0") )
  comb.info <- rbind( comb.info, c("rsd_sum", T, "rsd", "sum") )
  comb.info <- rbind( comb.info, c("rsd_ss", T, "rsd", "squared sum") )
  colnames(comb.info) <- c( "name", "regularization", "normalization", "aggregation")
  
  return(comb.info)
}


ClearSavedCombinationInfo <- function(pattern){
  
  ClearSavedInfoByPattern("score", pattern)
  ClearSavedInfoByPattern("evaluation",pattern)
  ClearSavedInfoByPattern("time_score", pattern)
}



## if you want the so-called 'error function'
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
## and the inverses
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
erfcinv <- function (x) qnorm(x/2, lower = FALSE)/sqrt(2)

PrunedSum <- function(x, threshold=0){
  if(is.vector(x)) x <- data.frame(x)
  
  if( length(threshold) == 1 ){
    x[which( x < threshold, arr.ind = T )] <- 0
  } else {
    if( ncol(x) != length(threshold)) {
      cat("PrunedSum Error: length of threshold (", 
          length(threshold),
          ") does not equal to ncol of dev(",
          ncol(x), ")\n")
      return(NULL)
    }
    x <- apply( rbind(threshold, x), 
                2, 
                function(y) {y[ y <= y[1] ] <- 0; return(y)} )
    x <- x[-1,]
  }
  x[ is.na(x)  ] <- 0
  return(rowSums(x))
}

## AOM
AverageOverMaximum <- function( dev, aggr.para ) {
  q <- as.numeric(aggr.para[1])
  its <- as.numeric(aggr.para[2])
  if( ncol(dev) <= q ) q <- round( ncol(dev) / 2 )
  
  if( (ncol(dev)> 150) && (its > 1) ) para <- T else para <- F
  if(para) registerDoParallel( detectCores()-1 )
  `%myinfix%` <- ifelse(para, `%dorng%`, `%do%`)
  
  it.score <- foreach( xxx=1:its, .combine = rbind )  %myinfix% {
    n.bucket <- floor(ncol(dev) /q)
    bucks <- list()
    selected <- c()
    for( i in 1:n.bucket){
      bucks[[i]] <- sample( (1:ncol(dev))[!(1:ncol(dev)) %in% selected], q, replace = F )
      selected <- c(selected, bucks[[i]])
    }
    if( any( ! (1:ncol(dev)) %in% unlist(bucks))) 
      bucks[[n.bucket+1]] <- (1:ncol(dev))[!(1:ncol(dev)) %in% unlist(bucks)]
    
    bucks.max <- foreach( i = 1:length(bucks), .combine = cbind) %do%{
      one.buck <- bucks[[1]]
      apply( dev[,one.buck], 1, max )
    }
    s.x <- apply(bucks.max, 1, mean)
    s.x[is.na(s.x)] <- 0
    s.x
  }
  if(para) stopImplicitCluster()
  
  if( is.vector(it.score) ) return(it.score)
  
  return( colMeans(it.score) )
}

SimplyZscore <- function(x){
  s.x <- (x-mean(x) )/ mean(abs( x - mean(x) ))
  s.x[is.na(s.x)] <- 0
  return(s.x)
}  

RobustSimplyZscore <- function(x){
  s.x <- (x-median(x) )/ mean(abs( x - median(x) ))
  s.x[is.na(s.x)] <- 0
  return(s.x)
}

GaussianScaling <- function(x){
  cus.base <- mean(x)
  cus.sd <- sd(x)
  s.gs <- pmax(0, erf(  (x - cus.base)/ (cus.sd * sqrt(2)) ))
  s.gs[is.na(s.gs)] <- 0
  return(s.gs)
}

RobustZscore <- function(x){
  center <- median(x)
  sc <- sqrt(sum( ( x - center )^2 )/ (length(x)-1))
  s.x <- (x-center)/sc
  s.x[is.na(s.x)] <- 0
  return( s.x )
}

Zscore90 <- function(x){
  # thd = 2
  # x.pc <- x[(x > (mean(x) - thd*sd(x)))]
  # x.pc <- c(x.pc, x.pc[(x.pc<(mean(x)+thd*sd(x)))])
  #x.pc <- x[x < quantile(x, 0.95)]
  x.pc <- x[label == "normal"]
  x.center <- mean(x.pc, na.rm = T)
  #x.sd <- sd(x.pc, na.rm=T)
  x.sd <- mean( abs(x.pc-x.center))
  s.x <- ( x - x.center ) / x.sd
  s.x[is.na(s.x)] <- 0
  return(s.x)
}


# azm
ZscoreMedian <- function(x){
  x.center <- median(x)
  sds <- abs(x - x.center)
  x.sd <- mean(sds)
  #cat("\n in azm: deviation is ", x.sd)
  
  s.x <- ( x - x.center ) / x.sd
  return(s.x)
}

#rsd
ReverseStandardDeviation <- function(x){
  x.center <- median(x)
  x.sd <- mean(sqrt(abs(x - x.center)))^2
  s.x <- (x - x.center) / x.sd
  cat("\n in rsd: devataion is ", x.sd)
  return(s.x)
}




## Log related ------------------------------

SaveAlgInfo <- function( x, folder=NULL, sample.info=NULL, alg=NULL, full.name=NULL){
  
  if(is.null(x)) return()
  
  if( missing(full.name) ){
    log.path <- GenSaveDir( folder, sample.info )
    log.name <- GetLogFullName( folder, sample.info, alg)
  } else {
    log.name <- full.name
    folder <- GetFolderFromFullName( full.name )
    log.path <- GetPathFromFullName(full.name)
  }
  
  ## if saving runtime, add computer name
  if( str_sub( folder, 1, 4) == "time" ){
    
    print(folder)
    print(x)
    x <- data.frame( computer = Sys.info()["nodename"],
                     runtime = x)
    
    if( file.exists(log.name) ){
      ## check if the computer in existing saved time file
      rt <- readRDS(log.name)
      idx <- which( rt$computer == x$computer )
      if( length(idx)==0 ){
        # no record of this computer, add it
        x <- rbind(rt, x)
      } else {
        ## record exists, update it
        rt[idx,] <- x
        x <- rt
      }
    } 
  }
  
  
  if( !dir.exists(log.path)) dir.create(log.path, recursive = T)
  saveRDS( x, log.name ) 
  
  return()
}

LoadAlgInfo <- function( folder=NULL, sample.info=NULL, alg=NULL, full.name=NULL){
  
  if( missing(full.name) ){
    log <- GetLogFullName( folder, sample.info, alg )  
  } else {
    log <- full.name
  }
  
  if( file.exists(log) ){
    x <- readRDS( log )
    return(x)
  }
  
  return(NULL)
}


GenSaveDir <- function( folder, sample.info = NULL,id = NULL ){
  require( stringr )
  
  path.exp <- file.path(
    str_sub( getwd(), end = str_locate( str_to_lower(getwd()),"paper3")[2]),
    "Experiments" )  
  
  if( ! (folder %in% c( "Experiments", dir(path.exp) ))) {
    return(folder)
  }
  
  ## get main experiments directory
  if(folder == "Experiments") return(path.exp)
  
  path.folder <-  file.path(path.exp, folder) 
  if( missing(sample.info) & missing(id) ) return( path.folder  )
  if( !missing(sample.info)){
    path.log <- file.path( path.folder, 
                           paste0( sample.info$id, "-", sample.info$name) )
  }
  if(!missing(id)){
    path.log <- file.path( path.folder,
                           paste0(id, "-", GetDataName(id)))
  }
  
  return(path.log)
}

GenSavedFileName <- function( folder, sample.info, alg ){
  
  ## section 1: data info
  log.name <- paste0( sample.info$id, 
                      "-", sample.info$name, 
                      "-r_", sample.info$ratio, 
                      "-", sample.info$index )
  
  ## section 2: alg info
  if(missing(alg)){
    alg.info <- GenSavedFileAlgInfo(folder)
  } else{
    alg.info <- GenSavedFileAlgInfo(folder, alg)
  } 
  
  ## section 3: extention
  ext <- str_sub(folder,1,3)
  
  ## get full name
  if( is.null(alg.info) ){
    log.name <- paste0( log.name, ".", ext)
  }else {
    log.name <- paste0( log.name, "--", alg.info, ".", ext)
  }
  
  return(log.name)
}


IsDepadMethod <- function(alg){
  if( str_sub(alg,1,4) == "dep-" ) return(T)
  return(F)
}


GetDepadComponent <- function(alg, component){
  if( component == "algorithm") return( alg)
  
  require(stringr)
  if( ! IsDepadMethod(alg) ){
    if( (alg == "mbom") & (component == "rel")) return("mb_0.01")
    return(NULL)
  } 
  
  ## The following is for depad algorithms to get suffix
  alg.elmt <- str_split_fixed(alg,"-",4)
  if( component == "rel") return( alg.elmt[2] )
  if( component == "pred") return( alg.elmt[3] )
  if( component == "comb") return( alg.elmt[4])
  if( component == "model") return( paste0(alg.elmt[2], "-", alg.elmt[3]) )
  return(NULL)
}

GenSavedFileAlgInfo <- function(folder, alg){
  if( ! folder %in% dir(GenSaveDir("Experiments"),recursive = F) ){
    cat( "Error: the input folder is unknown: folder=", folder, "\n")
    return(NULL)
  }
  
  if( folder %in% c("dataset", "label") ){
    return(NULL)
  }  else{
    if( missing(alg) ){
      cat( "Error: need to input algorithm name!")
      return(NULL)
    } 
  }
  if( folder %in% c( "deviation", "model", "time_pred", "time_dev")) 
    alg.info <- "model"
  
  if( folder %in% c("rel_var", "time_rel") ) alg.info <- "rel"
  
  if( folder %in% c("score", "evaluation", "time_test", "time_score") ) 
    alg.info <- "algorithm"
  
  return( GetDepadComponent(alg, alg.info) )
  
}



GetLogFullName <- function( folder, sample.info, alg = NULL){
  log.path <- GenSaveDir(  folder, sample.info )
  log.name <- GenSavedFileName( folder, sample.info,alg)
  return( file.path( log.path , log.name ) )
}

GetFolderFromFullName <- function( full.name ){
  
  ## change full name starts with /folder name/...
  full.name <- str_sub(full.name, 
                       start = str_locate(full.name, "Experiments")[2] + 1, # "Experiments
                       end = str_length(full.name) )
  
  ## folder is the substring between first and second / 
  sep.loc <- str_locate_all(full.name, "/")
  folder <- str_sub( full.name,
                     start = sep.loc[[1]][1,2] + 1, # the end of fist / + 1
                     end = sep.loc[[1]][2,1] - 1) # the start of second / -1
  
  ## check the validation of folder
  if( ! folder %in% dir( GenSaveDir("Experiments"))  ){
    cat("Error: The folder retrived from full name is wrong, folder=", folder, "\n")
    return(NULL)
  }
  
  return(folder)
}

GetPathFromFullName <- function(full.name){
  end <- str_locate_all(full.name, "/")[[1]]
  end <-  end[nrow(end), "end"] - 1
  return( str_sub(full.name, start = 1, end = end) )
}



## Process Saved Info -----------------------

ShortenAlgName <- function(alg){
  ## alg = "dep-mb_0.01-linear-R.AOM"
  
  require(stringr)
  
  if( !IsDepadMethod(alg) ) return( alg )
  
  ## if dep, combine each component
  comp <- c("rel", "pred", "comb")
  alg <- foreach(i = comp, .combine = c, .inorder = T) %do% {
    str_split_fixed( GetDepadComponent(alg, i), "_", 2 )[1]
  } 
  alg <- paste0(alg, collapse = "-")
  return(alg)
}


GenEvaluationTable <- function(ids){
  require(stringr)
  
  cat("\n retrieving saved info...")
  f.all <- GetSavedFileName( folders ="evaluation", ids = ids, full.name =  T)
  rst <- ConvertFullNameToDF(files = f.all)
  rst$folders <- NULL
  
  ## add column of if the proposed methods
  rst <- cbind(rst, IsDep = sapply(rst$algorithm, IsDepadMethod ) )
  
  
  ## add components info for proposed methods. Other methods set to NA
  prop.idx <- which( rst$IsDep == T)
  comp <- c("rel", "pred", "comb")
  for( c in comp ){
    # c=comp[1]
    cat("\n processing component:",c)
    comp.lst <- sapply(rst[prop.idx, "algorithm"], GetDepadComponent, c)
    comp.lst <- str_split_fixed(comp.lst, "_", 2)
    rst[prop.idx, c] <- comp.lst[,1]
    rst[prop.idx, paste0(c, ".para")] <- comp.lst[,2]
  }
  
  ## add evaluation results
  eva <- foreach( i = 1:nrow(rst), .combine = rbind ) %do% {
    ## i = 1
    if( i %% 100 == 0 ) cat( "\n", i , "out of ", nrow(rst), "..." )
    sample.info <- list(id = rst[i,"id"], 
                        name = rst[i,"name"],
                        index = rst[i,"index"],
                        ratio = rst[i,"ratio"])
    
    s.eva <- LoadAlgInfo( "evaluation", sample.info, rst[i,"algorithm"] )
    if(is.null(s.eva)) s.eva <- rep(NA,4)
    s.eva
  }
  rst <- cbind(rst, eva)
  
  write.csv( x = rst, 
             file = file.path(GenSaveDir("Experiments"), "evaluations.csv"), 
             row.names = F)
  return(rst)
}



GetEvaluation <- function( ids, 
                           algs, 
                           ratio = 0.01, 
                           metrics, 
                           type = "value"
) {
  ## ids = 1
  
  require(foreach)
  eva <- read.csv( file.path( GenSaveDir("Experiments"), "evaluations.csv") )
  if(missing(ids)) ids <- sort(unique( eva$id ))
  if( missing(algs) ) algs <- unique( eva$algorithm )
  
  
  eva <- eva[eva$id %in% ids,]
  eva <- eva[eva$algorithm %in% algs,]
  mc <- colnames(eva)[ (ncol(eva)-3):ncol(eva)]
  
  rst <- foreach( m = mc ) %do% {
    rst.id <- foreach( i = ids, .combine = rbind, .inorder = T) %do% {
      rst.alg <- foreach( a = algs, .combine = cbind) %do% {
        ## i = ids[1]
        ## a = algs[1]
        idx <- which( (eva$id == i) & (eva$algorithm == a) & (eva$ratio == ratio) )
        if(type == "value") ra <- data.frame(matrix(mean(eva[idx, m], na.rm = T), ncol=1))
        if(type == "sd") ra <- data.frame(matrix(sd(eva[idx, m], na.rm = T), ncol=1))
        ra
      }
      # shorten names may be duplicated
      #colnames(rst.alg) <- sapply(algs, ShortenAlgName)
      colnames(rst.alg) <- algs
      rst.alg
    }
    
    # if( nrow(rst.id) > 1 ){
    #   ## add average and sd
    #   rst.id <- rbind( rst.id, 
    #                    average = colMeans(rst.id, na.rm = T),
    #                    sd = apply(rst.id, 2, sd, na.rm = T) )
    #   rownames(rst.id) <- c(ids, "average", "sd")
    # } else rownames(rst.id) <- ids
    rst.id <- cbind(id = ids, rst.id)
    
    rst.id <- round( rst.id, 3)
    rst.id
  }
  names(rst) <- mc
  
  if( missing(metrics) ) return( rst) 
  return( rst[[metrics]])
}




GetSavedFileName <- function( folders, ids, full.name = T ){
  
  if( missing(folders) ) {
    folders <- dir( GenSaveDir( "Experiments"), recursive = F, full.names = F)
  }
  
  registerDoParallel( min(length(folders), (detectCores()-1)) )
  if( missing(ids) ){
    ## no input ids, list all files in folders
    files <- foreach ( f = folders, .combine = c, .export=c("GenSaveDir")) %dopar% {
      dir( GenSaveDir( f ), full.name = full.name, recursive = T)
    }
  }else {
    ## with input ids, list files under ids
    files <- foreach( f = folders, .combine = c,.export=c("GenSaveDir")) %dopar% {
      file.ids <- foreach( i = ids, .combine = c) %do% {
        # i = ids[1]
        cat("\n retrieveing evaluations for data=", i, "--", which(ids==i), "/", length(ids),"--" )
        dir( GenSaveDir(f, id=i),
             recursive = T,
             full.names = full.name)
      }
    }
  }
  stopImplicitCluster()
  
  if( is.null(files) ) return(NULL)
  return(files)
}

ConvertFullNameToDF <- function(files) {
  
  require(caret)
  
  if(length(files) > 5000){
    ## parallel computation
    n.files <- length(files)
    
    starts <- c(1, (1:floor(n.files/5000)) * 5000 + 1)
    flds <- sapply(starts[-length(starts)], function(x) list(c(x:(x+5000-1))))
    flds[[length(flds)+1]] <- c(starts[length(starts)]:n.files )
    
  } else {
    n.cores <- 1
    flds <- list(1:length(files))
  }
  
  # get folders
  registerDoParallel(detectCores()-1)
  files.folder <- foreach( i = 1:length(flds), .combine = c, .inorder = T, 
                           .export=c("GetFolderFromFullName","GenSaveDir")) %dopar% {
                             require(stringr)
                             unlist(lapply(files[flds[[i]]], GetFolderFromFullName)) 
                           }
  stopImplicitCluster()
  
  # remove ext
  files <- str_sub( files, start=1, end = str_length(files)-4 )
  
  ## get filename without ext
  f.name <- str_split(files,"/",simplify = T)
  f.name <- f.name[,ncol(f.name)]  ## a string vector
  
  ## get each sector in the name: id,name,ratio,index,alg
  # first split data info section and alg info section
  f.name <- str_split(f.name, "--", simplify = T)
  if(ncol(f.name)>1) alg.names <- f.name[,2] else alg.names <- NULL
  
  ## second, split data info section
  f.name <- f.name[,1]
  f.name <- str_split(f.name, "-", simplify = T)
  
  # combine all the components into data.frame
  if(is.null(alg.names)){
    f.name <- data.frame( f.name, files.folder )
    colnames(f.name) <- c("id","name","ratio","index","folders")
  }else {
    f.name <- data.frame( f.name, alg.names, files.folder )
    colnames(f.name) <- c("id","name","ratio","index","algorithm", "folders")
  } 
  
  f.name$ratio <- as.numeric( str_split(f.name$ratio, "_", simplify = T)[,2] )
  f.name$id <- as.numeric( f.name$id )
  f.name$index <- as.numeric( f.name$index)
  
  return(f.name)
}


ClearSavedInfoByAlg <- function( alg, id, index, ratio, folder ){
  sample.info <- GenSampleInfo( id = id, index = index, ratio = ratio)
  full.name <- GetLogFullName( folder, sample.info, alg )
  if( file.exists( full.name) ) unlink(full.name)
  cat("file deleted: ", full.name, "\n")
}


ClearSavedInfoByPattern <- function(folder, pattern){
  files <- dir(GenSaveDir(folder), full.names = T, recursive = T, include.dirs = F)
  idx <- which(str_detect(files, pattern ))
  for( i in idx) unlink( files[i] )
  cat("delete ", length(idx), "files. \n")
}



ChangeSavedFileName <- function( old, new, folder="Experiments" ){
  require(stringr)
  require(foreach)
  
  ## modify folder and subfolder's name if it include old
  dirs <- list.dirs( GenSaveDir(folder), recursive = T, full.names = T)
  idx <- which(str_detect( dirs, old ))
  if(length(idx) > 0 ){
    rst <- foreach( i = idx, .combine = c ) %do% {
      file.rename( dirs[i], str_replace_all(dirs[i], old, new  ) )
    }
  } else rst <- 0
  cat("Renamed ", sum(rst), "folders. \n")
  
  
  files <- dir( GenSaveDir(folder), 
                recursive = T,
                full.names = T,
                include.dirs = F )
  ## modify files name
  idx <- which(str_detect( files, old ))
  if( length(idx) > 0 ) {
    rst <- foreach( i = idx, .combine = c ) %do% {
      file.rename( files[i],  str_replace_all(files[i], old, new  ) )
    }
  }else  rst <- 0
  cat("Renamed ", sum(rst), "files. \n")
  closeAllConnections()
}


TransSaveInfoToTxt <- function(folder){
  require(stringr)
  if( !folder %in% 
      list.dirs(GenSaveDir("Experiments"), full.names = F, recursive = F)){
    cat("\nWrong input folder:", folder)
    cat("\nChoices:", 
        list.dirs(GenSaveDir("Experiments"), full.names = F, recursive = F), 
        "\n")
    return()
  }
  
  ## all files under the folder
  files <- list.files( GenSaveDir(folder), full.names = T, recursive = T )
  
  ## read and write to different format
  for( i in 1:length(files)){
    #i = 1
    f = files[i]
    
    if( i %% 100 == 0) cat("\n", i, "/", length(files), "...")
    
    ## if it is already a txt file, skip
    if( str_sub(f, str_length(f)-2, str_length(f)) %in% c("txt","csv","nam") ) next
    
    ## read saved file
    info <- readRDS(f)
    
    ## for datasets, save label and data seperately
    if( folder == "dataset" ){
      
      ## save label
      label <- info$class
      save.name <- str_replace(f,"dataset","label")
      save.name <- str_replace(save.name,".dat$",".lab.txt")
      locslash <- str_locate_all(save.name, "/")[[1]]
      save.path <- str_sub( save.name, 
                            1,
                            locslash[nrow(locslash),"end"])
      if( !dir.exists(save.path)) dir.create(save.path)
      write.table(label, 
                  file = save.name, 
                  row.names = F,
                  col.names = F)
      
      ## the saved data (txt) doesn't include label for using in CFS
      info$class <- NULL
      
      ## save colnames
      write.table(colnames(info), 
                  file = str_replace(f,".dat$",".nam"), 
                  row.names = F,
                  col.names = F)
      
      write.table(info, file = paste0(f,".txt"), row.names = F, col.names = F)
    } else {
      write.table(info, file = paste0(f,".txt"), row.names = F)
    }
  }
  cat("\n done! \n")
}



##__Relevant Variables ----
GenRelVarCountsTable <- function(computer = "galaxy-ubuntu"){
  
  cat("\n retrieving saved info...")
  fname <- GetSavedFileName( folders ="rel_var", full.name =  T)
  
  cat("\n converting format...")
  rel.df <- ConvertFullNameToDF( files =  fname)
  rel.df$folders <- NULL
  
  ## get counts
  ct <- foreach( i = 1:length(fname), .combine = rbind ) %do% {
    ## i = 1
    if( i %% 100 == 0) cat("\n",i,"/",length(fname),"..." )
    cat("\n")
    rel <- LoadAlgInfo(full.name = fname[i])
    ct.one <- sapply( rel, function(x) length(x$rel.var) )
    ct.one <- round( sum(ct.one)/length(ct.one), 1)
    
    ct.one
  }
  
  rel.df <- cbind(rel.df, counts = ct)
  write.csv( x = rel.df, 
             file = file.path(GetRstSummaryPath(computer),"rel_var_counts.csv"), 
             row.names = F)
  return(rel.df)
}

##__Running Time ----
GenRtTable <- function(computer = "galaxy-ubuntu"){
  require(foreach)
  
  cat("\n retrieving saved info...")
  time.folders <- c("time_rel", "time_pred", "time_dev", "time_score", "time_test")
  fname <- GetSavedFileName( folders = time.folders, full.name =  T)
  fname <- str_subset(fname, ".tim$")
  
  cat("\n converting format...")
  rt.df <- ConvertFullNameToDF( files = fname )
  
  ## get running times
  cat("\n saving...")
  registerDoParallel( detectCores() )
  rt.all <- foreach( i = 1:length(fname), .combine = rbind ) %dopar% {
    ## i = 1
    rt.one <- LoadAlgInfo(full.name = fname[i])
    if( ! is.data.frame(rt.one)) return()
    
    rt.one <- cbind( rt.df[i,], rt.one )
    rt.one
  }
  stopImplicitCluster()
  
  write.csv( x = rt.all, 
             file = file.path(GetRstSummaryPath(computer), "rt.csv"), 
             row.names = F)
  return(rt.all)
}

ShowRuntime <- function( id,  
                         alg = NULL, 
                         index=1, 
                         type = "time_train",  
                         ratio=0.01, 
                         computer = "galaxy-ubuntu" ){
  # id = 1
  # alg = alg.all[1]
  
  if( missing(index) ) index <- 1:20
  if( missing(computer) ) computer  <-  "galaxy-ubuntu"
  if( missing(alg)) alg <- GenDepadAlgName(list("mb_0.01","cart","E.Manhattan"))
  
  if( IsDepadMethod(alg) ){
    folder <- c( "time_rel", "time_pred", "time_dev", "time_score")
  } else folder <- "time_test"
  
  
  rt.lst <- foreach( i = index, .combine = rbind) %do% {
    ## i = 1
    sample.info <- GenSampleInfo(id = id,index = i, ratio = ratio)
    rt.fld <- foreach( f = folder, .combine = cbind ) %do% {
      rt <- file.path( GenSaveDir( f , sample.info),
                       GenSavedFileName( f, sample.info, alg ))
      if( file.exists(rt) ){
        rt <-  LoadAlgInfo( full.name=rt )
        ## filter the computer
        rt <- rt[ rt$computer == computer, "runtime" ]
      }  else{
        rt <- NA
      } 
    }
    data.frame( matrix( rt.fld, nrow=1))
  }
  
  rt.lst
  ## remove all NA row
  rt.lst <- rt.lst[ apply( rt.lst, 
                           1, 
                           function(x) ifelse( sum(is.na(x))==ncol(rt.lst), F, T)), ]
  
  if( is.vector(rt.lst) ) rt.lst <- data.frame(rt.lst)
  ## add computer name and rt.train and rt.test for depad algs
  if(nrow(rt.lst) > 1) {
    rt.lst <- rbind(rt.lst, colMeans(rt.lst))
    rownames(rt.lst) <- c( 1: (nrow(rt.lst)-1), "mean")
  } 
  rt.lst <- round(rt.lst, digits = 2)
  
  
  colnames(rt.lst) <- folder
  if( IsDepadMethod(alg)){
    rt.lst <- cbind( computer = rep( computer, nrow(rt.lst)),
                     time_train = rt.lst$time_rel + rt.lst$time_pred,
                     time_test = rt.lst$time_dev + rt.lst$time_score,
                     rt.lst)
  } else{
    rt.lst <- cbind( computer = rep( computer, nrow(rt.lst)),
                     rt.lst)
  }
  
  if( !missing(type) ) rt.lst <- rt.lst[,type]
  
  return(rt.lst)
}


ChangeComputerNameInTime <- function(old, new){
  flds <- c( "time_rel", "time_pred", "time_dev", "time_score", "time_test")
  registerDoParallel( detectCores() )
  foreach( fld = flds ) %dopar% {
    # fld = flds[1]
    files <- list.files( path = GenSaveDir(fld), full.names = T, recursive = T  )
    for( f in files ){
      # f = files[1]
      rt <- LoadAlgInfo( full.name = f)
      rt$computer <- str_replace( tolower(rt$computer), old, new)
      saveRDS( rt, f )
    }
  }
  stopImplicitCluster()
}
