## ============================================================
## DepAD: Dependency-based Anomaly Detection
##
## Paper: "Dependency-based anomaly detection: A general framework
##         and comprehensive evaluation"
## Journal: Expert Systems with Applications, Vol. 297, 2026, 129249
## DOI: 10.1016/j.eswa.2025.129249
## Authors: Sha Lu, Lin Liu, Kui Yu, Thuc Duy Le, Jixue Liu, Jiuyong Li
## Contact: Sha.Lu@unisa.edu.au
## ============================================================

rm(list = ls())
gc()
gcinfo(FALSE)
cat("\014")
closeAllConnections()

## Environment ==============================
options(stringsAsFactors = FALSE)

## ==============================================================
## USER SETUP: Edit the two lines below before running
## ==============================================================

## 1. Set working directory to the folder containing the R scripts
setwd("/path/to/DepAD")   # <- CHANGE THIS to where you saved the R files

## 2. Set path to the directory containing the benchmark datasets
##    See README.md for download links for each dataset
path.data <- "/path/to/datasets"  # <- CHANGE THIS

## ==============================================================

## Packages ==================
need.pkgs <- c( "foreach","doParallel","doRNG","PRROC","data.table",
                "readr","farff","dbscan","ipred","rpart","stringr",
                "dplyr","caret","bnlearn",
                "glmnet","abodOutlier","HighDimOut",
                "IsolationForest","Rlof", "e1071", "FSinR", "FNN")
n.pkg <- length( need.pkgs[ !need.pkgs %in% installed.packages()[,1]] )
if( n.pkg > 0 ){
  cat("Installing missing packages:",
      need.pkgs[ !need.pkgs %in% installed.packages()[,1]], "\n")
  install.packages( need.pkgs[ !need.pkgs %in% installed.packages()[,1]],
                    repos = "https://cloud.r-project.org")
  ## Note: IsolationForest is hosted on R-Forge
  install.packages("IsolationForest",
                   repos = "http://R-Forge.R-project.org")
}


## Source ==============================
source("functions.R")

## Settings =======================================
## __ datasets  ----
data.info <- GetRealDataInfo()
data.info <- data.info[ order(data.info$n.col), ]
data.ids.low <- data.info$id[ which( data.info$n.col < 150) ]
data.ids.high <- data.info$id[ which( data.info$n.col >= 150) ]
data.ids <- data.info$id

## __ algorithms ----
depad.type <- list(
  type.rel =  c("mi", "iepc", "dc","mb_0.01", "pc_0.01"),
  type.pred = c("bcart", "lasso", "linear", "ridge",
                "lassocv", "ridgecv", "banotree_25"),
  type.com =  c("Sum", "GS", "azm_ps","Max", "Thresh")
)

### Set running algorithms
## Available comparison algorithms:
## c("lof", "wknn", "iforest", "mbom", "abod", "sod",
##   "combn", "also_m5p",
##   "ocsvm_0.01","ocsvm_0.03", "ocsvm_0.05", "ocsvm_0.07","ocsvm_0.09")
alg.comp <- c()
alg.depad <- GenDepadAlgName( depad.type )
alg.all <- c( GenDepadAlgName(list("mb_0.01","bcart","azm_ps")),
              GenDepadAlgName(list("pc_0.01","bcart","azm_ps")),
              GenDepadAlgName(list("mb_0.01","bcart","Thresh")))


## __ experiment settings ----
at.lst <- data.ids          # datasets to evaluate
ratio.ano <- 0.01           # anomaly ratio in sampled data
dft.n.it <- 20              # number of sampling iterations per dataset
k.nn <- 10                  # number of nearest neighbours (for proximity-based methods)

## __ workflow control ----
## Controls which step to (re-)run from.
## Options: "rel" | "pred" | "comb" | "eva" | "auto"
## NOTE: if not "auto", rel/pred will be re-trained for each combination method.
exe.from <- "comb"

## __ test mode ----
## Set test.mode <- TRUE to run a quick single-iteration check
test.mode <- FALSE
if( test.mode ){
  dft.n.it <- 1
  curr.it   <- 1
}

## Functions ==============================
AnomalyDetect <- function( sample.info, train, alg ){
  alg.fullname <- alg

  ## depad
  if( IsDepadMethod(alg.fullname) ) {
    score <- AlgDepad( sample.info, train, alg )
  } else {
    alg.split <- str_split_fixed(alg, "_", 2)
    alg <- alg.split[1]
    para <- alg.split[2]

    rt.test <- Sys.time()

    if ( alg == "lof" )    score <- AlgLof( MyScale(train), k.nn )
    if ( alg == "wknn" )   score <- AlgWknn( MyScale(train), k.nn )
    if ( alg == "iforest") score <- AlgIforest( train )
    if ( alg == "also")    score <- AlgAlso(train, method = para)
    if ( alg == "mbom")    score <- AlgMbom( dat = MyScale(train), sample.info, k.nn)
    if ( alg == "combn")   score <- AlgCombn( train, sample.info )
    if ( alg == "abod")    score <- AlgAbod( MyScale( train ), k.nn)
    if ( alg == "sod")     score <- AlgSod( MyScale( train ), k.nn)
    if ( alg == "ocsvm")   score <- AlgOcsvm( train, sample.info, ga = as.numeric(para))

    if( alg != "mbom"){
      rt.test <- difftime(Sys.time(), rt.test, units = "secs")[[1]]
      SaveAlgInfo( rt.test, "time_test", sample.info, alg)
    }
  }

  if(!is.null(score)) SaveAlgInfo(score, "score", sample.info, alg.fullname)
  return( score)
}


## Main Flow =======================================
rst.at <- foreach (at = at.lst, .inorder = T) %do% {

  sample.info <- GenSampleInfo( id = at,
                                index = NULL,
                                ratio = ratio.ano,
                                dft.n.it = dft.n.it)

  rst.it <- foreach( it = 1:sample.info$n.smp, .inorder = T) %do%{
    if(test.mode) it = curr.it

    cat("\n ============= starting at:", at, "  it:", it, " =====================\n")

    sample.info$index <- it
    set.seed( dget("data/seed_list.txt")[sample.info$index] )

    rst.alg <- foreach (alg = alg.all, .combine = rbind, .inorder = T) %do% {

      cat("\n starting algorithm:", alg, "  exe.from=", exe.from, "\n")

      if( IsDepadMethod(alg) ){
        if( exe.from == "rel" ){
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "evaluation")
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "score")
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "deviation")
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "rel_var")
        }
        if( exe.from == "pred"){
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "evaluation")
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "score")
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "deviation")
        }
        if( exe.from == "comb"){
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "evaluation")
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "score")
        }
        if( exe.from == "eva"){
          ClearSavedInfoByAlg(alg, at, it, ratio.ano, "evaluation")
        }
      }

      label = LoadAlgInfo( "label", sample.info, alg)

      eva <- LoadAlgInfo ( "evaluation", sample.info, alg)
      if( !is.null(eva) ){
        print(eva)
        return( eva )
      }

      score <- LoadAlgInfo( "score", sample.info, alg)
      if( is.null(score)){
        train <- LoadSampledData( sample.info )
        label <- train$class
        SaveAlgInfo(label, "label", sample.info, alg)
        train$class <- NULL
        score <- AnomalyDetect( sample.info, train, alg )
      } else {
        label <- LoadAlgInfo("label", sample.info, alg = "")
        if( is.null(label) ){
          label<- LoadSampledData( sample.info )$class
          SaveAlgInfo(label, "label", sample.info, alg)
        }
      }

      if(is.null(score)) return( data.frame( roc.auc = NA,
                                             pr.auc = NA,
                                             r.precision = NA,
                                             ap = NA) )

      eva <- EvaluateScore(score, label)
      print(eva)
      SaveAlgInfo( eva,"evaluation", sample.info, alg)

      data.frame( roc.auc = eva["roc.auc"],
                  pr.auc = eva["pr.auc"],
                  r.precision = eva["r.precision"],
                  ap = eva["ap"] )
    }

    rownames( rst.alg ) <- alg.all
    closeAllConnections()
    print(rst.alg)
    rst.alg
  }

  m.it <- foreach( i = 1:length(alg.all), .combine = rbind, .inorder = T ) %do%{
    m <- sapply( rst.it, function(x) return(x[i,]))
    m <- rowMeans( apply(m,2, as.double), na.rm = T )
  }
  if( is.vector(m.it) ) m.it <- data.frame( matrix(m.it, nrow=1) )
  dimnames(m.it) <- list( alg.all, colnames(rst.it[[1]]) )

  m.it
}

names(rst.at) <- at.lst
print(rst.at)

## END =============================
