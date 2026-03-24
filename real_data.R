require(dplyr)

## path.data is set in main.R — please configure it there before sourcing this file.
## If running this file independently, set path.data to your datasets directory:
##   path.data <- "/path/to/your/datasets"
if (!exists("path.data")) {
  stop("'path.data' is not set. Please set path.data in main.R (or before sourcing real_data.R).")
}

# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function(path) {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  mnist.train <<- load_image_file(file.path(path, 'train-images-idx3-ubyte'))
  mnist.test <<- load_image_file(file.path(path, 't10k-images-idx3-ubyte'))
  
  mnist.train$y <<- load_label_file(file.path(path,'train-labels-idx1-ubyte'))
  mnist.test$y <<- load_label_file(file.path(path, 't10k-labels-idx1-ubyte'))
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}


# convert the categorical variable into dummy variables

GenDummyDF <- function(da){
  
  da <- as.data.frame(da)
  
  fc <- which(vapply( da, is.character, logical(1) ))
  if( length(fc) == 0 ) return(da)
  da <- mutate_if(da, is.character, as.factor)
  
  da.dm <- foreach( i = iter(fc), .combine = cbind) %do%{
    if( length(levels(da[,i])) == 1 ){
      dm <- as.double(da[,i])
    } else {
      dm <- as.data.frame( model.matrix(~da[,i]+0))
      colnames(dm) <- paste0(colnames(da)[i],".", unique(da[,i]) )
    }
    
    dm
  }

  cbind(da[,-fc], da.dm)
}

LoadRealData <- function( data.id, load.data = T ){
  
  data.name <- NULL #if could find dataid, return  NULL
  class.ano <- NULL
  class.nor <- NULL
  
  
  ## 1 Waveform--------------
  if (data.id == 1) {
    data.name <- "waveform"
    if( load.data) {
      ods <- read.table(file.path(path.data,  "Waveform/waveform.data"),
                        header=FALSE, sep=",", fill=TRUE)
      colnames(ods)[ncol(ods)] <- "class"
    }
    
    
    # class:0 - 1657    1 - 1647    2 - 1696
    class.nor <- 2
    class.ano <- 0
  }
  
  ## 2 WBC--------------------
  if (data.id== 2) {
    data.name <- "WBC"
    if( load.data) {
      require(farff) #used to handl arff file
      ods <- readARFF( file.path(path.data,  "from evaluation paper/literature/WBC/WBC_v01.arff"))
      ods$id <- NULL
      colnames(ods)[colnames(ods)=="outlier"] <- "class"
    }
    class.ano <- "yes"
    class.nor <- "no"  
  }
  
  
  ## 3 breast cancer (breast cancer) #row:569, #col:30----------
  if (data.id== 3) {  
    data.name <- "breast cancer" 
    if( load.data) {
      ods <-read.table( file.path(path.data,  "Breast-Cancer-Wisconsin/wdbc.data"), 
                        head=FALSE, sep=",", fill=TRUE)
      
      ods[,1] <- NULL #id
      colnames(ods)[1] <- "class" # Diagnosis (M = malignant, B = benign)
    }
    
    class.ano <- "M"
    class.nor <- "B"  
  }
  
  
  ## 4 lette ------------------
  if (data.id== 4) {
    data.name <- "Letter"  
    if( load.data) {
      ods <- read.table( file.path(path.data,  "Letter/letter-recognition.data"), 
                        head=FALSE, sep=",", fill=TRUE)
      colnames(ods)[1] <- "class"
    }
    
    class.nor <- "A" 
    class.ano <- "M" 
  }
  
  ## 5 Gamma --------------------
  if (data.id== 5) {  
    data.name <- "Gamma"
    if( load.data) {
      ods <- read.table(file.path(path.data,  "Gamma-Tele/magic04.data"),
                        header=FALSE, sep=",", fill=TRUE)   
      colnames(ods)[11] <- "class"
    }

    #g     h 
    #12332  6688
    class.nor <- "g"
    class.ano <- "h"  
  }
  
  ## 6 wine --------------------
  if (data.id== 6) {  
    data.name <- "wine"  
    if( load.data) {
      ods <- read.table(file.path(path.data,  "Wine-Quanlity/winequality-white.csv"), header=TRUE, sep=";", fill=TRUE)  
      colnames(ods)[colnames(ods)=="quality"] <- "class"
      #table(ods$class)  #3:20  4:163 5:1457  6:2198  7:880  8:175  9:5
    }
    class.nor <- c(5,6,8,4,7)
    class.ano <- c(3,9)
  }
  
  
  ## 7  pageBlocks --------------------
  if (data.id== 7) {  
    data.name <- "pageBlocks"  
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "pageblocks/page-blocks.data"), header=F, fill=T)  
      colnames(ods)[ncol(ods)] <- "class"
    }
    
    #   1    2    3    4    5 
    #4913  329   28   88  115 
    class.ano <- 3
    class.nor <- 1
  }
  
  
  ## 8 stamps --------------------
  if (data.id == 8) {  
    data.name <- "Stamps"
    if( load.data) {
      require(farff) #used to handl arff file
      ods <- readARFF(file.path(path.data,
                              "from evaluation paper/semantic/Stamps/Stamps_withoutdupl_02_v01.arff"))
      ods$id <- NULL
      colnames(ods)[colnames(ods)=="outlier"] <- "class"
    }
    class.ano <- "yes"
    class.nor <- "no"  
  }
  
  
  ## 9 wilt --------------------
  if (data.id == 9) {  
    data.name <- "wilt"  
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "wilt/training.csv"), header=T, fill=T, sep=",")
      ods <- rbind( ods, read.table(
        file.path(path.data, "wilt/testing.csv"), header=T, fill=T, sep=","))
    }
    
    class.ano <- "w"
    class.nor <- "n"
  }
  
  ## 10 pima --------------------
  if (data.id == 10) {  
    data.name <- "pima"
    if( load.data) {
      library(farff) #used to handl arff file
      ods <- readARFF(file.path(path.data,"from evaluation paper/semantic/Pima/Pima_withoutdupl_35.arff"))
      ods$id <- NULL
      colnames(ods)[colnames(ods)=="outlier"] <- "class"
    }
    class.ano <- "yes"
    class.nor <- "no"  
  }
  
  ## 11 glass --------------------
  if (data.id == 11) {  
    data.name <- "glass"
    if( load.data) {
      ods <-read.table(file.path(path.data,  "GlassIdentification/glass.data"),
                       head=FALSE, sep=",", fill=TRUE)
      ods$V1 <- NULL #id
      colnames(ods)[colnames(ods)=="V11"] <- "class"
    }
    
    #class:   1  2  3  5  6  7 
    #samples:70 76 17 13  9 29 
    class.ano <- 6
    class.nor <- c(1:5,7)
  }
  
  ## 12 HeartDisease --------------------
  if (data.id == 12) {  
    data.name <- "HeartDisease"  
    if( load.data) {
      ods <-read.table( file.path(path.data,  "HeartDisease/processed.cleveland.data"),
                       head=FALSE, sep=",", fill=TRUE)
      ## label column V14: normal:0, outlier class:1-4
      dim(ods)
      colnames(ods)[colnames(ods)=="V14"] <- "class"  
      ods$V12 <- as.numeric(ods$V12)
      ods$V13 <- as.numeric(ods$V13)
    }
    class.nor <- c(0,1,3)
    class.ano <- c(2,4)
  }
  
  ## 13 ionosphere --------------------
  if (data.id == 13) {  
    data.name <- "ionosphere"  
    if( load.data) {
      ods <- readARFF( file.path(path.data, "from evaluation paper/literature/Ionosphere/Ionosphere_withoutdupl_norm.arff"))
      ods$id <- NULL
      colnames(ods)[colnames(ods)=="outlier"] <- "class"  
    }
    class.ano <- "yes"
    class.nor <- "no"  
  }
  
  
  ## 14 PenDigits --------------------
  if (data.id == 14) {  
    data.name <- "PenDigits"    
    if( load.data) {
      ods <- read.table(file.path(path.data,"PenDigits/pendigits.tra"), 
                        head=FALSE, sep=",", fill=TRUE)
      ods <- rbind(ods,read.table(file.path(path.data,"PenDigits/pendigits.tes"), head=FALSE, sep=",", fill=TRUE))
      colnames(ods)[17] <- "class"
    }
    class.ano <- 0
    class.nor <- 1:9
  }
  
  ## 15 Cardiotocography  --------------------
  #row:569, #col:30 
  if (data.id == 15) {  
    data.name <- "Cardiotocography" 
    if( load.data) {
      ods <- read.table(file.path(path.data,"Cardiotocography/CTG.csv"), 
                        head=T, sep=",", fill=TRUE)
      colnames(ods)[ncol(ods)-1] <- "nsp.class"
      colnames(ods)[ncol(ods)] <- "class"
      ods <- ods[complete.cases(ods),]
    }
    
    class.ano <- 3
    class.nor <- 1
  }
  
  ## 16 leaf  --------------------
  #row:340 #col:15
  if (data.id == 16){
    data.name <- "leaf"
    if( load.data) {
      ods <-read.table( file.path(path.data,  "leaf/leaf.csv"),
                       head=FALSE, sep=",", fill=TRUE)
      colnames(ods)[1] <- "class"
    }
    
    class.ano <- 36
    class.nor <- 1:35
  }
  
  ## 17 biodegradation #row:1055 #col:41 --------------------
  if (data.id == 17){
    data.name <- "biodegradation"
    if( load.data) {
      ods <-read.table( file.path(path.data,  "biodegradation/biodeg.csv"),
                       head=FALSE, sep=";", fill=TRUE)
      colnames(ods)[42] <- "class"  # 42 experimental class: ready biodegradable (RB) and not ready biodegradable (NRB)
    }
    class.ano <- "NRB"
    class.nor <- "RB"
  }
  
  ## 18 parkinson --------------------
  if (data.id == 18) {
    data.name <- "parkinson"
    if( load.data) {
      ods <-read.table(file.path(path.data,  "Parkinson/parkinsons.data"), head=TRUE, sep=",", fill=TRUE)
      ods$name <- NULL
      colnames(ods)[colnames(ods)=="status"] <- "class"
    }
    class.nor <- 1
    class.ano <- 0
  }
  
  ## 19 spambase  --------------------
  #row:4601 #col:57
  if (data.id == 19){
    data.name <- "spambase"
    if( load.data) {
      ods <-read.table(file.path(path.data,  "spambase/spambase.data"),
                       head=FALSE, sep=",", fill=TRUE)
      
      colnames(ods)[58] <- "class"  # 1: spam, 0: non-spam classes
    }
    class.ano <- 1
    class.nor <- 0
  }
  
  ## 20 libras movement --------------------
  #row:360 #col:90
  if (data.id == 20){
    data.name <- "libras movement"
    if( load.data) {
      ods <-read.table(file.path(path.data,  "libras movement/movement_libras.data"),
                       head=FALSE, sep=",", fill=TRUE)
      
      colnames(ods)[91] <- "class"  # 1:15: 6.66% for each of 15 classes.
    }
    class.ano <- 1
    class.nor <- 2:15
  }
  
  # ## 21 madelon  --------------------
  # #row:2000 #col:500
  # if (data.id == 21){
  #   data.name <- "madelon"
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data,  "madelon/madelon_train.data"),
  #                      head=FALSE, sep=" ", fill=TRUE)
  #     lab <- read.table(file.path(path.data, "madelon/madelon_train.labels"),
  #                       head=FALSE, sep=" ", fill=TRUE)
  #     colnames(ods)[501] <- "class"
  #     
  #     ods[lab == -1, "class"] <- 0
  #     ods[lab == 1, "class"] <- 1
  #   }
  #   class.ano <- 0
  #   class.nor <- 1
  # }
  # 
  # ## 22 isolet --------------------
  # #row:1560 #col:617
  # if (data.id == 22){
  #   data.name <- "isolet"
  #   class.ano <- 1
  #   class.nor <-2:10
  #   
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data, "isolet/isolet5.data"),
  #                      head=FALSE, sep=",", fill=TRUE)
  #     colnames(ods)[618] <- "class"
  #     ods <- rbind( ods[ods$class %in% class.nor, ],
  #                   ods[sample( which(ods$class == class.ano), 10), ] )
  #   }
  #   # 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
  #   #60 60 60 60 60 60 60 60 60 60 60 60 59 60 60 60 60 60 60 60 60 60 60 60 60 60 
  # }
  
  ## 23 spectf --------------------
  #row:267 #col:44 
  # if (data.id == 23){
  #   data.name <- "spectf"
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data,  "spectf/SPECTF.test"),
  #                      head=FALSE, sep=",", fill=TRUE)
  #     ods <- rbind(ods,read.table(file.path(path.data, "spectf/SPECTF.train"),
  #                                 head=FALSE, sep=",", fill=TRUE))
  #     colnames(ods)[1] <- "class"  # OVERALL_DIAGNOSIS: 0,1 (class attribute, binary)
  #   }
  #   class.ano <- 0
  #   class.nor <- 1
  # }
  
  ## 24 arrhythmia --------------------
  if (data.id == 24){
    data.name <- "arrhythmia" 
    if( load.data) {
      ods <-read.table(file.path(path.data,  "arrhythmia/arrhythmia.data"),
                       head=FALSE, sep=",", fill=TRUE)
      # there are four variables should be numberic but is stored in charactor
      fc <- which(vapply( ods, is.character, logical(1) ))
      ods[, fc] <- NULL
      label <- ods[,ncol(ods)]
      #ods <- cbind( GenDummyDF( ods[,1:(ncol(ods)-1)]), label)
      colnames(ods)[ncol(ods)] <- "class"
    }
    
    # #   1   2   3   4   5   6   7   8   9  10  14  15  16 
    # # 245  44  15  15  13  25   3   2   9  50   4   5  22 
    class.ano <- 14
    class.nor <- c(1,2,10)
  }
  
  ## 25 wbpc (breast cancer) --------------------
  #row:569, #col:30 
  if (data.id == 25) {  
    data.name <- "wbpc" 
    if( load.data) {
      ods <-read.table( file.path(path.data,  "Breast-Cancer-Wisconsin/wpbc.data"), 
                        head=FALSE, sep=",", fill=TRUE)
      ods$V1 <- NULL #id
      ods$V3 <- NULL #id
      ods$V34 <- NULL # categorical
      ods$V35 <- NULL # categorical
      # class label V2: N(nonrecur):151 R(recur):47
      colnames(ods)[1] <- "class"
      ods <- ods[complete.cases(ods),]
    }
    class.ano <- "R"
    class.nor <- "N"  
  }
  
  ## 26 mnist --------------------
  #train.row 60000  #test.row:10000 #col 784
  if (data.id == 26){
    data.name <- "mnist" 
    class.ano <- 0
    class.nor <- 7
    
    if( load.data) {
      load_mnist( file.path(path.data, "MNIST/original version/") )
      ods <- data.frame(mnist.test$x[mnist.test$y %in% c(class.ano,class.nor),],
                        class=mnist.test$y[mnist.test$y %in% c(class.ano,class.nor)])
      ods <- unique(ods)
    }
  }
  
  # ## 27 waveform mc --------------------
  # if (data.id == 27) {
  #   data.name <- "waveform mc"
  #   if( load.data) {
  #     ods <- read.table(file.path(path.data,  "Waveform/waveform.data"),
  #                       header=FALSE, sep=",", fill=TRUE)
  #     colnames(ods)[ncol(ods)] <- "class"
  #   }
  #   # class:0 - 1657    1 - 1647    2 - 1696
  #   class.nor <- 1:2
  #   class.ano <- 0
  # }
  # 
  # 
  # ## 28 letter mc --------------------
  # if (data.id == 28) {
  #   data.name <- "letter mc"  
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data, "Letter/letter-recognition.data"), 
  #                       head=FALSE, sep=",", fill=TRUE)
  #     
  #     colnames(ods)[1] <- "class"
  #   }
  #   class.ano <- "A"  
  #   class.nor <- LETTERS[2:8]
  # }
  # 
  # ## 29 wine mc --------------------
  # if (data.id == 29) {  
  #   data.name <- "wine mc"  
  #   if( load.data) {
  #     ods <- read.table(file.path(path.data,  "Wine-Quanlity/winequality-white.csv"), header=TRUE, sep=";", fill=TRUE)  
  #     colnames(ods)[colnames(ods)=="quality"] <- "class"
  #   }
  #   #table(ods$class)  #3:20  4:163 5:1457  6:2198  7:880  8:175  9:5
  #   class.nor <- c(5,6,7)
  #   class.ano <- c(3,8,9)
  # }
  # 
  # 
  # ## 30 PageBlocks mc --------------------
  # if (data.id == 30) {  
  #   data.name <- "PageBlocks mc"  
  #   if( load.data) {
  #     ods <- read.table(
  #       file.path(path.data, "pageblocks/page-blocks.data"), header=F, fill=T)  
  #     colnames(ods)[ncol(ods)] <- "class"
  #   }
  #   # table(ods$class)
  #   #   1    2    3    4    5 
  #   #4913  329   28   88  115 
  #   
  #   class.nor <- c(1,2,5)
  #   class.ano <- c(3,4)
  # }
  # 
  # ## 31 mnist mc  --------------------
  # if (data.id == 31){
  #   data.name <- "mnist mc" 
  #   class.ano <- 0
  #   class.nor <- 4:7
  #   
  #   if( load.data) {
  #     load_mnist(file.path(path.data, "MNIST/original version/"))
  #     table(mnist.test$y        )
  #     ods <- data.frame(mnist.test$x[mnist.test$y %in% c(class.ano,class.nor),],
  #                       class=mnist.test$y[mnist.test$y %in% c(class.ano,class.nor)])
  #     
  #     ods <- unique(ods)
  #   }
  # }
  
  ## 32 ads --------------------
  if (data.id == 32){
    data.name <- "ads" 
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "Internet Ads/ad.data"), header=F, sep=",", fill=T)  
      dim(ods)
      for( i in 1:5){
        ods[,i] <- as.double(ods[,i])
      }
      ods <- ods[complete.cases(ods),]
      colnames(ods)[ncol(ods)] <- "class"
      # table(ods$class)
      # ad. nonad. 
      # 459   2820 
    }
    
    class.nor <- "nonad."
    class.ano <- "ad."
    
    # for( i in 1:ncol(ods)) print(length(unique(ods[,i])))
  }
  
  ## 33 secom --------------------
  if (data.id == 33){
    data.name <- "secom" 
    class.nor <- -1
    class.ano <- 1
    
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "SECOM/secom.data"), header=F, sep="", fill=T)  
      dim(ods)
      
      label.ods <- read.table(
        file.path(path.data, "SECOM/secom_labels.data"), header=F, sep="", fill=T)
      label.ods <- as.vector( label.ods[,1])
      length(label.ods)
      
      ods <- data.frame(ods, class=label.ods)
      colnames(ods)
      
      table(ods$class)
      # -1    1 
      # 1463  104 
      
      for( i in 1:ncol(ods)){
        ods[is.na(ods[,i]),i] <- rep( mean(ods[!is.na(ods[,i]),i]), sum(is.na(ods[,i])))
      }
    }
  }
  
  ## 34 calTech16 --------------------
  if (data.id == 34){
    data.name <- "CalTech16"
    class.nor <- 1
    class.ano <- 53
    
    if( load.data) {
      library(R.matlab)
      org <- readMat( file.path(path.data, "CalTech/caltech101_silhouettes_16.mat") )
      ods <- as.data.frame(org$X)
      dim(ods)
      label <- as.vector(org$Y)
      
      ods <- cbind( ods[label %in% c(class.nor, class.ano),],
                    label[label %in% c(class.nor, class.ano)] )
      colnames(ods)[ncol(ods)] <- "class"
      dim(ods)
      summary(ods)
      table(ods$class)
      for( i in 1:ncol(ods)){
        ods[is.na(ods[,i]),i] <- rep( mean(ods[!is.na(ods[,i]),i]), sum(is.na(ods[,i])))
      }
    }
  }
  
  ## 35 aid362 --------------------
  if (data.id == 35){
    data.name <- "aid362"
    class.nor <- "Inactive"
    class.ano <- "Active"
    
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "AID/VirtualScreeningData/AID362red_train.csv"), 
        header = T, sep=",", fill=T)
      ods <- rbind( ods,  read.table(
        file.path(path.data, "AID/VirtualScreeningData/AID362red_test.csv"),
        header = T, sep=",", fill=T)  )
      
      #ods <- ods[,-c(113:142,144)]
      colnames(ods)[ncol(ods)] <- "class"
      # table(ods$class)
      # Active Inactive 
      # 60     4219
      for( i in 1:(ncol(ods)-1)){
        ods[is.na(ods[,i]),i] <- rep( mean(ods[!is.na(ods[,i]),i]), sum(is.na(ods[,i])))
      }
    }
  }
  
  ## 36 backdoor --------------------
  if (data.id == 36){
    data.name <- "backdoor"   
    class.nor <- "Normal"
    class.ano <- "Backdoor"
    
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "backdoor/UNSW_NB15_testing-set.csv"), 
        header = T, sep=",", fill=T)
      
      ods <- ods[ ods$attack_cat %in% c(class.nor, class.ano),]
      ods <- ods[,-c(1, ncol(ods))]
      # ods$proto <- NULL
      # ods$service <- NULL
      # ods$state <- NULL
      
      ods <- cbind( GenDummyDF(ods[,1:(ncol(ods)-1)]), ods[,ncol(ods)])
      colnames(ods)[ncol(ods)] <- "class"
      d.col <- which( sapply(ods, function(x) length(unique(x))==1 ))
      if(length(d.col)>0) ods <- ods[, -d.col]
      ods <- ods[, -c(135,145,154,194)] #NA column
      
      # table(ods$class)
      # 0     1 
      # 56000  1746 
    }
  }
  
  ## 37 calTech28  --------------------
  if (data.id == 37){
    data.name <- "calTech28"
    class.nor <- 1
    class.ano <- 34
    
    if( load.data) {
      library(R.matlab)
      org <- readMat( file.path(path.data, "CalTech/caltech101_silhouettes_28.mat") )
      ods <- as.data.frame(org$X)
      dim(ods)
      label <- as.vector(org$Y)
      table(label)
      
      
      
      ods <- cbind( ods[label %in% c(class.nor, class.ano),],
                    label[label %in% c(class.nor, class.ano)] )
      colnames(ods)[ncol(ods)] <- "class"
      dim(ods)
      summary(ods)
      table(ods$class)
      for( i in 1:ncol(ods)){
        ods[is.na(ods[,i]),i] <- rep( mean(ods[!is.na(ods[,i]),i]), sum(is.na(ods[,i])))
      }
    }
  }
  
  
  ## 38 bank --------------------
  if (data.id == 38){
    data.name <- "bank"   
    class.nor <- "no"
    class.ano <- "yes"
    
    if( load.data) {
      ods <- read.table( file.path(path.data, "campaign/bank.csv"), 
                         header = T, sep=";", fill=T )
     
      ods <- cbind( GenDummyDF(ods[,1:(ncol(ods)-1)]), ods[,ncol(ods)])
      colnames(ods)[ncol(ods)] <- "class"
      table(ods$class)
      # no  yes 
      # 4000  521 
      
      
      d.col <- which( sapply(ods, function(x) length(unique(x))==1 ))
      if (length(d.col) > 0) ods <- ods[, -d.col]
    }
  }
  
  ## 39 census  --------------------
  if (data.id == 39){
    data.name <- "census"   
    class.nor <- "low"
    class.ano <- "high"
    
    if( load.data) {
      ods <- read.table(
        file.path(path.data, "census/census-income.test"), 
        header = F, sep=",", fill=F)
      head(ods)
      summary(ods)
      dim(ods)
      #[1] 99762    42
      
      
      del.c <- which( apply(ods, 1, function(x) any(x == " delete"))  == T)
      ods <- ods[-del.c,]
      dim(ods)
      #[1] 47391    42
      
      ods <- cbind( GenDummyDF(ods[,1:(ncol(ods)-1)]), ods[,ncol(ods)])
      dim(ods)
      #[1] 47391   409
      colnames(ods)[1:ncol(ods)] <- paste0("V", 1:ncol(ods) )
      colnames(ods)[ncol(ods)] <- "class"
      colnames(ods)
      table(ods$class)
      # high   low 
      # 2683 44708 
      summary(ods)
      
      
      d.col <- which( sapply(ods, function(x) length(unique(x))==1 ))
      if (length(d.col) > 0) ods <- ods[, -d.col]
    }
  }
  
  
  ## 40 fasion  --------------------
  if (data.id == 40){
    data.name <- "fashion" 
    class.ano <- 0
    class.nor <- 1
    
    if( load.data) {
      load_mnist(file.path(path.data, "Fashion/"))
      #table(mnist.test$y )
      #par(mfrow=c(10,10), mar=c(0,0,0,0))
      # 0	T-shirt/top
      # 1	Trouser
      # 2	Pullover
      # 3	Dress
      # 4	Coat
      # 5	Sandal
      # 6	Shirt
      # 7	Sneaker
      # 8	Bag
      # 9	Ankle boot
      
      
      
      ods <- data.frame(mnist.test$x[mnist.test$y %in% c(class.ano,class.nor),],
                        class=mnist.test$y[mnist.test$y %in% c(class.ano,class.nor)])
      
      ods <- unique(ods)
      table(ods$class)
    }
  }
  
  # ## 41 solar flare  --------------------
  # if ( data.id == 41 ){
  #   data.name <- "solar flare"
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data,  "solar flare/flare.data2"), 
  #                      head=FALSE, sep=" ", fill=TRUE)
  #     
  #     #                 0    1  2  3  4  5  6  7  8  Total
  #     #C-class flares  884 112 33 20  9  4  3  0  1  1066  (V11)
  #     #M-class flares 1030  29  3  2  1  0  1  0  0  1066  (V12)
  #     #X-class flares 1061   4  1  0  0  0  0  0  0  1066  (V13)
  #     
  #     colnames(ods)[12] <- "class"    
  #     colnames(ods)
  #     table(ods[,"class"])
  #     
  #     ods <- cbind( GenDummyDF(ods[,colnames(ods) != "class"]), 
  #                   class = ods[,"class"])
  #     ods <- as.data.frame(ods)
  #   }
  #   class.ano <- 1:8
  #   class.nor <- 0
  # }
  # 
  # ## 42  covertype  --------------------
  # if ( data.id == 42 ){
  #   data.name <- "covertype"
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data,  "Covertype/covtype.data"), 
  #                      head=FALSE, sep=",", fill=TRUE)
  #     ods <- ods[,11:ncol(ods)] # first 10 variables are numeric
  #     ods <- as.data.frame( apply(ods,2, as.factor))  #convert to categorical
  #     colnames(ods)[ncol(ods)] <- "class"    
  #     table(ods$class )
  #     #    1      2      3      4      5      6      7 
  #     #211840 283301  35754   2747   9493  17367  20510 
  #   }
  #   
  #   class.ano <- 1
  #   class.nor <- 4
  # }
  # 
  # ## 43  MiniBooNE  --------------------
  # if ( data.id == 43 ){
  #   data.name <- "MiniBooNE"
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data,  "MiniBooNE/MiniBooNE_PID.csv"), 
  #                      head=FALSE, sep=",", fill=TRUE) 
  #     dim(ods)
  #     ods <- cbind( ods, class = c( rep("electron", 36499), rep("muon", 93565) ))
  #     
  #     colnames(ods)[ncol(ods)] <- "class"    
  #     table(ods$class )
  #     # electron     muon 
  #     # 36499       93565
  #   }
  #   
  #   class.ano <- "electron"
  #   class.nor <- "muon"
  # }
  # 
  # ## 44  SteelFault  --------------------
  # if ( data.id == 44 ){
  #   data.name <- "SteelFault"
  #   if( load.data) {
  #     ods <-read.table(file.path(path.data,  "SteelFault/Faults.NNA"), 
  #                      head=FALSE, sep=",", fill=TRUE)
  #     ods.name <- unlist( read.table(file.path(path.data,  "SteelFault/Faults27x7_var"), 
  #                                    head=FALSE, sep=",", fill=TRUE) )
  #     class <- rep(NA, nrow(ods))
  #     
  #     for(i in 1:7 ){
  #       class[ which(ods[, 27+i ] == 1) ] <- ods.name[ 27+i]
  #     }
  #     table(class)
  #     # Bumps    Dirtiness     K_Scatch Other_Faults       Pastry       Stains    Z_Scratch 
  #     # 402           55          391          673          158           72          190 
  #     ods <- cbind(ods[,1:27], class=class)
  #   }
  #   
  #   class.ano <- "Bumps"
  #   class.nor <- "Pastry"
  # }
  
  
  
  
  if( is.null(data.name) ) return(NULL)
  
  
  if(load.data){
    ods <- ods[ods$class %in% c(class.nor, class.ano),]
    ods[, colnames(ods) != "class"] <- apply( ods[, colnames(ods) != "class"],
                                              2,
                                              as.double)
    ## remove rows with NA
    ods <- ods[complete.cases(ods),]
    head(ods)
    summary(ods)
    
    ## replace special sign, this will lead error in prediction model
    colnames(ods) <- str_replace_all( colnames(ods), "-", ".")
    colnames(ods) <- str_replace_all( colnames(ods), "/", ".")
    colnames(ods) <- str_replace_all( colnames(ods), "\\+", ".")
 
    
    return( list( id = data.id,
                  name = data.name, 
                  data.org = ods,
                  ratio.org = sum(ods$class %in% class.ano) / 
                              sum(ods$class %in% c( class.ano, class.nor)),
                  class.ano = class.ano, 
                  class.nor = class.nor) )
    
  } else {
    return( list( id = data.id,
                  name = data.name, 
                  class.ano = class.ano, 
                  class.nor = class.nor) )
  }
}
  





