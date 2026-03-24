

source("real_data.R")

DownSampleAnomaly <- function(sample.info, data.org){
  
  set.seed( dget("seed_list.txt")[sample.info$index] )
  
  n.ano <- sum(data.org$class %in% sample.info$class.ano)
  n.nor <- sum(data.org$class %in% sample.info$class.nor)
  if.sample <- ifelse( (n.ano/(n.ano+n.nor))>sample.info$ratio, T, F )
  
  if ( if.sample == T) {
    sample.from <- which(data.org$class %in% sample.info$class.ano) 
    
    ratio.nao <- sample.info$ratio
    # n.down is the ratio.ano of the (n.down + n.nor)
    n.down <- floor( n.nor * ratio.ano / (1 - ratio.ano) )
    
    # when n.nor <= 0.5 * (1-ratio.ano) / ratio.ano, n.down == 0
    # e.g. when (ratio.ano == 0.01) and (n.nor <= 49), n.down == 0
    # e.g. when (ratio.ano == 0.05) and (n.nor <= 9), n.down == 0
    if(n.down == 0) n.down <- 1
    
    # sample
    sample.idx <- sample(sample.from, n.down, replace = FALSE)
    
    # mix the postion of anomaly and normal
    n.all <- n.down + n.nor 
    
    down.data <- data.frame( matrix(NA, nrow = n.all, ncol = ncol(data.org)))
    colnames(down.data) <- colnames(data.org)
    
    # add sampled anomalies in
    pos.ano <- sample(1:n.all, n.down)
    down.data[pos.ano, ] <- data.org[sample.idx, ]
    down.data[pos.ano, "class"] <- "anomaly"
    
    # add sampled anomalies in
    down.data[ !(1:n.all %in% pos.ano), ] <- data.org[ !(data.org$class %in% sample.info$class.ano), ]
    down.data[ !(1:n.all %in% pos.ano), "class"] <- "normal"
  } else {  # no sample is needed
    down.data <- data.org
    down.data$class <- "normal"
    down.data[ (data.org$class %in% sample.info$class.ano), "class" ] <- "anomaly"
  }
  
  # remove columns contain all equal value
  equ.col <- which(apply(down.data, 2, function(x) length(unique(x)) )==1)
  if( length(equ.col) > 1 ) down.data <- down.data[,-equ.col]
  
  # integer could not be accepted by bnlearn
  # TBD: why does the type of data is changed to integer from double after sampling??
  down.data[, colnames(down.data) != "class"] <- 
    apply( down.data[, colnames(down.data) != "class"],
           2, 
           as.double)
  
  return( down.data  )
}


GenDataSample <- function( sample.info ){
  
  ## load dataset-----------------
  real.data <- LoadRealData( sample.info$id, load.data = T )
  ## recover the class to the orginal format ( not shorten format)
  sample.info$class.ano <- real.data$class.ano
  sample.info$class.nor <- real.data$class.nor
  
  
  ## down sample ------------------------------
  data.smp <- DownSampleAnomaly( sample.info, real.data$data.org )
  
  ## save sampled data ------------------------------
  SaveAlgInfo( data.smp,  "dataset", sample.info, NULL)
  SaveAlgInfo ( data.smp$class, "label", sample.info, NULL)
  return(data.smp)
}

GetDataInfo <- function(data.id){
  if(missing(data.id)){
    return(GettAllDataInfo()) 
  } else{
    return( foreach( i = data.id ) %do% LoadRealData(data.id, load.data=F) )
  }
} 

ShortenLabels <- function(class){
  
  if( !is.vector(class)) return(class)
  if( length(class)==1 ) return(class)
  
  # if( is.character(class)) class <- str_to_lower(class)
  # # shorten for letters in a row
  # if( sum(class %in% letters) == length(class) ){
  #   # all letters
  #   nc <-  which( letters %in% sort(class))
  #   if( length(nc[1]:nc[length(nc)]) != length(nc) ) return(class)
  #   if( all.equal( nc[1]:nc[length(nc)], nc) ){
  #     return( paste0(letters[nc[1]],"-",letters[nc[length(nc)]]) )
  #   }
  # }
  # 
  
  if( !is.numeric(class)) return(class)

  class <- sort(class)
  if( length(class[1]:class[length(class)]) != length(class) ) return(class)
  if( all.equal( class[1]:class[length(class)], class) ){
    return( paste0(class[1],"-",class[length(class)]) )
  }
  
  return( class)
}

GettAllDataInfo <- function(){
  data.all <- data.frame( matrix(NA, nrow =200, ncol=4) )
  colnames(data.all) <- c("id", "name","normal.class","anomaly.class")
  n.data <- 0
  for(i in 1:200){
    data.one <- GetDataInfo(i)
    if( !is.null(data.one$name) ){
      n.data <- n.data + 1
      data.all[n.data,] <- c( n.data,
                              data.one$name, 
                              paste( ShortenLabels(data.one$class.nor), collapse = " "),
                              paste( ShortenLabels(data.one$class.ano), collapse = " "))
    }
  } 
  data.all <- data.all[1:n.data,]
  return(data.all)
}


## if it exists, load the saved data sample. 
## if it doesn't exist, generate the data sample.
LoadSampledData <- function( sample.info ){
  smp <- GetLogFullName( "dataset", sample.info )
  if( file.exists(smp) ) return( LoadAlgInfo( full.name = smp ) )
    
  return( GenDataSample(sample.info) )
}




  