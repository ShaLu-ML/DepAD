require(ggplot2)
require(ggplot)
require(ggpubr)
require(foreach)
require(stringr)
source("functions.R")

##======== Settings===========
options(stringsAsFactors = FALSE)
options(scipen = 6, digits = 4)
#palette = c("#00AFBB", "#E7B800", "#FC4E07","#BB3099","#EE0099","#0000AC","#E69F00")
#palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#___________________========

#========= Functions ======================
OrganizeEva <- function( eva.m ){
  ## remove invalid algorithms
  idx.valid <- which( sapply( colnames(eva.m), function(x) x %in% alg.all.short ) )
  eva.m <- eva.m[, idx.valid]
  
  ## get the best results of ocsvm
  idx.svm <- which( str_detect( colnames(eva.m), "ocsvm" ) )
  rst.svm <- eva.m[, idx.svm]
  svm.max <- apply(rst.svm, 1, max)
  eva.m <- eva.m[, -idx.svm]
  eva.m <- cbind(eva.m, svm.best = svm.max)
  
  
  ## modify names with paper
  ## relevant variables
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "mb-", "MB." )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "pc-", "PC." )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "cor-", "ZC." )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "i.effects-", "IE." )
  
  ## prediction
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "cart-", "CART." )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "lasso-", "Lasso." )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "linear-", "Linear." )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "elastic-", "Elastic." )
  
  ## combination
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "Thresh", "PS" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), ".R.PS", ".S-PS" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "R.HeDES", "S-HeDES" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "R.AOM", "S-AOM" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "E.Euclidean", "Euc" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "E.Mahalanobis", "Mah" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "E.Manhattan", "Man" )
  
  ## comparison
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "abod", "FastABOD" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "also_m5p", "ALSO" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "combn", "COMBN" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "svm.best", "OCSVM" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "sod", "SOD" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "wknn", "wkNN" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "lof", "LOF" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "iforest", "iForest" )
  colnames(eva.m) <- str_replace_all( colnames(eva.m), "mbom", "MBOM" )
  #colnames(eva.m) <- str_replace_all( colnames(eva.m), "MB.CART.PS", "LoPAD" )
  
  return( eva.m)
  
}


ConvertToLatexTable <- function(x, 
                                file.name = "latex-tables.tex", 
                                caption = "Add Caption",
                                label = "tbl:add label",
                                bold.title = T,
                                mark.max = F,
                                max.col = NULL,
                                max.row = NULL) {
  ## for debug
  # x <- data.frame( a=1:3, b=2:4, c=3:5, d=4:6)
  # x
  ## for debug:  unlink(file.name)
  
  require(stringr)
  if( !file.exists(file.name) ) file.create(file.name)
  
  cat( paste0("\n\n", as.character( Sys.time())), file = file.name, sep="\n", append = T)
  cat( "----------------------------------------------", file = file.name, sep="\n", append = T)
  cat( "\\begin{table}[ht] \\centering", file = file.name, sep="\n", append = T)
  cat( paste0("  \\caption{", caption, "}  \\label{", label, "}" ), file = file.name, sep="\n", append = T)
  cat( "  \\resizebox{1\\textwidth}{!}{", file = file.name, sep="\n", append = T)
  cat( paste0("    \\begin{tabular}{ ", 
              paste0( rep("c ", ncol(x)), collapse = " "), 
              " }" ) , file = file.name, sep="\n", append = T)
  ## title
  cat( "      \\hline ", file = file.name, sep="\n", append = T)
  if(bold.title){
    ss <- paste0(" ", paste0( "\\textbf{",colnames(x),"}"), collapse = " & ")
  }  else {
    ss <- paste0(" ", colnames(x), collapse = " & ")  
  }
  ss <- str_replace_all( ss, "_", "\\\\textunderscore ")
  cat( paste0("      ",ss, "  \\", "\\" ) , 
       file = file.name, 
       sep="\n", 
       append = T)
  cat( "      \\hline ", file = file.name, sep="\n", append = T)
  
  ## table content
  for( i in 1:nrow(x) ){
    # i = 1
    ss <- paste0("      ", paste0( x[i,], collapse = " & "), 
                 "  \\", "\\" ) 
    ## replace special characters
    ss <- str_replace_all( ss, "_", "\\\\textunderscore ")
    ## replace special characters
    ss <- str_replace_all( ss, "NaN", "-")
    ss <- str_replace_all( ss, "Na", "-")
    
    ## if markmax, change to bold
    if( (mark.max) & ( i %in% max.row)) {
      x.max <- max(as.numeric(x[i,max.col]), na.rm = T)
      ss <- str_replace_all( ss, 
                             paste0("& ", x.max), 
                             paste0("& \\\\textbf{", x.max, "}") )
    }
    
    cat( ss , file = file.name, sep="\n", append = T)
    
  }
  cat( "      \\hline", file = file.name, sep="\n", append = T)
  cat( "    \\end{tabular}", file = file.name, sep="\n", append = T)
  cat( "  }", file = file.name, sep="\n", append = T)
  cat( "\\end{table}", file = file.name, sep="\n", append = T)
  
  closeAllConnections()
  
}

Separation <- function( x, y){
  ## x = 1:3
  ## y = 3:5
  mean( unlist(sapply(x, function(e) abs(y - e))), na.rm = T)
}


GetSensitivityIndex <- function( x ){
  # x <- eva.si
  dim(x)
  
  pair <- combn( 1:nrow(x), 2 )
  
  pair.sep <- foreach( i = 1:ncol(pair), .combine = c ) %do%{
    ## i=2
    ## e=x[1,1]
    p1 <- pair[1,i]
    p2 <- pair[2,i]
    Separation( x[p1,], x[p2,])
  }
  max(pair.sep)
}


## with OneDrive synchronizing, some new results are saved in files named xxx-GALAXY.xxx
## this function conduct:
##  (1) delete old files with same name withou -GALAXY
##  (2) rename files with -GALAXY to withou -GALAXY
UpdateSavedInfoFromGalaxy <- function( folder){
  ## folder = "evaluation"
  str.key <- "-GALAXY"
  files <- list.files(GenSaveDir(folder), full.names = T, recursive = T)
  head(files)
  
  upd.idx <- which(str_detect(files, str.key ))
  if( length(upd.idx) == 0 ){
    print( "No files need to update.")
    return()
  }
  head(upd.idx)
  
  
  del.files <- files[upd.idx]
  head(del.files)
  del.files <- str_sub(del.files, 1, str_length(del.files)- str_length(str.key)-4)
  del.files <- paste0(del.files, ".", str_sub(files[upd.idx], 
                                              str_length(files[upd.idx])-2,
                                              str_length(files[upd.idx])) )
  head(del.files)
  sapply(del.files, function(x) unlink(x))
  
  n.upd <- sum(sapply( 1:length(upd.idx), 
                       function(i) file.rename( files[upd.idx[i]], del.files[i] ) ))
  cat("\n update ", n.upd, "files.\n")
}


NamesCodeToPaper <- function(c.names){
  code.names <- foreach( s = c("rel", "pred", "comb", "alg.comp"), .combine = c, .inorder = T ) %do%{
    names.table[[s]]$code
  }
  paper.names <- foreach( s = c("rel", "pred", "comb", "alg.comp"), .combine = c, .inorder = T ) %do%{
    names.table[[s]]$paper
  }
  for(i in 1:length(code.names) ){
    c.names[ which(c.names == code.names[i] ) ] <- paper.names[i]
  }
  return(c.names)
  
}


SeparateDepSteps <- function(dep.names){
  ## remove dep-
  idx <- which(  str_sub(dep.names, 1, 4) == "dep-" )
  if(length(idx) > 0) dep.names[idx] <- str_sub(dep.names[idx],5,-1) 
  
  ## split to fixed length
  names.split <- data.frame(str_split_fixed(dep.names, "-", 3))
  colnames(names.split) <- c("Rel","Pred","Comb")
  return(names.split)
}




GetStepRt <- function(rt, folder){
  ## folder = "time_test"
  if( folder == "time_rel")  paper.name <- names.table$rel$paper
  if( folder == "time_pred")  paper.name <- names.table$pred$paper
  if( folder == "time_dev")  paper.name <- names.table$pred$paper
  if( folder == "time_score")  paper.name <- names.table$comb$paper
  if( folder == "time_test")  paper.name <- unique(names.table$alg.comp$paper)
  
  rt.step <- rt[ rt$folders == folder, ]
  rt.step <- cbind(rt.step, SeparateDepSteps( dep.names = rt.step$algorithm ) )
  
  if( folder == "time_rel")  idx.col <- which(colnames(rt.step) == "Rel")
  if( folder == "time_pred")  idx.col <- which(colnames(rt.step) == "Pred")
  if( folder == "time_dev")  idx.col <- which(colnames(rt.step) == "Pred")
  if( folder == "time_score")  idx.col <- which(colnames(rt.step) == "Comb")
  if( folder == "time_test")  idx.col <- which(colnames(rt.step) == "Rel")
  
  ## change code names to paper names
  rt.step$Rel <- NamesCodeToPaper(rt.step$Rel)
  rt.step$Pred <- NamesCodeToPaper(rt.step$Pred)
  rt.step$Comb <- NamesCodeToPaper(rt.step$Comb)
  
  rt.step.paper <- foreach( step.one = paper.name, .combine = cbind, .inorder = T ) %do% {
    foreach( id = dat$id, .combine = c, .inorder = T ) %do% {
      round( mean( rt.step[ which( rt.step$id == id & rt.step[,idx.col] == step.one ) , "runtime"], 
                   na.rm = T ), 2)
    }
  }
  # rt.step.paper
  
  ## add names
  colnames(rt.step.paper) <- paper.name
  rownames(rt.step.paper) <- 1:nrow(rt.step.paper)
  
  ## add data name
  rt.step.paper <- data.frame(Name=dat$name, rt.step.paper)
  
  ## add average
  rt.step.paper <- rbind(rt.step.paper,
                         c("Average", round( colMeans(rt.step.paper[,2:ncol(rt.step.paper)], na.rm = T), 2)))
  rownames(rt.step.paper)[nrow(rt.step.paper)] <- "Average"
  
  return(rt.step.paper)
  
}
#___________________========

# 
# ##============= Algorithms ===================
# # ## names in code
# # comb.all <- GetCombInfo()$name 
# # comb.all
# 
# # ## names in paper
# # rel.all.paper <- c("PC", "MB", "IE", "ZC")
# # pred.all.paper <- c("CART", "Elastic", "Linear", "Lasso")
# # comb.all.paper <- c("D-Man", "HeDES", "S-PS","PS","Sum","Max", "GS" )
# # alg.comp.paper <- c("LOF","wkNN","FastABOD", "iForest", "MBOM","SOD","OCSVM","ALSO","COMBN")
# 
# ## __ names.table ----
# names.table <- list( rel = data.frame( code = c("mb_0.01", "pc_0.01", "iepc", "mi", "dc"),
#                                        paper = c("FBED", "HiTON-PC", "IEPC", "MI", "DC")),
#                      pred = data.frame( code = c("anotree_25", "bcart", "linear", "lassocv", "ridgecv"),
#                                         paper = c("AnoTree", "CART", "Linear", "Lasso", "Ridge" )),
#                      comb = data.frame( code = c("azm_ps","Thresh", "Sum", "Max", "GS" ),
#                                         paper = c("RZPS", "PS", "Sum","Max","GS" )),
#                      alg.comp = data.frame( code = c("lof", "wknn", "abod", "iforest",  "mbom",  "sod", 
#                                                      "ocsvm_0.01", "ocsvm_0.03","ocsvm_0.05", "ocsvm_0.07","ocsvm_0.09",
#                                                      "also_m5p", "combn"),
#                                             paper =c("LOF","wkNN","FastABOD", "iForest", "MBOM","SOD",
#                                                      "OCSVM","OCSVM","OCSVM","OCSVM","OCSVM",
#                                                      "ALSO","COMBN"))
# )
# 
# names.table
# 
# alg.depad <- GenDepadAlgName( list( type.rel = names.table$rel$code, 
#                                     type.pred = names.table$pred$code,
#                                     type.com =  names.table$comb$code) )
# 
# # names.table[["alg.all"]] <- data.frame( code = c(alg.depad, names.table$alg.comp$code),
# #                                         paper =sapply( alg.all, ShortenAlgName ))
# #alg.all <- c(alg.depad, names.table$alg.comp$code) 
# alg.all <- c(alg.depad) 
# 


# names.table$alg.all$paper
#___________________========


# ============== Datasets ==============
##__data info----
dat <- GetRealDataInfo(data.ids)
dat <- dat[order(dat$n.col),]
rownames(dat) <- 1:nrow(dat)
dat$n.smp <- ifelse( dat$ratio.ano <= 0.01, 1, 20 )
dat


## __convert to latex----
# x <- dat
# x$id <- NULL
# x$ratio.ano <- NULL
# x <- x[, c(1,3,2,4,5) ]  # swap n.col and n.row columns
# colnames(x) <- c( "name", "\\#varaibles", "\\#objects",  "anomaly class", "normal class" )
# x
# ConvertToLatexTable( x = x, 
#                      caption = "Summary of Datasets Used in Experiments",
#                      label = "tbl:data")
#___________________========


#================= Evaluations =========================
## all results
rel.code  <-  c("mb_0.01", "pc_0.01", "iepc", "mi", "dc")
rel.paper  <-  c("FBED", "HiTON-PC", "IEPC", "MI", "DC")
pred.code  <-  c("banotree_25", "bcart", "linear", "lassocv", "ridgecv")
pred.paper <- c("AnoTree", "CART", "Linear", "Lasso", "Ridge" )
comb.code <-  c("azm_ps","Thresh", "Sum", "Max", "GS" )
comb.paper <-  c("RZPS", "PS", "Sum","Max","GS" )

alg.all <- GenDepadAlgName(list( rel.code, pred.code, comb.code))

eva.all <- data.frame()
for(alg in alg.all){
  #alg=alg.all[1]
  cat("\n processing alg:", alg,"--",which(alg.all==alg),"/",length(alg.all),"--")
  for(i.dat in 1:nrow(dat)){
    # i.dat=1
    id <- dat[i.dat, "id"] 
    roc <- c()
    ap <- c()
    for(index in 1:dat[i.dat, "n.smp"] ){
      # index=1
      
      eva <- LoadAlgInfo(folder="evaluation", 
                         sample.info = GenSampleInfo(id,index), 
                         alg = alg)
      if(is.null(eva)){
        if(str_detect(alg,"lassocv")){
          eva <- LoadAlgInfo(folder="evaluation", 
                             sample.info = GenSampleInfo(id,index), 
                             alg = str_replace(alg,"lassocv","lasso"))
          
        }
        if(str_detect(alg,"ridgecv")){
          eva <- LoadAlgInfo(folder="evaluation", 
                             sample.info = GenSampleInfo(id,index), 
                             alg = str_replace(alg,"ridgecv","ridge"))
          
        }
      }
      
      if(!is.null(eva)){
        roc <- c(roc, eva$roc.auc )
        ap <- c(ap,eva$ap)
      }
    }
    
    # save
    if(length(roc) == 0){
      roc.one = NA
      ap.one = NA
    }else{
      roc.one = mean(roc)
      ap.one = mean(ap)
    }
    eva.one <- data.frame(id=id,
                          name = dat[i.dat,"name"],
                          rel= rel.paper[rel.code==GetDepadComponent(alg,"rel")],
                          pred= pred.paper[pred.code==GetDepadComponent(alg,"pred")],
                          comb= comb.paper[comb.code==GetDepadComponent(alg,"comb")],
                          roc = roc.one,
                          ap = ap.one)
    if(nrow(eva.all)==0){
      eva.all <- eva.one
    } else {
      eva.all <- rbind(eva.all, eva.one)
    }
  }
}

write.csv( x = eva.all, 
           file = file.path(GenSaveDir("Experiments"), "evaluations_paper.csv"), 
           row.names = F)


aa <- eva.all[is.na(eva.all$roc),]
dim(aa)
aa
summary(factor(aa[,2]))

dim(eva.all)
125*32

#___________________========

##================= Components =========================
## need run the above sections before this
## __evaluation with components-----------
eva.com <- foreach( rel = rel.paper, .combine = rbind) %do% {
  foreach( pred = pred.paper, .combine = rbind) %do% {
    foreach( comb = comb.paper, .combine = rbind ) %do% {
      all.data.roc <- eva.all[ eva.all$rel == rel & 
                             eva.all$pred == pred & 
                             eva.all$comb == comb , "roc" ]
      all.data.ap <- eva.all[ eva.all$rel == rel & 
                                 eva.all$pred == pred & 
                                 eva.all$comb == comb , "ap" ]
      data.frame( eva.all = rel,
                  eva.all = pred,
                  eva.all = comb,
                  roc = mean( all.data.roc),
                  ap = mean(all.data.ap)
                 )
    }
  }
}
head(eva.com)
dim(eva.com)
summary(eva.com)


for( i.s in 1:3){
  
  ## __Set Component -----
  s <- i.s
  # s <- 1  ## 1: "rel", 2:"pred", 3:"comb"
  if( s == 1 ) techs <- names.table$rel$paper
  if( s == 2 ) techs <- names.table$pred$paper
  if( s == 3 ) techs <- names.table$comb$paper
  
  ##__Sensitivity Index ----
  ## range of ROC.AUC of techniques
  eva.si <- foreach( i = techs, .inorder = T, .combine = rbind) %do% {
    ## i = techs[1]
    eva.com[which(eva.com[,s]==i), "ROC.AUC"]
  }
  rownames(eva.si) <- techs
  si <- GetSensitivityIndex( eva.si )
  cat("\n Sensitivity Index:", si, "\n")
  
  
  ## __Boxplots -------
  #theme_set(theme_gray())
  x = factor( eva.com[,s], level = techs)
  xlab <- paste0("Sensitivity Index: ", round(si,3))
  # if( s == 1 ) xlab <- "Relevant Variable Selection Techniques"
  # if( s == 2 ) xlab <- "Dependency Acquisition Techiniques"
  # if( s == 3 ) xlab <- "Anomaly Score Generation Techniques"
  
  b <- ggboxplot( eva.com,
                  color = colnames(eva.com)[s],
                  x = colnames(eva.com)[s],
                  y = "ROC.AUC",
                  palette = "jco",
                  ylab = "ROC AUC",
                  add = c("dotplot")
  )  +
    theme(   axis.title.x = element_text(size = 20),
             axis.title.y = element_text(size = 15),
             axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             legend.position = "none" ) 
  f <- b + stat_compare_means(comparisons = combn(techs, 2, simplify = F), 
                              method = "wilcox.test",
                              paired = T,
                              label = "p.format",
                              method.args = list(alternative = "greater")
  ) + 
    scale_x_discrete( name=xlab) + 
    scale_y_continuous( breaks = seq(0.4,1,0.05)) 
  
  if( s== 1 ) ggexport(f, filename = "comp rel_1.png")
  if( s== 2 ) ggexport(f, filename = "comp pred_1.png")
  if( s== 3 ) ggexport(f, filename = "comp comb_1.png")
}

##__Rel Var Counts----
computer = "galaxy-ubuntu"
refresh.rel <- F
if(refresh.rel){
  rel.ct <- GenRelVarCountsTable(computer)
}  else {
  rel.ct <- read.csv( file.path(GetRstSummaryPath(computer), 
                                "rel_var_counts.csv") )
}

rel.ct <- rel.ct[which(rel.ct$algorithm %in% names.table$rel$code),]
rel.ct <- rel.ct[which(rel.ct$id %in% dat$id),]
rel.ct <- rel.ct[which(rel.ct$ratio == 0.01),]
rel.ct$algorithm <- str_replace_all(rel.ct$algorithm, "cor_0.4", "ZC")
rel.ct$algorithm <- str_replace_all(rel.ct$algorithm, "mb_0.01", "MB")
rel.ct$algorithm <- str_replace_all(rel.ct$algorithm, "pc_0.01", "PC")
rel.ct$algorithm <- str_replace_all(rel.ct$algorithm, "i.effects_10", "IE")

rel.ct <- foreach( one.tech = names.table$rel$paper, .combine = cbind, .inorder = T) %do%{
  rel.ct.dat <- foreach( one.dat = dat$id, .combine = rbind, .inorder = T) %do% {
    round( mean( rel.ct[which(rel.ct$id == one.dat & rel.ct$algorithm == one.tech), "counts"], 
                 na.rm = T ), 
           1 )
  }
}
colnames(rel.ct) <- names.table$rel$paper
rownames(rel.ct) <- 1:nrow(rel.ct)
rel.ct <- as.data.frame(rel.ct)

rel.ct <- cbind(name=dat$name, n.col=dat$n.col, rel.ct)
rel.ct <- cbind( rel.ct, apply(rel.ct[,3:ncol(rel.ct)], 2, function(x) round(100 * x/rel.ct[,2],1) ))
colnames(rel.ct)[7:ncol(rel.ct)] <- paste0("compress.",names.table$rel$paper)
rel.ct <- rbind(rel.ct, c( "Average", round(colMeans(rel.ct[,2:ncol(rel.ct)], na.rm = T)),1 ))
# rel.ct <- rel.ct[, c("name",
#                      "n.col",
#                      "PC",
#                      "compress.PC",
#                      "MB",
#                      "compress.MB",
#                      "IE",
#                      "compress.IE",
#                      "ZC",
#                      "compress.ZC")]
#   
rel.ct

ConvertToLatexTable(x = rel.ct, 
                    caption = "The Average Size of Relevant Variables",
                    label = "tbl:rel sizes")

#___________________========


#=========== Interactions ==================
## need eva.com in 'component' step
s <- 1  ## 1: "rel", 2:"pred", 3:"comb"
if( s == 1 ) techs <- names.table$rel$paper
if( s == 2 ) techs <- names.table$pred$paper
if( s == 3 ) techs <- names.table$comb$paper
dim(eva.com)
head(eva.com)
t.s <- 2
## __boxplot----
b <- ggboxplot( eva.com,
                color = colnames(eva.com)[s],
                shape = colnames(eva.com)[t.s],
                x = colnames(eva.com)[s],
                y = "ROC.AUC",
                palette = "jco",
                ylab = "ROC AUC",
                add = c("dotplot"),
                add.params = list(fill = colnames(eva.com)[t.s],
                                  shape = colnames(eva.com)[t.s])
) 
b

## __add shape to dotplot ----
## only works for s=1, t.s=2
bp <- ggplot_build( b )
pts <- bp$data[[2]]
dim(pts)

x.pos <- pts$x
gap.even <- 0.06
gap.odd <- 0.06
for(x.c in unique(pts$x) ){
  ## x.c = 3.1
  print(x.c)
  gp <- pts[which( pts$x == x.c),]
  gp
  
  gp[,"xmax"] - gp[,"xmin"]
  ct <- unique(gp[,"count"])
  
  pos.one <- gp$x
  for( ct.one in ct) {
    
    ## ct.one = ct[2]
    n <- max( gp[(gp$count == ct.one), "countidx"])
    if( n == 1) next
    
    idx.dp <- which( gp$count == ct.one )
    if(n%%2 ==0){
      if( n == 2){
        pos.one[idx.dp] <- c( x.c -  gap.even/2, 
                              x.c + gap.even/2)
      } else {
        pos.one[idx.dp] <- c( x.c - gap.even/2 -1:(floor(n/2)-1) * gap.even, 
                              x.c - gap.even/2, 
                              x.c + gap.even/2,
                              x.c + gap.even/2 + 1:(floor(n/2)-1) * gap.even)
      }
      
    } 
    if(n%%2 !=0) pos.one[idx.dp] <- c( x.c - 1:floor(n/2) * gap.odd, x.c, x.c + 1:floor(n/2) * gap.odd)
    
  }
  x.pos[which( pts$x == x.c)] <- pos.one
}

pts <- bp$data[[2]]
head(pts)
pts <- cbind(xpos = x.pos, pts)
dim(pts)
# 21: CART, 22:Lasso, 23: Elastic, 24: Linear
#shapes <- eva.com[which(!is.na(eva.com$ROC.AUC)), "Pred.Tech"]
#table(eva.com[which(is.na(eva.com$ROC.AUC)), "Pred.Tech"])
shapes <- rep( c(rep("Lasso",7),rep("Linear",7),rep("Elastic",7),rep("CART",7)), 4)
length(shapes)
pts <- cbind(shape = shapes, pts)
##__ add shaped plot ----
b <- ggboxplot( eva.com,
                color = "black",
                x = colnames(eva.com)[s],
                y = "ROC.AUC",
                palette = "jco",
                ylab = "ROC AUC"
) +
  theme_bw()+
  theme(   axis.title.x = element_blank(),
           axis.title.y = element_text(size = 15),
           axis.text.x = element_text(size = 12),
           axis.text.y = element_text(size = 12),
  ) 

f <- b + 
  geom_point(data=pts,aes(xpos,y, fill = shape, shape=shape),
             #fill = pts$fill,
             #shape = pts$shape,
             col = "white",  size=5) + 
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF","#CD534CFF", "#868686FF" ), # get_palette("jco",4), 
                    label = paste0(names.table$pred$paper, "      ")
  )+
  scale_shape_manual(values = c(21,23,24,22),
                     label = paste0(names.table$pred$paper, "      ")
  )  +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.spacing.x = unit(0.3, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.background = element_rect(fill = "gray90")) +
  # geom_hline(aes(yintercept=0.81), col="blue", linetype="dotted") +
  # geom_hline(aes(yintercept=0.83), col="blue", linetype="dotted") +
  # geom_hline(aes(yintercept=0.845), col="blue", linetype="dotted") +
  scale_y_continuous(name = "ROC AUC", 
                     #limits = c(0.78,0.86),
                     breaks=seq(0.78,0.86,0.01) ) +
  guides(color = FALSE)   ## remove one legend

f
ggexport(f, filename = "Interaction Rel Pred.png",
         height = 600,
         width = 960)

# if( s== 1 ) ggexport(f, filename = "interaction rel pred.png")
# if( s== 2 ) ggexport(f, filename = "interaction pred.png")
# if( s== 3 ) ggexport(f, filename = "interaction comb.png")

##__interation table----
inter <- data.frame( name = names.table$rel$paper,
                     CART = c("good", "excellent", "good","moderate"),
                     Elastic = c("good", "good", "poor", "poor"),
                     Lasso = c("poor", "poor", "poor", "poor"),
                     Linear = c("moderate", "poor", "moderate", "poor")
)


inter

ConvertToLatexTable(inter)
#___________________========






#================= With Benchmarks =========================
dep.select <- c("MB.CART.Man","PC.CART.Man","IE.CART.Man")
overall.algs <- c(dep.select, unique(names.table$alg.comp$paper) )

##__ format data ----
for( eva.type in c("ROC AUC", "AP") ){
  ## eva.type <- "ROC AUC" ## c("ROC AUC", "AP")
  if(eva.type == "ROC AUC"){
    e <- eva.roc[,c("data.id",overall.algs)]
    limits = c(0.45,1)
    breaks = seq(0.4,1,0.1) 
    name.x = 0.9
    name.y = 0.5
  }
  if(eva.type == "AP"){
    e <- eva.ap[,c("data.id",overall.algs)]
    limits = c(0,0.8) 
    breaks = seq(0,1,0.1) 
    name.x =  0.7
    name.y = 0.05
  } 
  
  # "MB.CART.Man" "PC.CART.Man" "IE.CART.Man"
  # "LOF" "wkNN"  "FastABOD" "iForest" "MBOM"  "SOD"  "OCSVM"  "ALSO"    "COMBN"
  e
  # ConvertToLatexTable(cbind(name=dat$name, e), caption = "comparason with benchmark methods")
  
  # avg <- round( colMeans(e, na.rm = T), 3)
  # avg
  
  ##__ Plots-----
  plot.lst <- list()
  for( ben.name in unique(names.table$alg.comp$paper)  ){
    ## ben.name = names.table$alg.comp$paper[1]
    ben.one <- foreach( dep.name = dep.select, .combine = rbind, .inorder = T ) %do% {
      ## dep.name = dep.select[1]
      
      data.frame( data.id = e$data.id,
                  data.name = sapply(e$data.id, function(x) return( dat[ which(dat$id ==x) ,"name"])),
                  dep.name = dep.name,
                  dep.eva = e[,dep.name],
                  ben.name = ben.name,
                  ben.eva = e[, ben.name]
                  #,p.value = p.value
      )
    }
    # ben.one
    
    
    ##__ plot one ---- 
    order.aes <- factor( ben.one$dep.name, levels = dep.select )
    # legend.lables <- sapply(levels(order.aes), function(x)  
    #   paste0( x,": ", avg[x], "/" , ben.one[ which(ben.one$dep.name == x)[1], "p.value"]) )
    # legend.name <- paste0("Means/p-values: \n(mean of ",ben.name, ":", avg[ben.name], ")")
    f <- ggplot(ben.one, aes(x=ben.eva, y=dep.eva )) +
      # geom_point( aes( color = order.aes, shape = order.aes),
      #             size = 5, stroke = 3) +
      geom_point( size = 1, stroke = 2) +
      theme(   axis.title.x = element_blank(), # remove x label
               axis.title.y = element_blank(),  #element_text(size = 25),
               axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
               # legend.position = c(0.7, 0.2),
               # legend.direction = "vertical",
               # legend.margin = margin(0.4, 0.6, 0.4, 0.5, "cm"), # t r b l unit
               # legend.title = element_text( size = 25),
               # legend.text = element_text(size = 25),
               # legend.spacing.y = unit(0.5, "cm"), # spacing from title to keys
               # legend.spacing.x = unit(0.4, "cm"), # spacing from keys to their text
               # legend.key = element_rect(size = 6),
               # legend.key.size = unit(1.4, "lines"),
               # legend.key.height = unit(1.5, "cm"),
               # legend.key.width = unit(1.5, "cm")
      ) + 
      geom_text(x=name.x, y=name.y, label=ben.name,size=8, col="black") +
      scale_y_continuous( #name = paste0("Proposed Methods (",eva.type, ")"),
        limits = limits, 
        breaks = breaks ) +
      scale_x_continuous(#name = paste0(ben.name, " (", eva.type, ")"),
        limits = limits, 
        breaks = breaks ) +
      # scale_colour_manual(name = legend.name,
      #                     labels = legend.lables,
      #                     values = get_palette("jco", length(dep.select))) +   
      # scale_shape_manual(name = legend.name,
      #                    labels = legend.lables,
      #                    values = 1:(1+length(unique(ben.one$dep.name))) )+
      geom_abline(slope=1,intercept = 0, color = "steelblue", linetype="dashed",size=1.2)
    
    plot.lst[[ which(unique(names.table$alg.comp$paper) == ben.name) ]] <- f
    
    ## save plot
    # ggexport(f, 
    #          filename = paste0("comp ben-",ben.name, "-",eva.type, ".png"),
    #          width = 960, 
    #          height = 960
    #)
    
  }
  f <- ggarrange( plot.lst[[1]],plot.lst[[2]],plot.lst[[3]],
                  plot.lst[[4]],plot.lst[[5]],plot.lst[[6]],
                  plot.lst[[7]],plot.lst[[8]],plot.lst[[9]],
                  ncol = 3, nrow=3)
  ggexport( f, 
            filename = paste0("comp ben", "-",eva.type, ".png"),
            width = 1280, 
            height = 1280 )
}

## __ p-value ----
p.ben <- foreach( eva.type = c("ROC AUC", "AP"), .combine = rbind, .inorder = T ) %do% {
  ## eva.type <- "ROC AUC" ## c("ROC AUC", "AP")
  if(eva.type == "ROC AUC") e <- eva.roc[,c("data.id",overall.algs)]
  if(eva.type == "AP")  e <- eva.ap[,c("data.id",overall.algs)]
  
  p.lst <- foreach( dep.one = dep.select, .combine = rbind, .inorder = T) %do% {
    ## dep.one = dep.select[1]
    foreach( ben.one = unique(names.table$alg.comp$paper), .combine = c, .inorder = T  ) %do%{
      ## ben.one = unique(names.table$alg.comp$paper)[2]
      round( wilcox.test(e[,dep.one], 
                         e[,ben.one],
                         paired = T, 
                         alternative = "greater")$p.value, 3)
    }
  }
  rownames(p.lst) <- paste( eva.type, dep.select)
  colnames(p.lst) <- unique(names.table$alg.comp$paper)
  p.lst
}
p.ben <- cbind(Methods = rownames(p.ben), p.ben)
ConvertToLatexTable(p.ben, 
                    caption = "p-values of Wilcox Sum Test of Proposed Methods Pairwise with Benchmark Methods",
                    bold.title = F,
                    label = "tbl:p value"
)


#___________________====


#=========== Running Time ==================
refresh.rt <- F
computer = "galaxy-ubuntu"
if( refresh.rt){
  rt <- GenRtTable(computer)
} else{
  rt <- read.csv( file.path( GetRstSummaryPath(computer), "rt.csv") )
}
head(rt)

rt <- rt[which(rt$id %in% dat$id),]
unique(rt$id)
rt <- rt[which(rt$ratio == 0.01),]
unique(rt$ratio)
rt <- rt[which(rt$computer == computer),]
unique(rt$computer)

## __ steps ----
rt.rel <- GetStepRt(rt, "time_rel")
rt.pred <- GetStepRt(rt, "time_pred")
rt.dev <- GetStepRt(rt, "time_dev")
rt.score <- GetStepRt(rt, "time_score")

rt.paper.train <- cbind( rt.rel,rt.pred[2:ncol(rt.pred)])
ConvertToLatexTable(x=rt.paper.train, 
                    caption = "Running Time of Training Phase of the DepAD Framework")

rt.paper.test <- cbind( rt.dev,rt.score[2:ncol(rt.score)])
ConvertToLatexTable(x=rt.paper.test, 
                    caption = "Running Time of Testing Phase of the DepAD Framework")                   

## __ benchmark ----
rt.ben <- GetStepRt(rt, "time_test")

#___________________====



#=========== Paired Interactions ==================
## __Set Components ---------

# source compnents
s.s <- 1  ## 1: "rel", 2:"pred", 3:"comb" 
if( s.s == 1 ) techs <- names.table$rel$paper
if( s.s == 2 ) techs <- names.table$pred$paper
if( s.s == 3 ) techs <- names.table$comb$paper

tech12 <- combn(techs, 2, simplify = F)
for( i in 1:length(tech12)){
  tech1 <- tech12[[i]][1]
  tech2 <- tech12[[i]][2]
  
  # tech1 <- "PC"
  # tech2 <- "MB"
  
  # target compnents
  t.s <- 2  ## 1: "rel", 2:"pred", 3:"comb" 
  if( t.s == 1 ) t.techs <- names.table$rel$paper
  if( t.s == 2 ) t.techs <- names.table$pred$paper
  if( t.s == 3 ) t.techs <- names.table$comb$paper
  
  anno.s <- (1:3)[-c(s.s, t.s)]
  
  ## __Paired ---------
  cond1 <- eva.com[which(eva.com[,s.s]== tech1),]
  ## make sure cond2 has the totally same order on other variables with cond1
  cond2 <- foreach( i = 1:nrow(cond1), .combine = rbind, .inorder = T ) %do% {
    c2 <- cond1[i,1:3]
    c2[,s.s] <- tech2
    c2.idx <- apply(eva.com[,1:3], 1, function(x) all(x == c2))
    eva.com[c2.idx,]
  }
  cond2
  eva.pair <- rbind(cond1, cond2)
  
  
  ## paired plot
  theme_set(theme_gray())
  b <- ggpaired(eva.pair, 
                x = colnames(eva.com)[s.s], 
                y = "ROC.AUC",
                color = colnames(eva.com)[s.s], 
                line.color = "gray", 
                line.size = 0.4,
                label = colnames(eva.com)[anno.s],
                repel = T,
                label.select = list( top.up = 14, top.down = 0),
                palette = "jco"
  ) +
    #geom_hline(aes(yintercept=0.795), col="blue", linetype="dotted") +
    geom_hline(aes(yintercept=0.81), col="blue", linetype="dotted") +
    geom_hline(aes(yintercept=0.83), col="blue", linetype="dotted") +
    geom_hline(aes(yintercept=0.845), col="blue", linetype="dotted") +
    # geom_text(x=0.6, y=0.785, label="inferior",size=4, col="darkblue") +
    # geom_text(x=0.6, y=0.80, label="poor",size=4, col="darkblue") +
    # geom_text(x=0.6, y=0.82, label="moderate",size=4, col="darkblue") +
    # geom_text(x=0.6, y=0.84, label="good",size=4, col="darkblue") +
    # geom_text(x=0.6, y=0.855, label="excellent",size=4, col="darkblue") +
    scale_y_continuous(name = "ROC AUC", 
                       #limits = c(0.78,0.86),
                       breaks=seq(0.78,0.86,0.01) ) +
    scale_x_discrete( name= element_blank()) +
    theme(   axis.title.x = element_text(size = 15),
             axis.title.y = element_text(size = 15),
             axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             legend.position = "none" ) #+
  #ylim(0.76, 0.86)
  # b
  
  
  f <- b + 
    #stat_compare_means(paired = TRUE) + 
    geom_point(aes( shape = eva.pair[,t.s], fill = eva.pair[,t.s]), size = 4) + 
    scale_shape_manual(values = (24-length(t.techs)+1):24) +
    theme(legend.position = "top",
          legend.text = element_text(size = 12),
          #legend.title = element_text(size = 13),
          #legend.justification=c(0,0), 
          #legend.position=c(0.05,0.05),
          legend.title = element_blank(),
          legend.background = element_rect(fill="gray90", size=.8, linetype="dotted")) +
    guides(color = FALSE)  ## remove one legend
  f
  
  ## save plots
  ggexport(f, filename = paste0(tech1,"-",
                                tech2,"-",
                                str_split_fixed (colnames(eva.com)[t.s], "\\.", 2)[,1],
                                ".png"))
}



#=====___________________====



##------- temp -------------------------------

bd <- rt.pred[ which(rt.pred$name == "backdoor") ,]
bd <- bd[ which(bd$Rel == "MB") ,]
bd



e.pair <- eva.com[which(eva.com$Rel.Tech=="MB" | eva.com$Rel.Tech=="PC"),]

b <- ggplot(e.pair, aes(factor(Rel.Tech, level = names.table$rel$paper) , ROC.AUC)) +
  theme(   axis.title.x = element_text(size = 14),
           axis.title.y = element_text(size = 14),
           axis.text.x = element_text(size = 12), # angle = 45,hjust = 1),
           axis.text.y = element_text(size = 12),
           legend.text = element_text(size = 12),
           legend.title = element_text(size = 13) ) +
  scale_x_discrete( name="Relevant Variable Selection Techiniques")
f1 <- b + geom_boxplot(aes(col = Rel.Tech)) +
  geom_point(aes(col=Pred.Tech)) +
  stat_compare_means(comparisons = combn(names.table$pred$paper, 2, simplify = F), 
                     method = "wilcox.test",
                     paired = T) 


theme(legend.position = "none") 
f1


## pairwise of MB and PC
e.pair <- eva.com[which((eva.com$Rel.Tech=="MB" | eva.com$Rel.Tech=="PC")),]
ggpaired(e.pair, x = "Rel.Tech", y = "ROC.AUC",
         color = "Rel.Tech", line.color = "gray", line.size = 0.4,
         palette = "jco"
)+
  geom_point(aes( shape= Pred.Tech, fill = Pred.Tech), size=4) + 
  #scale_fill_manual(values = palette) + 
  scale_shape_manual(values = 21:24)

## pairwise of PC and IE
e.pair <- eva.com[which((eva.com$Rel.Tech=="PC" | eva.com$Rel.Tech=="IE")),]
e.pair
ggpaired(e.pair, x = "Rel.Tech", y = "ROC.AUC",
         color = "Rel.Tech", line.color = "gray", line.size = 0.4,
         palette = "jco"
)+
  geom_point(aes( shape= Pred.Tech, fill = Pred.Tech), size=4) + 
  #scale_fill_manual(values = palette) + 
  scale_shape_manual(values = 21:24)


## pairwise of MB and IE
e.pair <- eva.com[which((eva.com$Rel.Tech=="IE" | eva.com$Rel.Tech=="ZC")),]
e.pair
ggpaired(e.pair, x = "Rel.Tech", y = "ROC.AUC",
         color = "Rel.Tech", line.color = "gray", line.size = 0.4,
         palette = "jco"
)+
  geom_point(aes( shape= Pred.Tech, fill = Pred.Tech), size=4) + 
  #scale_fill_manual(values = palette) + 
  scale_shape_manual(values = 21:24)



## pairwise of MB and IE
e.pair <- eva.com[which(eva.com$Pred.Tech=="CART"),]
e.pair <- rbind(e.pair, eva.com[which(eva.com$Pred.Tech=="Elastic"),])
e.pair
ggpaired(e.pair, x = "Pred.Tech", y = "ROC.AUC",
         color = "Pred.Tech", line.color = "gray", line.size = 0.4,
         palette = "jco"
) +
  geom_point(aes( shape= Comb.Tech, fill = Comb.Tech), size=4) + 
  #scale_fill_manual(values = palette) + 
  scale_shape_manual(values = 19:25)







ggpaired(e.pair, x = "Rel.Tech", y = "ROC.AUC",
         color = "Rel.Tech", 
         line.color = "gray", 
         line.size = 0.4,
         #palette = "jco"
)+
  geom_point(aes( shape= Comb.Tech, fill = Comb.Tech), size=4) + 
  #scale_color_manual(values = palette) + 
  scale_shape_manual(values = 19:25)


palette


(112) * ( (32-4) *20 + 4 )
112 * 32




p <-
  ggplot(mtcars, aes(x = mpg)) + 
  geom_dotplot(binwidth = 1.5, dotsize = 1) +
  ylim(-0.1, 1.1)


# this is the "instructions" of the plot
gpb <- ggplot_build(p)


# gpb$data is a list, you need to use the first element
gpb$data[[1]] %>% 
  ggplot(aes(x, stackpos/max(stackpos))) +
  geom_point(shape = 22, size = 14, fill = "blue") +
  ylim(-0.1, 1.1)


bp
pts <- bp$data[[2]]

ggplot(pts, aes(x,y)) +
  geom_point(shape = pts$shape, col=pts$colour, fill=pts$fill) +
  ylim(-0.1, 1.1)



## =========== Save for Later ===============
# ## categorize by Pred.Tech
# f2 <-  b + geom_boxplot(aes(color = Pred.Tech)) +
#   scale_color_discrete(name="Dependency Acquisition Techiniques")+
#   theme(legend.position = "top")
# f2
# 
# ## categorize by Comb.Tech 
# f3 <-  b + geom_boxplot(aes(color = Comb.Tech)) +
#   scale_color_discrete(name="Anomaly Score Generation Techiniques")+
#   theme(legend.position = "top")
# f3


best.algs <- sapply(e$best.alg, paste0, collapse=", ")
eva.one <- cbind( name = dat$name,
                  e$value[,overall.algs], 
                  best = e$best,
                  best.algs = best.algs)

## wilcoxon test
# tgt <- eva.one$`MB.CART.S-PS`
# wil1 <- apply( eva.one[, 4:(ncol(eva.one)-2)], 
#                2, 
#                function(x) wilcox.test(tgt, x, paired = T, alternative = "greater")$p.value ) 
# wil1 <- round(wil1, 5)

tgt <- eva.one$`MB.CART.D-Man`
wil2 <- apply( eva.one[, 4:(ncol(eva.one)-2)], 
               2, 
               function(x) wilcox.test(tgt, x, paired = T, alternative = "greater")$p.value ) 
wil2 <- round(wil2, 5)


## add mean
eva.one <- rbind( eva.one, 
                  average = c("average", round( colMeans(eva.one[2:(ncol(eva.one)-1)], na.rm = T), 3), "-") )
rownames(eva.one) <- c( 1:(nrow(eva.one)-1), "average")
eva.one

## add p-values of wilcoxon test 
eva.one <- rbind(eva.one, 
                 #                 c(rep("-",3), wil1, rep("-",2)),
                 c(rep("-",3), wil2, rep("-",2)))
eva.one

## output to tex file
ConvertToLatexTable( x = eva.one, 
                     #caption = "Experimental Results (ROC AUC)",
                     #label = "tbl:ROC",
                     caption = "Experimental Results (AP)",
                     label = "tbl:AP",
                     mark.max = T,
                     max.col = 2:(ncol(eva.one)-3),
                     max.row = 1:(nrow(eva.one)-2) )



## boxplot
e <- roc.lst$value
#e <- ap.lst$value


# filter out the comparison methods
e <- e[, which (!colnames(e) %in% names.table$alg.comp$code$paper)]

# transfer the format of result
e.df <- foreach( i = 1:nrow(e), .combine = rbind) %do% {
  foreach ( j = 1:ncol(e), .combine = rbind ) %do% {
    s.name <- str_split_fixed( colnames(e)[j], "\\.", 3)
    data.frame( data.name = dat$name[i],
                ROC.AUC = e[i,j],
                Rel.Tech = s.name[1],
                Pred.Tech = s.name[2],
                Comb.Tech = s.name[3]
    )
  }
}

dim(e.df)


cmp <- roc.lst$value
#cmp <- ap.lst$value
idx.cmp <- which( colnames(cmp) %in% names.table$alg.comp$paper)
cmp <- cmp[,idx.cmp]
cmp <- cbind(data.name = dat$name, cmp)
cmp.df <- foreach( i = 2:ncol(cmp), .combine = rbind) %do% {
  data.frame( data.name = cmp$data.name,
              alg.type = "benchmark",
              alg.name = rep( colnames(cmp)[i], nrow(cmp) ),
              ROC.AUC = cmp[,i])
}

all.df <- rbind( cmp.df, data.frame( data.name = e.df$data.name,
                                     alg.type = "proposed",
                                     alg.name = "dependency",
                                     ROC.AUC = e.df$ROC.AUC) )

idx.one <- which( e.df$Rel.Tech == "MB" & 
                    e.df$Pred.Tech == "CART" & 
                    e.df$Comb.Tech == "D-Man")
dep.bench <- ggplot(all.df, aes( factor( data.name, level = dat$name), ROC.AUC)) +
  geom_boxplot( aes( color= factor(alg.type, level = c("proposed", "benchmark")))
                ,size = 0.8) +
  geom_point( data = e.df[idx.one,], 
              aes( factor( data.name, level = dat$name), ROC.AUC ), 
              shape = 24, size = 2, col = "blue", fill="blue") +
  #geom_jitter( color = "light grey", alpha = 0.5) +
  scale_y_continuous(name = "ROC AUC", breaks=seq(0, 1, 0.1)) +
  #scale_y_continuous(name = "AP", breaks=seq(0, 1, 0.1)) +
  scale_x_discrete( name="Datasets") +
  scale_color_discrete(name="Methods", 
                       labels = c("proposed methods","comparison methods")) +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45,hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        #legend.justification=c(0,0), 
        #legend.position=c(0.05,0.05),
        legend.background = element_rect(fill="gray90", size=.8, linetype="dotted")
  )

dep.bench

T1 <- 1:3
T2 <- 4:6
t <- data.frame(T1, T2)




