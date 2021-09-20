###############################################################
###############################################################
### B2 - DFL decomposition of PP gap

########################################################
# Define file name where data is to be save
fn <- "2-deco-pp-gap.rda"
fnBS <- "2-deco-pp-gap-BOOTSTRAP.rda"

###############################################################
# Perfom decomposition: Performance pay gap 1996 and 2010 

# Set quantiles
tau_rw <- seq(1,99,1)/100

m.DFL.detail <- wage ~ berufst + taetigk + nog_agg + untgroe_f + geschle*(ausbild + potex_f + dienstja_f)
#m.DFL.detail <- wage ~ berufst + taetigk + nog_2 + untgroe + I(untgroe^2) + geschle*(ausbild + potex_f + dienstja_f)

# Decomposition 1996
# Load data 
setwd(wd_LSE_data)
load("lse1996.rda")

tic("deco1996")
ppgap1996 <- dfl_deco(m.DFL.detail, data=lse,
                          group=pp,
                          ws_formula=NULL,
                          reference=0,
                          tau=tau_rw,
                          weights = gewibgrs,
                          firstrw = "conditional",
                          stats=TRUE,
                          all.stats=TRUE,
                          log.trans=TRUE)
toc()

# Decomposition 2010
# Load data 
setwd(wd_LSE_data)
load("lse2010.rda")


tic("deco2010")
ppgap2010 <- dfl_deco(m.DFL.detail, data=lse,
                          group=pp,
                          ws_formula=NULL,
                          reference=0,
                          tau=tau_rw,
                          weights = gewibgrs,
                          firstrw = "conditional",
                          stats=TRUE,
                          all.stats=TRUE,
                          log.trans=TRUE)
toc()

setwd(wd_results)
save(ppgap1996,ppgap2010,file=fn)
setwd(wd)

###############################################################
# Bootstrap 
if(bsexecute==TRUE){
  
  #Progress bar
  library("pbapply")
  
  cat("Bootstraping... \n")  
  
  # Define bootstrap proceudre
  bootit <- function(j){
    #for(j in 1:length(bs_comp)){
    
    #cat("Iteration: ", j,"\n")  
    
    # Set data set and weights
    lse[bsample[[j]],"bweights"] <- lse[bsample[[j]],"gewibgrs"]*bwadj[[j]]
    
    # Aggregate RW 
    # Decomposition 1996
    b.ppgap1996 <- dfl_deco(m.DFL.detail, lse[bsample[[j]],],
                            group=pp,
                            ws_formula=NULL,
                            reference=0,
                            tau=tau_rw,
                            weights = bweights,
                            firstrw = "conditional",
                            stats=TRUE,
                            all.stats=TRUE,
                            log.trans=TRUE)
    
    # Store bootstraped statistic into separate matrices
    bs.agg.O.96 <- c(t(b.ppgap1996[["quantile"]][,2]),t(b.ppgap1996[["other.stats"]][,1])) #rbind(bs.agg.O.96,c(t(b.ppgap1996[["quantile"]][,2]),t(b.ppgap1996[["other.stats"]][,1])))
    bs.agg.X.96 <- c(t(b.ppgap1996[["quantile"]][,3]),t(b.ppgap1996[["other.stats"]][,2])) #rbind(bs.agg.X.96,c(t(b.ppgap1996[["quantile"]][,3]),t(b.ppgap1996[["other.stats"]][,2])))
    bs.agg.S.96 <- c(t(b.ppgap1996[["quantile"]][,4]),t(b.ppgap1996[["other.stats"]][,3])) #rbind(bs.agg.S.96,c(t(b.ppgap1996[["quantile"]][,4]),t(b.ppgap1996[["other.stats"]][,3])))
    
    return(list(bs.agg.O.96,
                bs.agg.X.96,
                bs.agg.S.96))
    
    # Continousely save bootstrap results
    #save( bs.agg.O.96,
    #      bs.agg.X.96,
    #      bs.agg.S.96,
    #      bs.agg.O.10,
    #      bs.agg.X.10,
    #      bs.agg.S.10,
    #      file=fnBS)
    
  } #End bootit function 
  
  ##############################
  ## Bootstrap 1996 gap
  setwd(wd_LSE_data)
  load("lse1996.rda")
  load("bsamples/bsample1996.rda")
  setwd(wd)

  lse$bweights <- NA
  
  ## Apply bootit function to multiple cores
  numCores <- ncores#detectCores()-4
  cl <- makeCluster(numCores)
  # Shift data and functions to clusters
  clusterExport(cl, "lse")
  clusterExport(cl, "bsample")    
  clusterExport(cl, "bwadj")
  clusterExport(cl, "wd")
  clusterExport(cl, "wd_results")
  clusterExport(cl, "m.DFL.detail")
  clusterExport(cl, "tau_rw")
  clusterExport(cl, "bootit")
  clusterEvalQ(cl, {
    setwd(wd)
    source("functions/r-DFL-deco-2-0.R")
  })
  
  # Perform actual bs selection with parLapply
  it <- length(bsample)
  #bsresults_ppgap <- parLapply(cl,1:it,bootit)
  bsresults_ppgap96 <- pblapply(1:it,bootit, cl=cl)

  # Stop cluster
  stopCluster(cl)
  
  # Prepare 
  bs.agg.O.96 <-  matrix(unlist(lapply(bsresults_ppgap96, function(x) x[[1]])),byrow=TRUE,nrow=it)
  bs.agg.X.96 <-  matrix(unlist(lapply(bsresults_ppgap96, function(x) x[[2]])),byrow=TRUE,nrow=it)
  bs.agg.S.96 <-  matrix(unlist(lapply(bsresults_ppgap96, function(x) x[[3]])),byrow=TRUE,nrow=it)
  
  #Compute standard errors and store with other results
  se.agg.96 <- cbind(sqrt(diag(var(bs.agg.O.96))),
                     sqrt(diag(var(bs.agg.X.96))),
                     sqrt(diag(var(bs.agg.S.96))))
  
  se.quantile.agg.96 <- cbind(tau_rw,se.agg.96[1:length(tau_rw),])
  se.other.stats.agg.96 <- se.agg.96[(length(tau_rw)+1):nrow(se.agg.96),]
  
  colnames(se.quantile.agg.96) <- colnames(ppgap1996$quantile)
  colnames(se.other.stats.agg.96) <- colnames(ppgap1996$other.stats)
  
  bootstrap.se.ppgap <- list(se.quantile.agg.96,
                             se.other.stats.agg.96)
  
  setwd(wd_results)
  save(res,deco,bootstrap.se.ppgap,bsresults_ppgap96,file=fn)
  
  ##############################
  ## Bootstrap 2010 gap
  setwd(wd_LSE_data)
  load("lse2010.rda")
  load("bsamples/bsample2010.rda")
  
  lse$bweights <- NA
  
  ## Apply bootit function to multiple cores
  numCores <- ncores #detectCores()-4
  cl <- makeCluster(numCores)
  # Shift data and functions to clusters
  clusterExport(cl, "lse")
  clusterExport(cl, "bsample")    
  clusterExport(cl, "bwadj")
  clusterExport(cl, "wd")
  clusterExport(cl, "wd_results")
  clusterExport(cl, "m.DFL.detail")
  clusterExport(cl, "tau_rw")
  clusterExport(cl, "bootit")
  clusterEvalQ(cl, {
    setwd(wd)
    source("functions/r-DFL-deco-2-0.R")
    #source("functions/r-rif-regression.R")
  })
  
  # Perform actual bs with parLapply
  it <- length(bsample)
  #bsresults_ppgap <- parLapply(cl,1:it,bootit)
  bsresults_ppgap10 <- pblapply(1:it,bootit, cl=cl)
  
  # Stop cluster
  stopCluster(cl)
  
  # Prepare 
  bs.agg.O.10 <-  matrix(unlist(lapply(bsresults_ppgap10, function(x) x[[1]])),byrow=TRUE,nrow=it)
  bs.agg.X.10 <-  matrix(unlist(lapply(bsresults_ppgap10, function(x) x[[2]])),byrow=TRUE,nrow=it)
  bs.agg.S.10 <-  matrix(unlist(lapply(bsresults_ppgap10, function(x) x[[3]])),byrow=TRUE,nrow=it)
  
  #Compute standard errors and store to other results
  se.agg.10 <- cbind(sqrt(diag(var(bs.agg.O.10))),
                     sqrt(diag(var(bs.agg.X.10))),
                     sqrt(diag(var(bs.agg.S.10))))
  
  se.quantile.agg.10 <- cbind(tau_rw,se.agg.10[1:length(tau_rw),])
  se.other.stats.agg.10 <- se.agg.10[(length(tau_rw)+1):nrow(se.agg.10),]
  colnames(se.quantile.agg.10) <- colnames(ppgap2010$quantile)
  colnames(se.other.stats.agg.10) <- colnames(ppgap2010$other.stats)
  
  bootstrap.se.ppgap <- list(se.quantile.agg.96,
                             se.other.stats.agg.96,
                             se.quantile.agg.10,
                             se.other.stats.agg.10)
  
  setwd(wd_results)
  save(ppgap1996,ppgap2010,bootstrap.se.ppgap,bsresults_ppgap96,bsresults_ppgap10,file=fn)
  setwd(wd)
  
}  ### End bootrap loop


