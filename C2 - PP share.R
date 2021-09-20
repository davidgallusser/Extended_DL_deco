########################################################
########################################################
### C2 - Performance-pay share by year

########################################################
# Define file name where data is to be save
fn <- "4-pp-jobs-shares.rda"
fnBS <- "4-pp-jobs-shares-BOOTSTRAP.rda"

########################################################
# Define years
years <- seq(1994,2016,2)

# Define empty result vector
ppshares <- NULL 

### Load data sets
for(t in years){

print(t)
setwd(wd_LSE_data)
load(paste0("lse",t,".rda"))
setwd(wd)
  
### Pre-whitening 
# Get rid of horticulture
#lse <- lse[which(lse$nog_2!="nog_211"),]
# Performance pay dummy
lse$pp <- as.numeric(lse$pp.share>0)

### Compute PPJ shares
ppshares <- rbind(ppshares,c(t,weighted.mean(lse$pp,w=lse$gewibgrs, na.rm = TRUE)))
colnames(ppshares) <- c("erhebja","ppshare")

setwd(wd_results)
save(ppshares,file=fn)
setwd(wd)

}

###############################################################
# Bootstrap 
if(bsexecute==TRUE){
  
  # Set empty list for bootstrap results
  b.ppshares.all <- NULL
  
  # Bootstrap function
  #for(j in 1:length(bs_comp)){
  bootit <- function(j){  
  #  cat("Iteration: ", j,"\n")  
    
    # Set data set and weights
    lse <- lse_comp[bsample[[j]],]
    lse$gewibgrs <- lse$gewibgrs*bwadj[[j]]
    
    ### Compute PPJ shares
    b.ppshares <- weighted.mean(lse$pp,w=lse$gewibgrs, na.rm = TRUE) #c(b.ppshares,weighted.mean(lse$pp,w=lse$gewibgrs))
    return(b.ppshares)
  }
  
  cat("Bootstraping... \n")  
  for(t in years){
    cat(paste0(t,"\n"))
    setwd(wd_LSE_data)
    load(paste0("lse",t,".rda"))
    load(paste("bsamples/bsample",t,".rda",sep=""))
    setwd(wd)
    
    # Create pp dummy
    #lse$pp <-as.numeric(lse$pp.share>0)
    lse_comp <-lse[, c("pp","gewibgrs")]
    
    # Set empty matrix for bootstrap results 
    #b.ppshares <- NULL 
    
    ## Apply bootit function to multiple cores
    numCores <- detectCores()-2
    cl <- makeCluster(numCores)
    
    #b.wages <- NULL 
    clusterExport(cl, "lse_comp")
    clusterExport(cl, "bsample")
    clusterExport(cl, "bwadj")
    clusterEvalQ(cl, {
      library(Hmisc)
    })
    
    # Perform actual bs selection with parLapply
    it <- length(bsample)
    #b.wages <- parLapply(cl,1:it,bootit)
    b.ppshares <- pblapply(1:it,bootit, cl=cl)
    
    stopCluster(cl)
    
    b.ppshares.all <- cbind(b.ppshares.all,unlist(b.ppshares))
    colnames(b.ppshares.all)[which(years==t)] <- t
    setwd(wd_results)
    save(b.ppshares.all,file=fnBS)
    setwd(wd)
}
  
  # Compute bs standard errors
  ppshares.se <- apply(b.ppshares.all , 2, function(x) sqrt(var(x, na.rm=TRUE)))# rbind(ppshares[,1],sqrt(diag(var(b.ppshares.all))))
  names(ppshares.se) <- years
  
  # Save results
  setwd(wd_results)
  save(ppshares,ppshares.se,file=fn)
  setwd(wd)
  
}
