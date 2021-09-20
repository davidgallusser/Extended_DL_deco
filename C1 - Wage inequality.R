########################################################
########################################################
### C1 - Wage inequality per year

########################################################
# Define file name where data is to be save
fn <- "3-wage-inequality.rda"
fnBS <- "3-wage-inequality-BOOTSTRAP.rda"

########################################################
# Load library
#library(Hmisc)

# Define years and quantiles
years <- seq(1994,2016,2)
tau <- c(0.1,0.5,0.9,0.99)

# Define empty result vector
wages <- NULL 

### Load data sets
for(t in years){
  print(t)
  setwd(wd_LSE_data)
  load(paste0("lse",t,".rda"))
  
  ### Compute PPJ shares
  wages <- rbind(wages,c(t,wtd.quantile(lse$wage,weights=lse$gewibgrs,probs=tau)))
  colnames(wages) <- c("erhebja",tau)
 
  setwd(wd_results)
  save(wages,file="3-wage-inequality.rda")
  setwd(wd)
  
}

quantileratios <- cbind(wages[,1],
                        exp(wages[,4])/exp(wages[,2]),
                        exp(wages[,3])/exp(wages[,2]),
                        exp(wages[,4])/exp(wages[,3]),
                        exp(wages[,5])/exp(wages[,4]))

colnames(quantileratios) <- c("erhebja","p90/p10","p50/p10","p90/p50","p99/p90")
setwd(wd_results)
save(wages,quantileratios,file=fn)
setwd(wd)

###############################################################
# Bootstrap 
if(bsexecute==TRUE){
  
  #Progress bar
  library("pbapply")
  
  # Actual bootstrap function
  #for(j in 1:length(bs_comp)){
  bootit <- function(j){  
    cat("Iteration: ", j,"\n")  
    
    # Set data set and weights
    lse <- lse_comp[bsample[[j]],]
    lse$gewibgrs <- lse$gewibgrs*bwadj[[j]]
    
    ### Compute PPJ shares
    b.wages <- wtd.quantile(lse$wage,weights=lse$gewibgrs,probs=tau) #rbind(b.wages,wtd.quantile(lse$wage,weights=lse$gewibgrs,probs=tau))
    return(b.wages)
  }
  
  # Set empty list for bootstrap results
  b.wages.all <- list()
  
  cat("Bootstraping... \n")  
  for(t in years){
    cat(paste0(t,"\n"))
    setwd(wd_LSE_data)
    load(paste0("lse",t,".rda"))
    load(paste("bsamples/bsample",t,".rda",sep=""))
    setwd(wd)
    lse_comp <-lse[,c("wage","gewibgrs")]
    
    # Set empty matrix for bootstrap results 
    ## Apply bootit function to multiple cores
    numCores <- ncores#detectCores()-1
    cl <- makeCluster(numCores)
    #b.wages <- NULL 
    clusterExport(cl, "lse_comp")
    clusterExport(cl, "bsample")
    clusterExport(cl, "bwadj")
    clusterExport(cl, "tau")
    clusterEvalQ(cl, {
                library(Hmisc)
    })
    
    # Perform actual bs selection with parLapply
    it <- length(bsample)
    #b.wages <- parLapply(cl,1:it,bootit)
    b.wages <- pblapply(1:it,bootit, cl=cl)
    
    # Stop cluster
    stopCluster(cl)
    
    b.wages.all[[which(years==t)]] <- matrix(unlist(b.wages),nrow=it,byrow=TRUE) #b.wages
    names(b.wages.all)[[which(years==t)]]  <- t
    setwd(wd_results)
    save(b.wages.all,file=fnBS)
    setwd(wd)
  }
  
  # Compute standard errors
  wages.se <- NULL
  quantileratios.se <- NULL
  for(t in 1:length(years)){
    wages.se <- rbind(wages.se,c(years[t],sqrt(diag(var(b.wages.all[[t]])))))
    b.qr <- cbind(exp(b.wages.all[[t]][,3])/exp(b.wages.all[[t]][,1]),
                  exp(b.wages.all[[t]][,2])/exp(b.wages.all[[t]][,1]),
                  exp(b.wages.all[[t]][,3])/exp(b.wages.all[[t]][,2]),
                  exp(b.wages.all[[t]][,4])/exp(b.wages.all[[t]][,3]))
    quantileratios.se <-  rbind(quantileratios.se,c(years[t],sqrt(diag(var(b.qr)))))
  }
  
  colnames(wages.se) <- colnames(wages)
  colnames(quantileratios.se) <- c("erhebja","p90/p10","p50/p10","p90/p50","p99/p90")
  
  # Save results
  setwd(wd_results)
  save(wages,quantileratios,wages.se,quantileratios.se,file=fn)
  setwd(wd)
}


