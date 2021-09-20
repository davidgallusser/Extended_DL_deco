##########################################################################
### A2 Create half sample bootstrap samples

setwd(wd_LSE_data)
memory.limit(size=100000)
it <- 1:250 # bootstrap iterations
erhebja <- seq(1994,2016,2) 
bsample <- list()
bwadj <- list()

# Bootit function for parallel processing of resample function
bootit <- function(iteration){
  bsample <- bs(lse, erhebja=l)
  bwadj <- bw(bsample,lse,erhebja=l)
  return(list(bsample,bwadj))
}

# Actual loop for all years
tic("sampling")
for(l in erhebja){
  cat("\n",l,":\n")
  
  load(paste("lse",l,".rda",sep=""))

    #Using loop
  set.seed(123) # resetting seed for every year
  
  ## Using parLapply
  # Find all cores/set clusters
  numCores <- detectCores()-1
  cl <- makeCluster(numCores)
  
  # Shift data and functions to clusters
  clusterExport(cl, "lse")
  clusterExport(cl, "l")
  clusterExport(cl, "bs")
  clusterExport(cl, "bw")
  clusterExport(cl, "half.sample")
  clusterExport(cl, "du")
  clusterExport(cl, "bootit")
  
  #use L'Ecuyer-CMRG and distribute streams to the clusters
  parallel::clusterSetRNGStream(cl, round(runif(1,0,100000)))

  # Perform actual bs selection with parLapply
  #b_res <- parLapply(cl,it,bootit)
  b_res <- pblapply(it,bootit, cl=cl)
  
  # Stop cluster
  stopCluster(cl)
  
  # Saving bsample for every year
  bsample <- lapply(b_res, function(x) x[[1]])
  bwadj <- lapply(b_res, function(x) x[[2]])
  
  setwd("bsamples")
  save(bsample,bwadj, file=paste("bsample",l,".rda", sep=""))
  setwd(wd_LSE_data)
}
toc()


### Test
test=FALSE

if(test==TRUE){
  erhebja=1996
  # Resampling
  set.seed(100)
  select <- bs(lse,erhebja=erhebja)
  length(select)/nrow(lse)
  
  # Weight adjustment
  rw <- bw(select,lse,erhebja=erhebja)
  lse$gwadj <- NA
  lse[select,"gwadj"] <- rw*lse[select,"gewicht"]
  sum(lse[select,"gwadj"])/sum(lse[,"gewicht"])
  sum(lse[select,"gewicht"])/sum(lse[,"gewicht"])
  
  wtd.quantile(lse[,"mbls"],probs=1:9/10, w=lse[,"gewibgrs"])
  wtd.quantile(lse[select,"mbls"],probs=1:9/10, w=rw*lse[select,"gewibgrs"])

}

setwd(wd)