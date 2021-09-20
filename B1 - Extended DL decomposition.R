########################################################
########################################################
### B1 Extended DL decomposition of wage change
###    

########################################################
# Define file name where data is to be save
fn <- "1-decomposition-results.rda"
fnBS <- "1-decomposition-results-BOOTSTRAP.rda"

########################################################
# Load data 
setwd(wd_LSE_data)
load("lse1996.rda")
df <- lse
load("lse2010.rda")
lse <- rbind(df,lse)
setwd(wd)


########################################################
# Define model

# Set quantiles
tau_rw <- seq(1,99,1)/100

# Set model 
m.DFL.detail <- wage ~ pp*(berufst + nog_agg + untgroe_f + geschle*ausbild) + taetigk + geschle*(potex_f + dienstja_f)
m.DFL.ws <- pp ~ berufst + taetigk + nog_agg + untgroe_f + geschle*(ausbild + potex_f + dienstja_f)

###############################################################
# Perfom decomposition: Contribution of PP to wage inequality
tic("deco")
# DFL decomposition incl. DL decomposition
res <- dfl_deco(m.DFL.detail, data=lse,
                group=lse10,
                ws_formula=m.DFL.ws,
                reference=1,
                tau=tau_rw,
                weights = gewibgrs,
                firstrw = "conditional",
                stats=TRUE,
                all.stats=TRUE,
                log.trans=TRUE)

# Extended DL decomposition
deco <- dfl_deco_ws(res)
toc()

###############################################################
# Compute reweighting factor weights
wstats <- function(x){c(min(x),quantile(x,probs=c(0.01,0.1,0.5,0.9,0.99)),max(x))}
select <- which(res$mf[,"groupN"]==1)

tabW <- rbind(wstats(res$psi[select,1]),
              wstats(deco$psi_M),
              wstats(deco$psi_J),
              wstats(deco$psi_S1.0),
              wstats(deco$psi_S1.1))

###############################################################
# Save all results
setwd(wd_results)
save(res,deco,tabW,file=fn)
setwd(wd)

###############################################################
# Bootstrap 
if(bsexecute==TRUE){
  
  # Load progress bar for *apply functions
  #library("parallel")
  #library("pbapply")

  tic("bootstrapping")

  cat("Bootstraping... \n")  
  
  ## Load original data sets
  lse_comp <- lse[which(lse$erhebja==com.year),]
  lse_ref <- lse[which(lse$erhebja==ref.year),]
 
  ## Load bootstrap samples and weight adjustments
  setwd(wd_LSE_data)
  load(paste("bsamples/bsample",com.year,".rda",sep=""))
  bs_comp <- bsample
  bs_wadj_comp <- bwadj
  load(paste("bsamples/bsample",ref.year,".rda",sep=""))
  bs_ref <- bsample
  bs_wadj_ref <- bwadj
  setwd(wd_results)
  
  # Remove unused objects
  rm(bsample,bwadj,lse)

  # Create empty saving matrices
  #bs.agg.O <- NULL
  #bs.agg.S <- NULL
  #bs.agg.X <- NULL
  #bs.agg.WS1 <- NULL
  #bs.agg.WSother <- NULL
  
  #bs.WS.marginal <- NULL
  #bs.WS.sorting <- NULL
  #bs.WS.wage_structure <- NULL
  
  #bs.W.stats <- list()
  
  ## Re-estimations
  #for(j in 1:length(bs_comp)){
    
  bootit <- function(j){
    
    #cat("Iteration: ", j,"\n")  
    
    # Set data set and weights
    lse <- rbind(lse_comp[bs_comp[[j]],],lse_ref[bs_ref[[j]],])
    lse$gewibgrs <- lse$gewibgrs*c(bs_wadj_comp[[j]], bs_wadj_ref[[j]])
    
    # Aggregate RW 
    b.res <- dfl_deco(m.DFL.detail, data=lse,
                      group=lse10,
                      ws_formula=m.DFL.ws,
                      reference=1,
                      tau=tau_rw,
                      weights = gewibgrs,
                      firstrw = "conditional",
                      stats=TRUE,
                      all.stats=TRUE,
                      log.trans=TRUE)
    
    # Detailed rw
    b.deco <- dfl_deco_ws(b.res)
    
    # Compute reweight weights statistics 
    wstats <- function(x){c(min(x),quantile(x,probs=c(0.01,0.1,0.5,0.9,0.99)),max(x))}
    select <- which(b.res$mf[,"groupN"]==1)
    
    b.tabW <- rbind(wstats(b.res$psi[select,1]),
                  wstats(b.deco$psi_M),
                  wstats(b.deco$psi_J),
                  wstats(b.deco$psi_S1.0),
                  wstats(b.deco$psi_S1.1))
    
    # Store bootstraped statistic into separate matrices
    bs.agg.O <- c(t(b.res[["quantile"]][,2]),t(b.res[["other.stats"]][,1])) #rbind(bs.agg.O,c(t(b.res[["quantile"]][,2]),t(b.res[["other.stats"]][,1])))
    bs.agg.X <- c(t(b.res[["quantile"]][,3]),t(b.res[["other.stats"]][,2])) #rbind(bs.agg.X,c(t(b.res[["quantile"]][,3]),t(b.res[["other.stats"]][,2])))
    bs.agg.S <- c(t(b.res[["quantile"]][,4]),t(b.res[["other.stats"]][,3])) #rbind(bs.agg.S,c(t(b.res[["quantile"]][,4]),t(b.res[["other.stats"]][,3])))
    bs.agg.WS1 <- c(t(b.res[["quantile"]][,5]),t(b.res[["other.stats"]][,4])) #rbind(bs.agg.WS1,c(t(b.res[["quantile"]][,5]),t(b.res[["other.stats"]][,4])))
    bs.agg.WSother <- c(t(b.res[["quantile"]][,6]),t(b.res[["other.stats"]][,5])) #rbind(bs.agg.WSother,c(t(b.res[["quantile"]][,6]),t(b.res[["other.stats"]][,5])))
    
    bs.WS.marginal <- c(t(b.deco[[1]][,2]),t(b.deco[[2]][,1])) # rbind(bs.WS.marginal,c(t(b.deco[[1]][,2]),t(b.deco[[2]][,1])))
    bs.WS.sorting <- c(t(b.deco[[1]][,3]),t(b.deco[[2]][,2])) #rbind(bs.WS.sorting,c(t(b.deco[[1]][,3]),t(b.deco[[2]][,2])))
    bs.WS.wage_structure <- c(t(b.deco[[1]][,4]),t(b.deco[[2]][,3])) #rbind(bs.WS.wage_structure,c(t(b.deco[[1]][,4]),t(b.deco[[2]][,3])))
    
    #bs.W.stats[[j]] <- b.tabW
    bs.W.stats <- b.tabW
    
    return(list( bs.agg.O,
          bs.agg.X,
          bs.agg.S,
          bs.agg.WS1,
          bs.agg.WSother,
          bs.WS.marginal,
          bs.WS.sorting,
          bs.WS.wage_structure,
          bs.W.stats))
          
    
    # Continuously save bootstrap results
    # save(bs.agg.O,
    #      bs.agg.X,
    #      bs.agg.S,
    #      bs.agg.WS1,
    #      bs.agg.WSother,
    #      bs.WS.marginal,
    #      bs.WS.sorting,
    #      bs.WS.wage_structure,
    #      bs.W.stats,
    #      file=fnBS)
    
    }  ### Boot function 
  #}  ### End interation loop
  
  
  ## Apply bootit function to multiple cores
  numCores <- min(2,ncores) #detectCores()-6
  cl <- makeCluster(numCores)
  # Shift data and functions to clusters
  clusterExport(cl, "lse_comp")
  clusterExport(cl, "lse_ref")
  clusterExport(cl, "bs_comp")    
  clusterExport(cl, "bs_wadj_comp")
  clusterExport(cl, "bs_ref")
  clusterExport(cl, "bs_wadj_ref")
  clusterExport(cl, "wd")
  clusterExport(cl, "wd_results")
  clusterExport(cl, "m.DFL.detail")
  clusterExport(cl, "m.DFL.ws")
  clusterExport(cl, "tau_rw")
  clusterExport(cl, "bootit")
  clusterEvalQ(cl, {
            setwd(wd)
            source("functions/r-DFL-deco-2-0.R")
            setwd(wd_results)
  })
  
  # Perform actual bs selection with parLapply
  it <- length(bs_comp)
  #bsresults <- parLapply(cl,1:it,bootit)
  bsresults <- pblapply(1:it,bootit, cl=cl)
  
  # Stop cluster
  stopCluster(cl)
 
  #Export results
  bs.agg.O <-  matrix(unlist(lapply(bsresults, function(x) x[[1]])),byrow=TRUE,nrow=it)
  bs.agg.X <-  matrix(unlist(lapply(bsresults, function(x) x[[2]])),byrow=TRUE,nrow=it)
  bs.agg.S <-  matrix(unlist(lapply(bsresults, function(x) x[[3]])),byrow=TRUE,nrow=it)
  bs.agg.WS1 <-  matrix(unlist(lapply(bsresults, function(x) x[[4]])),byrow=TRUE,nrow=it)
  bs.agg.WSother <-  matrix(unlist(lapply(bsresults, function(x) x[[5]])),byrow=TRUE,nrow=it)
  bs.WS.marginal <-  matrix(unlist(lapply(bsresults, function(x) x[[6]])),byrow=TRUE,nrow=it)
  bs.WS.sorting  <-  matrix(unlist(lapply(bsresults, function(x) x[[7]])),byrow=TRUE,nrow=it)
  bs.WS.wage_structure <-  matrix(unlist(lapply(bsresults, function(x) x[[8]])),byrow=TRUE,nrow=it)
  bs.W.stats <-  lapply(bsresults, function(x) x[[9]])
  
  #Compute standard errors and store to other results
  se.agg <- cbind(sqrt(diag(var(bs.agg.O))),
                  sqrt(diag(var(bs.agg.X))),
                  sqrt(diag(var(bs.agg.S))),
                  sqrt(diag(var(bs.agg.WS1))),
                  sqrt(diag(var(bs.agg.WSother))))
  
  se <- cbind(sqrt(diag(var(bs.WS.marginal))),
                       sqrt(diag(var(bs.WS.sorting))),
                       sqrt(diag(var(bs.WS.wage_structure))))
  
  se.quantile.agg <- cbind(tau_rw,se.agg[1:length(tau_rw),])
  se.other.stats.agg <- se.agg[(length(tau_rw)+1):nrow(se.agg),]
  se.quantile <- cbind(tau_rw,se[1:length(tau_rw),])
  se.other.stats <- se[(length(tau_rw)+1):nrow(se),]
  
  colnames(se.quantile.agg) <- colnames(res$quantile)
  colnames(se.other.stats.agg) <- colnames(res$other.stats)
  colnames(se.quantile) <- colnames(deco$quantile)
  colnames(se.other.stats) <- colnames(deco$other.stats)
  
  se.tabW <- NULL
  for(k in 1:length(bs.W.stats[[1]])){
  se.tabW <- c(se.tabW,sqrt(var(unlist(lapply(bs.W.stats, function(x) x[k])))))
  }  
  se.tabW <- matrix(se.tabW,ncol=ncol(bs.W.stats[[1]]),byrow=FALSE)
  
  bootstrap.se <- list(se.quantile.agg,
                       se.other.stats.agg,
                       se.quantile,
                       se.other.stats,
                       se.tabW)
  
  setwd(wd_results)
  save(res,deco,tabW,bootstrap.se,bsresults,file=fn)
  setwd(wd)
  
  # Remove unsed objects
  rm(bs_comp, bs_ref, bs_wadj_comp, bs_wadj_ref)
  
  toc()
  
}  ### End bootstrap loop

  
#pryr::mem_used()
  
  













