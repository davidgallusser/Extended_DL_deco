############################################
###### DFL decomposition function  #########
############################################

## Version 2.0, 26. Juni 2019

## Note: 
## Group 1 is the reference group. Its wage structure 
# is used for the counterfactuals.
# In order to sequentially decompose the composition effect, 
# variable have to be entered seperated by |
# If sequence="marginal" the marginal of the last variable entered
# is reweighted first.
# If firstrw="conditional" the conditional distribution of the first
# variable entered in the formula is reweigted first.

# library(AER)
# data("CPS1985")
# f <- wage ~ gender | experience | education | region

# result <- dfl_deco(f, data=CPS1985, group=union, tau=c(0.5,0.75,0.8), log.trans=FALSE, trim=TRUE)
# result$quantile
# result$other.stats

###########################################################
## Package to be loaded
require(Formula) #for extended formulas
#require(reldist) #for weighted distributional statistics (i.e. quantiles, gini, etc.)
require(Hmisc) #for weighted distributional statistics (i.e. quantiles, etc.)
require(survey) #for glm models with survey data
require(ggplot2) #for the plotting function
require(reshape2) #for reshaping the data.frames
require(fastglm) #for optimzed glm solving algorithm


###########################################################
# Actual decomposition function  

dfl_deco <- function(formula, data, weights, group, 
                     ws_formula=NULL,
                     reference=c(1,0), 
                     na.action = na.exclude, 
                     tau=c(10,50,90,99)/100, 
                     firstrw=c("marginal","conditional"),
                     stats=TRUE,
                     all.stats=FALSE,
                     log.trans=TRUE,
                     fast=TRUE,
                     trim=FALSE){
  
  # group: indicator variable defining the distribution to be compared
  # reference: defining the group which will be reweighted, i.e. which wage structure is to be used. 
  #            Composition effect: Difference between observed reference group and reweighted reference group
  #            Wage structure effect: Difference between reweighted reference group and comparison group
  # tau: quantiles to be evaluated
  # firstrw=conditional: sequential decompositon by reweighting conditional distribution
  #            of first entered covariate first, otherwise marginal distribution of
  #            last covariate is reweigted fist. 
  # fast:      estimation with fastglm
  # trim:      automatically trims dataset in order to discard observation with very large propensities
  
  ##########################################################
  ##########################################################
  ## 1) Set up data
  
  #Use match.call function to call data.vectors
  mf = match.call()
  m = match(c("formula", "data", "weights", "na.action","group"), names(mf), 0)
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE
  
  # Retrieve formula as.Formula and model.frame
  f <- as.Formula(formula)
  
  # Make sure that all variables (incl. for detailed WS effect are selected)
  if(!is.null(ws_formula)){
    fws <- Formula(ws_formula)
    f <- update(f,as.Formula(paste(". ~ . + ", as.character(fws, lhs=1)[2], "+", as.character(fws, rhs=1)[3])))
  }
  
  mf[[1]] = as.name("model.frame")
  mf$formula <- f
  mf = eval.parent(mf)
  mt = attr(mf, "terms")
  
  # Store "orignal" decomposition model again
  f <- as.Formula(formula)
  
  # Extract variables, weights and group identifier
  #reg = get_all_vars(mt, mf)
  dep = model.response(mf, "numeric")
  weight = model.weights(mf)
  if (!is.null(weight) && !is.numeric(weight)) {
    stop("'weights' must be a numeric vector")
  }
  if (!is.null(weight)) {
    weight = weight
  }
  else {
    weight = rep(1, length(dep))
  }
  
  #weight = weight/sum(weight)
  groupN = mf[, ncol(mf)]
  #reg$groupN <- groupN
  mf$groupN <- groupN
  
  # Number of covariates for decomposition of composition effect
  nvar = length(f)[2]
  
  # Set reference group to 1 if not determined
  if(length(reference)==2){reference=1}
  cat("Reference group (to be reweighted): ", reference,"\n \n") 
  
  # If first reweighting not specified choose marginal first as default
  firstrw = ifelse(length(firstrw)==2,"marginal",firstrw)
  
  # Set Progress Bar 
  cat("Probit estimation...\n")
  pb <- txtProgressBar(min=0,max=1, style=3)
  nmods <- 1+nvar+ifelse(!is.null(ws_formula),4,0)
  
  ##########################################################
  ##########################################################
  ### 2) Fit probit models & estimate reweighting factors
  
  ##########################################################
  ### Unconditional/Sample probabilities
  mod <- groupN ~ 1
  p1 <- mean(pfit(mod,mf,weight))
  p0 <- 1-p1
  probs <- rep(p0/p1,nrow(mf))
  setTxtProgressBar(pb,1/nmods) 
  
  ##########################################################
  ### Conditional probabilities
  ### Beginning with the LAST variable entered in Formula,
  ### estimating then with prob
  
  m <- 0 # only for ProgressBar
  
  for(i in nvar:1){
    
  mod <- update(formula(f, rhs=nvar:i, collapse=TRUE), groupN ~ .)
  p1 <- pfit(mod,mf,weight)
  p0 <- 1-p1
  probs <- cbind(probs,p0/p1) 

  m <- m + 1 #Progressbar
  setTxtProgressBar(pb,(1+m)/nmods) 
  }
  
  ##########################################################
  ### Reweigthing factor
  ###
  ### psi_s contains relative probabilites: 
  ### first [P(t=1)/P(P=0)]*[P(t=1|X1)/P(P=0|X1)],
  ### second [P(t=0|X1)/P(P=1|X1)]*[P(t=1|X2,X1)/P(P=0|X2,X1)]
  ### etc.
  ### X1 is the variable entered last, X2 the variable entered second last etc.
  
  psi_s <- (probs[,1]^-1)*probs[,2]
  if(nvar==1){
  #if there is only one variable stick to psi_s and use it as weight
   psi <- as.matrix(psi_s)
  }
  else{
  #procedure to compute the weights if nvar>1
  for(i in 2:nvar){
    psi_s <- cbind(psi_s,(probs[,i]^-1)*probs[,i+1])
  }
  
  # Marginal of last entered variable or conditional of first entered 
  # variable to be reweighted first?
  first <- ifelse(firstrw=="marginal",1,nvar)
  last <- ifelse(firstrw=="marginal",nvar,1)
  correct <- ifelse(firstrw=="marginal",1,-1)
  loopvals <- seq(first+correct,last)
  
  # psi contains the weights actually used to reweighted the distribution
  # psi contains on the first position the weight used first
  # (i.e. [P(t=1)/P(P=0)]*[P(t=1|X1)/P(P=0|X1)] if the "marginal" and
  # [P(t=1|X1,...,X(M-1))/P(P=0||X1,...,X(M-1))]*[P(t=1|X1,...,X(M))/P(P=0|X1,...,X(M))]
  # if "conditional")
  psi <- as.matrix(psi_s[,first])
  colnames(psi) <- paste("psi_X",first,sep="")
  
  for(i in loopvals){
    psi <- cbind(psi,apply(psi_s[,first:i],1,prod))
    colnames(psi)[length(colnames(psi))] <- paste("psi_X",first,"to",i,sep="")
  }
  
  # Remove unused object
  rm(psi_s) 
  
  #end weight competion procedure if nvar>1
  }
  
  ###########################################################
  ### Weights for decomposition of wage structure effect
  
  if(!is.null(ws_formula)){
    
  # What's the group variable value of reference group?
  if(is.factor(groupN)){
    group_val <- levels(groupN)[1+reference]
  } else {
    group_val = reference
  }
    
  # Set up model for unconditional prob
  mod <- as.Formula(paste(as.character(fws, lhs=1)[2],"~ 1"))
  p_S_1 <- pfit(mod,mf[which(groupN==group_val),],weight[which(groupN==group_val)])  
  p_S_0 <- pfit(mod,mf[which(groupN!=group_val),],weight[which(groupN!=group_val)]) 
  
  setTxtProgressBar(pb,(nmods-2)/nmods)
  # Model for conditional prob
  mod <- fws 
  p_S_X_1 <- pfit(mod,mf[which(groupN==group_val),],weight[which(groupN==group_val)]) 
  p_S_X_0 <- pfit(mod,mf[which(groupN!=group_val),],weight[which(groupN!=group_val)]) 
  
  psi_S_1 <- (1-p_S_1)/(1-p_S_X_1)
  psi_S_0 <- (1-p_S_0)/(1-p_S_X_0)
  
  # Add wage structure weight to main data.frame
  psi_S <- rep(1,nrow(mf))
  select <- which(groupN==group_val)
  psi_S[select] <- psi_S_1
  select <- which(groupN!=group_val)
  psi_S[select] <- psi_S_0
  

  # set text bar
  setTxtProgressBar(pb,1)
  }
  
  cat("\n\n")
  
  ###########################################################
  ###########################################################
  ### Compute decomposition terms
  
  untrimmed <- NULL
  trimshare <- 0
  
  ##########################################################
  # compute decomposition if stats==TRUE
  if(stats==TRUE){
  

  ##########################################################  
  # Trimming of weights beeing to large
  if(trim==TRUE){
  
   
  #trim main reweighting factor  
  trimselect <- trimming(psi[,nvar],groupN,reference)
  
  #trim ws deco rw factor if required
  if(!is.null(ws_formula)){
  #Find ws variable
  ws_var <- as.character(fws, lhs=1)[2]
  select <- which(groupN==group_val)
  trimselect[select] <- trimming(psi_S[select],mf[select,ws_var],0)
  select <- which(groupN!=group_val)
  trimselect[select] <- trimming(psi_S[select],mf[select,ws_var],0)
  }
  
  #Compute trim stat:
  trimshare <- 1- sum(trimselect)/length(trimselect)
  
  if(trimshare>0){
  # Prepare to export untrimmed data
  untrimmed <- data.frame(trimselect,dep,weight,groupN,psi)
  
  # Create trimmted data
  dep <- dep[which(trimselect==1)]
  weight <- weight[which(trimselect==1)]
  groupN <- groupN[which(trimselect==1)]
  psi <- as.matrix(psi[which(trimselect==1),])
  
  if(!is.null(ws_formula)){
  untrimmed <- cbind(untrimmed,mf[,ws_var],psi_S)
  names(untrimmend)[ncol(untrimmed)-1] <- "ws_var"
  psi_S <- psi[which(trimselect==1)]
    }
  }

  }
  
  ##########################################################
  ### Observed distributions
  F1 <- stat(dep,weight,groupN,group=1,rwfactor=rep(1,length(weight)),
             tau=tau, all.stats=all.stats, log.trans=log.trans)
  F0 <- stat(dep,weight,groupN,group=0,rwfactor=rep(1,length(weight)), 
             tau=tau, all.stats=all.stats, log.trans=log.trans)

  
  ##########################################################
  # Counterfactual distribution(s)
  
  #if reference group==0 all rw factors are the inverse of the computed
  pow <- ifelse(reference==1,1,-1) 
  first <- ifelse(reference==1,1,nvar)
  last <- ifelse(reference==1,nvar,1)
  loopvals <- seq(first,last)  
  
  FC <- NULL
  for(i in 1:nvar){
    FC <- cbind(FC,stat(dep,weight,groupN,group=reference,rwfactor=psi[,i]^pow,
                        tau=tau,all.stats=all.stats, log.trans=log.trans))
  }
  
  if(nvar==1){
    FC <- as.matrix(FC)
  }

  ##########################################################
  # Decomposition of aggregate effects and detailed composition effects
  Delta <- cbind(F1 - F0, F1 - FC[,nvar], FC[,nvar] - F0)
  if(reference==1){
  colnames(Delta) <- c("Delta_O","Delta_X","Delta_S")
  } else {
  colnames(Delta) <- c("Delta_O","Delta_S","Delta_X")
  }
  
  if(nvar>1){
  if(reference==1){
      Delta <- cbind(Delta,F1-FC[,1])
      colnames(Delta)[length(colnames(Delta))] <- paste("Delta_X",1,sep="")
      
      for(i in 2:nvar){
      Delta <- cbind(Delta,FC[,i-1]-FC[,i])
      colnames(Delta)[length(colnames(Delta))] <- paste("Delta_X",i,sep="")

      }
  } else {
      for(i in nvar:2){
      Delta <- cbind(Delta,FC[,i]-FC[,i-1])
      colnames(Delta)[length(colnames(Delta))] <- paste("Delta_X",i,sep="")
      
      }
      Delta <- cbind(Delta,FC[,1]-F0) 
      colnames(Delta)[length(colnames(Delta))] <- paste("Delta_X",1,sep="")
    }
  }
 
  
  ###########################################################
  ### Decomposition of wage structure effect
  
  if(!is.null(ws_formula)){
    
  ws_var <- as.character(fws, lhs=1)[2]
  ws_var_val <- 1
  if(is.factor(mf[,ws_var])){
    ws_var_val <- levels(mf[,ws_var])[2]
  }
  
  # Select only observations withs ws group 0
  select <- which(mf[,ws_var]!=ws_var_val)
  
  # Compute counterfactual values  
  FCW1 <- stat(dep[select],weight[select],groupN[select],group=1,rwfactor=psi_S[select],
               tau=tau, all.stats=all.stats, log.trans=log.trans)
  FCW0 <- stat(dep[select],weight[select],groupN[select],group=0,rwfactor=psi_S[select], 
               tau=tau, all.stats=all.stats, log.trans=log.trans) 
  
  Delta_WS_X1 <- (F1-FCW1) - (F0-FCW0)
  Delta_WS_other <- Delta[,ifelse(reference==1,3,2)] - Delta_WS_X1
  
  Delta <- cbind(Delta,Delta_WS_X1,Delta_WS_other)
  
  }
  
  ##########################################################
  # Prepare results of decomposition for export
  quantile=cbind(tau,Delta[1:length(tau),]) 
  other.stats=Delta[(length(tau)+1):nrow(Delta),]
  
  } else {
  ##########################################################
  #if no stats return empty objects
  quantile=NULL
  other.stats=NULL
  }
 
  ##########################################################
  ### Export results
  res <- list(quantile=quantile,
              other.stats=other.stats,
              formula=formula, 
              mf=mf, 
              weight=weight,
              psi=psi, 
              reference=reference, 
              tau=tau,
              firstrw=firstrw,
              all.stats=all.stats,
              log.trans=log.trans,
              untrimmed=untrimmed,
              trimshare=trimshare)
  
  if(!is.null(ws_formula)){
    # Add WS weights to the weight matrix for export
    psi <- cbind(psi, psi_S)
    res <- list(quantile=quantile,
                other.stats=other.stats,
                formula=formula, 
                mf=mf, 
                weight=weight,
                psi=psi, 
                reference=reference, 
                tau=tau,
                firstrw=firstrw,
                formula.rw=fws,
                all.stats=all.stats,
                log.trans=log.trans,
                untrimmed=untrimmed,
                trimshare=trimshare)
  }
  
  
  return(res)   
  
}

#############################################################
### Function that decomposes the wage structure effect of
### DiNardo/Lemieux 1997
dfl_deco_ws <- function(res){
  
  # Extract results
  mf <- res$mf
  weight <- res$weight
  groupN <- mf[,"groupN"]
  reference <- res$reference
  
  # What's the group variable value of reference group?
  if(is.factor(groupN)){
    group_val <- levels(groupN)[1+reference]
  } else {
    group_val = reference
  }
  
  # Wage structure effect variable
  fws <- res$formula.rw
  if(is.null(fws)){stop("No wage structure decomposition in first place")}
  ws_var <- as.character(fws, lhs=1)[2]
  ws_var_val <- 1
  if(is.factor(mf[,ws_var])){
    ws_var_val <- levels(mf[,ws_var])[2]
  }
  
  # Retrieve rw factors
  psi_X <- res$psi[,ncol(res$psi)-1]
  psi_S <- res$psi[,ncol(res$psi)]
  
  # Other parameter
  tau <- res$tau
  all.stats=res$all.stats
  log.trans=res$log.trans
  
  ###########################################
  # Select group 1 and 2 observation as well
  # as observation which are NOT in group
  # for which detaild ws effect is estimated
  
  select1 <- which(groupN==group_val)
  select0 <- which(groupN!=group_val)
  selectWS <- which(mf[,ws_var]!=ws_var_val)
  selectWS1 <- intersect(selectWS,select1)
  selectWS0 <- intersect(selectWS,select0)
  selectNonWS1 <- intersect(which(mf[,ws_var]==ws_var_val),select1)
  
  ###########################################
  # Estimations
  
  # Set up model for unconditional prob
  mod <- as.Formula(paste(as.character(fws, lhs=1)[2],"~ 1"))
  p_S_0 <- mean(pfit(mod,mf[select0,],weight[select0]))
  p_S_1 <- mean(pfit(mod,mf[select1,],weight[select1]))  
  p_S_1.C <- mean(pfit(mod,mf[select1,],weight[select1]*psi_X[select1]))  

  # Model for conditional prob
  mod <- fws 
  p_S_X_1.C <- pfit(mod,mf[select1,],weight[select1]*psi_X[select1]) 
  
  #Marginal weights
  psi_S_1.M <- rep(1,nrow(mf))
  psi_S_1.M[selectNonWS1] <- p_S_0/p_S_1
  
  p_S_X_1 <- 1-(1-p_S_1)/psi_S[selectWS1] #can be retrieved from the WS factor for psi_1,S1
  #psi_S_1.M[selectWS1] <- psi_S[selectWS1]*(1 - (p_S_0/p_S_1)*p_S_X_1)
  psi_S_1.M[selectWS1] <- (psi_S[selectWS1]/(1-p_S_1))*(1 - (p_S_0/p_S_1)*p_S_X_1)
  psi_S_1.M.all <-  psi_S_1.M
  
  #Sorting weights
  psi_S_1.J <- (1-p_S_1.C)/(1-p_S_X_1.C)
  #psi_S_0 <- (1-p_S_0)/(1-p_S_X_0)
  
  # Add wage structure weight to main data.frame
  #psi_S_1.M.all <- rep(1,nrow(mf))
  #psi_S_1.M.all[select1] <-  psi_S_1.M
  
  psi_S_1.J.all <- rep(1,nrow(mf))
  psi_S_1.J.all[select1] <- psi_S_1.J
  #select <- which(groupN!=group_val)
  #psi_S[select] <- psi_S_0
  
  ###########################################
  # Statistics
  
  # Reference group
  pow <- ifelse(reference==1,1,-1)
  dep <- mf[,1]
  
  ### Observed distributions
  F1 <- stat(dep,weight,groupN,group=1,rwfactor=rep(1,length(weight)),
             tau=tau, all.stats=all.stats, log.trans=log.trans)
  F0 <- stat(dep,weight,groupN,group=0,rwfactor=rep(1,length(weight)), 
             tau=tau, all.stats=all.stats, log.trans=log.trans)
  
  ### Counterfactual 1: Aggregate counterfactual
  FC1 <- stat(dep,weight,groupN,group=reference,rwfactor=psi_X^pow,
                     tau=tau,all.stats=all.stats, log.trans=log.trans)
  
  ### Counterfactual 2&3: Wage structure counterfactual like in DiNardo&Lemieux 1997
  FCW1 <- stat(dep[selectWS],weight[selectWS],groupN[selectWS],group=1,rwfactor=psi_S[selectWS],
               tau=tau, all.stats=all.stats, log.trans=log.trans)
  FCW0 <- stat(dep[selectWS],weight[selectWS],groupN[selectWS],group=0,rwfactor=psi_S[selectWS], 
               tau=tau, all.stats=all.stats, log.trans=log.trans) 
  
  ### Counterfactual 4: FCW0 with wage structure of t=1 
  FC1J <- stat(dep[selectWS],weight[selectWS],groupN[selectWS],group=1,rwfactor=psi_X[selectWS]*psi_S_1.J.all[selectWS],
               tau=tau, all.stats=all.stats, log.trans=log.trans)
  
  ### Counterfactual 5: Distribution in t=1 with marginal x1 like in t=0 but x2 like in t=1
  FC1M <- stat(dep,weight,groupN,group=1,rwfactor=psi_S_1.M.all,
               tau=tau, all.stats=all.stats, log.trans=log.trans)
  
  ###########################################
  ### Decomposition: 
  Delta_Marginal <- F1 - FC1M
  Delta_Sorting <- (FC1M - FCW1) - (FC1 -  FC1J)
  Delta_WS <- (FC1 - FC1J) - (F0 -  FCW0)

  l <- 1:length(tau)
  quantile <- data.frame(tau=tau,
                            marginal=Delta_Marginal[l],
                            sorting=Delta_Sorting[l],   
                            wage_structure=Delta_WS[l])
  
  l <- (length(tau)+1):length(Delta_Sorting)
  other.stats <- data.frame(marginal=Delta_Marginal[l],
                            sorting=Delta_Sorting[l],
                            wage_structure=Delta_WS[l])
  
  ###########################################
  ### Return results 
  res <- list(quantile=quantile,
              other.stats=other.stats,
              psi_S1.1=psi_S[selectWS1],
              psi_S1.0=psi_S[selectWS0],
              #psi_M=psi_S_1.M.all[selectWS1],
              psi_M=psi_S_1.M.all,
              psi_J=psi_S_1.J.all[selectWS1]
              )
  
}


#############################################################
### Plot function for composition effect results

dfl_deco_plot <- function(result,type=c(1,2,3)){
  
  result <- result[["quantile"]]
  
  if(type==1|ncol(result)==4){
    ## type 1: Observed difference and main decomposition terms (S,X)
    diff <- as.data.frame(result[,c(1:4)])
    
  } else if(type==2){
    ## type 2: All individual terms besides observed difference
    diff <- as.data.frame(result[,-2])
    
  } else{
    ## type 3: Only detailed terms
    diff <- as.data.frame(result[,-c(2:4)])  
  } 
  diff <- melt(diff, id.vars="tau", measure.vars = names(diff)[-1], variable.name= "effect", value.name="delta")
  plot <- ggplot(diff, aes(tau,delta, colour = effect)) + geom_hline(yintercept = 0, colour="grey")  + geom_line()+geom_point(aes(shape=effect, color=effect)) + scale_shape_manual(values=c(15:20,0:14,15:20,0:14))
  
  return(plot)
}

#############################################################
### DFL deco: Counterfactual sorting condtional on x_1

counter_cond <- function(formula, data, weights, group, 
                     na.action = na.exclude, 
                     tau=c(10,50,90,99)/100,
                     all.stats=FALSE,
                     log.trans=TRUE){

  ##########################################################
  ##########################################################
  ## 1) Set up data
  
  #Use match.call function to call data.vectors
  mf = match.call()
  m = match(c("formula", "data", "weights", "na.action","group"), names(mf), 0)
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE
  
  # Retrieve formula as.Formula and model.frame
  f <- as.Formula(formula)
  if(length(f)[2] < 2) stop("Define a grouping variable!")
  
  # Extract model.frame
  mf[[1]] = as.name("model.frame")
  mf$formula <- f
  mf = eval.parent(mf)
  mt = attr(mf, "terms")
  
  # Extract variables, weights and group identifier
  #reg = get_all_vars(mt, mf)
  dep = model.response(mf, "numeric")
  weight = model.weights(mf)
  if (!is.null(weight) && !is.numeric(weight)) {
    stop("'weights' must be a numeric vector")
  }
  if (!is.null(weight)) {
    weight = weight
  } else {
    weight = rep(1, length(dep))
  }
  
  #weight = weight/sum(weight)
  groupN = mf[, ncol(mf)]
  #reg$groupN <- groupN
  mf$groupN <- groupN
  
  # Number of covariates for decomposition of composition effect
  nvar = length(f)[2]
  
  reference=1
  
  ##########################################################
  ##########################################################
  ## 2) Extracting reference group value of main decomposition variable
  ##    and conditiong variable's levels
  
  # What's the group variable value of reference group? [Maid decomposition]
  if(is.factor(groupN)){
    group_val <- levels(groupN)[1+reference]
  } else {
    group_val = reference
  }
  
  # What is the name of the conditioning var?
  cond_var <- as.character(update(formula(f, rhs=nvar, collapse=TRUE), . ~ .))[3]
  mf$cond_var <- mf[,cond_var]
  if(is.factor(mf[,cond_var])){
    group_val_cond <- levels(mf[,cond_var])[2]
  } else {
    group_val_cond = 1
  }
  
  ##########################################################
  ##########################################################
  ## 3) Compute reweighting factor for scenario:
  ## What would distribution look like if distribution of x_2|x_1 
  ## was like in t' but marginal distribution of x_1 and x_2 was like in t? 
  
  ##################################
  ## Set the progress bar
  cat("Probit estimation...\n")
  pb <- txtProgressBar(min=0,max=1, style=3)
  nmods <- 12
  
  ##################################
  ## 1) RW factor for x1=1: Main decomposition only in group x_1=1
  mod <- update(formula(f, rhs=1:(nvar-1), collapse=TRUE), groupN ~ .)
  select <- which(mf[,"cond_var"]==group_val_cond)
  p_t_x2x1.0 <- pfit(mod,mf[select,],
                   weight[select],
                   newdata = mf)
  
  mod <- groupN ~ 1
  p_t_x1.0 <- mean(pfit(mod,mf[select,],
                        weight[select]))
  
  PsiA <- ((1-p_t_x2x1.0)/p_t_x2x1.0)*((p_t_x1.0)/(1-p_t_x1.0))
  
  # PB
  setTxtProgressBar(pb,2/nmods) 
  
  ##################################
  ## 2) Reweigting factor B for x1=0
  
  #P(t)
  mod <- groupN ~ 1
  p_t.1 <- mean(pfit(mod,mf,weight))
  
  #P(t|x'1)
  mod <- groupN ~ 1  
  select <-  which(mf[,"cond_var"]!=group_val_cond)
  p_t.1_x1.0 <- mean(pfit(mod,mf[select,],weight[select]))
  
  #P(t|x2)
  mod <- update(formula(f, rhs=1:(nvar-1), collapse=TRUE), groupN ~ .)
  p_t.1_x2 <- pfit(mod,mf,
                   weight,
                   newdata = mf)
  
  
  # PB
  setTxtProgressBar(pb,5/nmods) 
  
  #P(t|x2,x'1)
  mod <- update(formula(f, rhs=1:(nvar-1), collapse=TRUE), groupN ~ .)
  select <- which(mf[,"cond_var"]!=group_val_cond)
  p_t.1_x2x1.0 <- pfit(mod,mf[select,],
                       weight[select],
                       newdata = mf)
  
  # PB
  setTxtProgressBar(pb,6/nmods) 
  
  #P(x'1)
  mod <- cond_var ~ 1  
  p_x1.0 <- 1-mean(pfit(mod,mf,weight))
  
  #P(x'1|x2)
  mod <- as.character(update(formula(f, rhs=1:(nvar-1), collapse=TRUE), . ~ .))[3]
  mod <- as.formula(paste(cond_var,"~",mod,sep=""))
  p_x1.0_x2 <- 1-pfit(mod,mf,
                      weight,
                      newdata = mf)
  
  # PB
  setTxtProgressBar(pb,8/nmods) 
  
  
  # The factor
  #          P(t|x2)    P(t|x'1)    P(x'1)
  # PsiB = ----------- ---------- ----------
  #        P(t|x2,x'1)    P(t)     P(x'1|x2)
  
  PsiB = (p_t.1_x2/p_t.1_x2x1.0)*
         (p_t.1_x1.0/p_t.1)*
         (p_x1.0/p_x1.0_x2)
  
  
  ##################################
  ## 3) Reweigting factor C for x1=0
  
  #P(t) (computed above:  p_t.1)
  #P(t') 
  
  #P(x1|t)
  #P(x'1|t)
  mod <- cond_var ~ 1 
  select <- which(groupN==group_val)
  p_x1.1_t.1 <- mean(pfit(mod,mf[select,],weight[select]))
  
  
  #P(x1|t')
  mod <- cond_var ~ 1 
  select <- which(groupN!=group_val)
  p_x1.1_t.0 <- mean(pfit(mod,mf[select,],weight[select]))
  
  # PB
  setTxtProgressBar(pb,10/nmods) 
  
  #P(t|x2) (computed above:  p_t.1_x2)
  #P(t'|x2)
  
  #P(x1|x2,t')
  mod <- as.character(update(formula(f, rhs=1:(nvar-1), collapse=TRUE), . ~ .))[3]
  mod <- as.formula(paste(cond_var,"~",mod,sep=""))
  select <- which(groupN!=group_val)
  p_x1.1_x2t.0 <- pfit(mod,mf[select,],weight[select],
                       newdata = mf)
  
  #P(x'1|x2,t)
  mod <- as.character(update(formula(f, rhs=1:(nvar-1), collapse=TRUE), . ~ .))[3]
  mod <- as.formula(paste(cond_var,"~",mod,sep=""))
  select <- which(groupN==group_val)
  p_x1.0_x2t.1 <-1-pfit(mod,mf[select,],weight[select],
                        newdata = mf)
  
  # PB
  setTxtProgressBar(pb,12/nmods) 
  cat("\n")
  
  # The factor
  #                P(x1|x2,t') P(x'1|t) P(t'|x2) P(t)
  # PsiC = P(x1|t) ----------- -------- -------- ----
  #                P(x'1|x2,t) P(x1|t') P(t|x2) P(t')
  
  PsiC <- p_x1.1_t.1*(p_x1.1_x2t.0/p_x1.0_x2t.1)*
                     ((1-p_t.1_x2)/p_t.1_x2)*
                     ((1-p_x1.1_t.1)/p_x1.1_t.0)*
                     (p_t.1/(1-p_t.1))  
  
  
  ##########################################################
  ##########################################################
  ## 4) Contruct weights for second counterfactual: 
  ## What is the wage distribution if not only x_2|x_1 was like in t' but 
  ## also the marginal of x_1 while the marginal of x_2 was still like in t.
  
  PsiA.2 <- (p_x1.1_t.0/p_x1.1_t.1) * PsiA
  PsiB.2 <- PsiB
  PsiC.2 <- (p_x1.1_t.0/p_x1.1_t.1) * PsiC
  
  ##################################
  ## 4) Add weights two mf
  
  mf$psi <- NA
  mf$psi.2 <- NA
  select <- which(mf$groupN==group_val&mf$cond_var==group_val_cond) 
  mf[select,"psi"]   <- PsiA[select]
  mf[select,"psi.2"]   <- PsiA.2[select]
  select <- which(mf$groupN==group_val&mf$cond_var!=group_val_cond) 
  mf[select,"psi"]   <- PsiB[select]-PsiC[select]
  mf[select,"psi.2"]   <- PsiB.2[select]-PsiC.2[select]
  
  #Return stats about psi
  cat("Summary of psi:\n")
  print(summary(mf$psi))
  cat("\nShare of negative weights:\n")
  cat(length(mf$psi[which(mf$psi<0)])/length(mf$psi[which(is.na(mf$psi)==FALSE)]),"\n")
  
  cat("\nSummary of psi.2:\n")
  print(summary(mf$psi.2))
  cat("\nShare of negative weights:\n")
  cat(length(mf$psi.2[which(mf$psi.2<0)])/length(mf$psi.2[which(is.na(mf$psi.2)==FALSE)]),"\n")
  
  ##########################################################
  ##########################################################
  ## 5) Create reweighting factor for group x=0 in t=1
  
  F1 <- stat(dep,weight,groupN,group=1,rwfactor=rep(1,length(weight)), 
             tau=tau, all.stats=all.stats, log.trans=log.trans)
  FC.1 <- stat(dep,weight,groupN,group=1,rwfactor=mf$psi, 
               tau=tau, all.stats=all.stats, log.trans=log.trans)
  FC.2 <- stat(dep,weight,groupN,group=1,rwfactor=mf$psi.2,
               tau=tau, all.stats=all.stats, log.trans=log.trans)
  
  Delta <- cbind(F1-FC.1,FC.1-FC.2)
  colnames(Delta) <- c("Delta_Sorting_X1","Delta_Marginal_X1")
  
  quantile=cbind(tau,Delta[1:length(tau),]) 
  other.stats=Delta[(length(tau)+1):nrow(Delta),]
  
  res <- list(quantile=quantile,
              other.stats=other.stats,
              formula=f, 
              mf=mf, 
              weight=weight,
              psi=mf$psi, 
              psi.2=mf$psi.2, 
              reference=reference, 
              tau=tau)
  return(res)   
  
}

#############################################################
### dfl_diag():
### Diagnosis tool to compare covariates distribution
### of actual and reweighted distribution

dfl_diag <- function(result, compareRef=FALSE, psi.2=FALSE){

  #model and reference group
  f <- as.Formula(result$formula)
  reference <- result$reference
  
  #data
  mf <- result$mf
  weight <- result$weight

  #weights
  if(psi.2==FALSE){
  psi <- as.matrix(result$psi)  
  } else {
  psi <- as.matrix(result$psi.2) 
  }
  # Select psi 
  if(ncol(psi)==1){
  psi <- psi[,1]
  } else if(colnames(psi)[ncol(psi)]=="psi_S"){
  psi <- psi[,ncol(psi)-1]  
  } else {
  psi <- psi[,ncol(psi)]
  }
  
  # Select observations of reference group
  if(is.factor(mf$groupN)){
    reference <- levels(mf$groupN)[reference + 1]
  }
  
  selectRef <- which(mf$groupN==reference)
  # If cond==FALSE use comparison group
  # for comparison to actual values;
  # else use reference group. 
  if(compareRef==FALSE){
  selectCom <- which(mf$groupN!=reference)
  } else {
  selectCom <- selectRef  
  }
  
  #Prepare df
  mod <- formula(f, collapse=TRUE)
  mRef <- model.matrix(mod,mf)[selectRef,-1]
  mCom <- model.matrix(mod,mf)[selectCom,-1]
  wRef <- weight[selectRef]
  wCom <- weight[selectCom]
  psi <- psi[selectRef]
  
  #Find means, diff in means, var/sd 
  mean_obs <- apply(mCom,2,function(x) wtd.mean(x, weights=wCom))
  mean_rw <- apply(mRef,2,function(x) wtd.mean(x, weights=psi*wRef))
  
  sd_ob <- apply(mCom,2,function(x) wtd.var(x, weights=wCom))
  sd_rw <- apply(mRef,2,function(x) wtd.var(x, weights=psi*wRef))
 
  mean_diff <- mean_obs - mean_rw
  
  sd_diff <- sqrt(sd_ob + sd_rw)
  sd_ob <- sqrt(sd_ob)
  sd_rw <- sqrt(sd_rw)
  
  #Export table
  res <- t(rbind(mean_obs,mean_rw,mean_diff,sd_ob, sd_rw,sd_diff))
  return(res)
}

#############################################################
### dfl_stat():
### Returns decripitive statistics of covariates

dfl_stat <- function(formula,
                     data,
                     weights,
                     group,
                     na.action = na.exclude,
                     reference=1,
                     constant=FALSE){
  
  mf = match.call()
  m = match(c("formula", "data", "weights", "na.action","group"), names(mf), 0)
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE
  
  # Retrieve formula as.Formula and model.frame
  f <- as.Formula(formula)
  
  mf[[1]] = as.name("model.frame")
  mf$formula <- f
  mf = eval.parent(mf)
  mt = attr(mf, "terms")
  
  # Store "orignal" decomposition model again
  f <- as.Formula(formula)
  
  # Extract variables, weights and group identifier
  #reg = get_all_vars(mt, mf)
  weight = model.weights(mf)
  if (!is.null(weight) && !is.numeric(weight)) {
    stop("'weights' must be a numeric vector")
  }
  if (!is.null(weight)) {
    weight = weight
  }
  else {
    weight = rep(1, nrow(mf))
  }
  
  #weight = weight/sum(weight)
  groupN = mf[, ncol(mf)]
  #reg$groupN <- groupN
  mf$groupN <- groupN
  
  # Select observations of reference group
  comparison <- ifelse(reference==1,0,1) 
  if(is.factor(mf$groupN)){
    reference <- levels(mf$groupN)[reference + 1]
    comparison <- levels(mf$groupN)[which(levels(mf$groupN)!=reference)]
  }
  
  selectRef <- which(mf$groupN==reference)
  selectCom <- which(mf$groupN!=reference)
  
  #Prepare df
  mod <- formula(f, collapse=TRUE) #include reference group of cat. variables by +0
  if(constant==FALSE){
    mRef <- as.matrix(model.matrix(mod,mf)[selectRef,-1])
    mCom <- as.matrix(model.matrix(mod,mf)[selectCom,-1])
  }else{
    mRef <- as.matrix(model.matrix(mod,mf)[selectRef,])
    mCom <- as.matrix(model.matrix(mod,mf)[selectCom,])
  }
  wRef <- weight[selectRef]
  wCom <- weight[selectCom]
  
  #Find means, diff in means, var/sd 
  mean_Ref <- apply(mRef,2,function(x) wtd.mean(x, weights=wRef))
  mean_Com <- apply(mCom,2,function(x) wtd.mean(x, weights=wCom))
  
  sd_Ref <- apply(mRef,2,function(x) wtd.var(x, weights=wRef))
  sd_Com <- apply(mCom,2,function(x) wtd.var(x, weights=wCom))
  
  mean_diff <- mean_Ref - mean_Com
  
  sd_diff <- sqrt(sd_Ref + sd_Com)
  sd_Ref <- sqrt(sd_Ref)
  sd_Com <- sqrt(sd_Com)
  
  # Sum of weights
  N <- matrix(c(length(wRef),length(wCom),sum(wRef),sum(wCom)),ncol=2,byrow=TRUE)
  colnames(N) <- c(reference,comparison)
  rownames(N) <- c("Obs.","Sum of weights")
  
  #Export table
  res <- t(rbind(mean_Ref,mean_Com,mean_diff,sd_Ref, sd_Com,sd_diff))
  colnames(res) <-  c(paste(rep("mean",3),c(reference,comparison,"diff"),sep="_"),paste(rep("sd",3),c(reference,comparison,"diff"),sep="_"))

  res <- list(means=res, N=N)
  return(res)
}


#############################################################
## Function for fitting and predicting Conditional Probabilities
pfit <- function(mod,df,w, newdata=NULL, fast=TRUE){
  
  # Without survey package
  #dep <- model.frame(mod,df)[,1]
  #reg <- model.matrix(mod,df)
  
  #probit <- glm(dep~reg, weights=w, family = binomial(link = "probit"), na.action=na.exclude, y=FALSE, model=FALSE)
  
  # With survey package
  #design <- svydesign(~0, data=df, weights=~w)
  #m1 <- svyglm(mod, data=df, design=design,family=quasibinomial(link="probit"))
  
  df <- cbind(df,w)
  
  if(fast==FALSE){
    # With glm
    m1 <- glm(mod, data=df, family=binomial(link="logit"),weights=w)
    p_X_1  <- predict.glm(m1, newdata=newdata, type="response", na.action = na.exclude)
  }else{
    ## With fastglm
    df <- model.frame(mod, data=df,weights=w)
    if(!is.numeric(df[,1])){
      df[,1] <- as.numeric(df[,1]==unique(df[,1])[2])
    }
    m1 <- fastglm(model.matrix(mod,df),df[,1],  
                  family = binomial(link = "logit"), 
                  weights=df$`(weights)`, fitted=FALSE)
    
    logit <- function(x){1/(1+exp(-x))}
    if(is.null(newdata)){
      p_X_1 <- logit(as.numeric(model.matrix(mod,df)%*%coef(m1)))
    }else{
      p_X_1 <- logit(as.numeric(model.matrix(mod,newdata)%*%coef(m1)))
    }
  }
  
  # Truncate weights 
  #p_X_1[which(p_X_1 < 0.01)] <- 0.01
  #p_X_1[which(p_X_1 > 0.99)] <- 0.99
  
  return(p_X_1)
  
}

#############################################################
### Trimming function 
# Adapted trimming function as suggested in Huber, Lechner, Wunsch (2013: 9f.)
trimming <- function(rwfactor,groupN,group=c(0,1)){
  
  # Factor variables for group selection allowed
  if(is.factor(groupN)){
    group <- levels(groupN)[1+group]
  }  
  
  # Set trimming threshold 
  n <- length(groupN)
  t <- sqrt(n)/n
  
  # Normalize weights
  rwfactor[which(groupN== group)] <- rwfactor[which(groupN== group)]/sum(rwfactor[which(groupN==group)])
  rwfactor[which(groupN!= group)] <- rwfactor[which(groupN!= group)]/sum(rwfactor[which(groupN!=group)])
  
  # Which observations to drop?
  #all in treatment group that have more weight than threshold
  sel1 <- which(groupN==group&rwfactor>t) 
  if(length(sel1)>0){
    #all in control group that have a weight like the smallest weight droped in treatment group
    sel1 <- c(sel1,which(groupN!=group&rwfactor>min(rwfactor[sel1]))) 
  }
  
  if(length(sel1)>0){
    sel <-  as.numeric(!is.element(1:n,sel1))
  }else{
    sel <- rep(1,n)
  }
  return(sel)  
}

#############################################################
### Gini function (code by Rothe(2015))
Gini <- function (x, w) {
  n <- length(x)
  w <- w/sum(w)
  G <- sum(x[order(x)] * 1:n * w[order(x)])
  G <- 2 * G/(n*sum(x[order(x)]  * w[order(x)]))
  G - 1 - (1/n)
}

#############################################################
### Function for distributional statistics
stat <- function(dep,weight,groupN,group=c(0,1),rwfactor,
                 tau=c(10,50,90,99)/100,
                 all.stats=FALSE,log.trans=FALSE){
  
  # Factor variables for group selection allowed
  if(is.factor(groupN)){
    group <- levels(groupN)[1+group]
  }
  
  # Select variables
  dep <- dep[which(groupN==group)]
  weight <- weight[which(groupN==group)]
  rwfactor <- rwfactor[which(groupN==group)]
  
  # Normalize weights 
  #rwfactor <- rwfactor/sum(rwfactor) 
  
  # Add to weighting vector
  w <- weight*rwfactor
  
  ### If all stats required: Are the required quantiles estimated?
  if(all.stats==TRUE){
    # make sure all relevant quantiles are estimated
    tau <- union(c(0.1,0.5,0.9,0.95,0.99),tau)
    tau <- tau[order(tau)]
  }
  
  # get quantiles statistics
  quantile <- wtd.quantile(dep,weight=w,probs=tau)
  
  # is dep variable log transformed?
  if(log.trans==TRUE){
    dep1 <- exp(dep)  
    quantile1 <- exp(quantile)
  } else {
    dep1 <- dep
    quantile1 <- quantile
  }
  
  #Get mean and var
  mu <- wtd.mean(dep1, weight=w)
  sd <- sqrt(wtd.var(dep1, weight=w))
  
  # Estimate additional stats if all stats required
  if(all.stats==TRUE){  
    
    #Overall gini and income share of top 10%
    gini <- Gini(dep1, w) 
    select <- which(dep>=quantile[match(0.95,tau)])
    #gini.top <- Gini(dep1[select], w[select]) 
    s_top05 <- (wtd.mean(dep1[select], weight=w[select])/mu)*0.05
    
    #Decile ratios
    p90p10 <- quantile1[match(0.9,tau)]/quantile1[match(0.1,tau)]
    p90p50 <- quantile1[match(0.9,tau)]/quantile1[match(0.5,tau)]
    p50p10 <- quantile1[match(0.5,tau)]/quantile1[match(0.1,tau)]
    p99p90 <- quantile1[match(0.99,tau)]/quantile1[match(0.9,tau)]
    
    res <- c(quantile,mu,sd,gini,
             p90p10,p90p50,p50p10,p99p90,
             s_top05)
    names(res)[(length(tau)+1):length(res)] <- c("mean","sd","gini",
                                                 "p90p10","p90p50","p50p10","p99p90",
                                                 "top 5% share")
    return(res)
  } else {
    # Return results if not all stats required
    res <- c(c(quantile,mu,sd))
    names(res)[(length(tau)+1):length(res)] <- c("mean","sd")
    return(res)
  }
}

#############################################################
### Function for kernel density estimates

kden <- function(dep,weight=NULL,
                 groupN=NULL,group=c(0,1),
                 rwfactor=NULL,
                 px=NULL,
                 bw = "nrd0",
                 kernel="gaussian",
                 n=512,
                 na.rm = TRUE){
  
  # Factor variables for group selection allowed
  if(is.factor(groupN)){
    group <- levels(groupN)[1+group]
  }
  if(is.null(groupN)){group <- "all"}
  
  # Prepare weights
  if(is.null(weight)){weight <- rep(1,length(dep))} 
  if(is.null(rwfactor)){rwfactor <- rep(1,length(dep))}      
  
  # Select variables
  if(is.null(groupN)==FALSE){
    select <- which(groupN==group)
    dep <- dep[select]
    weight <- weight[select]
    rwfactor <- rwfactor[select]
  }
  
  #Adjust weights
  w <- weight*rwfactor
  wsum <- sum(w)
  w <- w/wsum
  if(is.null(px)){px <- 1} else {px <- wsum/px}
  
  if(sum(is.na(dep))!=0&na.rm==TRUE){
    rm <- which(is.na(dep))
    dep <- dep[-rm]
    w <- w[-rm]
  }
  
  #Estimate density
  d <- density(dep,weights=w, 
               kernel=kernel, bw=bw,n=n)
  
  #Return results
  d <- data.frame(group=rep(group,n),x=d$x,density=d$y*px)
  return(d)
}




