########################################################
########################################################
### 6. Check for sample balance

#load results data
setwd(wd_results)
load("1-decomposition-results.rda")
setwd(wd)

head(res$mf)
nrow(res$mf)
length(deco$psi_S1.1)
length(deco$psi_S1.0)
length(deco$psi_M)
length(deco$psi_J)

########################################################
### Sample balance: Actual 1996 vs. without PPJ 1996
selectCom <- which(res$mf$groupN==0)
selectRef <- which(res$mf$groupN==0&res$mf$pp==0)
psi <- deco$psi_S1.0

wCom <- res$mf[selectCom,"(weights)"]
wRef <- res$mf[selectRef,"(weights)"]

f <- as.formula(paste0("wage ~ ",paste0(names(res$mf)[-c(1,(ncol(res$mf)-2):ncol(res$mf))],collapse = "+")))
mod <- formula(f, collapse=TRUE)
mRef <- model.matrix(mod,res$mf)[selectRef,-1]
mCom <- model.matrix(mod,res$mf)[selectCom,-1]

mean_obs <- apply(mCom,2,function(x) wtd.mean(x, weights=wCom))
mean_rw <- apply(mRef,2,function(x) wtd.mean(x, weights=psi*wRef))

sd_ob <- apply(mCom,2,function(x) wtd.var(x, weights=wCom))
sd_rw <- apply(mRef,2,function(x) wtd.var(x, weights=psi*wRef))

mean_diff <- mean_obs - mean_rw
sd_diff <- sqrt((sd_ob + sd_rw))
mean_dff <- mean_diff/sd_diff

balance.tab1 <- cbind(mean_obs,mean_rw,mean_diff)
colnames(balance.tab1) <- c("Mean observed","Mean reweighted","Std. diff.")
balance.tab1

########################################################
### Sample balance: Actual 2010 vs. without PPJ 2010 
selectCom <- which(res$mf$groupN==1)
selectRef <- which(res$mf$groupN==1&res$mf$pp==0)
psi <- deco$psi_S1.1

wCom <- res$mf[selectCom,"(weights)"]
wRef <- res$mf[selectRef,"(weights)"]

f <- as.formula(paste0("wage ~ ",paste0(names(res$mf)[-c(1,(ncol(res$mf)-2):ncol(res$mf))],collapse = "+")))
mod <- formula(f, collapse=TRUE)
mRef <- model.matrix(mod,res$mf)[selectRef,-1]
mCom <- model.matrix(mod,res$mf)[selectCom,-1]

mean_obs <- apply(mCom,2,function(x) wtd.mean(x, weights=wCom))
mean_rw <- apply(mRef,2,function(x) wtd.mean(x, weights=psi*wRef))

sd_ob <- apply(mCom,2,function(x) wtd.var(x, weights=wCom))
sd_rw <- apply(mRef,2,function(x) wtd.var(x, weights=psi*wRef))

mean_diff <- mean_obs - mean_rw
sd_diff <- sqrt((sd_ob + sd_rw))
mean_dff <- mean_diff/sd_diff

balance.tab2 <- cbind(mean_obs,mean_rw,mean_diff)
colnames(balance.tab2) <- c("Mean observed","Mean reweighted","Std. diff.")
balance.tab2

########################################################
### Sample balance: Actual 2010 vs. with lower PPJ share 2010 
selectCom <- which(res$mf$groupN==1)
selectRef <- which(res$mf$groupN==1)
psi <- deco$psi_M[selectRef]

wCom <- res$mf[selectCom,"(weights)"]
wRef <- res$mf[selectRef,"(weights)"]

f <- as.formula(paste0("wage ~ ",paste0(names(res$mf)[-c(1,(ncol(res$mf)-2):ncol(res$mf))],collapse = "+")))
mod <- formula(f, collapse=TRUE)
mRef <- model.matrix(mod,res$mf)[selectRef,-1]
mCom <- model.matrix(mod,res$mf)[selectCom,-1]

mean_obs <- apply(mCom,2,function(x) wtd.mean(x, weights=wCom))
mean_rw <- apply(mRef,2,function(x) wtd.mean(x, weights=psi*wRef))

sd_ob <- apply(mCom,2,function(x) wtd.var(x, weights=wCom))
sd_rw <- apply(mRef,2,function(x) wtd.var(x, weights=psi*wRef))

mean_diff <- mean_obs - mean_rw
sd_diff <- sqrt((sd_ob + sd_rw))
mean_dff <- mean_diff/sd_diff

balance.tab3 <- cbind(mean_obs,mean_rw,mean_diff)
colnames(balance.tab3) <- c("Mean observed","Mean reweighted","Std. diff.")
balance.tab3

########################################################
### Sample balance: Actual 1996 vs. RW 2010
selectCom <- which(res$mf$groupN==0)
selectRef <- which(res$mf$groupN==1)
psi <- res$psi[selectRef,1]

wCom <- res$mf[selectCom,"(weights)"]
wRef <- res$mf[selectRef,"(weights)"]

f <- as.formula(paste0("wage ~ ",paste0(names(res$mf)[-c(1,(ncol(res$mf)-2):ncol(res$mf))],collapse = "+")))
mod <- formula(f, collapse=TRUE)
mRef <- model.matrix(mod,res$mf)[selectRef,-1]
mCom <- model.matrix(mod,res$mf)[selectCom,-1]

mean_obs <- apply(mCom,2,function(x) wtd.mean(x, weights=wCom))
mean_rw <- apply(mRef,2,function(x) wtd.mean(x, weights=psi*wRef))

sd_ob <- apply(mCom,2,function(x) wtd.var(x, weights=wCom))
sd_rw <- apply(mRef,2,function(x) wtd.var(x, weights=psi*wRef))

mean_diff <- mean_obs - mean_rw
sd_diff <- sqrt((sd_ob + sd_rw))
mean_dff <- mean_diff/sd_diff

balance.tab4 <- cbind(mean_obs,mean_rw,mean_diff)
colnames(balance.tab4) <- c("Mean observed","Mean reweighted","Std. diff.")
balance.tab4

########################################################
setwd(wd_results)
save(balance.tab1,
     balance.tab2,
     balance.tab3,
     balance.tab4,file="6-sample-balance.rda")
setwd(wd)
