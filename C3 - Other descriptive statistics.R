########################################################
########################################################
### C3 - Other descriptive statistics

########################################################
# Load data 
setwd(wd_LSE_data)
load("lse1996.rda")
df <- lse
load("lse2010.rda")
lse <- rbind(df,lse)
setwd(wd)

########################################################
vars <- c("geschle","ausbild","potex","dienstja", "berufst","untgroe","nog_agg", "taetigk") 

select96 <- which(lse$erhebja==1996)
select10 <- which(lse$erhebja==2010)
stats1996 <- NULL
stats2010 <- NULL

df96 <- model.frame(as.formula(paste0("wage ~gewibgrs + pp + ", paste(vars, collapse="+"))),
                    data=lse[select96,])
df10 <- model.frame(as.formula(paste0("wage ~gewibgrs + pp + ", paste(vars, collapse="+"))),
                    data=lse[select10,])

# Stats 
for(i in 1:length(vars)){
  m.DFL.detail <- as.formula(paste0("wage ~",vars[i],"+ 0"))
  stats1996 <- rbind(stats1996,
                     dfl_stat(m.DFL.detail, data=df96,
                              weights = gewibgrs,
                              group=pp,
                              reference=0,
                              constant=TRUE)[[1]])
  
  stats2010 <- rbind(stats2010,
                     dfl_stat(m.DFL.detail, data=df10,
                              weights = gewibgrs,
                              group=pp,
                              reference=0,
                              constant=TRUE)[[1]])
}

#Observations
m.DFL.detail <- wage ~ geschle 
obs1996 <- dfl_stat(m.DFL.detail, data=df96,
                    weights = gewibgrs,
                    group=pp,
                    reference=0,
                    constant=TRUE)[[2]]

obs2010 <-   dfl_stat(m.DFL.detail, data=df10,
                      weights = gewibgrs,
                      group=pp,
                      reference=0,
                      constant=TRUE)[[2]]


#######################################
# Performance-pay share

ppshareres96 <- NULL 
ppshareres10 <- NULL

select96 <- which(lse$erhebja==1996)
select10 <- which(lse$erhebja==2010)

vars <- c("geschle","ausbild","berufst","untgroe_f","nog_agg", "taetigk") 

for(i in 1:length(vars)){
  ppshareres96 <- c(ppshareres96,unlist(lapply(split(lse[select96,],lse[select96,vars[i]]), function(x) weighted.mean(x$pp, weights=x$gewibgrs))))
  ppshareres10 <- c(ppshareres10,unlist(lapply(split(lse[select10,],lse[select10,vars[i]]), function(x) weighted.mean(x$pp, weights=x$gewibgrs))))
}


########################################################
### Save results
setwd(wd_results)
save(ppshareres96, ppshareres10, obs1996,obs2010,stats1996,stats2010,file="5-other-descriptive-statistics.rda")
setwd(wd)
