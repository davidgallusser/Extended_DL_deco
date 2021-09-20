########################################################
########################################################
### D1 - Plot deco results

#load data
setwd(wd_results)
load("1-decomposition-results.rda")
load("2-deco-pp-gap.rda")
setwd(wd_plots)

########################################################
## DL decomposition 

df.plot <- as.data.frame(res$quantile[,c(1,5)])
df.plot$effect <- "total"
names(df.plot) <- c("tau","delta", "effect")

# Without title
plot <- ggplot(df.plot ,aes(tau,delta, fill=effect, color=effect)) +
  geom_smooth(method = "loess", span = 0.2, se=FALSE) +
  geom_point(size=1, alpha=0.7) + 
  geom_hline(yintercept = 0, colour="grey")  + 
  xlab("Quantile") + ylab("Wage change (in log points)") +
  theme(legend.position="none")
plot

# Plot bootstrap confidence bands
if(exists("bootstrap.se")==TRUE){
  df.plot <- cbind(df.plot,bootstrap.se[[1]][,5])
  names(df.plot)[4] <- "se"
  plot <- ggplot(df.plot ,aes(tau,delta, fill=effect, color=effect)) +
    geom_hline(yintercept = 0, colour="grey", size=0.75)  + 
    #geom_ribbon(aes(ymin=delta-se*1.959964, ymax=delta+se*1.959964), alpha=0.3, colour=NA) +
    #geom_errorbar(aes(ymin=delta-se*1.959964, ymax=delta+se*1.959964), alpha=0.5) + 
    geom_linerange(aes(ymin=delta-se*1.959964, ymax=delta+se*1.959964), alpha=0.7) + 
    geom_point(size=1, alpha=1) +
    #geom_smooth(method = "loess", span = 0.2, se=FALSE, alpha=1, size=0.5) + 
    xlab("Quantile") + ylab("Wage change (in log points)") +
    theme(legend.position="none")
  plot
}

ggsave("deco_pp_wage-inequality_agg_quantile_plot.pdf", width=w, height=h, unit="cm")


########################################################
## Extended DL decomposition 

quants.detail  <- melt(deco$quantile,
                       variable.name="effect",
                       id.var="tau", 
                       measure.vars = names(deco$quantile)[-1],
                       value.name="delta")

levels(quants.detail$effect) <- c("Group size effect","Group composition effect","Group wage structure effect")

plot <- ggplot(quants.detail,aes(tau,delta, color=effect, fill=effect)) +
  geom_smooth(method = "loess", span = 0.2, se=FALSE) + geom_point() + 
  geom_hline(yintercept = 0, colour="grey")  + 
  xlab("Quantile") + ylab("Wage change (in log points)") +
  labs(color='Effect', fill='Effect') 
plot

# Plot bootstrap confidence bands
if(exists("bootstrap.se")==TRUE){
  bs.se <-  melt(as.data.frame(bootstrap.se[[3]]),
                 variable.name="effect",
                 id.var="tau", 
                 measure.vars = colnames(bootstrap.se[[3]])[-1],
                 value.name="se") 
  quants.detail <- cbind(quants.detail,bs.se[3])
  plot <- ggplot(quants.detail,aes(tau,delta, color=effect, fill=effect, shape=effect)) +
    geom_hline(yintercept = 0, colour="grey", size=0.75)  + 
    #geom_ribbon(aes(ymin=delta-se*1.959964, ymax=delta+se*1.959964), alpha=0.4, colour=NA) +
    geom_linerange(aes(ymin=delta-se*1.959964, ymax=delta+se*1.959964), alpha=0.7) + 
    #geom_smooth(method = "loess", span = 0.2, se=FALSE, alpha=1, size=0.4, alpha=0.5) + 
    geom_point(size=1, alpha=1) +
    labs(x="Quantile", y="Wage change (in log points)", color='', fill='', shape='') +
    facet_wrap(~effect, ncol=1) +
    # scale_fill_discrete(guide = guide_legend()) +
    # scale_colour_discrete(guide = guide_legend()) +
    # scale_shape_discrete(guide = guide_legend()) +
    theme(legend.position="none")# + 
    #theme(legend.position="top")
  plot
}


ggsave("deco_pp_wage-inequality_quantile_plot.pdf", width=w, height=h*2, scale=1, unit="cm")


########################################################
# Decomposition table
tabDeco <- deco$other.stats
tabDeco <- cbind(res$other.stats[,1],rowSums(tabDeco),tabDeco)

labels1 <-   c("Observed Change",
               "Aggregate PPJ Effect",
               "Group Size Effect ",
               "Group Compos. Effect",
               "Group WS Effect")
labels2 <-   c("$\\hat\\Delta^{\\nu}_{O}$",
               "$\\hat\\Delta^{\\nu}_{S_1}$",
               "$\\hat\\Delta^{\\nu}_{G_1}$",
               "$\\hat\\Delta^{\\nu}_{C_1}$",
               "$\\hat\\Delta^{\\nu}_{W_1}$")
#labels1 <- c("Observed diff.", "Total PPJ (1)+(2)+(3)", "Group size (1)","Sorting (2)","Pure WS (3)")
#labels2 <- c("Obs. diff. $\\Delta^{F(y)}_{O}$", "Total PPJ $\\Delta^{F(y)}_{S_1}$", "Group size $\\Delta^{F(y)}_{G_1}$","Sorting $\\Delta^{F(y)}_{J_1}$","Pure WS $\\Delta^{F(y)}_{W_1}$")

## Add 2 lines of colnames
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c(paste0(paste0(c("",labels1), collapse=" &"),"\\\\"),
                      paste0(paste0(c("",labels2), collapse=" &")," \\\\"))

colnames(tabDeco) <- labels2 
labels3 <-      c("Mean in CHF","Std. Dev.","Gini",
                       "log(p90/p10)","log(p90/p50)","log(p50/p10)","log(p99/p90)",
                            "Top 5\\% share")
rownames(tabDeco)  <- labels3
tabDeco

title <- "Results of DL decomposition and extended DL decomposition"
labeltab <- "tab:ineq"

tabDeco <- xtable(tabDeco, 
                  caption=title, 
                  type="latex",
                  booktabs = FALSE,
                  auto=TRUE)

digits(tabDeco) <- matrix(rep(c(1,1,3,3,3,3,3,3),6),ncol=ncol(tabDeco)+1,byrow=FALSE)
align(tabDeco) <- c("X",rep("c",ncol(tabDeco)))


# Save tables
mod_xtable("table_deco.tex",tabDeco,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity)


# Create table with bs s.e.
if(exists("bootstrap.se")==TRUE){
  se.other.stats.agg <- bootstrap.se[[2]]
  se.other.stats <- bootstrap.se[[4]]
  tabDeco <- deco$other.stats
  tabDeco <- cbind(res$other.stats[,1],rowSums(tabDeco),tabDeco)
  tabSe <- tabDeco
  tabSe   <- cbind(se.other.stats.agg[,c(1,4)],se.other.stats)
       
  #round
  tabDeco <- rbind(round(tabDeco[1:2,],1),round(tabDeco[3:8,],3))
  tabSe  <-  rbind(round(tabSe[1:2,],1),round(tabSe[3:8,],3))
  
  # Add "thousand comma" and s.e.-brackets
  tabDeco <- apply(tabDeco,2, function(x) ifelse(x>999,paste0(floor(x/1000),",",x-floor(x/1000)*1000),x))
  tabSe <- apply(tabSe,2, function(x) ifelse(x>999,paste0(floor(x/1000),",",x-floor(x/1000)*1000),x))
  tabSe <- apply(tabSe,2, function(x) paste0("(",x,")"))
  
  tabDeco <- rbind(tabDeco,tabSe)
  select  <- c(1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16)
  tabDeco <- tabDeco[select,]
  
  # Colnames/rownames
  tabDeco <- cbind(c(labels3,rep("",8))[select],tabDeco)
  colnames(tabDeco) <- c(" ",labels2)
  #rownames(tabDeco) <- c(labels3,rep("",8))[select]
  tabDeco[15,1] <- "Top 5% share"
  
   #Create xtables
  tabDeco <- xtable(tabDeco, 
                    caption=title, 
                    label = labeltab,
                    type="latex",
                    booktabs = FALSE,
                    auto=TRUE)
  
  #align(tabDeco) <- c("X",rep("c",ncol(tabDeco)))
  align(tabDeco) <-  c("l","X",rep("C{2cm}",ncol(tabDeco)-1))
  
  # Add comment about s.e. after last row
  comm <- paste0("\\hline \n \\multicolumn{6}{l}",
                 "{\\footnotesize{Standard errors in brackets are based on 250 repeated half-sample bootstrap replications.}} \n")
  
  
  # Add two lines of colnames to first line
  addtorow <- list()
  addtorow$pos <- list(0, 0, 16)
  addtorow$command <- c(paste0(paste0(c("",labels1), collapse=" &"),"\\\\"),
                        paste0(paste0(c("",labels2), collapse=" &")," \\\\"),
                        comm)
  #addtorow <- list(pos = list(16),    command = comm)
  
  
  # Save tables
  mod_xtable("table_deco.tex",tabDeco,
             booktabs = TRUE, 
             caption.placement = "top",
             format.args=list(big.mark = ",",  decimal.mark = "."),
             tabular.environment = 'tabularx', width="1\\textwidth",
             sanitize.colnames.function = identity, 
             sanitize.rownames.function = identity,
             include.rownames=FALSE,
             hline.after=c(-1, 0),
             include.colnames = FALSE, 
             add.to.row = addtorow)
  
}



########################################################
# Descriptive statistics weights

labels1 <- c("min","p1","p10","p50","p90","p99","max")
labels2 <- c("$\\hat{\\Psi}_{X}$",
                    "$\\hat{\\Psi}_{G_1}$",
                    "$\\hat{\\Psi}_{X\\cdot S_1}$",
                    "$\\hat{\\Psi}_{S_1,1996}$",
                    "$\\hat{\\Psi}_{S_1,2010}$")
colnames(tabW) <- labels1
rownames(tabW) <- labels2
tabW2 <- tabW

title <- "Summary statistics of estimated reweighting factors \\label{tab:desc_rwfactors}"
labeltab <- "tab:desc_rwfactors"
tabW2 <- xtable(tabW2, 
               label=labeltab,
               caption=title, 
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabW2) <- matrix(rep(2,5*(ncol(tabW2)+1)),ncol=ncol(tabW2)+1,byrow=FALSE)
align(tabW2) <- c("X",rep("C{1.25cm}",ncol(tabW2)))

# Saving stargazer output in text files
mod_xtable <- function(output.file, ...) {
  output <- capture.output(print(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=FALSE)
}

#align(tab.exp) <- xalign(tab.exp)

# Save tables
mod_xtable("summary_rwfactor.tex",tabW2,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity)

if(exists("bootstrap.se")==TRUE){
  tabW2 <- tabW
  se.tabW <- tabW2
  se.tabW <- bootstrap.se[[5]]
  
  tabW2 <- round(tabW2,2)
  se.tabW <- round(se.tabW,3)
  

  
  # Add "thousand comma" and s.e.-brackets
  #tabDeco <- apply(tabDeco,2, function(x) ifelse(x>999,paste0(floor(x/1000),",",x-floor(x/1000)*1000),x))
  #tabSe <- apply(tabSe,2, function(x) ifelse(x>999,paste0(floor(x/1000),",",x-floor(x/1000)*1000),x))
  se.tabW <- apply(se.tabW,2, function(x) paste0("(",x,")"))
  
  # Set erronous bootstrapped minima/maxima to NA
  se.tabW[,c(1,7)] <- "-"
  
  tabW2 <- rbind(tabW2,se.tabW)
  select  <- c(1,6,2,7,3,8,4,9,5,10)
  tabW2 <- tabW2[select,]
  
  # Colnames/rowname
  #tabW2 <- cbind(c(labels2,rep("",5))[select],tabW2)
  colnames(tabW2) <- labels1
  rownames(tabW2) <- c(labels2, c("","\\quad","\\quad "," \\quad"," \\quad "))[select]
  
  #Create xtables
  tabW2 <- xtable(tabW2, 
                    caption=title, 
                    type="latex",
                    booktabs = FALSE,
                    auto=TRUE)
  
  
  #align(tabDeco) <- c("X",rep("c",ncol(tabDeco)))
  align(tabW2) <-  c("X",rep("C{1.35cm}",ncol(tabW2)))
  
  # Add comment about s.e. after last row
  comm <- paste0("\\hline \n \\multicolumn{8}{l}",
                 "{\\footnotesize{Standard errors in brackets are based on 250 half-sample bootstrap replications.}} \n")

  # Save tables
  mod_xtable("summary_rwfactor.tex",tabW2,
             booktabs = TRUE, 
             caption.placement = "top",
             format.args=list(big.mark = ",",  decimal.mark = "."),
             tabular.environment = 'tabularx', width="1\\textwidth",
             sanitize.colnames.function = identity, 
             sanitize.rownames.function = identity,
             #include.rownames=FALSE,
             hline.after=c(-1, 0),#,
             add.to.row = list(pos = list(10),
                              command = comm)
             )
}

  
#############################################
#### Plot performance-pay gap

plot.df <- as.data.frame(rbind(ppgap2010$quantile,ppgap1996$quantile))
plot.df$year <- c(rep(2010,nrow(plot.df)/2),c(rep(1996,nrow(plot.df)/2)))

plot.df <- melt(plot.df,
                variable.name="effect",
                id.var=c("tau","year"),
                measure.vars = names(plot.df)[2:4],
                value.name="delta")

levels(plot.df$effect) <- c("Observed\nwage gap","Wage structure\neffect","Composition\neffect")

plot <- ggplot(plot.df,aes(tau,delta, color=effect, fill=effect)) +
  #geom_smooth(method = "loess", span = 0.2, se=FALSE) + 
  facet_wrap(~year) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 0, colour="grey")  + 
  xlab("Quantile") + ylab("Wage gap (in log points)") +
  labs(color='', fill='')
plot

# Plot bootstrap confidence bands
if(exists("bootstrap.se.ppgap")==TRUE){
  
  plot.df.se  <- as.data.frame(rbind(bootstrap.se.ppgap[[3]],bootstrap.se.ppgap[[1]]))
  plot.df.se$year <- c(rep(2010,nrow(plot.df.se)/2),c(rep(1996,nrow(plot.df.se)/2)))
  
  plot.df.se <-  melt(plot.df.se,
                 variable.name="effect",
                 id.var=c("tau","year"), 
                 measure.vars = names(plot.df.se)[2:4],
                 value.name="se") 
  plot.df$se  <- plot.df.se$se
  plot <- ggplot(plot.df,aes(tau,delta, color=effect, fill=effect)) +
    #geom_smooth(method = "loess", span = 0.2, se=FALSE) + 
    geom_hline(yintercept = 0, colour="grey", size=0.75)  + 
    geom_ribbon(aes(ymin=delta-se*1.959964, ymax=delta+se*1.959964), alpha=0.3, colour=NA) +
    geom_line() + 
    #geom_point() + 
    facet_wrap(~year) + 
    xlab("Quantile") + ylab("Wage gap (in log points)") +
    labs(color='', fill='') +
    theme(legend.position="top")
  plot
}

ggsave("performance-pay-gap-1996-2010.pdf", width=w*1.2, height=h*1.2, scale=1, unit="cm")
setwd(wd)

