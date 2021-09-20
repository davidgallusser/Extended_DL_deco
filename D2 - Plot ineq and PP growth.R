###########################################################
### D2 Plot inequality growth and 
### the rise of PPJ 

#load data
setwd(wd_results)
load("3-wage-inequality.rda")
load("4-pp-jobs-shares.rda")
setwd(wd_plots)

# Plot pp-share plot
ppshares <- as.data.frame(ppshares)
ppshares$cat <- "PPJ share"
plot_pp <- ggplot(ppshares ,aes(erhebja,ppshare, color=cat)) +
                  geom_line() + geom_point() + 
                  geom_hline(yintercept = 0, colour="darkgrey")  + 
                  xlab("Year") + ylab("Share of performance-pay jobs") +
                  labs(color='') +  theme(legend.position = "none")
plot_pp

# Plot bootstrap confidence bands if existant
if(exists("ppshares.se")==TRUE){
  #ppshares.se
  ppshares$se <- ppshares.se#[,2]
  plot_pp <- ggplot(ppshares ,aes(erhebja,ppshare, color=cat, fill=cat)) +
    geom_line() + geom_point() + 
    geom_ribbon(aes(ymin=ppshare-se*1.959964, ymax=ppshare+se*1.959964), alpha=0.3, colour=NA) +
    geom_hline(yintercept = 0, colour="darkgrey")  + 
    xlab("Year") + ylab("Share of performance-pay jobs") +
    labs(color='', fill='') +  theme(legend.position = "none")
  plot_pp
}


# Plot quantile ratio plot
df.plot <- as.data.frame(quantileratios)
df.plot <- melt(df.plot,
                id.vars="erhebja",
                measure.vars=names(df.plot)[-1],
                variable.name="percentile",
                value.name="ratio")

select <- which(df.plot$percentile!="p90/p10"&df.plot$erhebja<2014)

plot_ineq <- ggplot(df.plot[select,] ,aes(erhebja,ratio,color=percentile)) +
              geom_line() + geom_point() + 
              geom_hline(yintercept = 1, colour="darkgrey")  + 
              scale_y_continuous(limits = c(1, 2.5))+
              xlab("Year") + ylab("Percentile ratio") +
              labs(color='Ratio')
plot_ineq

# Plot bootstrap confidence bands if existant
if(exists("quantileratios.se")==TRUE){
#ppshares.se
  
df.plot.se <- as.data.frame(quantileratios.se)
df.plot.se <- melt(df.plot.se,
                  id.vars="erhebja",
                  measure.vars=names(df.plot.se)[-1],
                  variable.name="percentile",
                  value.name="se")
    
df.plot$se <- df.plot.se[,3]  
select <- which(df.plot$percentile!="p90/p10"&df.plot$erhebja<2012)

plot_ineq <- ggplot(df.plot[select,] ,aes(erhebja,ratio,color=percentile, fill=percentile)) +
  geom_line() + geom_point() + 
  geom_hline(yintercept = 1, colour="darkgrey")  + 
  xlab("Year") + ylab("Percentile ratio") +
  geom_ribbon(aes(ymin=ratio-se*1.959964, ymax=ratio+se*1.959964), alpha=0.3, colour=NA) +
  labs(color='Ratio', fill='Ratio') +
  scale_y_continuous(limits = c(1, 2.5))
plot_ineq

}


# Combine and save plots
ggarrange(plot_ineq, plot_pp,  
          labels = c("A", "B"),
          ncol = 2, nrow = 1, widths=c(0.57,0.43))
setwd(wd_plots)
ggsave("ineq-pp-growth-plot.pdf", width=w*(7/6), height=h, unit="cm")
setwd(wd)
