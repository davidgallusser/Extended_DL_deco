########################################################
########################################################
### Compare LSE earnings and AHV earnings

########################################################
# Define file name where data is to be save
fn <- "8-lse-vs-ahv.rda"

########################################################
# Define years
years <- seq(1994,2016,2)
tau <- c(0.5,0.99)

# Define empty result vector
wages <- NULL 

### Load data sets
oringinaldata=TRUE
if(oringinaldata==FALSE){
setwd(wd_results)
load(fn)
}else{
for(t in years){
  print(t)
  setwd(wd_LSE_data)
  load(paste0("lse",t,".rda"))
  
  ### Pre-whitening 
  # Get rid of horticulture
  #lse <- lse[which(lse$nog_2!="nog_211"),]
  lse$wage2 <- lse$blimok+lse$zulagen+lse$sonderza/12+lse$xiiimloh/12
  
  ### Compute PPJ shares
  wages <- rbind(wages,c(t,wtd.quantile(lse$wage2,weights=lse$gewicht,probs=tau,na.rm=FALSE)))
  colnames(wages) <- c("erhebja",tau)
  
  setwd(wd_results)
  save(wages,file=fn)
  setwd(wd)
}
}

# Load AHV earnings 
ahv <- readxl::read_xlsx("data/ahvloehne_82-18.xlsx")
names(ahv) <- c("erhebja","cat","obs","avg","sum",1:99/100,0.999)
ahv <- ahv[,-c(2,3,4,5)]
ahv <- tidyr::pivot_longer(ahv,cols=-c("erhebja"),names_to="tau")
ahv$tau <- as.numeric(ahv$tau)
ahv <- subset(ahv, tau %in% c(0.5,0.99))

ahv <- as.data.frame(ahv)
# # Load AHV earnings
# ahv <- read.table("data/ahvloehne.csv", sep=";", header=TRUE)
# ahv <- ahv[which(is.element(ahv[,1],1994:2017)&is.element(ahv[,2],tau)),]
# ahv[,3] <- as.numeric(as.character(ahv[,3]))

# LIK 2015, october values
LIK <- data.frame(year=1994:2020,LIK=c(89.1, 90.9, 91.6,91.9,91.9,93.0,94.3,94.9,96.0,96.5,97.7,99.0,99.3,100.5,103.1,102.3,102.5,102.4,102.1,101.9,101.9,100.5,100.3,100.9,102.1,101.8,101.2))
ahv[,3] <- ((ahv[,3]/LIK[match(ahv[,1],LIK$year),2])*LIK[which(LIK$year==2010),2])/12
names(ahv) <- c("erhebja","tau","earnings")
ahv$data <- "AHV"
ahv <- subset(ahv, erhebja>=1994)

# Add SESS wages
wages1 <- reshape2::melt(as.data.frame(wages),
     variable.name="tau",
     id.var="erhebja", 
     measure.vars = colnames(wages)[-1],
     value.name="earnings")

wages1$data <- "SESS"
wages1 <- rbind(wages1,ahv)


wages1[which(wages1$data=="SESS"&wages1$tau==0.5),"earnings"] <- wages1[which(wages1$data=="SESS"&wages1$tau==0.5),"earnings"]/wages1[which(wages1$data=="SESS"&wages1$tau==0.5&wages1$erhebja==1996),"earnings"]-1
wages1[which(wages1$data=="SESS"&wages1$tau==0.99),"earnings"] <- wages1[which(wages1$data=="SESS"&wages1$tau==0.99),"earnings"]/wages1[which(wages1$data=="SESS"&wages1$tau==0.99&wages1$erhebja==1996),"earnings"]-1
wages1[which(wages1$data=="AHV"&wages1$tau==0.5),"earnings"] <- wages1[which(wages1$data=="AHV"&wages1$tau==0.5),"earnings"]/wages1[which(wages1$data=="AHV"&wages1$tau==0.5&wages1$erhebja==1996),"earnings"]-1
wages1[which(wages1$data=="AHV"&wages1$tau==0.99),"earnings"] <- wages1[which(wages1$data=="AHV"&wages1$tau==0.99),"earnings"]/wages1[which(wages1$data=="AHV"&wages1$tau==0.99&wages1$erhebja==1996),"earnings"]-1

wages1$erhebja <- as.numeric(wages1$erhebja)

plot1  <- ggplot(wages1, aes(erhebja, earnings, color=data, shape=tau)) + 
    geom_point() + geom_line(aes(linetype=tau)) + 
    labs(y="Cumulative growth since 1996", x="Year", color="Data source", shape="Earnings\npercentile", linetype="Earnings\npercentile") + 
    geom_hline(yintercept = 0, colour="grey") + 
    scale_x_continuous(breaks=seq(1996,2018,4))

setwd(wd_plots)
ggsave("lse-vs-ahv.pdf", plot=plot1, width=w, height=h, unit="cm")
setwd(wd)





