##########################################################################
### A1 - LSE data preprocessing  
##########################################################################

# Set memory limit
memory.limit(size=50000) 

##########################################################################
#### Read files 
# Select years
erhebja <- seq(1994,2016,2)

tic("LSE data preparation")

# Define empty df
lse <- NULL

## Select variables
# variables being part of every dataset
vars <- c("alter","anforni","anzbe","anzlohn","ausbild","berufst","burnr_n",
          "dienstja","geschle","taetigk","lohnver","natkat","gewibgrs","gewicht",
          "mbls","mnliu","privoef","gr",
          "sonderza","blimok","xiiimloh","zivsta","zulagen",
          "stra_n","untgroe","wart","erhebja")

# excluded variables 
# c("bzstd","buwaz1","buwaz1","blimok","ibgr","iwaz","lohnform", "sozabg", "verduz")

# year-specific variables
vars_94 <- "wklassen"
vars_9698 <- c("nog_2")
vars_00 <- c("nog_2","thi","th")
vars_0206 <- c("nog_2","thi","th","gr")
vars_0810 <- c("nog_2","nog_2_08", "noga08_2", "noga08","thi","th","gr")
vars_1216 <- c("entid_n","isco_2","k_untgroe","va_ps07","nog_2_08","noga08_ent","noga08_ent_2","thi","th","gr")
vars <- unique(c(vars,vars_94,vars_9698,vars_00,vars_0206,vars_1216))


## Load datasets
for(i in erhebja){
    file_name <- ifelse(i==1994,"LSE1994_base_wart.csv",paste("LSE",i,"_base_noga6.csv",sep=""))
    df <- read.csv(file=paste0(wd_LSE_data,"/LSE-Daten neu/",file_name), header = TRUE, sep=";") 
    
    inter <- intersect(vars,names(df))
    diff <- setdiff(vars,names(df))
    df2 <- df[,inter]
    df2[,diff] <- NA
    df2 <- df2[,order(names(df2))]
    
    lse <- rbind(lse,df2)
}

#Remove unused dfs from enivornment
rm(df)
rm(df2)
names(lse)

##########################################################################
##########################################################################
#### Missing values

# Missing values (they are coded as -9 in the original data set)
lse[lse==-9] <- NA
lse[which(lse$isco_2==99), "isco_2"] <- NA

##########################################################################
##########################################################################
#### Manipulate/Recode variables

##########################################################################
### log real wage variable 
###

#LIK 2015, october values
LIK <- data.frame(year=1994:2017,LIK=c(89.1, 90.9, 91.6,91.9,91.9,93.0,94.3,94.9,96.0,96.5,97.7,99.0,99.3,100.5,103.1,102.3,102.5,102.4,102.1,101.9,101.9,100.5,100.3,100.9))

#Create deflated log wage variable 
baseyear <- 2010
lse$deflator <- NA
for(i in erhebja){
  select <- which(lse$erhebja==i)
  lse[select,"deflator"] <-  LIK[which(LIK$year==baseyear),2]/LIK[which(LIK$year==i),2]
}

lse$wage <- lse$mbls*lse$deflator
lse$wage <- log(lse$wage)

# Performance pay share, standardized performance pay and base salary
lse$pp.share = (lse$sonderza/12)/(lse$blimok + lse$zulagen + lse$xiiimloh/12 + lse$sonderza/12) 
lse$pp.mbls = lse$mbls*lse$pp.share*lse$deflator
lse$base.mbls = lse$mbls*(1-lse$pp.share)*lse$deflator

# Performance-pay dummy
lse$pp <- as.numeric(lse$pp.share>0)

#Remove deflator variable
lse$deflator <- NULL

##########################################################################
### Potential experience, tenure/dienstja 

lse$potex <-  Recode(lse$ausbild, "1=-24; 2:4=-21; 5:6=-18; 7:9=-15")
lse$potex[is.na(lse$potex)==TRUE] <- -15
lse$potex <- lse$potex + lse$alter
lse$potex[lse$potex<0] <- 0

labels <- c("0to4", "5to14", "15to24", "25to34", "35to44", "45plus")
lse$potex_f <- cut(lse$potex,c(0,5,15,25,35,45,100),right=FALSE, labels=labels)
lse$potex_f <- factor(lse$potex_f)

labels <- c("0to1", "2to4", "5to14", "15to24", "25plus")
lse$dienstja_f <- cut(lse$dienstja,c(0,2,5,15,25,100), right=FALSE, labels=labels)
lse$dienstja_f <- factor(lse$dienstja_f, exclude=NA) #6% missing values in 1996!

##########################################################################
### Recode variables
### Gender, firm size, collective bargaining, public sector, region factor

lse$geschle<- as.factor(lse$geschle)
levels(lse$geschle) <- c("man","woman")

labels <- c("0to49", "50to449", "500plus")
lse$untgroe_f <- cut(lse$untgroe,c(0,50,500,100000), right=FALSE, labels=labels)
lse$untgroe_f <- factor(lse$untgroe_f, exclude=NA) #5% missing values in 1996!

lse$privoef<- as.factor(lse$privoef)
levels(lse$privoef) <- c("private","public")

# Create collective bargaining variable 
lse$gav <- as.numeric(lse$lohnver==1 | lse$lohnver==2)
lse$lohnver <- as.factor(lse$lohnver)
levels(lse$lohnver)[1:4] <- c("industry barg.","firm barg.","individ. barg","public/law")

# Create region factor
lse$gr <- as.factor(lse$gr)
levels(lse$gr) <- c("1 Lake of Geneva region",  "2 Espace Mittelland","3 North-Western Switzerland", "4 Zurich", "5 Eastern Switzerland",  "6 Central Switzerland", "7 Ticino")

##########################################################################
### Define factors

# There are two obs in 2004 with berufst=6
select <- which(lse$berufst==6) 
lse[select,"berufst"] <- 5 

# Define factors
lse$ausbild <- as.factor(lse$ausbild) 
lse$anforni <- as.factor(lse$anforni) 
lse$berufst <- as.factor(lse$berufst)
lse$taetigk <- as.factor(lse$taetigk)
lse$isco_2 <- as.factor(lse$isco_2)

##########################################################################
### Set reference category of factors

lse$potex_f <- relevel(lse$potex_f, ref="15to24")
lse$dienstja_f <- relevel(lse$dienstja_f, ref="5to14")

lse$geschle<- relevel(lse$geschle, ref="woman")
lse$untgroe_f <- relevel(lse$untgroe_f, ref="50to449")
lse$privoef <- relevel(lse$privoef, ref="private")
lse$lohnver <- relevel(lse$lohnver, ref="individ. barg")

lse$ausbild <- relevel(lse$ausbild, ref="6") # Lehre
lse$taetigk <- relevel(lse$taetigk, ref="33") # 33 Medizinische, pflegerische u. soziale T?tigkeiten
lse$anforni <- relevel(lse$anforni, ref="3") # second lowest
lse$berufst <- relevel(lse$berufst, ref="5") # non-management
lse$isco_2 <- relevel(lse$isco_2, ref = "52")


##########################################################################
### Recode of NOGA08 industry codes to NOGA02 codes for years 2012 and 2016

if(sum(as.numeric(is.element(c(2012,2014,2016),erhebja)))>=1){
  
  # Load conversion keys
  key <- read.csv(file="data/NOGA-Definitionen/NOGA08toNOGA02.csv", header = TRUE, sep=";") 
  key$noga2002 <- gsub("[.]","",key$noga2002)
  
  #match industries with conversion probability 1
  select <- which(key$p==1)
  matches <- match(lse[which(is.na(lse$wart)),"noga08_ent"],key[select,"noga2008"])
  lse[which(is.na(lse$wart)),"wart"] <- key[select,"noga2002"][matches]
  #lse[which(is.na(lse$wart)),"wart"] <- key[match(lse[which(is.na(lse$wart)),"noga08_ent"],key[which(key$p==1),"noga2008"]),"noga2002"]
  
  #stochastically match industries with conversion probability < 1
  set.seed(123)
  uni <- unique(lse[which(is.na(lse$wart)),"entid_n"])
  n <- length(uni)
  rn <- runif(n) #assuming uniform distribution
  
  select <- match(uni,lse$entid_n)
  noga_to_match <- lse[select,"noga08_ent"]
  noga_matched <- NULL
  for(j in 1:length(noga_to_match)){
    key_s <-  which(key$noga2008==noga_to_match[j])
    probs <- cumsum(key[key_s,"p"]) 
    probs <- c(0,probs[-length(probs)]) #create cdf of conversion probabilities
    noga_matched <- c(noga_matched,key[key_s,"noga2002"][max(which(probs<=rn[j]))]) #match CDF with rv
  }
  
  # Finally match the industries
  lse[which(is.na(lse$wart)),"wart"] <- noga_matched[match(lse[which(is.na(lse$wart)),"entid_n"],uni)]
  
  # Create aggregated two-digit nog_2-variable 
  lse[which(is.na(lse$nog_2)),"nog_2"] <- as.numeric(substr(lse[which(is.na(lse$nog_2)),"wart"], start = 1, stop = 2))
}

##########################################################################
### Recode NGAE85 industry code to NOGA02 codes for year 1994
if(is.element(1994,erhebja)==TRUE){
  
  # Load conversion keys
  key1 <- read.csv(file="data/NOGA-Definitionen/NGAE85toNOGA95.csv", header = TRUE, sep=";")
  key2 <- read.csv(file="data/NOGA-Definitionen/NOGA95toNOGA02.csv", header = TRUE, sep=";") 
  
  # Adapt values
  key1$p <- as.numeric(as.character(key1$p))
  
  key2$noga1995 <- gsub("[.]","",key2$noga1995)
  key2$noga2002 <- gsub("[.]","",key2$noga2002)
  
  key2$noga1995 <- gsub(" ","",key2$noga1995)
  key2$noga2002 <- gsub(" ","",key2$noga2002)
  
  # Enable conversion from 1985 to 2002
  key <- data.frame(ngae1985=key1[,1],noga1995=key1[,2],noga2002bis=key2[match(key1$noga1995,key2$noga1995),1], noga2002=key2[match(key1$noga1995,key2$noga1995),2],p=key1[,3])
  #key[which(is.na(key$noga2002)),]
  key$noga1995 <- as.character(key$noga1995)
  key$noga2002 <- as.character(key$noga2002)
  
  #Add for missing values on p-variable 1 (industry 7111 or 6512A)
  key[which(is.na(key$p)==TRUE),"p"] <- 1
  
  #match industries with conversion probability 1
  lse$wart_new <- NA
  select <- which(key$p==1)
  matches <- match(lse[which(lse$erhebja==1994),"wart"],key[select,"ngae1985"])
  lse[which(lse$erhebja==1994),"wart_new"] <- key[select,"noga2002"][matches]
  
  #stochastically match industries with conversion probability < 1
  set.seed(123)
  select <- which(lse$erhebja==1994&is.na(lse$wart_new)==TRUE)
  uni <- unique(lse[select, "burnr_n"])
  n <- length(uni)
  rn <- runif(n) #assuming uniform distribution
  
  select <- match(uni,lse$burnr_n)
  noga_to_match <- lse[select,"wart"]
  noga_matched <- NULL
  for(j in 1:length(noga_to_match)){
    key_s <-  which(key$ngae1985==noga_to_match[j])
    probs <- cumsum(key[key_s,"p"]) 
    probs <- c(0,probs[-length(probs)]) #create cdf of conversion probabilities
    noga_matched <- c(noga_matched,key[key_s,"noga2002"][max(which(probs<=rn[j]))]) #match CDF with rv
  }

  # Finally match the industries
  lse[which(is.na(lse$wart_new)),"wart_new"] <- noga_matched[match(lse[which(is.na(lse$wart_new)),"burnr_n"],uni)]
  lse[which(lse$erhebja==1994),"wart"] <-   lse[which(lse$erhebja==1994),"wart_new"]

  ### Attention: There are missing values on wart variable in 1994;
  ### These are actually only 5 Firms in wklassen 91 92 61 66 91 and all 
  ### in the public sector (e.g. the firm in 66 is probably PTT).
  
  ### Match 2-digit-industries
  
  #match 2-digit industries with conversion probability 1
  lse$nog_2_94 <- NA
  select <- which(key$p==1)
  matches <- match(lse[which(lse$erhebja==1994),"wart"],key[select,"ngae1985"])
  lse[which(lse$erhebja==1994),"nog_2_94"] <- key[select,"noga2002"][matches]
  
  set.seed(123)
  select <- which(lse$erhebja==1994&is.na(lse$nog_2_94)==TRUE) 
  uni <- unique(lse[select, "burnr_n"])
  n <- length(uni)
  rn <- runif(n) #assuming uniform distribution
  
  select <- match(uni,lse$burnr_n)
  noga_to_match <- lse[select,"wklassen"]
  noga_matched <- NULL
  for(j in 1:length(noga_to_match)){
    key_s <-  which(key$ngae1985==noga_to_match[j])
    probs <- cumsum(key[key_s,"p"]) 
    probs <- c(0,probs[-length(probs)]) #create cdf of conversion probabilities
    noga_matched <- c(noga_matched,key[key_s,"noga2002"][max(which(probs<=rn[j]))]) #match CDF with rv
  }
  
  # Finally match the industries
  lse[which(is.na(lse$nog_2_94)),"nog_2_94"] <- noga_matched[match(lse[which(is.na(lse$nog_2_94)),"burnr_n"],uni)]
  
  # Remove unused objects
  lse$wart_new <- NULL
  rm("key1")
  rm("key2")
  rm("key")
}

###########################################################################
#### Create aggregated industry variables

# Noga02 "Abschnitte"/"Aggregated"
lse$nog_abs <- Recode(lse$nog_2, "1:5='AB'; 10:14='C'; 15:37='D'; 40:41='E'; 45='F'; 50:52='G'; 55='H'; 60:64='I'; 65:67='J'; 70:74='K'; 75='L'; 80='M'; 85='N'; 90:93='O'")
lse$nog_abs <- as.factor(lse$nog_abs) 

# Create new nog_2 variable: For every 2-digit industry a single level:
nog_2_level <- sort(unique(substr(unique(lse$wart), start = 1, stop = 2)))
lse$nog_2 <- "01"

for(i in 1:length(nog_2_level)){
  print(nog_2_level[i])
  select <- which(substr(lse$wart, start = 1, stop = 2)==nog_2_level[i])
  lse[select,"nog_2"] <- nog_2_level[i]  
}

if(is.element(1994,erhebja)){
  select <- which(is.na(lse$wart)==TRUE&lse$erhebja==1994)
  lse[select,"nog_2"] <- lse[select,"nog_2_94"]  
  lse$nog_2_94 <- NULL
}

lse$nog_2 <- as.factor(lse$nog_2)

###########################################################################
# Create nog_agg: A new industry aggregation

# 1:14:           ABC       "1st sector incl. mining and quarrying"
# 15:16:          DA        HERSTELLUNG VON NAHRUNGS- UND GENUSSMITTELN, TABAKVERARBEITUNG
# 17:22; 25:26;   DB        Textil, Holz, Papier, Glas, Keramik (ohne Verlage/Zeitungen: 2211A, 2212A, 2213A,2214A,2215A)
# 23:24:          DC        Chemie, Pharma
# 27:28, 36:37:             Metallherstellung & -verarbeitung; HERSTELLUNG VON M?BELN, SCHMUCK, MUSIKINSTRUMENTEN, SPORTGER?TEN, SPIELWAREN UND SONSTIGEN ERZEUGNISSEN; R?CKGEWINNUNG    
# 29,34:35                  Maschinen- und Fahrzeugherstellung
# 30:33                     Herstellung Datenverarbeitungsger?te, Uhren, Pr?zisionsinstrumente
# 40:41:          E         ENERGIE- UND WASSERVERSORGUNG
# 45:             F         Bau
# 51:             G 51      Grosshandel 
# 50:52:          G 50,52   Detailhandel; Handel mit Autos
# 55:             H         BEHERBERGUNGS- UND GASTST?TTEN
# 60:64           I         VERKEHR UND NACHRICHTEN?BERMITTLUNG
# 65,67           J         Banking&Finance (65,c("6711A", "6712A","6712B","6713A"))
# 66:                       Insurance (66,6720A)
# 70,71           K 70,71   Real Estate/Vermietung beweglicher Sachen,  )
# 72              K 72      IT
# 74              K 74a     Consulting/Management Services c("7411A", "7411B","7412A", "7414A","7415A", "7415B")
# 73.74b          K 73,74b  F&E and other Business-Service  
# 75              L         ?ffentliche Verwaltung
# 80              M         Bildung  
# 85              N         Gesundheitwesen
# 90,91,93        O1        Pers. Services
# 92              O2        Information and Entertainment (inkl. Verlage/Zeitungen: 2211A, 2212A, 2213A,2214A,2215A)

wart_list <- list(1:14,15:16,c(17:22,25:26),23:24,c(27:28,36:37),c(29,34:35),30:33,c(40:41),45,c(50,52),51,55,60:64,c(65,67),66,70:71,72,73:74,c(75,91),80,85,c(90,93),92)
wart_list <- lapply(wart_list, function(x) as.character(x))
wart_list[[1]][1:2] <- c("01","02")

labels <- c("A. Primary", "D. Manufac Nutrition", "D. Manufac Durables", "D. Pharma, Chemical", "D. Manufac Metal", "D. Manufac Machines & Vehicles", "D. Manufac Instruments & Watches",
            "E. Energy & Water Supply", "F. Construction",  "G. Retail Trade", "G. Wholesale Trade", "H. Restaurants & Hotels",
            "I. Transport & Communication", "J. Banking & Finance", "J. Insurance", "K. Real Estate","K. IT", "K. F&E; o. Business Services",
            "L. Public Admin & NGO", "M. Education", "N. Health & Social Services", "O. Personal Services", "O. Information & Entertainment")

lse$nog_agg <- NA
for(i in 1:length(wart_list)){
  print(wart_list[[i]])
  select <- which(is.element(substr(lse$wart, start = 1, stop = 2),wart_list[[i]]))
  lse[select,"nog_agg"] <- labels[i] 
}

lse[which(is.element(lse$wart, c("2211A", "2212A", "2213A","2214A","2215A"))),"nog_agg"] <- "O. Information & Entertainment"
lse[which(is.element(lse$wart, c("7411A", "7411B","7412A", "7414A","7415A", "7415B"))),"nog_agg"] <- "K. Consulting"
lse[which(lse$wart=="6720A"),"nog_agg"] <- "J. Insurance"
lse$nog_agg <- as.factor(lse$nog_agg)
lse$nog_agg <- relevel(lse$nog_agg, ref="N. Health & Social Services") # health

###########################################################################
# Set reference industries for newly created variables
lse$nog_2 <- relevel(lse$nog_2, ref="85") # health
lse$nog_agg <- relevel(lse$nog_agg, ref="N. Health & Social Services") # health
lse$nog_abs <- relevel(lse$nog_abs, ref="N") # health

#########################################################################
### Create year dummies
years <- seq(1994,2016,2)
for(i in seq(1994,2016,2)){
  lse[,paste("lse",substr(as.character(i),start=3,stop=4),sep="")] <- as.numeric(lse$erhebja==i)
}

#########################################################################
# Restrict sample to:
# - 18-64 year old workers 
# - workers in the private sector 
# - workers with wages blow the 0.5%-quantile

# Old and young workers, private sector
lse <- subset(lse, alter>17&alter<65)
lse <- subset(lse, privoef=="private")

# Lowest quantile
select <- NULL
for(j in erhebja){
  q <- wtd.quantile(lse[which(lse$erhebja==j),"mbls"],probs=0.005,weights=lse[which(lse$erhebja==j),"gewibgrs"])
  select <- c(select,which(lse$erhebja==j&lse$mbls<q))
}
lse <- lse[-select,]

##########################################################################
##### Create wage percentile variables 
lse$wage_pc<- NA

# Set number of quantiles 
q = 100 
# create weighted quantile indicator
for(j in erhebja){
lse[which(lse$erhebja==j),"wage_pc"] <- xtile(lse[which(lse$erhebja==j),"wage"], n = q, w = lse[which(lse$erhebja==j),"gewibgrs"])
}
lse$wage_pc <- factor(lse$wage_pc)

# Create aggregate pc-factor
lse$top_pc <- "bottom 90%"
lse[which(as.numeric(lse$wage_pc)>90&as.numeric(lse$wage_pc)<=99), "top_pc"] <- "other 9%"
lse[which(as.numeric(lse$wage_pc)>99), "top_pc"] <- "top 1%"
lse$top_pc  <- as.factor(lse$top_pc)

##########################################################################
# Save data 
lse_all <- lse

#save each erhebja in a separate file
setwd(wd_LSE_data)
for(i in erhebja){
  lse <- lse_all[which(lse_all[,"erhebja"]==i),]
  save(lse, file=paste0("lse",i,".rda"))
}

toc()
