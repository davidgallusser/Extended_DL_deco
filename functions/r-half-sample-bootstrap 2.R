##################################
### Repeated half-sample
### bootstap resampleing adapted
### this version: 26/6/2019
##################################

### See:
### van Kerm (2013): http://repec.org/usug2013/vankerm.uk13.pdf
### Saigo, Shao, Sitter (2001): A Repeated Half-Sample Bootstrap and Balanced Repeated Replications for Randomly
###                             Imputed Data. Survey Methodology, 27(2): 189-196.

# Uni
#setwd("C:/Users/gallusse/switchdrive/Uni Basel/R Code/LSE")
# Laptop
#setwd("C:/Users/David Gallusser/switchdrive/Uni Basel/R Code/LSE")

# LSE data
# load("LSE-Daten/lse1996.rda")

# LSE stratification variable 
# lse$stra_n
# Firm size variable
# lse$anzbe
# PSU/firm id
# lse$burnr_n

#### LSE survey design 1994-2000 
## Strata: ~ 50 nog_2 X 5 anzbe size categories
## PSU: Firm
## Sampling probabilities:
#   TA    Size      PSU:    USU:   PSU response rate:
#   1     2-4:      ?       100%   60%
#   2     5-19:     ?       100%   75%
#   3     20-49:    ?       72%    75%
#   4     50-499:   100%    34%    80%
#   5     500+:     100%    62%    85%
#
# Note: obs. in 1994-1998 were rescaled to match FTE of BZ

#### LSE survey design since 2002
## Strata: ~ 50 nog_2 X 3 anzbe categories X 7 regions
## PSU: Firm
##  Sampling probabilities:
#   TA    Size      PSU:    USU:   PSU response rate:
#   1     3-19:     ?       100%   65%
#   2     20-49:    ?       50%    75%
#   3     50+:      100%    30%    80%

# N: Number of PSU per stratum
# N < 10 all firms are sampled, independent of size
# However, there is variance due to non-response

### Reweight gewicht to fit again sum of 
### gewicht per stratum 

### New Algorithm: 'Sequenced' half sample bootstrap

### 1.: Half-sample resampling PSU
    
        ## For all but the large firms (anzbe>50):
        
        # if N even: 
        # sample N/2 PSU per stratum without replacement
        # duplicate samples PSU with their observations 

        # if N odd: 
        # sample (N-1)/2 PSU per stratum
        # duplicate sampled PSU with their observations 
        # add 1 sampled PSU a third time with its observations

        ## For large firms (anbze>50):

        # Sample floor((N/10)*8) of PSU per stratum without replacement
        # duplicate floor((N/10)*2) of PSU per stratum 
        
        # if length of sampled vector < N, triple as many sample observations
        # in order to get a sample as large as the original vector.

### 2.: Half-sample resampling USU
        
        # if N even: 
        # sample N/2 USU without replacement
        # duplicate samples USU with their observations 

        # if N odd: 
        # sample (N+1)/2 USU
        # duplicate samples USU with their observations 
        # remove 1 USU with its observations

## Half-sample sampling function
half.sample <- function(x,share=0.5){
  # determine length of x and
  if(share==1){return(x)}
  l <- length(x)
  if(l==1){return(x)}
  #n sample draws 
  n <- max(floor(share*l),1)
  #sampling
  s <- sample(x,n,replace=FALSE)
  #duplicate sampled observation in order to match l
  #if the number to be duplicated is larger than the sample
  #multiply the draws more the two times
  dupli <- l-n
  sel <- if(dupli>n){c(rep(1:n,floor(l/n)),c(1:n)[1:(l-n*floor(l/n))])}else{c(1:n,1:dupli)}
  if(share<0.5){sel <- sel[1:l]}
  return(s[sel])
}

# Checking for duplicates in a vector: Return unique, duplicated, trippled, ... elements
# into a separate list element
du <- function(x) {res <- list()
                  k <- 1  
                  while(l>0){
                              res[[k]] <- x[!duplicated(x)]
                              x <- x[duplicated(x)]
                              l <- length(x)
                              k <- k + 1
                            }
                  return(res)
                  }


# Resampling function 
bs <- function(df,erhebja=1996){
  
  ######################################################
  # Prepare stratum variable
  df$stra_n <- as.factor(df$stra_n)
  
  #define psu ID variable (i.e. firm identifyer)
  if(erhebja<2012){
  df$psuID <- df$burnr_n
  }else{
  df$psuID <- df$entid_n
  }
  
  # Select different shares to be reampled
  # PSU: Firms
  # USU: Workers within firms
  # if anzbe > 50:        resample 50% PSU, resample 50% USU
  # if anzbe >20&<50:     resample 50% PSU, resample 50% USU
  # if anbze >20&<50:     resample 50% PSU, resample 50% USU
  
  if(erhebja<2000){
  #Firm size: large, medium, medium to small, small
  psu.shares <- c(1,0.5,0.5,0.5)#c(0.8,0.5,0.5,0.5)
  usu.shares <- c(0.5,1,1,1)#c(0.5,0.5,1,1)
  } else {
  #Firm size: large, medium, small
  psu.shares <- c(1,0.5,0.5)#c(0.8,0.5,0.5)
  usu.shares <- c(0.5,1,1)#c(0.5,0.5,1)  
  }
  
  # Select different cases:
  if(erhebja<2000){
    s1 <- which(df$anzbe>=50)
    s2 <- which(df$anzbe>=20&df$anzbe<50)
    s3 <- which(df$anzbe>=5&df$anzbe<20)
    s4 <- which(df$anzbe<5)
    s <- list(s1,s2,s3,s4)
  } else if(erhebja<2012){
    s1 <- which(df$anzbe>=50)
    s2 <- which(df$anzbe>=20&df$anzbe<50)
    s3 <- which(df$anzbe<20)
    s <- list(s1,s2,s3)
  } else {
    s1 <- which(df$k_untgroe>=3)
    s2 <- which(df$k_untgroe==2)
    s3 <- which(df$k_untgroe==1)  
    s <- list(s1,s2,s3)
  }

  ### Define empty selction vector
  select.export <- NULL
  
  ### Begin loop with i for different firm sizes
  for(i in 1:length(s)){
  df2 <- df[s[[i]],]
  
  ### select row numbers for each stra_n; save them separately into a list element
  select2 <- lapply(as.list(unique(df2$stra_n)), function(x){which(df$stra_n==x)})
  select2 <- lapply(select2, function(x) {intersect(x,s[[i]])})
  
  ### Find unique firm per stratum
  psu0 <- lapply(select2, function(x){unique(df[x,"psuID"])})

  ### Resample psu (firms)
  psu <- lapply(psu0, function(x) half.sample(x,share=psu.shares[i]))

  ### Resample usu (workers) if required
  if(usu.shares[i]==1){
  # Use faster id selection procedure if usu resampling not required
  usu <- unlist(lapply(du(unlist(psu)), function(x) which(df$psuID %in% x)))
  } else {
  # If resampling require select row number of usu by psu  
  usu <- unlist(lapply(psu, function(x) {sel <- unlist(lapply(x, function(x) which(df$psuID==x)))
                                        return(half.sample(sel,share=usu.shares[i]))
                                        }))
  }
  
  
  #length(usu)
  #length(usu2)
  #sum(as.numeric(usu %in% usu2))
  #identical(sort(usu),sort(usu2))
  
  #print(i)
  #print("PSU:")
  #print(length(psu0))
  #print(length(psu))
  #print("USU:")
  #print(length(unlist(usu00)))
  #print(length(unlist(usu0)))
  #print(length(unlist(usu)))
  
  # Attach to export vector
  select.export <- c(select.export,usu)
  
  } ## End lop firm sizes
  
  return(select.export)
}

#tic("Test")
#test <- bs(lse, erhebja=2010)
#toc()

# Weight adjustment function 
bw <- function(select,df,erhebja=1996){
  
  ######################################################
  # Prepare stratum variable
  df$stra_n <- as.factor(df$stra_n)
  
  # Resampled data.frame
  df.B <- df[select,]
 
   ######################################################
   # Sum weights within stratum in "observed" data set
   # Sum weights within stratum in resampled data set
   # Weigting adjustment: Divide observed weights by reampled sum and multiply it by observed sum
   # in order to keep sum of weights in resampled sample as large like in observed sample.
  
   #Unique strata
   stra_n <- unique(df$stra_n)
  
   #Sum of weights by strata
   weights.O <- unlist(lapply(as.list(stra_n), function(x) sum(df[which(df$stra_n==x),"gewicht"])))
   weights.B <- unlist(lapply(as.list(stra_n), function(x) sum(df.B[which(df.B$stra_n==x),"gewicht"])))
   rw <- data.frame(stratum=stra_n,rw=weights.O/weights.B)
   
   #Construct weight adjustment
   df.B$rw <- NA
   df.B[,"rw"] <- rw[match(df.B[,"stra_n"],rw[,1]),2]
   
   return(df.B$rw)
   
   #sum(df$gewicht)/(sum(df.B$gewicht*df.B$rw))
   #a <- unlist(lapply(split(df.B,df.B[,"stra_n"]), function(x) sum(x$gewicht*x$rw)))
   #b <- unlist(lapply(split(df,df[,"stra_n"]), function(x) sum(x$gewicht)))
   #tab <- data.frame(names(a),a/b)
   #head(tab, n=50)
   #df.B[which(df.B$stra_n==1),c("gewicht","rw","entid_n")]
   #df[which(df$stra_n==1),c("gewicht","entid_n")]
}

