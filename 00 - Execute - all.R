########################################################
##
## A matter of size, composition, or structure?
## Decomposing inequality associated 
## with a binary covariate
## R code for the empirical analysis of SESS data 
##
## David Gallusser, david.gallusser@unibas.ch
## September 2021
##
#########################################################
##
## To replicate results, set 'wd' to the directory 
## where the replication files are saved. 
## 
## Warning: 
## The bootstrap is time-consuming.
##
########################################################
#### Set working directory

# Main working directory
wd <- "C:/Users/gallusse/switchdrive/Uni Basel/Dissprojekte/1 Lohnungleichheit in der Schweiz/2 Extended DL deco/R code and results/Extended_DL_deco"
setwd(wd)
wd <- "C:/Users/David Gallusser/switchdrive/Uni Basel/Dissprojekte/1 Lohnungleichheit in der Schweiz/2 Extended DL deco/R code and results/Extended_DL_deco"
setwd(wd)
wd <- getwd()

# Directory where SESS data is saved
wd_LSE_data <- "Z:/_LSE/LSE-Daten"

# Directory where to save results
wd_results <- paste0(wd,"/../Extended_DL_deco_results")

# Directory where to save plots and tables
wd_plots <- paste0(wd,"/../../../../_Thesis/Manuscript/Chapter1/plots/") 

########################################################
# Bootstrap inference?
bsexecute <- TRUE
ncores <- 4 #number of cores for bootstrap parallelization

########################################################
# Which years?
com.year=1996
ref.year=2010

##############################################################
### Install required packages, load relevant libraries

# Packages to be loaded
package_Depends <- c("tictoc",
                     "parallel",
                     "pbapply",
                     "Hmisc",
                     "survey",
                     "fastglm",
                     "Formula",
                     "statar",
                     "car",
                     "reshape2",
                     "ggplot2",
                     "ggthemes",
                     "ggpubr",
                     "stargazer",
                     "xtable")


# Packages to be installed
package_Imports <-   c("readxl",
                       "dplyr",
                       "tidyr")

packages <- c(package_Imports,package_Depends)

# Check if installed. If not, install binary version.
if(length(setdiff(packages, rownames(installed.packages()))) > 0) {
  packages_to_install <- setdiff(packages, rownames(installed.packages()))
  cat(paste0("Packages to install:\n",paste0(packages_to_install,collapse=", ")))
  install.packages(packages_to_install ,
                   type="binary")
}

# Load packages
for(i in package_Depends){
  require(i,character.only = TRUE)
}

########################################################
#Functions
setwd(wd)
source("functions/r-DFL-deco-2-0.R")
source("functions/r-half-sample-bootstrap 2.R")

########################################################
#Load plot setting
source("functions/00 - table output function and plot settings.R")

########################################################
#Set memory limit
memory.limit(size=100000)

########################################################
# A - data preparation

# A1 - prepare LSE data
setwd(wd)
source("A1 - LSE data prep.R")

# A2 - create half-sample bootstrap samples 
setwd(wd)
source("A2 - Create half-sample bootstrap samples.R")

########################################################
# B - Decompositions

# B1 - Extended DL decomposition
setwd(wd)
source("B1 - Extended DL decomposition.R")

# B2 - DFL decomposition of PP gap 
setwd(wd)
source("B2 - DFL decomposition of PP gap.R")

########################################################
# C - Descriptive statistics

# C1 - Wage inequality
setwd(wd)
source("C1 - Wage inequality.R")

# C2 - PP share
setwd(wd)
source("C2 - PP share.R")

# C3 - Other descriptive statistics
setwd(wd)
source("C3 - Other descriptive statistics.R")

# C4 - Check sample balance
setwd(wd)
source("C4 - Sample balance.R")

########################################################
# D - Plots  and tables

# D1 - Plot decomposition results (incl. table)
setwd(wd)
source("D1 - Plot deco results.R")

# D2 - Plot inequality and performance-pay growth
setwd(wd)
source("D2 - Plot ineq and PP growth.R")

# D3 - Plot inequality and performance-pay growth
setwd(wd)
source("D3 - Tables descriptive statistics.R")

# D4 - Tables sample balance
setwd(wd)
source("D4 - Tables tables sample balance.R")

# D5 - LSE vs. AHV data (incl. data preparation)
setwd(wd)
source("D5 - LSE vs AHV earnings.R")


