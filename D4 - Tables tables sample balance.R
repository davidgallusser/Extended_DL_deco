########################################################
########################################################
### D4 Tables sample balance

########################################################
#### Descriptive statistics table by Non-PPJ and PPJ
#load data
setwd(wd_results)
load("6-sample-balance.rda")
setwd(wd_plots)

########################################################
# Saving stargazer output in text files
mod_xtable <- function(output.file, ...) {
  output <- capture.output(print(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=FALSE)
}

########################################################
# Select table rows

sel3a <- grep("taetig",rownames(balance.tab1))
sel4 <- grep("nog",rownames(balance.tab1))
sel1 <- 1:length(rownames(balance.tab1))

sel1 <- setdiff(sel1,c(sel3a,sel4)) #setdiff(sel1,c(sel2a,sel3a,sel4))
sel2 <- sel3a #c(sel2a,sel3a)
sel3 <- sel4

sel2a <- grep("beruf",rownames(balance.tab1))
sel5 <- grep("ausbild",rownames(balance.tab1))
sel6 <- grep("untgroe",rownames(balance.tab1))
sel7 <- grep("geschle",rownames(balance.tab1))
sel8 <- grep("potex",rownames(balance.tab1))
sel9 <- grep("dienstja",rownames(balance.tab1))

sel1 <- c(1,sel7,sel5,sel8,sel9,sel6,sel2a)

########################################################
# Rowname tables

balance.tabs <- list(balance.tab1,balance.tab2,balance.tab3,balance.tab4)

for(i in 1:length(balance.tabs)){
rownames(balance.tabs[[i]]) <- gsub("nog_agg","",rownames(balance.tabs[[i]]))
rownames(balance.tabs[[i]]) <- gsub("&","\\\\&",rownames(balance.tabs[[i]]))
rownames(balance.tabs[[i]]) <- gsub("_","\\\\_",rownames(balance.tabs[[i]]))
rownames(balance.tabs[[i]])<- gsub("Manufac", "Manufacture of", rownames(balance.tabs[[i]]))
rownames(balance.tabs[[i]]) <- gsub("D. Pharma, Chemical","D. Manufacture of Pharma \\\\& Chemicals", rownames(balance.tabs[[i]]))
rownames(balance.tabs[[i]]) <- gsub("F\\\\&E; o. Business Services","R\\\\&D; other Business Services", rownames(balance.tabs[[i]]))


rownames(balance.tabs[[i]])[1] <- "Performance-pay Job"
rownames(balance.tabs[[i]])[sel2a] <- c("Position: Cadre 1",
                                   "Position: Cadre 2",
                                   "Position: Cadre 3",
                                   "Position: Cadre 4")
rownames(balance.tabs[[i]])[sel3a] <- c("10 Manufacturing, processing", 
                                  "11 Act. construction sector",
                                  "12 Fitting, operation, maint. of machinery",
                                  "13 Restoration, crafts",
                                  "20 Def. of corporate targets a. strategy",
                                  "21 Accounting and personnel",
                                  "22 Secretarial, office work",
                                  "23 Other comm., admin. functions",
                                  "24 Logistics, staff duties",
                                  "25 Evaluation, consultancy, certification",
                                  "26 Buying/selling of basic mater. a. indust. goods",
                                  "27 Retail sale of consumer goods and services",
                                  "28 Research and developement",
                                  "29 Oper.	Analysis, programming, operating",
                                  "30 Planning, design, draftsmanship, layout",
                                  "31 Passenger, goods transport a. commun.",
                                  "32 Security, surveillance",
                                  "34 Personal hygiene, dress care",
                                  "35 Cleaning and public hygiene",
                                  "36 Teaching activities",
                                  "37 Hotel, catering trade work, housework",
                                  "38 Culture, info., recreation, sport a. leisure",
                                  "40 Other activities")
rownames(balance.tabs[[i]])[sel5] <- c("Edu.: University",
                                  "Edu.: Institute of higher education",
                                  "Edu.: Higher vocational education",
                                  "Edu.: Teacher certificate",
                                  "Edu.: University entrance certificate",
                                  #"Edu.: Completed vocational education",
                                  "Edu.: Enterprise internal education",
                                  "Edu.: No completed vocational education",
                                  "Edu.: Other completed educations")
rownames(balance.tabs[[i]])[sel6] <- c("Firm size 0-49", "Firm size $\\geq$ 500")
rownames(balance.tabs[[i]])[sel7] <- c("Male")
rownames(balance.tabs[[i]])[sel8] <- c("Potential exp. 0-4", 
                                  "Potential exp. 5-14", 
                                  "Potential exp. 25-34",
                                  "Potential exp. 35-44",
                                  "Potential exp. $\\geq$ 45")
rownames(balance.tabs[[i]])[sel9] <- c("Tenure 0-1", 
                                  "Tenure 2-4", 
                                  "Tenure 15-24",
                                  "Tenure $\\geq$ 25")
}

balance.tab1 <- balance.tabs[[1]]
balance.tab2 <- balance.tabs[[2]]
balance.tab3 <- balance.tabs[[3]]
balance.tab4 <- balance.tabs[[4]]

########################################################
# Table 1: 
title <- "Empty"
balance.tab1 <-xtable(cbind(balance.tab1, balance.tab2), 
               caption=title, 
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(balance.tab1) <- matrix(rep(c(3,3,3,4,3,3,4),nrow(balance.tab1)),ncol=7,byrow=TRUE)
align(balance.tab1) <- c("X",rep("C{1.1cm}",6))
balance.tab1

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("&\\multicolumn{3}{c}{1996} &\\multicolumn{3}{c}{2010} \\\\\n"," & Mean obs. & Mean rw. & Std. diff. & Mean obs. & Mean rw. & Std. diff.\\\\\n")


### Worker characteristics variables 
attr(balance.tab1,'caption') <- "Sample balance of worker characteristics shares.
                                 'Observed' values are computed with entire samples from 1996 and 2010, respectively. 
                                 'Reweighted' values are estimated with samples of workers without performance-pay in those years
                                 reweighted by $\\hat{\\Psi}_{S_1,1996}$ and $\\hat{\\Psi}_{S_1,2010}$, respectively.\\label{tab:balance1-1}"

fn <- "tab-sample-balance-1-1.tex"

# Save tables
mod_xtable(fn,balance.tab1[sel1,],
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = "tabularx", width="1\\textwidth",#, # width="\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "footnotesize" #scalebox = 0.8
           )

### Job variables
attr(balance.tab1,'caption') <- "Sample balance of task shares.
                                 'Observed' values are computed with entire samples from 1996 and 2010, respectively. 
                                 'Reweighted' values are estimated with samples of workers without performance-pay in those years
                                 reweighted by $\\hat{\\Psi}_{S_1,1996}$ and $\\hat{\\Psi}_{S_1,2010}$, respectively.\\label{tab:balance1-2}"
fn <- "tab-sample-balance-1-2.tex"

# Save tables
mod_xtable(fn,balance.tab1[sel2,],
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = "tabularx", width="\\textwidth",#, # width="\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "scriptsize" #scalebox = 0.65
)


### Industry variables
attr(balance.tab1,'caption') <- "Sample balance of industry shares.
                                 'Observed' values are computed with entire samples from 1996 and 2010, respectively. 
                                 'Reweighted' values are estimated with samples of workers without performance-pay in those years
                                 reweighted by $\\hat{\\Psi}_{S_1,1996}$ and $\\hat{\\Psi}_{S_1,2010}$, respectively.\\label{tab:balance1-3}"


fn <- "tab-sample-balance-1-3.tex"

# Save tables
mod_xtable(fn,balance.tab1[sel3,],
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = "tabularx", width="1\\textwidth",#, # width="\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "footnotesize" #, scalebox = 0.65
)




########################################################
# Table 2

balance.tab1 <-xtable(cbind(balance.tab3, balance.tab4), 
                      caption=title, 
                      type="latex",
                      booktabs = FALSE,
                      auto=TRUE)


rownames(balance.tab1) <- gsub("_","\\\\_",rownames(balance.tab1))

digits(balance.tab1) <- matrix(rep(c(3,3,3,4,3,3,4),nrow(balance.tab1)),ncol=7,byrow=TRUE)
align(balance.tab1) <- c("X",rep("C{1.1cm}",6))
balance.tab1

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("&\\multicolumn{3}{c}{$\\hat{\\Psi}_{G}$} &\\multicolumn{3}{c}{$\\hat{\\Psi}_{X}$} \\\\\n"," & Mean obs. & Mean rw. & Std. diff. & Mean obs. & Mean rw. & Std. diff.\\\\\n")


# Worker characteristics variables
attr(balance.tab1,'caption') <- "Sample balance of worker characteristics shares:
                                 'Observed' values in first column are computed with sample of 2010; those in forth column are computed with sample of 1996.
                                 'Reweighted' values in second column are estimated with sample of 2020 reweighted by factor $\\hat{\\Psi}_{G}$; 
                                 those in fifth column are estimated with sample of 2020 reweighted by factor $\\hat{\\Psi}_{X}$.\\label{tab:balance2-1}"



fn <- "tab-sample-balance-2-1.tex"

# Save tables
mod_xtable(fn,balance.tab1[sel1,],
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",#tabular.environment = 'longtable' # width="\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "footnotesize" #scalebox = 0.8
)

# Job variables
attr(balance.tab1,'caption') <- "Sample balance of task shares:
                                  'Observed' values in first column are computed with sample of 2010; those in forth column are computed with sample of 1996.
                                 'Reweighted' values in second column are estimated with sample of 2020 reweighted by factor $\\hat{\\Psi}_{G}$; 
                                 those in fifth column are estimated with sample of 2020 reweighted by factor $\\hat{\\Psi}_{X}$.\\label{tab:balance2-2}"
fn <- "tab-sample-balance-2-2.tex"

# Save tables
mod_xtable(fn,balance.tab1[sel2,],
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="\\textwidth",#tabular.environment = 'longtable' # width="\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "scriptsize" #scalebox = 0.65
)

# Industry variables.
attr(balance.tab1,'caption') <- "Sample balance of industry shares:
                                  'Observed' values in first column are computed with sample of 2010; those in forth column are computed with sample of 1996.
                                 'Reweighted' values in second column are estimated with sample of 2020 reweighted by factor $\\hat{\\Psi}_{G}$; 
                                 those in fifth column are estimated with sample of 2020 reweighted by factor $\\hat{\\Psi}_{X}$.\\label{tab:balance2-3}"
fn <- "tab-sample-balance-2-3.tex"

# Save tables
mod_xtable(fn,balance.tab1[sel3,],
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",#tabular.environment = 'longtable' # width="\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "footnotesize" # scalebox = 0.65
)

setwd(wd)
