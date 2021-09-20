########################################################
########################################################
### D3 Tables descriptive statistics

########################################################
#### Descriptive statistics table by Non-PPJ and PPJ
#load data
setwd(wd_results)
load("5-other-descriptive-statistics.rda")
setwd(wd_plots)

rownames(stats1996)
rownames(stats2010)

selectA <- c(2,4:8,3,9:19)
selectB <- c(22:41,20,42:43)
selectC <- grep("taetigk",rownames(stats2010))
selectC <- selectC[c(2:18,1,19:length(selectC))]

a1 <- stats1996[selectA,1:2]
b1 <- stats1996[selectB,1:2]
c1 <- stats1996[selectC,1:2]

a2 <- stats2010[selectA,1:2]
b2 <- stats2010[selectB,1:2]
c2 <- stats2010[selectC,1:2]

tabA <- cbind(a1,a2)
tabB <- cbind(b1,b2)
tabC <- cbind(c1,c2)

tabA <- rbind(tabA,
              cbind(obs1996,obs2010))

labelA <- c("Male",
            "Edu.: University",
            "Edu.: Inst. of higher edu.",
            "Edu.: Higher vocational edu.",
            "Edu.: Teacher certificate",
            "Edu.: Univ. entrance cert.",
            "Edu.: Vocatational education",
            "Edu.: Enterprise internal edu.",
            "Edu.: No compl. vocat. edu.",
            "Edu.: Other completed edu.",
            "Pot. experience (in years)",
            "Tenure (in years)", 
            "Position: No manager. func.",
            "Position: Cadre 1",
            "Position: Cadre 2",
            "Position: Cadre 3",
            "Position: Cadre 4",
            "Firm size (employees)",
            "Observations",
            "Full time equivalents (weights)")

labelB <- gsub("nog_agg","",rownames(stats1996)[selectB])
labelB <- gsub("&","\\\\&",labelB)
#labelB <- 1:length(labelB)
labelB <- gsub("Manufac", "Manufacture of", labelB)
labelB <- gsub("D. Pharma, Chemical","D. Manufacture of Pharma \\\\& Chemicals", labelB)
labelB <- gsub("F\\\\&E; o. Business Services","R\\\\&D; other Business Services", labelB)


labelC <- c("10 Manufacturing, processing", 
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
            "28 Research and development",
            "29 Oper.	Analysis, programming, operating",
            "30 Planning, design, draftsmanship, layout",
            "31 Passenger, goods transport a. commun.",
            "32 Security, surveillance",
            "33 Medical, nursing and social functions",
            "34 Personal hygiene, dress care",
            "35 Cleaning and public hygiene",
            "36 Teaching activities",
            "37 Hotel, catering trade work, housework",
            "38 Culture, info., recreation, sport a. leisure",
            "40 Other activities")

rownames(tabA) <- labelA
rownames(tabB) <- labelB
rownames(tabC) <- labelC

colnames(tabA) <- rep(c("Non-PPJ","PPJ"),2)
colnames(tabB) <- rep(c("Non-PPJ","PPJ"),2)
colnames(tabC) <- rep(c("Non-PPJ","PPJ"),2)

tabA 
tabB
tabC

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("&\\multicolumn{2}{c}{1996} &\\multicolumn{2}{c}{2010} \\\\\n"," & Non-PPJ & PPJ & Non-PPJ & PPJ\\\\\n")

# Saving stargazer output in text files
mod_xtable <- function(output.file, ...) {
  output <- capture.output(print(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=FALSE)
}

title <- "Summary statistics SESS data: Worker characteristics (sample shares if not indicated otherwise)."
labeltab <- "tab:desc1"
tabA <- xtable(tabA, 
               caption=title, 
               label=labeltab,
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabA) <- matrix(rep(c(rep(3,10),1,1,rep(3,5),0,0,0),5),ncol=5,byrow=FALSE)
align(tabA) <- c("X",rep("C{2cm}",4))
tabA


title <- "Summary statistics SESS data: Industry shares."
labeltab <- "tab:desc2"
tabB <- xtable(tabB, 
               caption=title, 
               label=labeltab,
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabB) <- matrix(rep(3,5*nrow(tabB)),ncol=5,byrow=FALSE)
align(tabB) <- c("X",rep("C{1.5cm}",4))


title <- "Summary statistics SESS data: Task shares."
labeltab <- "tab:desc3"
tabC <- xtable(tabC, 
               caption=title, 
               label=labeltab,
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabC) <- matrix(rep(3,5*nrow(tabC)),ncol=5,byrow=FALSE)
align(tabC) <- c("X",rep("C{1.25cm}",4))



#align(tab.exp) <- xalign(tab.exp)

# Save tables
mod_xtable("summary_desc1.tex",tabA,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE)

mod_xtable("summary_desc2.tex",tabB,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE)

mod_xtable("summary_desc3.tex",tabC,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity,
           add.to.row = addtorow, include.colnames = FALSE, size = "footnotesize" #scalebox = 0.8
           )

##############################################################################
# Performance-pay share tables

a1 <- ppshareres96[c(1:2,4:8,3,9:16,18,17,19)]
b1 <- ppshareres96[c(21:41,20,42:43)]
c1 <- ppshareres96[c(44:67)][c(2:18,1,19:length(c(44:67)))]

a2 <- ppshareres10[c(1:2,4:8,3,9:16,18,17,19)]
b2 <- ppshareres10[c(21:41,20,42:43)]
c2 <- ppshareres10[c(44:67)][c(2:18,1,19:length(c(44:67)))]

tabA <- cbind(a1,a2)
tabB <- cbind(b1,b2)
tabC <- cbind(c1,c2)

labelA <- c("Women",
            "Men",
            "Edu.: University",
            "Edu.: Institute of higher education",
            "Edu.: Higher vocational education",
            "Edu.: Teacher certificate",
            "Edu.: University entrance certificate",
            "Edu.: Completed vocational education",
            "Edu.: Enterprise internal education",
            "Edu.: No completed vocational education",
            "Edu.: Other completed educations",
            "Position: No managerial function",
            "Position: Cadre 1",
            "Position: Cadre 2",
            "Position: Cadre 3",
            "Position: Cadre 4",
            "Firm size $<$ 50",
            "Firm size 50-499",
            "Firm size $\\geq$ 500")

labelB <- gsub("&","\\\\&",names(b1))
#labelB <- 1:length(labelB)
labelB <- gsub("Manufac", "Manufacture of", labelB)
labelB <- gsub("D. Pharma, Chemical","D. Manufacture of Pharma \\\\& Chemicals", labelB)
labelB <- gsub("F\\\\&E; o. Business Services","R\\\\&D; other Business Services", labelB)

rownames(tabA) <- labelA
rownames(tabB) <- labelB
rownames(tabC) <- labelC

colnames(tabA) <- c("1996","2010")
colnames(tabB) <- c("1996","2010")
colnames(tabC) <- c("1996","2010")

tabA 
tabB
tabC

# Saving stargazer output in text files
mod_xtable <- function(output.file, ...) {
  output <- capture.output(print(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=FALSE)
}

title <- "Share of performance-pay workers by worker characteristics, 1996 and 2010"
labeltab <- "tab:pp1"
tabA <- xtable(tabA, 
               caption=title, 
               label=labeltab,
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabA) <- matrix(rep(3,nrow(tabA)*3),ncol=3,byrow=FALSE)
align(tabA) <- c("X",rep("C{2.5cm}",2))
tabA


title <- "Share of performance-pay workers by industry, 1996 and 2010"
labeltab <- "tab:pp2"
tabB <- xtable(tabB, 
               caption=title, 
               label=labeltab,
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabB) <- matrix(rep(3,nrow(tabB)*3),ncol=3,byrow=FALSE)
align(tabB) <- c("X",rep("C{2.5cm}",2))



title <- "Share of performance-pay workers by task, 1996-2010"
labeltab <- "tab:pp3"
tabC <- xtable(tabC, 
               caption=title, 
               label=labeltab,
               type="latex",
               booktabs = FALSE,
               auto=TRUE)

digits(tabC) <- matrix(rep(3,nrow(tabC)*3),ncol=3,byrow=FALSE)
align(tabC) <- c("X",rep("C{2.5cm}",2))


#align(tab.exp) <- xalign(tab.exp)

# Save tables
mod_xtable("PP_share1.tex",tabA,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity)

mod_xtable("PP_share2.tex",tabB,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity)

mod_xtable("PP_share3.tex",tabC,
           booktabs = TRUE, 
           caption.placement = "top",
           format.args=list(big.mark = ",",  decimal.mark = "."),
           tabular.environment = 'tabularx', width="1\\textwidth",
           sanitize.colnames.function = identity, 
           sanitize.rownames.function = identity, scalebox = 1)

setwd(wd)