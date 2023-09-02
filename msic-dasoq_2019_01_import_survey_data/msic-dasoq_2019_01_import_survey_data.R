####################################################
### STEP 1: PACKAGES & OPTIONS
####################################################

# environment
require(orgutils)
require(ggplot2)
require(data.table)
require(reshape2)
require(tikzDevice)

####################################################
### STEP 2: HELPER-FUNCTIONS
####################################################
createSummaries <- function(tbl){
  
  # table() needs factors to show all options
  tbl[ ,"Monetary Regime"] <- factor(tbl[ ,"Monetary Regime"], levels = c("IT", "MT", "ERT", "Other"))
  tbl[ ,"Exchange Rate Arrangement"] <- factor(tbl[ ,"Exchange Rate Arrangement"], levels = c("Hard Peg", "Soft Peg", "Free Float", "Float. w. Int.", "Residual"))
  tbl[ ,"Exchange Rate Arrangement (detailled)"] <- factor(tbl[ ,"Exchange Rate Arrangement (detailled)"], levels = c("Free float", "Float. w. Int.", "Conventional Peg", "Peg with Bands", "Crawling Peg", "Currency Board", "Residual"))
  
  colsel_std       <- c("Project (Stabilized Token)", "Index")
  colsel_MR        <- c("Monetary Regime")
  colsel_ERA       <- c("Exchange Rate Arrangement")
  colsel_ERA_detailled       <- c("Exchange Rate Arrangement (detailled)")
  colsel_status    <- c("Status")
  colsel_launched  <- c("Listed on Coinmarketcap")
  colsel_vuln      <- c("Vulnerability to Speculative Attacks")
  
  subtbl_MR        <- tbl[ , c(colsel_std, colsel_MR)]
  subtbl_ERA       <- tbl[ , c(colsel_std, colsel_ERA)]
  subtbl_ST        <-  tbl[ , !colnames(tbl) %in% c(colsel_std, colsel_MR, colsel_ERA, colsel_launched, colsel_status, colsel_vuln, colsel_ERA_detailled)]
  subtbl_launched  <- tbl[ , c(colsel_std, colsel_launched)]
  subtbl_status    <- tbl[ , c(colsel_std, colsel_status)]
  subtbl_vuln      <- tbl[ , c(colsel_std, colsel_vuln)]
  
  summary_MR       <- as.list(table(subtbl_MR[ ,"Monetary Regime"]))
  summary_ERA      <- as.list(table(subtbl_ERA[ ,"Exchange Rate Arrangement"]))
  summary_ST       <- lapply(subtbl_ST, function(x){sum(x == "x")})
  summary_launched <- as.list(table(subtbl_launched[ ,"Listed on Coinmarketcap"]))
  summary_status   <- as.list(table(subtbl_status[ ,"Status"]))
  summary_vuln     <- as.list(table(subtbl_vuln[ ,"Vulnerability to Speculative Attacks"]))
  
  summary_MR_perc        <- lapply(summary_MR, function(x){round((x / Reduce(sum, summary_MR))*100, digits = 2)})
  summary_ERA_perc       <- lapply(summary_ERA, function(x){round((x / Reduce(sum, summary_ERA))*100, digits = 2)})
  summary_ST_perc        <- lapply(summary_ST, function(x){round((x / nrow(tbl))*100, digits = 2)})
  summary_launched_perc  <- lapply(summary_launched, function(x){round((x / Reduce(sum, summary_launched))*100, digits = 2)})
  summary_status_perc    <- lapply(summary_status, function(x){round((x / Reduce(sum, summary_status))*100, digits = 2)})
  summary_vuln_perc      <- lapply(summary_vuln, function(x){round((x / Reduce(sum, summary_vuln))*100, digits = 2)})
  
  summary <- list()
  summary[["absolute"]] <- list()
  summary[["absolute"]][["MR"]]       <- summary_MR
  summary[["absolute"]][["ERA"]]      <- summary_ERA
  summary[["absolute"]][["ST"]]       <- summary_ST
  summary[["absolute"]][["launched"]] <- summary_launched
  summary[["absolute"]][["vuln"]]     <- summary_vuln
  
  summary[["relative"]] <- list()
  summary[["relative"]][["MR"]]       <- summary_MR_perc
  summary[["relative"]][["ERA"]]      <- summary_ERA_perc
  summary[["relative"]][["ST"]]       <- summary_ST_perc
  summary[["relative"]][["launched"]] <- summary_launched_perc
  summary[["relative"]][["vuln"]]     <- summary_vuln_perc
  
  return(summary)
  
}


extractSubSec <- function(tbl,
                          col,
                          choice){
  subsel <- tbl[tbl[ ,col] == choice, "Index"]
  
  return(subsel)
}


dictNamesFromIndex <- function(tbl,
                               indexname_vec){
  
  
  
  beautinames <- sapply(indexname_vec,
                        function(x){tbl[tbl$Index == x, "Project (Stabilized Token)"]})
  
  return(beautinames)
}





### Load Survey Data & Summaries ###
setwd("/Users/ingolfpernice/Documents/msic-dasoq_2019/msic-dasoq_2019_01_import_survey_data")
tbl <- readOrg("./table_stablecoin.org")
tbl[is.na(tbl)] <- ""
colsel_std  <- c("Project",
                 "Index")


## subset table for grafics
tbl_only_launched <- tbl[!grepl(x=tbl[ ,"Status"], pattern="Not Impl.|Partially Impl.|Retracted"), ]


## Create summaries
summary_only_launched <- createSummaries(tbl_only_launched)
summary_all           <- createSummaries(tbl)

# Create tables for ERA
table_all_ERA <- t(as.data.frame(summary_all$absolute$ERA, check.names = FALSE))
colnames(table_all_ERA) <- "Fraction"
table_only_launched_ERA <- t(as.data.frame(summary_only_launched$absolute$ERA, check.names = FALSE))
colnames(table_only_launched_ERA) <- "Fraction"


## Create tables for MR
table_all_MR <- t(as.data.frame(summary_all$absolute$MR, check.names = FALSE))
colnames(table_all_MR) <- "Fraction"
table_only_launched_MR <- t(as.data.frame(summary_only_launched$absolute$MR, check.names = FALSE))
colnames(table_only_launched_MR) <- "Fraction"

## Create tables for ST
table_all_ST <- t(as.data.frame(lapply(summary_all$absolute$ST, function(x){100*x/nrow(tbl)}), check.names = FALSE))
table_all_ST <- round(table_all_ST, digits = 2)
colnames(table_all_ST) <- "Fraction"

table_only_launched_ST <- t(as.data.frame(lapply(summary_only_launched$absolute$ST, function(x){100*x/nrow(tbl)}), check.names = FALSE))
table_only_launched_ST <- round(table_only_launched_ST, digits = 2)
colnames(table_only_launched_ST) <- "Fraction"
