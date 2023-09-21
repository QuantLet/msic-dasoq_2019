####################################################
### STEP 1: PACKAGES & OPTIONS
####################################################

##### Load packages ####

list_of_packages <- c("httr", "jsonlite", "tidyr", "lubridate", "dplyr", "curl", "ggplot2", "moments", "xts", "data.table", "psych", "stargazer", "tikzDevice") # "tikz"

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)){
  install.packages(new_packages)
} else {
  lapply(list_of_packages, require, character.only = TRUE)       
  # (Thanking: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them)
}

##### Settings Object #####
SETTINGS <- list()
SETTINGS$coins <- c(
  "ethereum",
  "tether",
  "trueusd",
  "digix-gold-token",
  "nubits",
  "bitusd",
  "karbo",
  "steem-dollars",
  "bitcoin",
  "stasis-eurs",
  "bitshares",
  "nushares",
  "usd-coin",
  "stronghold-usd",
  "ckusd",
  "usdcoin",
  #"stableusd", 
  "dai",
  "minexcoin",
  "susd")
SETTINGS$date_start <- "1400000000000"
SETTINGS$date_end   <- "1549474383067" #.getTimestamp() #Note: timestamp needs to be frozen, to find the files.


####################################################
### STEP 2: HELPER FUNCTIONS
####################################################
.extractCoins <- function(mixedvec){
    rn <- mixedvec
    rnnew <- NULL
    for(i in 1:length(rn)){ rnnew <- c(rnnew, gsub("[\\(\\)]", "", regmatches(rn, gregexpr("\\(.*?\\)", rn))[[i]]) )}
    return(rnnew)
}

.extractProjects <- function(mixedvec){
    rn <- mixedvec
    rnnew <- gsub("\\s*\\([^\\)]+\\)","",rn)
    return(rnnew)
}


makeDescTblForAll <- function(dta = data_list_nooutliers,
                              subselection = NULL){
  xts_list <- lapply(dta, getXTS)
  df <- xts_list[[1]]$price_usd
  
  .getDescTbl <- function(df,
                          vars){
    tbl <- NULL
    for(var in vars){
      df_sub <- df[,var]
      row <- psych::describe(df_sub)
      tbl <- rbind(tbl, row)
    }
    rownames(tbl) <- vars
    return(tbl)    
  }
  
  ## write table
  desctbls <- lapply(xts_list, .getDescTbl, c("price_usd"))
  desctbls <- do.call(rbind, desctbls)
  
  ## add daily average volatility
  desctbls$dailyvol_avg <- unname(unlist(lapply(data_list, function(x){mean(x[["vol.squaredreturns"]])})))
  
  ## select rows and columns
  desctbls <- desctbls[ ,c("n", "mean", "min", "max", "sd", "dailyvol_avg")]
  colnames(desctbls) <- c("Obs.", "Mean","Min.", "Max.", "Vol. (annualized)", "Vol. (daily averaged)")
  
  if(is.null(subselection) == FALSE){
    desctbls <- desctbls[subselection, ]
  }
  
  return(desctbls)
}


getDesc.element <- function(vec){
  desc <- list()
  vec <- as.vector(vec)
  
  desc[["mean"]] <- mean(vec)
  desc[["sd"]] <- sd(vec)
  desc[["skewness"]] <- skewness(vec)
  desc[["median"]] <- median(vec)
  
  desc <- lapply(desc, round, digits = 4)
  
  return(desc)
}                            # sd boundaries

getDesc <- function(df){
  
  cnames <- colnames(df)
  desc <- lapply(df, getDesc.element)
  
  return(desc)
  
}

getXTS <- function(df, name_of_time_col = "timestamp"){
  df.xts <- xts(df[ ,!colnames(df) %in% name_of_time_col],
                order.by=df[ ,name_of_time_col])
  
  return(df.xts)
}

dictNamesFromIndex <- function(tbl,
                               indexname_vec){
  
  
  
  beautinames <- sapply(indexname_vec,
                        function(x){tbl[tbl$Index == x, "Project (Stabilized Token)"]})
  
  return(beautinames)
}



####################################################
### STEP 3: LOAD DATA
#################################################### 
setwd("/Users/ingolfpernice/Documents/msic-dasoq_2019/msic-dasoq_2019_03_descriptives")
load("data.Rda")
tbl <- readOrg("./table_stablecoin.org")

####################################################
### STEP 4: SCRIPT
#################################################### 
## static characteristics
                                        # Mean, Median, Skewness, Variance
descriptives <- lapply(data_list, getDesc)

                                        # ... for example nubits or steem-dollar
descriptives.nubits <- descriptives[["nubits"]][["return_wrt_price_usd.simple"]]
descriptives.tether <- descriptives[["tether"]][["return_wrt_price_usd.simple"]]


## descriptive table
desctbl <- makeDescTblForAll(dta          = data_list,
                             subselection = tbl[tbl$'Listed on Coinmarketcap' == "Yes" ,"Index"])
                                        # read names from dictionary
rownames(desctbl) <- dictNamesFromIndex(tbl,
                                       rownames(desctbl))
##                                      # extract coin names
## rownames(desctbl) <- .extractProjects(unname(rownames(desctbl)))
## colnames(desctbl)[1] <- "Project"
                                        # sort wrt. names
desctbl <- desctbl[order(-desctbl[,"Obs."]), ]

## table to tex
stargazer(desctbl[ ,1:5]
         ,summary  = FALSE
         ,covariate.labels = c("Projects", "Obs.", "Mean", "Min.", "Max.", "Std. Dev.") 
         ,title    = "Descriptive statistics for implemented stablecoin projects"
         ,out      = "./descriptives.tex"
         ,float    = FALSE
         ,column.sep.width = "-7pt"
         ,digit.separate = 0
         ,align = TRUE
          )
