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

getCoinData <- function(coins, date_start, date_end){
  
  data_list <- list()
  file_list <- list.files(path = "./data/")
  for(coin in coins){
    
    # find file
    file <- file_list[grep(coin, c(file_list, date_start, date_end))]
    
    # error exeption
    if(length(file) > 1) {break; print("Check your downloads for duplicates!")}
    
    # load data
    dta <- read.csv(file = paste("./data/", file, sep=""))
    
    # save data in ram
    data_list[[which(coin == coins)]] <- dta
    
  }
  names(data_list) <- coins
  
  
  return(data_list) 
}

timestampsToPosixct <- function(df, cname){
  
  df[ ,cname] <- round_date(as.POSIXct(df[ ,cname]/1000, origin="1970-01-01"), "day")
  
  return(df)
  
}

addCoinSupply <- function(df,
                          cap_col = "market_cap_by_available_supply",
                          price_col = "price_usd",
                          replaceInf = TRUE,
                          replaceMult = 2,
                          replaceType = "mult",
                          removeFirstRow = FALSE){
  
  supply <- df[ ,cap_col, drop = FALSE] / df[ ,price_col, drop = FALSE]
  colnames(supply) <- "available_supply"
  df_new <- cbind(df, supply)
  
  supply_change <- return.element(as.vector(t(supply)))
  if(replaceInf == TRUE){
    supply_change <- replInfSup(sup = supply_change, multiplicator = replaceMult, type = replaceType)
  }
  supply_change <- as.data.frame(supply_change)
  colnames(supply_change) <- "supply_change_in_procent"
  df_new <- cbind(df_new, supply_change)
  
  if(removeFirstRow == TRUE){df_new <- df_new[-1, ]}
  
  
  return(df_new)
  
}

logReturn.element <- function(x){
  lr <- (log(x)-log(dplyr::lag(x)))
  return(lr)
}

return.element <- function(x){
  r <- ((x/dplyr::lag(x))-1)
  return(r)
}

replInfSup <- function(sup, multiplicator = 2, type=c("mult", "na")){
  
  sup.max <- max(sup)
  sup.min <- min(sup)
  
  if(type == "mult"){
    
    sup[sup == "Inf" ] <- sup.max * multiplicator
    sup[sup == "-Inf" ] <- sup.min * multiplicator
    
  } else if(type == "na"){
    
    sup[sup == "Inf" ] <- NA
    sup[sup == "-Inf" ] <- NA
    
  }
  
  return(sup)
  
}


addReturns <- function(raw_df,
                       type = "simple",
                       applLog = FALSE,
                       price_col = "price_usd",
                       removeFirstRow = TRUE){
  
  
  # extract correct colname for price index
  cname_prices <- colnames(raw_df)[grepl(x = colnames(raw_df), pattern = price_col)] 
  
  if (type == "simple"){
    
    # extract price index and calc returns / log returrs
    price_df <- raw_df[ ,cname_prices]
    ret <- if(applLog == FALSE){
      return.element(price_df)
    } else if (applLog == TRUE) {
      logReturn.element(price_df)
    }
    
    ret = as.data.frame(ret)
    colnames(ret) <-paste0("return_wrt_", price_col, ".",type)
    ret_df <- cbind(raw_df, ret)
  }
  
  
  if(class(ret_df$timestamps) == "string"){ret_df$timestamp <- as.numeric(as.POSIXct(ret_df$timestamp))}
  
  if(removeFirstRow == TRUE){ret_df <- ret_df[-1, ]}
  
  return(ret_df)
}

addVolatility <- function(raw_df,
                          type="squaredreturns",
                          return_col = "return_wrt_price_usd%simple%",
                          replaceInf = TRUE,
                          replaceMult = 2,
                          replaceType = "mult"){
  #calc Volatility wrt. type
  if (type == "squaredreturns"){
    
    vol <- (raw_df[ ,return_col])^2
    
  } else if (type == "realizedvolatility"){
    
    print("currently no HF data availble")
    
  }
  
  # replace inf
  if(replaceInf == TRUE){
    vol <- replInfVol(vol, multiplicator = replaceMult, type = replaceType)
  }
  
  vol_df <- as.data.frame(vol)
  colnames(vol_df) <- paste0("vol", ".",type)
  vol_df <- cbind(raw_df, vol_df)
  
  return(vol_df)
  
}

replInfVol <- function(vol, multiplicator = 2, type=c("mult", "na")){
  
  vol.max <- max(vol)
  vol.min <- min(vol)
  
  if(type == "mult"){
    
    vol[vol == "Inf" ] <- vol.max * multiplicator
    vol[vol == "-Inf" ] <- vol.min * multiplicator
    
  } else if(type == "na"){
    
    vol[vol == "Inf" ] <- NA
    vol[vol == "-Inf" ] <- NA
    
  }
  
  return(vol)
  
}

makeDevTable <- function(dlist = data_list,
                         peg = peg,
                         devs = devs){
  
  .makeDev <- function(peg, devs, data_vec){
    viol_perc <- sapply(devs, function(dev){round(sum(data_vec < peg - dev | data_vec > peg + dev)/length(data_vec), digits = 4)*100})
    names(viol_perc) <- devs
    return(viol_perc)
  }
  dev_list <- lapply(dlist, function(l){.makeDev(l[["price_usd"]],
                                                 peg = peg,
                                                 devs = devs)})
  tbl <- t(as.data.frame(dev_list, check.names = FALSE))
  tbl <- round(tbl, digits = 2)
  tbl <- tbl[order(rowSums(tbl)), ]
  return(tbl)
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


addDifferences <- function(df,
                           replaceInf = TRUE,
                           replaceMult = 2,
                           replaceType = "mult",
                           removeFirstRow = TRUE){
  
  timestamp <- df[ ,"timestamp", drop = FALSE]
  df_wo_time <- df[ ,!colnames(df) %in% "timestamp", drop = FALSE]
  
  D1  <- as.data.frame(lapply(df_wo_time, diff))
  D1  <- rbind(NA, D1) 
  colnames(D1) <- paste0("D1.", colnames(df_wo_time))
  
  D2  <- as.data.frame(lapply(df_wo_time, diff, differences = 2))
  D2  <-  rbind(NA, NA,  D2) 
  colnames(D2) <- paste0("D2.", colnames(df_wo_time))
  
  D3  <- as.data.frame(lapply(df_wo_time, diff, differences = 3))
  D3  <- rbind(NA, NA, NA, D3)
  colnames(D3) <- paste0("D3.", colnames(df_wo_time))
  
  
  if(replaceInf == TRUE){
    D1 <- as.data.frame(lapply(D1, replInfSup, multiplicator = replaceMult, type = replaceType))
    D2 <- as.data.frame(lapply(D2, replInfSup, multiplicator = replaceMult, type = replaceType))
    D3 <- as.data.frame(lapply(D3, replInfSup, multiplicator = replaceMult, type = replaceType))
    
  }
  
  df <- cbind(timestamp, df_wo_time, D1, D2, D3)
  
  if(removeFirstRow == TRUE){df <- df[-c(1:3), ]}
  
  
  return(df)
  
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

####################################################
### STEP 3: LOAD DATA
####################################################

setwd("/Users/ingolfpernice/Documents/msic-dasoq_2019/msic-dasoq_2019_02_prepare_data")
data_list <- getCoinData(SETTINGS$coins,
                         SETTINGS$date_start,
                         SETTINGS$date_end)
data_list <- lapply(data_list,
                    timestampsToPosixct, "timestamp")

####################################################
### STEP 3: SCRIPT
####################################################
## timeseries characteristics
                                        # supply and change in supply
data_list <- lapply(data_list,
                    addCoinSupply,
                    cap_col = "market_cap_by_available_supply",
                    price_col = "price_usd",
                    replaceInf = TRUE,
                    replaceMult = 2,
                    replaceType = "mult",
                    removeFirstRow = FALSE)

                                        # returns
data_list <- lapply(data_list,
                    addReturns,
                    type = "simple",
                    applLog = FALSE,
                    price_col = "price_usd",
                    removeFirstRow = TRUE)


                                        # volatility
data_list <- lapply(data_list,
                    addVolatility,
                    type="squaredreturns",
                    return_col = "return_wrt_price_usd.simple",
                    replaceInf=TRUE,
                    replaceMult=2,
                    replaceType="na")

data_list <- lapply(data_list,
                    addDifferences,
                    replaceInf = TRUE,
                    replaceMult = 2,
                    replaceType = "na",
                    removeFirstRow = TRUE)

####################################################
### STEP 4: SAVE DATA
#################################################### 
save(data_list, file="data.Rda")

