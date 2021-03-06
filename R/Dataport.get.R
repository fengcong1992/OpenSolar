#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to download and manamge the Dataport dataset
# 2 Coder: Cong Feng        Date: 2018/10/12       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------

Dataport.get <- function(usrname, pswd, hsid, timeres, qc){ # house id, time resolution
  library(RPostgreSQL)
  library(zoo)
  library(imputeTS)
  # read table from the database
  drv <- dbDriver("PostgreSQL") # set up SQL driver
  name_tb <- paste0('university.electricity_egauge_', timeres) # table name
  con <- dbConnect(drv, dbname = "postgres", # set up connections to Dataport database
                   host = "dataport.cloud", port = 5434,
                   user = usrname, password = pswd)
  data_tblist <- dbListTables(con)
  sqlcommand <- paste('SELECT * FROM ', name_tb,
                      ' where dataid=', hsid, sep = '')
  sqlcommand2 <- paste('SELECT total_amount_of_pv FROM ', 'university.metadata',
                       ' where dataid=', hsid, sep = '')
  data_house <- dbGetQuery(con, sqlcommand)
  cap <- as.double(dbGetQuery(con, sqlcommand2))
  dbDisconnect(con) # disconnect
  
  data_clean1 <- data_house[, colMeans(is.na(data_house)) <= .1] # columns with 90% NAs are removed
  for (no_col in 3:ncol(data_clean1)) {
    data_clean1[,no_col] <- as.double(data_clean1[,no_col])
  }
  if (qc == T) {
    # flag: 0-good data, 1-missing data, 2-out of bound data, 3-deleted data
    data_clean1$flag <- 0
    data_clean1$flag[is.na(rowSums(data_clean1[,3:ncol(data_clean1)]))] <- 1
    data_clean1$flag[(data_clean1$gen < 0)|(data_clean1$gen > cap)] <- 2
    
    # interplate NA values, bound gen between 0 and capacity
    if (ncol(data_clean1) >= 3) {
      for (no_col in 3:ncol(data_clean1)) {
        data_clean1[, no_col] <- na.interpolation(data_clean1[,no_col], option = "linear")
      }
      data_clean1[data_clean1 < 0] <- 0
      va_measure <- setdiff(colnames(data_clean1), c('dataid', 'localhour'))
      data_clean1[which(data_clean1$gen > cap), 'gen'] <- cap
      
      
      if (timeres == 'hours') {
        # detect missing rows
        time_start <- min(data_clean1$localhour)
        time_end <- max(data_clean1$localhour)
        timeseq <- as.data.frame(seq(time_start, time_end, by = 'hour'))
        colnames(timeseq) <- 'localhour'
        data_merge <- merge(timeseq, data_clean1, by.x = 'localhour', by.y = 'localhour', all = TRUE)
        
        # flag missing rows
        for (no_col in 3:ncol(data_merge)) {
          data_merge[,no_col] <- as.double(data_merge[,no_col])
        }
        data_merge$flag[is.na(rowSums(data_merge[,3:ncol(data_merge)]))] <- 3
        # interpolate values to missing rows
        for (no_col in 2:ncol(data_merge)) {
          data_merge[, no_col] <- na.interpolation(data_merge[,no_col], option = "linear")
        }
      }
      
      if (timeres == 'minutes') {
        # detect missing rows
        time_start <- min(data_clean1$localminute)
        time_end <- max(data_clean1$localminute)
        timeseq <- as.data.frame(seq(time_start, time_end, by = 'min'))
        colnames(timeseq) <- 'localhour'
        data_merge <- merge(timeseq, data_clean1, by.x = 'localhour', by.y = 'localminute', all = TRUE)
        
        # flag missing rows
        for (no_col in 3:ncol(data_merge)) {
          data_merge[,no_col] <- as.double(data_merge[,no_col])
        }
        data_merge$flag[is.na(rowSums(data_merge[,3:ncol(data_merge)]))] <- 3
        # interpolate values to missing rows
        for (no_col in 2:ncol(data_merge)) {
          data_merge[, no_col] <- na.interpolation(data_merge[,no_col], option = "linear")
        }
      }
      
      
      if (timeres == '15min') {
        # detect missing rows
        time_start <- min(data_clean1$local_15min)
        time_end <- max(data_clean1$local_15min)
        timeseq <- as.data.frame(seq(time_start, time_end, by = 15*60))
        colnames(timeseq) <- 'localhour'
        data_merge <- merge(timeseq, data_clean1, by.x = 'localhour', by.y = 'local_15min', all = TRUE)
        # flag missing rows
        for (no_col in 3:ncol(data_merge)) {
          data_merge[,no_col] <- as.double(data_merge[,no_col])
        }
        data_merge$flag[is.na(rowSums(data_merge[,3:ncol(data_merge)]))] <- 3
        # interpolate values to missing rows
        for (no_col in 2:ncol(data_merge)) {
          data_merge[, no_col] <- na.interpolation(data_merge[,no_col], option = "linear")
        }
      }
    }else{ # end of if(ncol(data_clean1) >= 3)
      data_merge <- NULL
    }
  } else  {# end of QC==T
    data_merge <- data_clean1
  }
  
  return(data_merge)
}

