#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to get meta data of the Dataport dataset
# 2 Coder: Cong Feng        Date: 2018/10/12       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------

Dataport.meta <- function(usrname, pswd, qc, ifdownload, root_save){
  library(RPostgreSQL)
  library(zoo)
  if (qc == F) { # no QC
    drv <- dbDriver("PostgreSQL") # set up SQL driver
    # set up connections to Dataport database
    con <- dbConnect(drv, dbname = "postgres",
                     host = "dataport.cloud", port = 5434,
                     user = usrname, password = pswd)
    data_tblist <- dbListTables(con)
    meta <- dbGetQuery(con, "SELECT * FROM university.metadata")
    dbDisconnect(con) # disconnect
    # extract house IDs with PV
    data_pv <- meta[which(meta$pv == 'yes'),]
    data_pvqc <- data_pv
  }
  if (qc == T) { # QC T, houses with PV installation and measurements
    drv <- dbDriver("PostgreSQL") # set up SQL driver
    # set up connections to Dataport database
    con <- dbConnect(drv, dbname = "postgres",
                     host = "dataport.cloud", port = 5434,
                     user = usrname, password = pswd)
    data_tblist <- dbListTables(con)
    meta <- dbGetQuery(con, "SELECT * FROM university.metadata")
    dbDisconnect(con) # disconnect
    # extract house IDs with PV
    data_pv <- meta[which(meta$pv == 'yes'),]
    # selecting by checking house by house
    table_pvcheck <- NULL
    for (no_id in 1:nrow(data_pv)) {#nrow(data_pv)
      cat(no_id, '/', nrow(data_pv), '\n')
      # variables
      house_id <- data_pv$dataid[no_id]
      time_resolution <- 'hours' # 'seconds', 'minutes', '15min', 'hours'
      drv <- dbDriver("PostgreSQL") # set up SQL driver
      name_tb <- paste0('university.electricity_egauge_', time_resolution) # table name
      con <- dbConnect(drv, dbname = "postgres", # set up connections to Dataport database
                       host = "dataport.cloud", port = 5434,
                       user = usrname, password = pswd)
      data_tblist <- dbListTables(con)
      sqlcommand <- paste('SELECT * FROM ', name_tb,
                          ' where dataid=', house_id, sep = '')
      data_house <- dbGetQuery(con, sqlcommand)
      dbDisconnect(con) # disconnect
      data_house <- data_house[, colMeans(is.na(data_house)) <= .1] # columns with 90% NAs are removed
      table_no <- c(house_id, min(as.character(data_house$localhour)), max(as.character(data_house$localhour)), length(setdiff(colnames(data_house), c('dataid', 'localhour'))),
                    'gen'%in%colnames(data_house))
      table_pvcheck <- rbind(table_pvcheck, table_no)
    }
    table_pvcheck <- as.data.frame(table_pvcheck)
    colnames(table_pvcheck) <- c('dataid', 'startdate', 'enddate', 'ncols', 'ifpv')
    data_pvqc <- data_pv[which(table_pvcheck$ifpv == TRUE),]
  }
  if (ifdownload == T) {
    write.table(data_pvqc,file = file.path(root_save,'Dataport_Metadata.csv'),row.names = F,na='',col.names = TRUE,sep = ',')
  }
  
  return(list(data_pvqc, data_pvqc))
}