#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to download the NREL Solar Radiation Research
#   Laboratory (SRRL) dataset;
# 2 Coder: Cong Feng        Date: 2018/09/25       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------

# SRRL.download
SRRL.download <- function(root_data, date_start, date_end, skyimg, tmseries, ifunzip, ifunique){
  library(utils)
  library(XML)
  library(reshape)
  library(jpeg)
  library(png)
  lgth_day <- as.numeric(as.Date(date_end) - as.Date(date_start)) + 1
  if (skyimg == T) { # if download sky image
    for (no_day in 1:lgth_day) {
      date <- as.Date(date_start) + no_day - 1
      year <- format(as.POSIXlt(date, format ="%Y-%m-%d"), "%Y")
      date2 <- format(as.POSIXlt(date, format ="%Y-%m-%d"), "%Y%m%d")
      URL <- file.path("https://midcdmz.nrel.gov/tsi/SRRLASI", year, paste0(date2, '.zip'))
      # creating and setting root for saving data
      root_save <- file.path(root_data, date2)
      if (file.exists(root_save)){
        setwd(root_save)
      } else {
        dir.create(root_save)
        setwd(root_save)
      }
      # download zip file from the URL
      file_name <- paste0(date2, '.zip')
      download.file(URL, destfile = file_name, method="curl")
      # unzip and manage sky images
      if (ifunzip == T) { # if unzip the file
        unzip(file_name, overwrite = T, unzip = getOption("unzip"))
        file.remove(file_name) # remove zip file
        list_files <- list.files()
        list_files2 <- unique(sub( ".txt.*$", "", sub( "_.*$", "", list_files)))
      }
      if (ifunique == T) { # remove redundant files
        file.remove(setdiff(list_files, c(paste0(list_files2, '_11_NE.jpg'),
                                          paste0(list_files2, '_1112_CDOC.png'))))
      }
    } # end of no_day
  } # end of if (skyimg == T)
  
  if (tmseries == T) {
    year_start <- format(as.POSIXlt(date_start, format ="%Y-%m-%d"), "%Y")
    month_start <- format(as.POSIXlt(date_start, format ="%Y-%m-%d"), "%m")
    day_start <- format(as.POSIXlt(date_start, format ="%Y-%m-%d"), "%d")
    year_end <- format(as.POSIXlt(date_end, format ="%Y-%m-%d"), "%Y")
    month_end <- format(as.POSIXlt(date_end, format ="%Y-%m-%d"), "%m")
    day_end <- format(as.POSIXlt(date_end, format ="%Y-%m-%d"), "%d")
    
    data_combine <- NULL
    for (no_year in 0:(as.numeric(year_end) - as.numeric(year_start))) {
      year_select <- as.numeric(year_start) + no_year
      
      # download data by hard code URL
      if (year_select == '2017') {
        URL1 <- 'https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;inst=3;inst=53;inst=67;type=data;endyear=2017;endmonth=11;endday=30'
        data_year1 <- readLines(URL1)
        data_year12 <- strsplit(data_year1, ",")
        data_new1 <- data.frame(matrix(unlist(data_year12), ncol=5, byrow=TRUE))
        colnames(data_new1) <- c('Date', 'Time', 'GHI', 'DNI', 'DHI')
        URL2 <- 'https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20171201;edy=31;emo=12;eyr=9999;year=2017;month=12;day=1;time=1;zenloc=209;inst=3;inst=55;inst=69;type=data;endyear=2017;endmonth=12;endday=31'
        data_year2 <- readLines(URL2)
        data_year22 <- strsplit(data_year2, ",")
        data_new2 <- data.frame(matrix(unlist(data_year22), ncol=5, byrow=TRUE))
        colnames(data_new2) <- c('Date', 'Time', 'GHI', 'DNI', 'DHI')
        data_new3 <- rbind(data_new1, data_new2)
      }
      if (year_select == '2018') {
        URL1 <- 'https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20171201;edy=31;emo=12;eyr=9999;year=2018;month=01;day=1;time=1;zenloc=209;inst=3;inst=55;inst=69;type=data;endyear=2018;endmonth=12;endday=31'
        data_year <- readLines(URL1)
        data_year2 <- strsplit(data_year, ",")
        data_new3 <- data.frame(matrix(unlist(data_year2), ncol=5, byrow=TRUE))
        colnames(data_new3) <- c('Date', 'Time', 'GHI', 'DNI', 'DHI')
      }
      data_combine <- rbind(data_combine, data_new3) # combine multiple years
    } # end of no_year
    
    # clean data
    data_clean <- data_combine[which(format(as.POSIXlt(as.character(data_combine$Date), format ="%m/%d/%Y"), "%Y") %in% c('2017', '2018')),]
    # convert negative to 0
    data_clean$Date <- as.Date(as.character(format(as.POSIXlt(as.character(data_clean$Date), format ="%m/%d/%Y"), "%m/%d/%Y")), format ="%m/%d/%Y")
    data_clean$GHI <- as.numeric(as.character(data_clean$GHI))
    data_clean$DHI <- as.numeric(as.character(data_clean$DHI))
    data_clean$DNI <- as.numeric(as.character(data_clean$DNI))
    
    data_clean$GHI[which(data_clean$GHI < 0)] <- 0
    data_clean$DHI[which(data_clean$DHI < 0)] <- 0
    data_clean$DNI[which(data_clean$DNI < 0)] <- 0
    
    data_final <- data_clean[(data_clean$Date >= as.Date(date_start)&data_clean$Date <= as.Date(date_end)),]
    
    # write out data
    write.table(data_final,file = file.path(root_data,'SRRL_measurement_timeseries.csv'),row.names = F,na='',col.names = TRUE,sep = ',')
  } # end of if (tmseries == T)
} # end of SRRL.download function
