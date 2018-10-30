#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to download and manamge the NREL Solar Radiation Research
#   Laboratory (SRRL) dataset;
# 2 The code includes SRRL.download and SRRL.read functions;
# 3 Coder: Cong Feng        Date: 2018/09/25       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------
#date_start <- '2017-12-30'
#date_end <- '2018-01-02'
#root_data <- '/Users/cfeng/Desktop/solar_data/data'
library(utils)
library(XML)
library(reshape)
library(jpeg)
library(png)

# SRRL.download
SRRL.download <- function(root_data, date_start, date_end, skyimg, tmseries, ifunzip, ifunique){
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
        #----------------------------- Part 1: 01/01/2017-11/30/2017
        # initialize data_new1 for merge()
        URL1 <- paste0('https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;inst=1;type=data;endyear=2017;endmonth=11;endday=30')
        data_year1 <- readLines(URL1)
        data_year12 <- strsplit(data_year1, ",")
        data_new1 <- data.frame(matrix(unlist(data_year12), ncol=(3), byrow=TRUE))[,1:2]
        name_col <- NULL
        for (nocol in 1:ncol(data_new1)) {
          name_col <- c(name_col, as.character(data_new1[1,nocol]))
        }
        colnames(data_new1) <- name_col # rename columns

        for (no_loop in 1:9) { # website only allow download 24 variables a time, devide into 9 steps
          lp_stt <- 3+24*(no_loop-1)
          lp_end <- 23 + lp_stt
          if (lp_stt < 3) { # maximum lp_end is 195
            lp_stt <- 3
          }
          if (lp_end > 195) { # maximum lp_end is 195
            lp_end <- 195
          }
          act_no <- lp_end - lp_stt + 1
          list_inst <- NULL
          for (no_inst in lp_stt:lp_end) { # create inst to download
            list_inst <- paste0(list_inst, 'inst=',no_inst,';')
          }# end of no_inst
          URL1 <- paste0('https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;',
                         list_inst,
                         'type=data;endyear=2017;endmonth=11;endday=30')
          data_year1 <- readLines(URL1)
          data_year12 <- strsplit(data_year1, ",")
          data_new12 <- data.frame(matrix(unlist(data_year12), ncol=(2+act_no), byrow=TRUE))
          # needs to extract variable name one by one
          name_col <- NULL
          for (nocol in 1:ncol(data_new12)) {
            name_col <- c(name_col, as.character(data_new12[1,nocol]))
          }
          colnames(data_new12) <- name_col # rename columns
          # combine columns
          data_new1 <- merge(data_new1, data_new12, by.x=c("DATE (MM/DD/YYYY)", "MST"), by.y = c("DATE (MM/DD/YYYY)", "MST"))
        }# end of no_loop


        #----------------------------- Part 2: 12/01/2017-12/31/2017
        # initialize data_new2 for merge()
        URL1 <- paste0('https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;inst=1;type=data;endyear=2017;endmonth=11;endday=30')
        data_year1 <- readLines(URL1)
        data_year12 <- strsplit(data_year1, ",")
        data_new2 <- data.frame(matrix(unlist(data_year12), ncol=(3), byrow=TRUE))[,1:2]
        name_col <- NULL
        for (nocol in 1:ncol(data_new2)) {
          name_col <- c(name_col, as.character(data_new2[1,nocol]))
        }
        colnames(data_new2) <- name_col # rename columns

        for (no_loop in 1:9) { # website only allow download 24 variables a time, devide into 9 steps
          lp_stt <- 3+24*(no_loop-1)
          lp_end <- 23 + lp_stt
          if (lp_stt < 3) { # maximum lp_end is 195
            lp_stt <- 3
          }
          if (lp_end > 195) { # maximum lp_end is 195
            lp_end <- 195
          }
          act_no <- lp_end - lp_stt + 1
          list_inst <- NULL
          for (no_inst in lp_stt:lp_end) { # create inst to download
            list_inst <- paste0(list_inst, 'inst=',no_inst,';')
          }# end of no_inst
          URL1 <- paste0('https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;',
                         list_inst,
                         'type=data;endyear=2017;endmonth=11;endday=30')
          data_year1 <- readLines(URL1)
          data_year12 <- strsplit(data_year1, ",")
          data_new12 <- data.frame(matrix(unlist(data_year12), ncol=(2+act_no), byrow=TRUE))
          # needs to extract variable name one by one
          name_col <- NULL
          for (nocol in 1:ncol(data_new12)) {
            name_col <- c(name_col, as.character(data_new12[1,nocol]))
          }
          colnames(data_new12) <- name_col # rename columns
          # combine columns
          data_new2 <- merge(data_new2, data_new12, by.x=c("DATE (MM/DD/YYYY)", "MST"), by.y = c("DATE (MM/DD/YYYY)", "MST"))
        }# end of no_loop
        # merge part 1 and part 2
        data_new3 <- rbind(data_new1, data_new2)
      }


      #----------------------------- Part 2: 12/01/2017-12/31/2017
      if (year_select == '2018') {
        # initialize data_new3 for merge()
        URL1 <- paste0('https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;inst=1;type=data;endyear=2017;endmonth=11;endday=30')
        data_year1 <- readLines(URL1)
        data_year12 <- strsplit(data_year1, ",")
        data_new3 <- data.frame(matrix(unlist(data_year12), ncol=(3), byrow=TRUE))[,1:2]
        name_col <- NULL
        for (nocol in 1:ncol(data_new3)) {
          name_col <- c(name_col, as.character(data_new3[1,nocol]))
        }
        colnames(data_new3) <- name_col # rename columns

        for (no_loop in 1:9) { # website only allow download 24 variables a time, devide into 9 steps
          lp_stt <- 3+24*(no_loop-1)
          lp_end <- 23 + lp_stt
          if (lp_stt < 3) { # maximum lp_end is 195
            lp_stt <- 3
          }
          if (lp_end > 195) { # maximum lp_end is 195
            lp_end <- 195
          }
          act_no <- lp_end - lp_stt + 1
          list_inst <- NULL
          for (no_inst in lp_stt:lp_end) { # create inst to download
            list_inst <- paste0(list_inst, 'inst=',no_inst,';')
          }# end of no_inst
          URL1 <- paste0('https://midcdmz.nrel.gov/apps/plot.pl?site=BMS;start=20150101;edy=31;emo=11;eyr=2017;year=2017;month=01;day=1;time=1;zenloc=200;',
                         list_inst,
                         'type=data;endyear=2017;endmonth=11;endday=30')
          data_year1 <- readLines(URL1)
          data_year12 <- strsplit(data_year1, ",")
          data_new12 <- data.frame(matrix(unlist(data_year12), ncol=(2+act_no), byrow=TRUE))
          # needs to extract variable name one by one
          name_col <- NULL
          for (nocol in 1:ncol(data_new12)) {
            name_col <- c(name_col, as.character(data_new12[1,nocol]))
          }
          colnames(data_new12) <- name_col # rename columns
          # combine columns
          data_new3 <- merge(data_new3, data_new12, by.x=c("DATE (MM/DD/YYYY)", "MST"), by.y = c("DATE (MM/DD/YYYY)", "MST"))
        }# end of no_loop
      }
      data_combine <- rbind(data_combine, data_new3) # combine multiple years
    } # end of no_year

    # format and clean data
    data_clean <- data_combine[which(format(as.POSIXlt(as.character(data_combine[,1]), format ="%m/%d/%Y"), "%Y") %in% c('2017', '2018')),]
    data_clean[,1] <- as.Date(as.character(format(as.POSIXlt(as.character(data_clean[,1]), format ="%m/%d/%Y"), "%m/%d/%Y")), format ="%m/%d/%Y")
    data_final <- data_clean[(data_clean[,1] >= as.Date(date_start)&data_clean[,1] <= as.Date(date_end)),]

    # write out data
    write.table(data_final,file = file.path(root_data,'SRRL_measurement_timeseries.csv'),row.names = F,na='',col.names = TRUE,sep = ',')
  } # end of if (tmseries == T)
} # end of SRRL.download function


# SRRL.read
SRRL.read <- function(timestamp, root_data, returnRGB, processraw, processadv){
  root_data2 <- file.path(root_data, format(as.POSIXlt(timestamp, format ="%Y-%m-%d %H:%M"), "%Y%m%d"))
  name_file <- paste0(format(as.POSIXlt(timestamp, format ="%Y-%m-%d %H:%M"), "%Y%m%d"),
                      format(as.POSIXlt(timestamp, format ="%Y-%m-%d %H:%M"), "%H%M"),'00')
  name_file1 <- paste0(name_file, '_11_NE.jpg')
  name_file2 <- paste0(name_file, '_1112_CDOC.png')

  if (processraw == T) {
    img_raw <- jpeg::readJPEG(file.path(root_data2, name_file1))
    R <- img_raw[, , 1]
    #G <- img[, , 2]
    B <- img_raw[, , 3]
    RGB_ratio <- (R-B)/(R+B)
    RGBR_mean <- mean(RGB_ratio, na.rm = T) # mean
    RGBR_sd <- sd(RGB_ratio, na.rm = T) # standard deviation
    alpha <- 2
    error_hist <- hist(RGB_ratio,breaks=50,plot=FALSE)
    RGBR_renyi <- 1/(1-alpha)*log(sum(error_hist$density^alpha)) # renyi entropy
    if (returnRGB) {
      imgraw_return <- img_raw
      rm(img_raw)
    } else {imgraw_return <- NULL}
  } else {
    imgraw_return <- NULL
    RGBR_mean <- NULL # mean
    RGBR_sd <- NULL # standard deviation
    RGBR_renyi <- NULL
  }

  if (processadv == T) {
    img_adv <- readPNG(file.path(root_data2, name_file2))
    R <- as.vector(img_adv[, , 1])*255
    thin_cc <- length(R[which(R == 224)])/(length(R[which(R == 224)])+length(R[which(R == 84)])+length(R[which(R == 149)]))
    thick_cc <- length(R[which(R == 149)])/(length(R[which(R == 224)])+length(R[which(R == 84)])+length(R[which(R == 149)]))
    if (returnRGB) {
      imgadv_return <- img_adv
      rm(img_adv)
    } else {imgadv_return <- NULL}
  } else {
    imgadv_return <- NULL
    thin_cc <- NULL
    thick_cc <- NULL
  }

  list_return <- list(imgraw_return, c(RGBR_mean,RGBR_sd,RGBR_renyi),
                      imgadv_return, c(thin_cc,thick_cc))
  return(list_return)
}
