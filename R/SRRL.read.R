#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to download the NREL Solar Radiation Research
#   Laboratory (SRRL) dataset;
# 2 Coder: Cong Feng        Date: 2018/09/25       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------

# SRRL.read
SRRL.read <- function(timestamp, root_data, returnRGB, processraw, processadv){
  library(utils)
  library(XML)
  library(reshape)
  library(jpeg)
  library(png)
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
