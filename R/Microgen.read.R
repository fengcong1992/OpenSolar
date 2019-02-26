#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to download and manamge the Microgen dataset from Sheffield Solar;
# 2 Coder: Cong Feng        Date: 2018/10/11       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------
Microgen.read <- function(root_data){
  library(magrittr)
  library(dplyr)
  # read in data
  data_info <- data.frame(read.table(file.path(root_data,'metadata.csv'),header = TRUE,sep = ","))
  data_all <- data.frame(read.table(file.path(root_data,'readings.csv'),header = TRUE,sep = ","))

  list_loc <- data_info$id # get location list

  # initial data_genall
  data_genall <- data_all[data_all$id == list_loc[1],]
  data_genall <- select(data_genall, c('date', 'time'))
  for (no_loc in 1:length(list_loc)) {
    #  no_loc <- 1
    no_id <- list_loc[no_loc]
    data_pv <- data_all[data_all$id == no_id,]
    data_hrbefore <- data_pv$cumulative_reading
    data_hrafter <- c(data_pv$cumulative_reading[2:nrow(data_pv)], data_pv$cumulative_reading[nrow(data_pv)])
    data_gen <- data.frame(data_pv[,1:(ncol(data_pv)-1)], (data_hrafter - data_hrbefore))
    data_gen2 <- select(data_gen, -c('id'))
    colnames(data_gen2)[ncol(data_gen2)] <- paste0('Location', unique(data_gen$id))
    data_genall <- merge(data_genall, data_gen2, by.x = c('date', 'time'), by.y = c('date', 'time'))
  }
  data_genall2 <- data_genall[order(data_genall$date, data_genall$time),]

  return(data_genall2)
}

