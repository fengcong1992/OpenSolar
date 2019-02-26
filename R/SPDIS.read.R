#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to manamge the Solar Power Data for Integration Studies (SPDIS);
# 2 The code includes SPDIS.download and SPDIS.read functions;
# 3 Coder: Cong Feng        Date: 2018/09/25       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------

# read data code
SPDIS.read <- function(root_data, name_st, list_files, readall){
  if (readall == T) {
    list_files <- list.files(file.path(root_data, name_st))} 
  data_locations <- data.frame(read.table(file.path(root_data, name_st, list_files[1]),header = TRUE,sep = ","))
  for (no_file in 2:length(list_files)) {
    data_file <- data.frame(read.table(file.path(root_data, name_st, list_files[no_file]),header = TRUE,sep = ","))
    data_locations <- merge(data_locations, data_file, 'LocalTime', 'LocalTime')
  }
  colnames(data_locations) <- c('LocalTime', paste0('Location', seq(1, length(list_files)), '_Power[MW]'))
  lat <- sub( "_.*$", "", sub(".*Actual_", "", sub( "_2006.*$", "", list_files)))
  logi <- sub( ".*_", "", sub(".*Actual_", "", sub( "_2006.*$", "", list_files)))
  cap <- sub( ".*PV_", "", sub( "MW.*$", "", list_files))
  info_location <- data.frame(seq(1,length(list_files)), lat, logi, cap)
  colnames(info_location) <- c('Location', 'Latitude', 'Longitude', 'Capacity [MW]')  
  list_return <- list(data_locations, info_location)
  return(list_return)
}