#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to download and manamge the Solar Power Data for Integration 
#   Studies (SPDIS);
# 2 The code includes SPDIS.download and SPDIS.read functions;
# 3 Coder: Cong Feng        Date: 2018/09/25       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------

# download data code
SPDIS.download <- function(root_data, list_st_download, ifunzip, actualonly){
  # hard-coded info
  list_st <- data.frame(tolower(state.abb), state.name)
  list_st_data <- c('Alabama', 'Arkansas', 'Connecticut', 'Delaware', 'Florida', 'Georgia', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana',
                    'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'New Hampshire', 'New Jersey',
                    'New Mexico', 'New York', 'North Carolina', 'Ohio', 'Oklahoma', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
                    'Texas', 'Vermont', 'Virginia', 'West Virginia', 'Wisconsin', 'Arizona', 'California', 'Colorado', 'Idaho', 'Montana', 'Nevada', 
                    'Oregon', 'Utah', 'Washington', 'Wyoming')
  # loop for each state
  for (no_lst in 1:length(list_st_download)) {
    name_st <- list_st_download[no_lst]
    cat('Download SPDIS State:', name_st, '\n')
    # check data availability
    if (!(name_st %in% list_st$state.name)) {
      stop('Wrong tate name!')
    } else{
      if (!(name_st %in% list_st_data)) {
        stop('No data available for this state!\n', 'The data is available for the following states: \n', list_st_data)
      } else{
        # creating and setting root for saving data
        root_save <- file.path(root_data, name_st)
        if (file.exists(root_save)){
          setwd(root_save)
        } else {
          dir.create(root_save)
          setwd(root_save)
        }
        # download data from SPDIS website
        name_stabb <- as.character(list_st$tolower.state.abb.[which(list_st$state.name == name_st)])
        file_name <- paste0(name_stabb, '-pv-2006.zip')
        URL <- paste0('https://www.nrel.gov/grid/assets/downloads/', file_name)
        setwd(root_save) # set saving directory
        download.file(URL, destfile = file_name, method="curl")
        if (ifunzip == T) { # if unzip the file
          unzip(file_name, overwrite = T, unzip = getOption("unzip"))
          file.remove(file_name) # remove zip file
        }
        if (actualonly == T) { # remove forecasting files
          list_files <- list.files(root_save)
          file.remove(list_files[which(gsub( "_.*$", "", list_files) != 'Actual')])
        }
      } # end of else
    } # end of else
  } # end of no_lst
} # end of the SPDIS.download function


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