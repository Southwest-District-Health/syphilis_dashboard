get_data_path <- function(
    data_type = "processed",
    data_name = "example_data.csv",
    max_height = 3) {
  # Code For function begins here
  start_path <- getwd()
  
  level <- 0L # Count starter for how many times allowed to go up a level.
  
  stop <- FALSE # Flag to break the while loop when TRUE
  
  # Data type means folder names we have created that house data
  if (data_type == "processed") {
    path_search <- "./processed_data"
  } else if (data_type == "raw") {
    path_search <- "./raw_data"
  } else {
    stop("data_type not recognized. Please specify either 'processed' or 'raw'.")
  }
  
  # Go up a directory level. Check if it has data folder we want. If not, go up again.
  while (level <= max_height & stop == FALSE) {
    setwd("..") # Go up a directory level
    dirs_list <- list.dirs(recursive = FALSE) # Get list of folders in current folder
    
    directory_present <- sum(dirs_list == path_search) # Is data folder present?
    
    if (directory_present > 0) {
      stop <- TRUE
    }
  }
  
  # Now that we are in folder that has data folder we are interested in, get data path
  setwd(path_search) # change directory to data directory we were looking for
  file_present <- sum(list.files() == data_name) # Is file present?
  if (file_present > 0) {
    data_file_path <- normalizePath(data_name) # Returns file's absolute path
  } else {
    stop(paste0(data_name, " not found. Please make sure you are looking for the right file in the right directory."))
  }
  setwd(start_path) # change working directory back to where we started
  return(data_file_path) # return the file path to data
}

point_data_path <- function(
    data_type = "processed",
    data_name = "example_data.csv", 
    choose_dir_option = 'rstudio') {
  # Code For function begins here
  start_path <- getwd()
  
  # Data type means folder names we have created that house data
  if (data_type == "processed") {
    path_search <- "./processed_data"
  } else if (data_type == "raw") {
    path_search <- "./raw_data"
  } else {
    stop("data_type not recognized. Please specify either 'processed' or 'raw'.")
  }
  
  print('Opening dialog box. Box may not appear in task bar and may be behind other windows.')
  
  if (choose_dir_option == 'rstudio'){
    general_path <- rstudioapi::selectDirectory(caption = 'Choose the General folder')
  } else if (choose_dir_option == 'windows'){
    general_path <- tk_choose.dir(caption = 'Choose the General folder')
  } else {
    warning('choose_dir_option selection not recognized. Using tk_choose.dir')
    general_path <- tk_choose.dir(caption = 'Choose the General folder')
  }
  
  
  setwd(general_path)
  
  print('Closed diaglog box.')
  
  setwd(path_search) # change directory to data directory we were looking for
  
  file_present <- sum(list.files() == data_name) # Is file present?
  
  if (file_present > 0) {
    data_file_path <- normalizePath(data_name) # Returns file's absolute path
  } else {
    stop(paste0(data_name, " not found. Please make sure you are looking for the right file in the right directory."))
  }
  setwd(start_path) # change working directory back to where we started
  return(data_file_path) # return the file path to data
} 

