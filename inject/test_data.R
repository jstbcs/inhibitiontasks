source("helper_functions.R")


# This function checks whether an element on the data-level consists of one task element,
# one overview element, one data element, and one within element

correct_elements_in_data_list <- function(object){
  names = names(object)
  
  # check if duplicates
  stop_if_names_duplicated(names)
  
  # give warning if more than 4 elements in object
  if(length(names) > 4){
    warning("The study object contains more than 4 elements. Only the task, overview, data, and within
            element will be extracted")
  }
  # error if not all required objects are present
  names_should <- c("task_info", "overview_info", "data", "within_info")
  for(element in names_should){
    if(!(element %in% names)){
      stop(c(element, " element is required but missing in data_NUMBER list"))
    }
  }
}


# This function checks whether all required columns of the task_info table are provided
correct_cols_in_task_info <- function(task_info){
  colnames = colnames(task_info)
  
  # check if object is data frame 
  if(!is.data.frame(task_info)){
    stop("task_info must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(task_info) > 1){
    stop("The task_info data frame can only contain 1 row")
  }
  
  # stop if required column names are not present
  names_should <- c("task", "task_description" )
  missing_cols <- c()
  for(element in names_should){
    if(!(element %in% colnames)){
      missing_cols <- c(missing_cols, element)
    }
  } 
  if (length(missing_cols) > 0){
    stop(c("Columnname(s) missing from task_info data frame: ", paste(missing_cols, collapse = ", ")))
  }
}


# TODO: distinguish between mandatory cols (error) and optional cols (warning)
# This function checks whether all required columns of the overview_info table are provided 
correct_cols_in_overview_info <- function(overview_info){
  colnames = colnames(overview_info)
  
  # check if object is data frame 
  if(!is.data.frame(overview_info)){
    stop("overview_info must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(overview_info) > 1){
    stop("The overview_info data frame can only contain 1 row")
  }
  
  # stop if required column names are not present
  names_should <- c("data_excl", "n_participants", "n_blocks", "n_trials", 
                    "neutral_trials", "fixation_cross", "time_limit", "github")
  missing_cols <- c()
  for(element in names_should){
    if(!(element %in% colnames)){
      missing_cols <- c(missing_cols, element)
    }
  } 
  if (length(missing_cols) > 0){
    stop(c("Columnname(s) missing from overview_info data frame: ", paste(missing_cols, collapse = ", ")))
  }
}


# This function checks whether all required columns of the data table are provided 
correct_cols_in_data <- function(data_df){
  colnames = colnames(data_df)
  
  # check if object is data frame 
  if(!is.data.frame(data_df)){
    stop("Data table must be a dataframe")
  }
  
  # stop if required column names are not present
  names_should <- c("subject", "block", "trial", "group",
                    "within", "congr", "accuracy", "rt")
  missing_cols <- c()
  for(element in names_should){
    if(!(element %in% colnames)){
      missing_cols <- c(missing_cols, element)
    }
  } 
  if (length(missing_cols) > 0){
    stop(c("Columnname(s) missing from data table: ", paste(missing_cols, collapse = ", ")))
  }
}


# This function checks whether all required columns of the within_info table are provided 
correct_cols_in_within_info <- function(within_info){
  colnames = colnames(within_info)
  
  # check if object is data frame 
  if(!is.data.frame(within_info)){
    stop("Within_info must be a dataframe")
  }
  
  # stop if required column names are not present
  names_should <- c("within_id", "within_description")
  missing_cols <- c()
  for(element in names_should){
    if(!(element %in% colnames)){
      missing_cols <- c(missing_cols, element)
    }
  } 
  if (length(missing_cols) > 0){
    stop(c("Columnname(s) missing from within_info data frame: ", paste(missing_cols, collapse = ", ")))
  }
}


# This function checks whether the number of rows in the within_info table equals the 
# number of within conditions coded in the data table and whether there are duplicate within_ids 
# in within_id
correct_n_of_withinid <- function(within_info, data_df){
  # check if within ids are unique
  if(length(unique(within_info$within_id)) != nrow(within_info)){
    stop("Dublicate within_id in within_info found. Make sure within_id is unique")
  }
  
  # check if number of within id matches within columns in data table
  if(length(unique(data_df$within)) > length(unique(within_info$within_id))){
    stop("Number of unique within conditions in data table is larger than in within_info table. 
         \nMake sure all within conditions are included in within_info.")
  } else if(length(unique(data_df$within)) < length(unique(within_info$within_id))){
    stop("The within_info table contains more unique within_ids than the data table does. 
         \nMake sure the within column in the data table is coded correctly and the 
         within_info table contains only relevant within conditions")
  }
}


# This function takes a data_[i] list element and checks its entire structure, 
# including the column names of the sub-element data frames 
check_data_list <- function(data_i){
  
  # check if input is a list
  if (inherits(data_i, "list") == FALSE)
  {
    stop("data_NUMBER object must be a list")
  } 
  
  # check if list contains correct elements
  correct_elements_in_data_list(data_i)
  
  # check if each element in data_i list is a df and contains required columns
  correct_cols_in_task_info(data_i$task_info)
  correct_cols_in_overview_info(data_i$overview_info)
  correct_cols_in_within_info(data_i$within_info)
  correct_cols_in_data(data_i$data)
  
  # check if number of within condition in data equals number of within_ids in within_info
  correct_n_of_withinid(data_i$within_info, data_i$data)
}


# check structure of inserted element on data level 