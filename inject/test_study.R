# CHECK STRUCTURE OF INSERTED ELEMENT ON STUDY LEVEL  

# Overview: study element is list; has following structure: 
#data1 = list(
#  task_info = task_name,
#  overview_info = dataset_overview,
#  data = data_table,
#  within_info = within_description
#),

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


