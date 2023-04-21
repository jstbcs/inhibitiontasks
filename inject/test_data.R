# CHECK STRUCTURE OF INSERTED ELEMENT ON DATA LEVEL  
# -----------------------------------------------------------------------------

# Overview: study element is list; has following structure: 
#data1 = list(
#  task_table = task_name,
#  dataset_table = dataset_overview,
#  data = data_table,
#  within_table = within_description
#)

source("./inject/helper_functions.R")

# ---

# This function checks whether an element on the data-level consists of one task element,
# one overview element, one data element, and one within element 

correct_elements_in_data_list <- function(object){
  names = names(object)
  
  # check if duplicates
  stop_if_names_duplicated(names)
  
  # give warning if more than 4 elements in object
  if(length(names) > 5){
    warning("The study object contains more than 4 elements. Only the task, overview, data, condition and within
            element will be extracted")
  }
  # error if not all required objects are present
  names_should <- c("task_table", "condition_table", "dataset_table", "within_table", "observation_table")
  for(element in names_should){
    if(!(element %in% names)){
      stop(c(element, " element is required but missing in data_NUMBER list"))
    }
  }
}


# This function checks whether all required columns of the task_table table are provided
correct_cols_in_task_table <- function(task_table){
  colnames = colnames(task_table)
  
  # check if object is data frame 
  if(!is.data.frame(task_table)){
    stop("task_table must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(task_table) > 1){
    stop("The task_table data frame can only contain 1 row")
  }
  
  # stop if required column names are not present
  confirm_object_names(task_table, table_info_db$task_table)
  
  # Check to see if task name is valid
  task_name = task_table$task_name
  if (!task_name %in% valid_task_names){
    # valid_task_names is from "helper_functions.R". Contains all 3 valid names
    msg = paste("Task name:", task_name, "is invalid. Please ensure you want to add that task name")
    continue_after_warning(msg)
  }
}


# This function checks whether all optional columns of the dataset_table table 
# are provided; since none are mandatory, no error occurs but warning is given
# and input from user is required
correct_cols_in_dataset_table <- function(dataset_table){
  colnames = colnames(dataset_table)
  
  # check if object is data frame 
  if(!is.data.frame(dataset_table)){
    stop("dataset_table must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(dataset_table) > 1){
    stop("The dataset_table data frame can only contain 1 row")
  }
  
  confirm_object_names(dataset_table, table_info_db$observation_table)
}


# This function checks whether all required columns of the data table are provided 
correct_cols_in_observation_table <- function(observation_table){
  colnames = colnames(observation_table)
  
  # check if object is data frame 
  if(!is.data.frame(observation_table)){
    stop("Data table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(observation_table, table_info_db$observation_table)
}


# This function checks whether all required columns of the within_table table are provided 
correct_cols_in_within_table <- function(within_table){
  colnames = colnames(within_table)
  
  # check if object is data frame 
  if(!is.data.frame(within_table)){
    stop("within_table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(within_table, table_info_db$within_table)
}

# This function checks whether all required columns of condition_table are provided 
correct_cols_in_condition_table <- function(condition_table){
  colnames = colnames(condition_table)
  
  # check if object is data frame
  if(!is.data.frame(condition_table)){
    stop("condition_table must be a dataframe")
  }
  
  # stop if required column names are not present
  confirm_object_names(condition_table, table_info_db$condition_table)
}


# This function checks whether the number of rows in the within_table table equals the 
# number of within conditions coded in the data table and whether there are duplicate within_ids 
# in within_id
correct_n_of_withinid <- function(within_table, observation_table){
  # check if within ids are unique
  if(length(unique(within_table$within_id)) != nrow(within_table)){
    stop("Duplicate within_id in within_table found. Make sure within_id is unique")
  }
  
  # check if number of within id matches within columns in data table
  if(length(unique(observation_table$within)) > length(unique(within_table$within_id))){
    stop("Number of unique within conditions in data table is larger than in within_table table. 
         \nMake sure all within conditions are included in within_table.")
  } else if(length(unique(observation_table$within)) < length(unique(within_table$within_id))){
    stop("The within_table table contains more unique within_ids than the data table does. 
         \nMake sure the within column in the data table is coded correctly and the 
         within_table table contains only relevant within conditions")
  }
}


# This function takes a data_[i] list element and checks its entire structure, 
# including the column names of the sub-element data frames 
check_data_level_structure <- function(data_i){
  
  # check if input is a list
  if (inherits(data_i, "list") == FALSE)
  {
    stop("data_NUMBER object must be a list")
  } 
  
  # check if list contains correct elements
  correct_elements_in_data_list(data_i)
  
  # check if each element in data_i list is a df and contains required columns
  correct_cols_in_task_table(data_i$task_table)
  correct_cols_in_dataset_table(data_i$dataset_table)
  correct_cols_in_within_table(data_i$within_table)
  correct_cols_in_condition_table(data_i$condition_table)
  correct_cols_in_observation_table(data_i$observation_table)
  
  # check if number of within condition in data equals number of within_ids in within_table
  correct_n_of_withinid(data_i$within_table, data_i$data)
}


# check structure of inserted element on data level 