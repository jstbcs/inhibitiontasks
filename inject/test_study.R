# CHECK STRUCTURE OF INSERTED ELEMENT ON STUDY LEVEL  
source("./inject/helper_functions.R")


# This function checks whether element on study level contains:
# 1. "study_table"
# 2. "between_table"
# 3. at least one data[NUMBER] element, e.g. data1
# 4. no duplicated names
which_element_wrong_study <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  
  if (!"study_table" %in% names)
  {
    stop("Object needs to have a 'study_table' element")
  } 
  if (!"between_table" %in% names)
  {
    stop("Object needs to have a 'between_table' element")
  }
  # This if checks if all names are valid
  if (!all(stringr::str_detect(names,
                               paste(
                                 regex_matches_data_names,
                                 "study_table",
                                 "between_table",
                                 sep = "|")
  )
  )
  )
  {
    error_name = names[which(stringr::str_detect(names,
                                                 paste(
                                                   regex_matches_data_names, 
                                                   "study_table",
                                                   "between_table",
                                                   sep = "|"
                                                 ),
                                                 negate = TRUE)
    )]
    error_message = paste(
      "Element-name:",
      error_name,
      "invalid.",
      "Elements can only be named 'study_table', 'between_table' or 'data[NUMBER]"
    )
    stop(error_message)
  }
  
  # This if checks if there is at least one data entry
  if (!any(stringr::str_detect(names, regex_matches_data_names)))
  { 
    error_name = names
    error_message = paste(
      "Object must contain at least one data element named 'data[NUMBER].",
      "Current names:",
      error_name
    )
    stop(error_message) 
  }
  
  stop_if_names_duplicated(names)
}

# check study info structure
check_study_table_structure <- function(study_table){
  names = names(study_table)
  if(is.data.frame(study_table) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(study_table) != 1){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 

  confirm_object_names(study_table, entry_list_info$study_table)
  
  if (is.na(study_table$comment) | is.null(study_table$comment) | study_table$comment == ""){
    stop("Comment can not be empty")
  }
}


# This checks structure of study$between_table
check_between_table_structure <- function(between_table){
  names = names(between_table)
  if(is.data.frame(between_table) == FALSE)
  {
    stop("Between-Table is not a dataframe")
  }
  
  if (nrow(between_table) > 1){
    if (!"group_description" %in% names){
      stop("Object needs to have a 'group_description' element")
    }
    
    if (any(is.na(between_table$group_description)) | any(is.null(between_table$group_description)) | any(between_table$group_description == "")){
      stop("group_description can not be empty")
    }
  }
  
  confirm_object_names(between_table, entry_list_info$between_table)
}

# This function checks the entries on study level to see if they have proper structure
check_study_level_structure <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  # This speed up processing if all elements are in correct order
  if (!all(names == c("study_table", "between_table", paste0("data", 1:(length-2)))))
  {
    which_element_wrong_study(object)
  }
  
  # Check the study info element
  check_study_table_structure(object$study_table)
  
  # Check group
  check_between_table_structure(object$between_table)
}
