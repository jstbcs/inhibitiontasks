# CHECK STRUCTURE OF INSERTED ELEMENT ON STUDY LEVEL  
source("./inject/helper_functions.R")


# This function checks whether element on study level contains:
# 1. "study_info"
# 2. "group_info"
# 3. at least one data[NUMBER] element, e.g. data1
# 4. no duplicated names
which_element_wrong_study <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  if (!"study_info" %in% names)
  {
    stop("Object needs to have a 'study_info' element")
  } 
  if (!"group_info" %in% names)
  {
    stop("Object needs to have a 'group_info' element")
  }
  # This if checks if all names are valid
  if (!all(stringr::str_detect(names,
                               paste(
                                 regex_matches_data_names,
                                 "study_info",
                                 "group_info",
                                 sep = "|")
  )
  )
  )
  {
    error_name = names[which(stringr::str_detect(names,
                                                 paste(
                                                   regex_matches_data_names, 
                                                   "study_info",
                                                   "group_info",
                                                   sep = "|"
                                                 ),
                                                 negate = TRUE)
    )]
    error_message = paste(
      "Element-name:",
      error_name,
      "invalid.",
      "Elements can only be named 'study_info', 'group_info' or 'data_[NUMBER]"
    )
    stop(error_message)
  }
  
  # This if checks if there is at least one data entry
  if (!any(stringr::str_detect(names, regex_matches_data_names)))
  { 
    error_name = names
    error_message = paste(
      "Object must contain at least one data element named 'data_[NUMBER].",
      "Current names:",
      error_name
    )
    stop(error_message) 
  }
  
  stop_if_names_duplicated(names)
}

# check study info structure
check_study_info_structure <- function(study_info){
  names = names(study_info)
  if(is.data.frame(study_info) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(study_info != 1)){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 

  
  # No need for n_groups. n_tasks and comment to be specified for entry
  
  confirm_columns_not_specified(c("n_groups", "n_tasks", "commend"), study_info)
}


# This checks structure of study$group_info
check_group_info_structure <- function(group_info){
  if(is.data.frame(group_info) == FALSE)
  {
    stop("Group-Info is not a dataframe")
  }
  if(nrow(group_info != 1))
  {
    stop("Group-Info contains more than one row")
  }
  confirm_columns_not_specified(c("mean_age", "percentage_female",
                                  "n_participants", "group_description"))
}

# This function checks the entries on study level to see if they have proper structure
# Former: check_object_elements
check_study_level_structure <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  # This speed up processing if all elements are in correct order
  if (names != c("study_info", "group_info", paste0("data", 1:(length-2))))
  {
    which_element_wrong_study(object)
  }
  
  # Check the study info element
  check_study_info_structure(object$study_info)
  
  # Check group
  check_group_info_structure(object$group_info)
}