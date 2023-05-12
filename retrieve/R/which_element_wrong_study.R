#' Check if elements on study level are structured correctly
#'
#' This function checks whether element on study level contains:
#' 1. "study_info"
#' 2. "group_info"
#' 3. at least one data[NUMBER] element, e.g. data1
#' 4. no duplicated names
#'
#' @param object The R object to be checked.
#' @export
#' @examples
#' # Example of valid study level object
#' study_info <- list(name = "My study", description = "A description")
#' group_info <- data.frame(id = c(1, 2), name = c("Group 1", "Group 2"))
#' data1 <- data.frame(id = c(1, 2), value = c(5, 10))
#' object <- list(study_info = study_info, group_info = group_info, data1 = data1)
#' which_element_wrong_study(object)
#'
#' # Example of invalid study level object - no study_info element
#' object <- list(group_info = group_info, data1 = data1)
#' which_element_wrong_study(object)
#'
#' # Example of invalid study level object - invalid element name
#' object <- list(study_info = study_info, group_info = group_info, wrong_element = data1)
#' which_element_wrong_study(object)
#' 
#' @seealso [stop_if_not_study_level()] to check if the object has the correct input level
#' @seealso [stop_if_names_duplicated()] to check if the object's names are unique.
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
  
#  This if checks if all names are valid
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
  
#  This if checks if there is at least one data entry
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