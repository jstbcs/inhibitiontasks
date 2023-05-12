#' Check and correct elements in a data list
#'
#' This function checks if the elements in a data list of a study object are present and named correctly. If the names
#' are not as expected, the function will rename them accordingly. If there are more than 5 elements in the list, a
#' warning will be issued. The function will throw an error if any required element is missing.
#'
#' @param object A list containing data related to a study object.
#' @export
#' @examples
#'
#' data_list <- list(task_info = NULL, overview_info = NULL, data = NULL, within_info = NULL, condition_info = NULL)
#' correct_elements_in_data_list(data_list)
#'
#' data_list <- list(task_info = NULL, overview_info = NULL, data = NULL, within = NULL, condition_info = NULL)
#' correct_elements_in_data_list(data_list)
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
  names_should <- c("task_info", "overview_info", "data", "within_info", "condition_info")
  for(element in names_should){
    if(!(element %in% names)){
      stop(c(element, " element is required but missing in data_NUMBER list"))
    }
  }
}
