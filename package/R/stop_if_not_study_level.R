#' Check if input object is a study-level object
#'
#' This function checks if a specified input object is at the study level, i.e.,
#' it contains all the necessary columns required to proceed with a specific operation.
#' The function checks for the presence of columns that are required at the study level
#' and throws an error if any of them are not present, with a message specifying that the
#' object passed to the function is not at the study level and needs to be modified.
#'
#' @param object The input object to check.
#' @export
#' @examples
#' # Sample data
#' data <- list(
#' study_info = data.frame(
#' study_id = c(1, 2),
#' study_name = c("Study 1", "Study 2"),
#' publication_name = c("Publication 1", "Publication 2")
#' ),
#' group_info = data.frame(
#' group_id = c(1, 2, 3, 4),
#' group_name = c("Group 1", "Group 2", "Group 3", "Group 4"),
#' study_id = c(1, 1, 2, 2)
#' ),
#' data1 = data.frame(
#' id = 1:10,
#' group_id = c(1, 1, 2, 2, 3, 3, 4, 4, 4, 4),
#' effect_size = c(0.2, 0.3, 0.5, 0.6, 0.1, 0.2, 0.7, 0.8, 0.9, 1.0),
#' se = c(0.1, 0.2, 0.15, 0.25, 0.05, 0.1, 0.3, 0.25, 0.15, 0.2)
#' )
#' )
#'
#' # This will raise an error
#' stop_if_not_study_level(data)
#'
#' # Modify the data to contain the necessary columns
#' study_data <- data[['data1']]
#'
#' # This will pass
#' stop_if_not_study_level(study_data)
#' 
stop_if_not_study_level <- function(object){
  if(do_elements_exist(c("study_info", "group_info", "data1"), object) == FALSE)
  {
    stop("This function takes a study-level object")
  }
}