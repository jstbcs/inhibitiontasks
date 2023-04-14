#' Check structure of study-level object
#'
#' This function checks whether the input object has the expected structure for a study-level object. It verifies the presence of "study_info" and "group_info" elements, and that the remaining elements begin with "data". It also checks the structure of the "study_info" and "group_info" elements using the functions check_study_info_structure() and check_group_info_structure(), respectively.
#'
#' @param object An object to be checked for study-level structure.
#' @export
#' @seealso [check_group_info_structure()] for more detailed checking of "group_info"
#' @seealso [check_study_info_structure()] for more detailed checking of "study_info"
#' @examples
#' # Example with a correct object
#' object <- list(study_info = list(study_name = "Example study"),
#' group_info = data.frame(group = c("Group A", "Group B")),
#' data_1 = data.frame(x = 1:10, y = rnorm(10)),
#' data_2 = data.frame(x = 1:10, y = rnorm(10)))
#' check_study_level_structure(object)
#'
#' # Example with a missing "group_info" element
#' object <- list(study_info = list(study_name = "Example study"),
#' data_1 = data.frame(x = 1:10, y = rnorm(10)),
#' data_2 = data.frame(x = 1:10, y = rnorm(10)))
#' check_study_level_structure(object)
check_study_level_structure <- function(object){
  stop_if_not_study_level(object)
  names = names(object)
  length = length(object)
  
 # This speed up processing if all elements are in correct order
  if (!all(names == c("study_info", "group_info", paste0("data", 1:(length-2)))))
  {
    which_element_wrong_study(object)
  }
  
#  Check the study info element
  check_study_info_structure(object$study_info)
  
#  Check group
  check_group_info_structure(object$group_info)
}