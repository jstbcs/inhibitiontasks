#' Check study info structure
#'
#' This function checks the structure of the study information data frame to ensure it contains
#' the necessary elements and values for the overall data structure to be valid.
#'
#' @param study_info A data frame containing study information.
#' @return No explicit return value, but the function will throw an error if the study information
#' is not structured correctly.
#' @export
#' @seealso [confirm_columns_not_specified()] to confirm that some elements are not specified
#' @examples
#' study_info <- data.frame(comment = "Example study", n_groups = 4, n_tasks = 6)
#' check_study_info_structure(study_info)
#' 
#' # This will throw an error because the study info data frame has more than one row:
#' study_info2 <- data.frame(comment = "Example study", n_groups = 4, n_tasks = 6, more_info = "Extra row")
#' check_study_info_structure(study_info2)
#'
#' # This will throw an error because the comment field is missing:
#' study_info3 <- data.frame(n_groups = 4, n_tasks = 6)
#' check_study_info_structure(study_info3)
#' 
#' # This will throw an error because the comment field is empty:
#' study_info4 <- data.frame(comment = "", n_groups = 4, n_tasks = 6)
#' check_study_info_structure(study_info4)
check_study_info_structure <- function(study_info){
  names = names(study_info)
  if(is.data.frame(study_info) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(study_info) != 1){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 
  
  if (!"comment" %in% names){
    stop("Object needs to have a 'comment' element")
  }
  
  if (is.na(study_info$comment) | is.null(study_info$comment) | study_info$comment == ""){
    stop("Comment can not be empty")
  }
  
  confirm_columns_not_specified(c("n_groups", "n_tasks"), study_info)
}
