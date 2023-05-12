#' Check for Correct Columns in Task Info
#'
#' This function checks if the task_info dataframe contains the correct column names and only one row of data.
#' The function also checks if the task name is valid.
#' @param task_info A dataframe containing information about the task.
#' @return No value is returned; this function is used for error checking and will throw an error if the task_info
#'         dataframe does not contain the correct column names or has more than one row of data or the task name
#'         is not valid.
#' @export
#' @examples
#' task_info <- data.frame(task = "Memory Game", task_description = "Remember the position of the cards")
#' correct_cols_in_task_info(task_info)
#' # No output, as the function is used for error checking
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
  
  # Check to see if task name is valid
  task_name = task_info$task
  if (!task_name %in% valid_task_names){
    msg = paste("Task name:", task_name, "is invalid. Please ensure you want to add that task name")
    continue_after_warning(msg)
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
