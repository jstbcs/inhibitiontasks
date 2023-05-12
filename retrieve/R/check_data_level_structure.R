#' Check the data level structure
#'
#' This function checks if the input list conforms to the expected structure of data used by the function.
#'
#' @param data_i A list of data frames with specific columns: task_info, overview_info, within_info and data.
#'
#' @seealso [correct_elements_in_data_list()] to check if all elements present are correct
#' @seealso [correct_cols_in_task_info()], [correct_cols_in_overview_info()], [correct_cols_in_within_info()], [correct_cols_in_data()], [correct_n_of_withinid()] to check object$task_info structure
#' @export
#' 
#' @return This function does not return anything. It checks if the input list conforms to the expected structure of data.
#' 
#' @examples
#' check_data_level_structure(data_list)
#' 

check_data_level_structure <- function(data_i){
  
  # check if input is a list
  if (!is.list(data_i))
  {
    stop("Input data must be a list.")
  } 
  
  # check if list contains correct elements
  correct_elements_in_data_list(data_i)
  
  # check if each element in data_i list is a data frame and contains required columns
  correct_cols_in_task_info(data_i$task_info)
  correct_cols_in_overview_info(data_i$overview_info)
  correct_cols_in_within_info(data_i$within_info)
  correct_cols_in_data(data_i$data)
  
  # check if number of within conditions in data equals number of within_ids in within_info
  correct_n_of_withinid(data_i$within_info, data_i$data)
}
