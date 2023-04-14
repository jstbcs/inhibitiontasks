#' Check if object is at data level
#'
#' Checks if the specified object is at the data level by checking if it contains
#' the necessary elements: task, overview, data and within. If the check fails,
#' the function throws an error message.
#'
#' @param object The R object to check
#' @export
#' @examples
#' # assume 'data_obj' is a data-level object
#' stop_if_not_data_level(data_obj)
#'
#' @return Throws an error message if the object is not at the data level
stop_if_not_data_level <- function(object){
  if (do_elements_exist(c("task", "overview", "data", "within"), object) == FALSE)
  {
    stop("This function takes a data-level object")
  }
}