#' Check if input object is at the top level
#'
#' This function checks if a specified input object is at the top level of a list, i.e.,
#' it contains the required elements to proceed with a specific operation. The function
#' checks for the presence of the publication element in the object, and if it is not
#' present, it throws an error with a message specifying that the object passed to the
#' function is not at the top level and needs to be modified.
#'
#' @param object The input object to check.
#' @export
#' @examples
#' # Sample data
#' data <- list(
#' author = "John Doe",
#' publication = list(
#' title = "The Art of Programming",
#' year = 2021,
#' editor = "Jane Smith"
#' )
#' )
#'
#' # This will not raise an error
#' stop_if_not_top_level(data)
#'
#' # This will raise an error
#' stop_if_not_top_level(data$publication)
stop_if_not_top_level <- function(object) {
  if (do_elements_exist(c("publication"), object) == FALSE) {
    stop("This function takes the overall list as input. \nMake sure the object passed to it is on highest level and not a data-sublist object")
  }
}