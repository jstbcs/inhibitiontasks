#' Check if element names are unique
#'
#' This function checks if the names of the elements in an object are unique.
#' If there are duplicates, an error message will be returned.
#'
#' @param names A character vector of element names.
#' @export
#' @examples
#' stop_if_names_duplicated(c("name1", "name2", "name3"))
#' stop_if_names_duplicated(c("name1", "name2", "name1"))
stop_if_names_duplicated <- function(names){
  if (any(duplicated(names)))
  {
    error_name = names[which(duplicated(names))]
    error_message = paste(
      "Element-name:",
      error_name,
      "is not unique.",
      "Elements in object must be uniquely named."
    )
    stop(error_message)
  }
}