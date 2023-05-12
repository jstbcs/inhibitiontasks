#' Check if column names exist in specified data object
#'
#' This function checks if a given set of column names exists in a specified data
#' object. If a column name is not present in the specified data object, a warning
#' message is displayed. This function returns a logical value indicating whether
#' all the specified column names are present in the specified data object.
#'
#' @param colnames A character vector of column names to check for existence.
#' @param object The data object to check for column names.
#' @return A logical value indicating whether all specified column names exist in
#' the specified data object.
#' @export
#' @examples
#' data <- data.frame(x = 1:10, y = 11:20)
#' do_elements_exist(c("x", "y"), data)
#' do_elements_exist(c("x", "z"), data)
do_elements_exist <- function(colnames, object) {
  vec <- c()
  for (i in seq_along(colnames)) {
    vec[i] <- exists(colnames[i], object)
    if (!exists(colnames[i], object)) {
      warning(paste("Column", colnames[i], "not present in data specified"))
    }
  }
  return(all(vec))
}