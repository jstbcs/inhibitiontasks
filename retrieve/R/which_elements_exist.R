#' Identify existing columns in an R object
#'
#' This function takes a character vector of column names and an R object as input
#' and returns a character vector of the column names that exist in the object.
#' If a column name does not exist, a warning is given.
#'
#' @param colnames A character vector of column names.
#' @param object The R object to check for column names.
#' @return A character vector of column names that exist in the object.
#' @seealso [do_elements_exist()] to check if all column names exist in the object.
#' @export
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' which_elements_exist(c("a", "d", "c"), df)
which_elements_exist <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
    if (!exists(colnames[i], object))
    {
      warning(paste("Column", colnames[i], "not present in specified object"))
    }
  }
  return(colnames[which(vec == TRUE)])
}