#' Check if column names are present in specified data object and prompt for confirmation
#'
#' This function checks if a given set of column names exists in a specified data
#' object. If a column name is not present in the specified data object, a warning
#' message is displayed and the user is prompted to confirm whether they want to continue
#' adding the data with NULL values for the missing columns. If the user chooses to cancel,
#' the function throws an error with a message "Process cancelled".
#'
#' @param colnames A character vector of column names to check for existence.
#' @param object The data object to check for column names.
#' @export
#' @examples
#' data <- data.frame(x = 1:10, y = 11:20)
#' confirm_columns_not_specified(c("x", "y"), data)
#' confirm_columns_not_specified(c("x", "z"), data)
confirm_columns_not_specified <- function(colnames, object) {
  vec <- c()
  for (i in seq_along(colnames)) {
    vec[i] <- exists(colnames[i], object)
  }
  if (all(vec) == FALSE) {
    message <- paste(
      "Caution, you have not specified the columns: \n",
      paste(colnames[which(vec == FALSE)], collapse = "; "),
      "\ndo you want to continue adding the data anyway? \nNULL will be entered into the database in columns not specified"
    )
    continue_after_warning(message)
  }
}