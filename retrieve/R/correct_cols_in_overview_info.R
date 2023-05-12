#' Correct columns in overview_info
#'
#' This function ensures that the provided `overview_info` object is a data frame
#' with a single row and that it contains specific optional columns. It throws
#' an error if the object does not meet these requirements.
#'
#' @param overview_info A data frame object containing experiment overview
#'   information.
#' @export
#' @examples
#' overview_info <- data.frame(
#'   data_excl = TRUE,
#'   fixation_cross = FALSE,
#'   time_limit = 10,
#'   github = "https://github.com/example-repo",
#'   comment = "Example comment"
#' )
#' correct_cols_in_overview_info(overview_info)
#' 
#' # Example of providing incorrect object
#' correct_cols_in_overview_info(list(a = 1, b = 2))

correct_cols_in_overview_info <- function(overview_info){
  colnames = colnames(overview_info)
  
  # check if object is data frame 
  if(!is.data.frame(overview_info)){
    stop("overview_info must be a dataframe")
  }
  # check if object contains more than 1 entry
  if(nrow(overview_info) > 1){
    stop("The overview_info data frame can only contain 1 row")
  }
  
  # warning and user input for missing columns 
  optional_cols <- c("data_excl", "fixation_cross", "time_limit", "github",
                     "comment")
  confirm_columns_not_specified(optional_cols, overview_info)
}
