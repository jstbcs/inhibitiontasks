#' Check and correct columns in data table
#'
#' This function ensures that the provided `data_df` object is a data frame and
#' contains required columns. It throws an error if the object does not meet
#' these requirements.
#'
#' @param data_df A data frame object containing experiment data.
#' @export
#' @examples
#' data_df <- data.frame(
#'   subject = c(1, 1, 1, 2, 2, 2),
#'   block = c(1, 1, 2, 1, 2, 2),
#'   trial = c(1, 2, 1, 1, 2, 3),
#'   group = c("control", "treatment", "control", "control", "treatment", "control"),
#'   within = c("low", "high", "low", "low", "high", "low"),
#'   congr = c("congruent", "incongruent", "incongruent", "incongruent", "congruent", "incongruent"),
#'   accuracy = c(1, 0, 1, 0, 1, 1),
#'   rt = c(0.5, 0.7, 0.6, 0.8, 0.9, 0.4)
#' )
#' correct_cols_in_data(data_df)
#' 
#' # Example of providing incorrect object
#' correct_cols_in_data(list(a = 1, b = 2))

correct_cols_in_data <- function(data_df){
  colnames = colnames(data_df)
  
  # check if object is data frame 
  if(!is.data.frame(data_df)){
    stop("Data table must be a dataframe")
  }
  
  # stop if required column names are not present
  names_should <- c("subject", "block", "trial", "group",
                    "within", "congr", "accuracy", "rt")
  missing_cols <- setdiff(names_should, colnames)
  if (length(missing_cols) > 0){
    stop(paste0("Columnname(s) missing from data table: ",
                paste(missing_cols, collapse = ", ")))
  }
}
