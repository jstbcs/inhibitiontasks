#' Check if required columns are present in within_info data frame
#'
#' This function checks if a given data frame contains the required columns for the within factors, which are "within_id" and "within_description".
#'
#' @param within_info A data frame containing information about the within factors.
#' @export
#' @return NULL if the data frame contains the required columns.

#' @examples
#' within_info <- data.frame(within_id = c(1, 2), within_description = c("condition A", "condition B"))
#' correct_cols_in_within_info(within_info)
correct_cols_in_within_info <- function(within_info){
  colnames = colnames(within_info)
  
  # check if object is data frame 
  if(!is.data.frame(within_info)){
    stop("within_info must be a dataframe")
  }
  
  # stop if required column names are not present
  names_should <- c("within_id", "within_description")
  missing_cols <- c()
  for(element in names_should){
    if(!(element %in% colnames)){
      missing_cols <- c(missing_cols, element)
    }
  } 
  if (length(missing_cols) > 0){
    stop(c("Columnname(s) missing from within_info data frame: ", paste(missing_cols, collapse = ", ")))
  }
}
