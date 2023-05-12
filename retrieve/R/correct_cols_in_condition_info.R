#' Check and correct column names for condition description data frame
#'
#' This function checks if the input object is a data frame and whether it has
#' the required column names: "percentage_congruent", "percentage_neutral",
#' "n_obs", and "mean_obs_per_participant". If any of these columns are missing,
#' the function raises an error indicating which column(s) are missing.
#'
#' @param condition_descr_info A data frame containing condition description information.
#' 
#' @return This function does not return anything. It raises an error if the column names are incorrect.
#'
#' @seealso [correct_cols_in_within_info()] to check and correct column names for within-subject factors.
#' @seealso [correct_cols_in_overview_info()] to check and correct column names for experiment overview data.
#' @seealso [correct_cols_in_data()] to check and correct column names for trial-level data.
#'
#' @export
#' 
#' @examples
#' correct_cols_in_condition_info(condition_descr_info)


correct_cols_in_condition_info <- function(condition_descr_info){
  colnames = colnames(condition_descr_info)
  
  # check if object is data frame
  if(!is.data.frame(condition_descr_info)){
    stop("condition_descr_info must be a dataframe")
  }
  
  # stop if required column names are not present
  names_should <- c("percentage_congruent", "percentage_neutral",
                    "n_obs", "mean_obs_per_participant")
  missing_cols <- c()
  for(element in names_should){
    if(!(element %in% colnames)){
      missing_cols <- c(missing_cols, element)
    }
  } 
  if (length(missing_cols) > 0){
    stop(c("Columnname(s) missing from condition_descr_info data frame: ", paste(missing_cols, collapse = ", ")))
  }
}
