#' Check for Correct Number of Within IDs
#'
#' Check that the within ids are unique and match the within columns in the data table.
#' Raises an error if the within ids are not unique or if the number of unique within conditions
#' in the data table is different from the number of unique within ids in the within_info table.
#'
#' @param within_info A data frame containing the within_id column.
#' @param data_df A data frame containing a within column.
#' @return An error if the within ids are not unique or if the number of unique within conditions
#'   in the data table is different from the number of unique within ids in the within_info table.
#' @export
#' @examples
#' within_info <- data.frame(within_id = c(1, 2, 3))
#' data_df <- data.frame(within = c(1, 2, 3, 1, 2, 3))
#' correct_n_of_withinid(within_info, data_df)
#'
#' # Throws an error due to duplicate within_id
#' within_info_dup <- data.frame(within_id = c(1, 2, 3, 3))
#' correct_n_of_withinid(within_info_dup, data_df)
#'
#' # Throws an error due to missing within condition in within_info
#' within_info_missing <- data.frame(within_id = c(1, 2))
#' correct_n_of_withinid(within_info_missing, data_df)
correct_n_of_withinid <- function(within_info, data_df) {
  # Check if within ids are unique
  if (length(unique(within_info$within_id)) != nrow(within_info)) {
    stop("Duplicate within_id in within_info found. Make sure within_id is unique")
  }
  
  # Check if number of within id matches within columns in data table
  if (length(unique(data_df$within)) > length(unique(within_info$within_id))) {
    stop("Number of unique within conditions in data table is larger than in within_info table. 
         \nMake sure all within conditions are included in within_info.")
  } else if (length(unique(data_df$within)) < length(unique(within_info$within_id))) {
    stop("The within_info table contains more unique within_ids than the data table does. 
         \nMake sure the within column in the data table is coded correctly and the 
         within_info table contains only relevant within conditions")
  }
}
