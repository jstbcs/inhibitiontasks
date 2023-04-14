#' Check group info structure
#'
#' The function checks if the provided object is a dataframe. It then checks if it contains the necessary columns (i.e., 'group') and optional columns (i.e., 'group_description', 'mean_age', 'percentage_female', and 'n_participants'). If the 'group_description' column is included, it checks if it has a non-empty value.
#'
#' @param group_info A dataframe containing information about the groups.
#'
#' @return The function returns nothing but throws an error message if the provided group_info object does not have the necessary structure.
#'
#'
#' @export
#' @seealso [confirm_columns_not_specified()] to confirm if specific columns are not included in the dataframe.
#'
#' @examples
#' group_info <- data.frame(group = c("Group A", "Group B"),
#' group_description = c("Description of Group A", "Description of Group B"))
#' check_group_info_structure(group_info)
#'
#' group_info2 <- list(group = c("Group A", "Group B"))
#' check_group_info_structure(group_info2) # throws error message
check_group_info_structure <- function(group_info){
  names = names(group_info)
  if(is.data.frame(group_info) == FALSE)
  {
    stop("Group-Info is not a dataframe")
  }
  
  if (nrow(group_info) > 1){
    if (!"group_description" %in% names){
      stop("Object needs to have a 'group_description' element")
    }
    if (is.na(group_info$group_description) | is.null(group_info$group_description) | group_info$group_description == ""){
      stop("group_description can not be empty")
    }
  }
  
  if (!"group" %in% names){
    stop("Object needs to have a 'group' element")
  }
  
  confirm_columns_not_specified(c("mean_age", "percentage_female",
                                  "n_participants", "group_description"),
                                group_info)
}