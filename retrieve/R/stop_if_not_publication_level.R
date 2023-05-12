#' Check if object is a publication-level object
#'
#' This function checks if an object is at the publication level by verifying
#' that it has column names that match a specific pattern.
#'
#' @param object The object to check.
#' @export
#' @seealso [stop_if_not_top_level()] to check if the object is at the top level.
#' @examples
#' pub_data <- list(
#' publication = "Some publication",
#' study_1 = list(
#' study_info = data.frame(id = 1),
#' group_info = data.frame(id = 1),
#' data1 = data.frame(id = 1)
#' ),
#' study_2 = list(
#' study_info = data.frame(id = 2),
#' group_info = data.frame(id = 2),
#' data1 = data.frame(id = 2)
#' )
#' )
#' stop_if_not_publication_level(pub_data) # this will run successfully
#' stop_if_not_publication_level(pub_data$study_1$study_info) # this will throw an error
stop_if_not_publication_level <- function(object){
  if (!all(str_detect(colnames(object), regex_matches_study_names))){
    stop("This function takes a publication-level object")
  }
}