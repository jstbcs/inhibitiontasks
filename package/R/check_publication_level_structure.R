#' Check if R object follows publication-level structure
#'
#' This function checks if an R object follows the expected publication-level structure.
#' It ensures that the object has unique names, starts with "study", followed by a number, and
#' that it has passed the check for publication-level structure using stop_if_not_publication_level() function.
#'
#' @param object An R object to be checked.
#'
#' @return This function does not return anything. It throws an error message if the object does not follow the expected structure.
#'
#' @export
#'
#' @examples
#' # This should not throw any errors
#' check_publication_level_structure(list(study1 = data.frame(x = 1:10, y = 11:20),
#' study2 = data.frame(x = 21:30, y = 31:40)))
#'
#' # This should throw an error about duplicate names
#' check_publication_level_structure(list(study1 = data.frame(x = 1:10, y = 11:20),
#' study1 = data.frame(x = 21:30, y = 31:40)))
#'
#' # This should throw an error about invalid name format
#' check_publication_level_structure(list(study1 = data.frame(x = 1:10, y = 11:20),
#' s1 = data.frame(x = 21:30, y = 31:40)))
#'
#' @seealso [stop_if_not_publication_level()] to check if the object has the correct structure.
#' @seealso [stop_if_names_duplicated()] to check if the object's names are unique.
check_publication_level_structure <- function(object){
  names = names(object)
  stop_if_not_publication_level(object)
  stop_if_names_duplicated(names)
  if (!all(str_detect(names, regex_matches_study_names)))
  {
    stop("Names can only be study[NUMBER]")
  }
}