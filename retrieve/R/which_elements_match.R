#' Return only elements that match a regular expression
#'
#' This function filters the input vector to return only the elements that match
#' a given regular expression.
#'
#' @param vector A vector of elements to filter.
#' @param regex A regular expression used to match the elements to keep.
#' @return A vector containing only the elements that match the regular expression.
#' @examples
#' vector <- c("apple", "banana", "carrot", "avocado", "pear")
#' which_elements_match(vector, "a")
#' which_elements_match(vector, "b.*a")
which_elements_match <- function(vector, regex){
  clean = vector[stringr::str_detect(vector, regex)]
  return(clean)
}