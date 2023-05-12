#' Check the overall structure of a list of entries
#'
#' This function checks if the overall structure of a list of entries complies with a predefined structure
#'
#' @param entry_list A list of entries.
#' @details Each entry should have a publication, study, and data level element.
#' @details A publication level element should have at least one study level element.
#' @details A study level element should have at least one data level element.
#' @details Each publication, study, and data level element should comply with a predefined structure.
#' @details The function stops with an error message if the object passed does not have valid publication elements.
#' @export
#' @seealso [check_publication_level_structure()], [check_study_level_structure()], and [check_data_level_structure()].
#' @examples
#' entry_list <- list(
#'   publication1 = list(
#'     study1 = list(
#'       data1 = data.frame(x = 1:3, y = 4:6),
#'       data2 = data.frame(x = 4:6, y = 7:9)
#'     ),
#'     study2 = list(
#'       data3 = data.frame(x = 7:9, y = 10:12),
#'       data4 = data.frame(x = 10:12, y = 13:15)
#'     )
#'   ),
#'   publication2 = list(
#'     study3 = list(
#'       data5 = data.frame(x = 13:15, y = 16:18),
#'       data6 = data.frame(x = 16:18, y = 19:21)
#'     )
#'   )
#' )
#' check_overall_structure(entry_list)
check_overall_structure <- function(entry_list){
  # Its possible that multiple publications are in one entry_list
  pub_names = which_elements_match(names(entry_list), regex_matches_publication_names)
  
  # If length pub_names is 0, wrong object passed
  if (length(pub_names) == 0){
    stop("Object has no valid publication elements")
  }
  
  for (publication in pub_names){
    # check publication level 
    check_publication_level_structure(entry_list[[publication]])
    
    # Now get names of all the study elements in that publication
    study_names = which_elements_match(names(entry_list[[publication]]), regex_matches_study_names)
    # loop over each study element and test structure on study level
    for(study in study_names) {
      check_study_level_structure(entry_list[[publication]][[study]])
      
      # now loop over data names
      data_names = which_elements_match(
        names(entry_list[[publication]][[study]]),
        regex_matches_data_names
      )
      
      # loop over each data element within each study element
      for(data in data_names) {
        check_data_level_structure(entry_list[[publication]][[study]][[data]])
      }
    }
  }
}
