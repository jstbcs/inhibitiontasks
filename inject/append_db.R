append_db <- function(conn, object, level, details, keys = NULL){
  # Find connection
  # Write level-appropriate functions returning the correct publication/study/group/within ids
  if (!level %in% c("study", "data")){
    stop("Can only append the database on study or data level")
  }
  
  check_append_details(details)
  
  if (level == "study"){
    append_db_study(conn, object, details)
  }
  
  if (level == "data"){
    if (is.null(keys)){
      stop("Adding a dataset requires specifying a key-dataframe for group keys")
    }
    append_db_data(conn, object, details, keys)
  }
  
}

check_append_details <- function(details){
  if (
    stringr::str_detect(details, regex_matches_publication_code) == FALSE &
    stringr::str_detect(details, "^\\d+$") == FALSE
  ){
    stop("Details need to be either a publication code or a study-id that a dataset should be added to.")
  }
}

append_db_study <- function(conn, object, details){
  # Need to return the id depending on the details
  # Details should be a list of publication_code or study_id
  check_study_level_structure(object)

  # now loop over data names
  data_names = which_elements_match(
    names(object),
    regex_matches_data_names
  )

  # loop over each data element within each study element
  for(data in data_names) {
    check_data_level_structure(object[[data]])
  }

  # If a publication code is given, return the id
  if (stringr::str_detect(details, regex_matches_publication_code) == TRUE){
    pub_id = return_publication_id(conn, details)
  } else { # else the id is just the details
    pub_id = details
  }
  
  add_study(conn, object, pub_id)
}

append_db_data <- function(conn, object, details, keys){
  # Data should get the study-level ids (pub, group and id)
  # but create its own within and condition ids
  check_data_level_structure(object)
  
  study_id = details
  
  add_data(conn, object, study_id, keys)
  
}
