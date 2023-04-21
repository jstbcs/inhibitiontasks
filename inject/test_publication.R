# # check structure of inserted element on  publication level
source("./inject/helper_functions.R")
# This function checks elements on publication level
check_publication_level_structure <- function(object){
  names = names(object)
  stop_if_not_publication_level(object)
  stop_if_names_duplicated(names)
  if (!all(str_detect(names, regex_matches_study_names) | str_detect(names, "publication_table")))
  {
    "Names can only be study[NUMBER] or publication_table."
  }
  check_publication_table(object$publication_table)
}

check_publication_table <- function(pub_table){
  name = names(pub_table)
  stop_if_names_duplicated(names)
  
  confirm_object_names(pub_table, table_info_db$publication_table)
}