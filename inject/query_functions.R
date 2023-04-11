# Functions that query the database and return certain objects
return_id_name <- function(type){
  if (type == "data" | type == "dataset_overview"){
    name = "dataset_id"
  } else if(type == "group_table") {
    name = "group_id"
  } else{
    name = paste0(type, "_id")
  }
  return(name)
}

return_publication_code <- function(publication_info){
  publication_code = publication_info$publication_code
  return(publication_code)
}

does_publication_code_exist <- function(conn, code){
  pub_table = tbl(conn, "publication")
  pub_id = pub_table %>% 
    filter(publication_code == code) %>% 
    pull(publication_id)
  length = length(pub_id)
  if (length == 0){
    return(FALSE)
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This publication code was already found twice in the database. Please investigate what went wrong here.")
  }
}

find_publication_id <- function(conn, code){
  pub_table = tbl(conn, "publication")
  pub_id = pub_table %>% 
    filter(publication_code == code) %>% 
    pull(publication_id)
  return(pub_id)
}

find_next_free_id <- function(conn, type){
  data = as.data.frame(tbl(conn, type))
  column = return_id_name(type)
  max = max(data[column], na.rm = TRUE)
  if (is.na(max) | max <= 0){
    max = 0
  }
  next_free = max + 1
  return(next_free)
}

find_next_free_publication_id <- function(conn){
  pub_table = tbl(conn, "publication")
  next_free = find_next_free(pub_table, "publication_id")
  return(next_free)
}

return_publication_id <- function(conn, code){
  # Finds the next free pub code if it doesnt already exist
  if (does_publication_code_exist(conn, code) == FALSE) 
  {
    continue_after_warning("This publication code does not currently exist. Want to add it to the study-table?")
    add_study_info(conn, object)
    find_study_id(conn, object)
  } else 
  {
    find_study_id(conn, object)
  }
}
