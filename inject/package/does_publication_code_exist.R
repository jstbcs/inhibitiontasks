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