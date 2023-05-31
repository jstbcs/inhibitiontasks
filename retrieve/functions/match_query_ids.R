extract_table_ids <- function(conn, table, name = NULL){
  # TODO: Replace this with SQL-Language
  which_columns_exist_query = paste0("PRAGMA table_info(", table, ")")
  table_columns = dbGetQuery(conn, which_columns_exist_query)
  ends_in_id =  table_columns$name[grep("_id$", table_columns$name)]

  sql_statement = paste0("SELECT ", paste(ends_in_id, collapse = ", "), " FROM ", table)

  ids = dbGetQuery(conn, sql_statement)
  
  if (!is.null(name)){
    data = as.data.frame(tbl(conn, table))
    
    ids = ids %>% select(contains({{name}}))
  }
  
  return(ids)
}

return_only_matches <- function(data1, data2, id = NULL, full = FALSE){
  if (!is.null(id)){
    matches = inner_join(data1, data2) %>% 
      select({{id}})
    
    return(matches)
  }
  
  common_cols = intersect(
    colnames(data1),
    colnames(data2)
  )
  
  if (full == FALSE){
    matches = inner_join(data1, data2) %>% 
      select({{common_cols}})
  } else {
    matches = inner_join(data1, data2)
  }
  
  
  return(matches)
}

