extract_table_ids <- function(conn, table, name = NULL){
  # TODO: Replace this with SQL-Language
  
  data = as.data.frame(tbl(conn, table))
  ids = data %>% 
    select(ends_with("_id"))
  
  if (!is.null(name)){
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

