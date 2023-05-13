return_entries_from_id <- function(conn, matches){
  tables = names(matches)
  
  data = list(
    publication_table = data.frame(),
    study_table = data.frame(),
    task_table = data.frame(),
    between_table = data.frame(),
    within_table = data.frame(),
    condition_table = data.frame(),
    dataset_table = data.frame(),
    observation_table = data.frame()
  )
  
  for (table in tables){
    id_name = return_id_name_from_table(table)
    
    query = write_id_query(table, id_name, matches[[table]])
    
    data[[table]] = dbGetQuery(conn, query)
  }
  
  return(data)
}

write_id_query <- function(table_name, variable_name, id_vector) {
  # Convert the ID vector to a comma-separated string
  id_str = paste0(id_vector, collapse = ",")
  
  # Construct the SQL query
  sql_query = sprintf("SELECT * FROM %s WHERE %s IN (%s)", table_name, variable_name, id_str)
  
  # Return the results
  return(sql_query)
}
