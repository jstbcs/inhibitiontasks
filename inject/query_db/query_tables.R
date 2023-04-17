# Function for selecting based on one or two values

check_filter_db_arguments <- function(data, column, cutoff_values, alternative){
  if (!alternative %in% c("greater", "less", "between", "two.sided")){
    stop("alternative can only be 'greater', 'less' or 'between'.")
  }
  if (alternative == "two.sided"){
    alternative == "between"
  }
  if (is.vector(cutoff_values) == FALSE){
    stop("Specify cutoff values as a vector")
  }
  if (length(cutoff_values) != 2 & length(cutoff_values) != 1){
    stop("Cutoff values can only be one or two values")
  }
  
  if (cutoff_values[1] > cutoff_values[2]){
    stop("The first cutoff value must be the minimum, the second the maximum")
  }
  
  if (alternative == "greater" | alternative == "less"){
    if (length(cutoff_values) != 1){
      stop("For alternatives 'greater' and 'less' there can only be one cutoff-value specified")
    }
  }
}

filter_db <- function(data, column, cutoff_values, alternative = "greater"){
  check_filter_db_arguments(data, column, cutoff_values, alternative)
  
  if (alternative == "greater"){
    filter = filter_greater(data, column, cutoff_values)
  }
  if (alternative == "less"){
    filter = filter_less(data, column, cutoff_values)
  }
  if (alternative == "between"){
    filter = filter_between(data, column, cutoff_values)
  }
  return(filter)
}

filter_greater <- function(data, column, cutoff_value){
  filtered_data = data %>% 
    filter({{column}} > cutoff_value)
  
  return(filtered_data)
}

filter_less <- function(data, column, cutoff_value){
  filtered_data = data %>% 
    filter({{column}} < cutoff_value)
  return(filtered_data)
}

filter_between <- function(data, column, cutoff_values){
  filtered_data = data %>% 
    filter({{column}} > cutoff_values[1] & {{column}} < cutoff_values[2])
  return(filtered_data)
}

query_db <- function(conn, arguments, target_level = "data"){
  # TODO: Testing fro structure
  # arguments = list(list(column = "column", values = c(value1, value2?)), list(...))
  
  # Querying starts
  column_names = get_column_names(conn)
  
  arguments_matches = list()
  
  for (i in seq_along(arguments)){
    relevant_tables = find_relevant_tables(conn, arguments[[i]]$column, column_names)
    
    # Init list
    table_match_ids = list(
      tables = relevant_tables
    )
    for (i in seq_along(relevant_tables)){
      ids = extract_table_ids(conn, relevant_tables[i])
      table_match_ids[[i+1]] = ids
    }
    arguments_matches[[i]] = table_match_ids
  }
}

get_column_names <- function(conn){
  tables = dbListTables(conn)
  tables = tables[tables != "sqlite_sequence"]
  column_names = data.frame(
    column = c(),
    table = c()
  )
  for (table in tables){
    columns = dbListFields(conn, table)
    for (column in columns){
      add = data.frame(
        column = column,
        table = table
      )
      column_names = rbind(column_names, add)
    }
  }
  return(column_names)
}

find_relevant_tables <- function(conn, column_name, info = NULL){
  if (is.null(info)){
    info = get_column_names(conn)
  }
  
  tables = info[info$column == column_name, "table"]
  
  if (length(tables) == 0){
    stop("This column does not exist in any table in this db")
  }
  
  return(tables)
}

# Questions should have a column name and value