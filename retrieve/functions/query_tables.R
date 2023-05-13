# Function for selecting based on one or two values
query_db <- function(conn, arguments, target_level = "data", argument_relation = "and"){
  # TODO: Testing fro structure
  # TODO: Target-level
  # TODO: Have argument-relation support. Different numbers are connected via AND, 
    # Same numbers are connected via OR
  # TODO: Have the first set of returned IDs be a baseline for the next query.
  # You can do this by having the first argument add ids into predefined structure
  # Then when you query the new argument, check which table it query, find out the ids that 
  # Match the agruments' condition and then only add ids that are new (not in the predefined structure)
    # Dont query all ids for all arguments. Think about at which points this is important
  #
  # arguments = list("argument1", "argument2")
  
  # Querying starts 
  column_names = get_column_names(conn)
  
  possible_ids = list(
    publication_id = list(),
    study_id = list(),
    between_id = list(),
    within_id = list(),
    condition_id = list(),
    task_id = list(),
    observation_id = list(),
    dataset_table = list()
  )
  
  argument_matches = list()
  
  for (i in seq_along(arguments)){
    argument_matches[[i]] = possible_ids
  }
  
  for (i in seq_along(arguments)){
    # Loop through arguments, find matching primary keys
    # Then find keys connected to these matches
    # If its the first iteration (first argument), then the init ids is the matches
    # Otherwise it is only the matches that are already present in the arguments
    if (i == 1){
      init_ids = 
    }
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