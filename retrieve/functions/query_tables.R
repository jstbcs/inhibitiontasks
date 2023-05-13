# Function for selecting based on one or two values
query_db <- function(conn, arguments, target_level = "data", argument_relation = "and"){
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
  argument_matches = list()

  argument_sequence = get_argument_sequence(arguments, argument_relation)
  
  for (i in seq_along(arguments)){
    # Loop through arguments, find matching primary keys
    # Then find keys connected to these matches
    # If its the first iteration (first argument), then the init ids is the matches
    # Otherwise it is only the matches that are already present in the arguments
    if (i == 1){
      # Find out which table is queried from
      table = which_table_is_queried(arguments[[i]])
      
      # Obtain the ids that match that query
      matching_ids = dbGetQuery(conn, arguments[[i]])[[return_id_name_from_table(table)]]
      
      # Return ids connected to this into the appropriate sublist in argument_matches
      argument_matches[[i]] = return_connected_ids(conn, table, matching_ids)
      
    } else {
      if (argument_sequence[i] != argument_sequence[i - 1]){
        # then it is an "and" connector, you should have starting ids
        table = which_table_is_queried(arguments[[i]])
        
        init_ids = argument_matches[[i - 1]][[table]]
        
        matching_ids = dbGetQuery(conn, arguments[[i]])[[return_id_name_from_table(table)]]
        
        relevant_ids = matching_ids[matching_ids %in% init_ids]
        
        argument_matches[[i]] = return_connected_ids(conn, table, relevant_ids)
       
      } else {
        # then it is an "or" connector, you should return all ids
        table = which_table_is_queried(arguments[[i]])
        
        matching_ids = dbGetQuery(conn, arguments[[i]])[[return_id_name_from_table(table)]]
        
        argument_matches[[i]] = return_connected_ids(conn, table, matching_ids)
      }
    }
  }
  
  # Now use the list of argument matches to return a proper list depending on structure
  proper_matches = return_proper_ids(argument_matches, argument_sequence)
  # Then use that list to query the db
  
  read_data = return_entries_from_id(conn, proper_matches)
  
  return(read_data)
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