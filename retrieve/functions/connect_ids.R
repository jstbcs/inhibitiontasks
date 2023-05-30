return_connected_ids <- function(conn, table, ids){
  # TODO: testing
  
  id_list = list()
  done = c()
  finished = c("publication_table", "study_table", "condition_table", 
               "dataset_table", "observation_table",
               "between_table", "within_table", "task_table")

  counter = 0
  
  query = list(list())
  max_i = 1
  
  while (!all(finished %in% done)){
    if (counter == 0){
      # Find which tables to query
      query[[1]]$table = table
      query[[1]]$ids = ids
      query[[1]]$mode = "forward"
      query[[1]]$origin_id = NULL
      counter = 1
    }

    for (i in max_i:length(query)){
      table_to_query = query[[i]]$table
      matching_ids = query[[i]]$ids
      mode = query[[i]]$mode
      origin_id = query[[i]]$origin_id
      
      print(paste("Starting query of table:", table_to_query))
      
      # Get their id-names
      id_name = return_id_name_from_table(table_to_query)
      
      # Find all other id-variables in that table
      found_ids = extract_table_ids(conn, table_to_query)
      
      # Filter to only include the relevant ids
      if (mode == "backward"){
        relevant_ids = found_ids[found_ids[[origin_id]] %in% matching_ids, ]
      } else {
        relevant_ids = found_ids[found_ids[[id_name]] %in% matching_ids, ]
      }
      
      # Add the table-primary key to the ids-list
      # Need to do this vector approach to combat no dataframe being extracted above
      if (is.vector(relevant_ids)){
        relevant_ids = as.data.frame(relevant_ids)
        colnames(relevant_ids) = id_name
      } 
      id_list[[table_to_query]] = unique(relevant_ids[[id_name]])
      
      
      # Add the table-name to the done-list
      done = c(done, table_to_query)
      done = unique(done)
      
      # Get finished ids
      finished_ids = c()
      for (j in 1:length(query)){
        finished_ids[j] = return_id_name_from_table(query[[j]]$table)
      }
      
      finished_tables = c()
      for (j in 1:length(finished_ids)){
        finished_tables[j] = return_table_name_from_id(finished_ids[j])
      }
            # Get all other tables that need to be queried
      # This is a forward search
      forward_ids = colnames(relevant_ids)[!colnames(relevant_ids) %in% finished_ids]
      forward_tables = c()
      if (length(forward_ids) != 0){
        for (j in 1:length(forward_ids)){
          forward_tables[j] = return_table_name_from_id(forward_ids[j])
        }
      }
      # Need to add backward search aswell
      backward_tables = find_relevant_tables(conn, id_name)
      backward_tables = backward_tables[!backward_tables %in% finished_tables]
      backward_ids = c()
      if (length(backward_tables) != 0){
        for (j in 1:length(backward_tables)){
          backward_ids[j] = return_id_name_from_table(backward_tables[j])
        }
      }
      
      forward_data = data.frame()
      if (length(forward_tables) != 0){
        forward_data = data.frame(
          table = forward_tables,
          ids = forward_ids,
          mode = "forward",
          origin = id_name
        )
      }
      
      backward_data = data.frame()
      if (length(backward_tables) != 0){
        backward_data = data.frame(
          table = backward_tables,
          ids = backward_ids,
          mode = "backward",
          origin = id_name
        )
      }
      
      other_table_data = data.frame()
      if (length(forward_tables) != 0 & length(backward_tables) != 0){
        other_table_data = rbind(forward_data, backward_data)
      } else if (length(forward_tables) == 0 & length(backward_tables) != 0){
        other_table_data = backward_data
      } else if (length(forward_tables) != 0 & length(backward_tables) == 0){
        other_table_data = forward_data
      }
      
      newlist = list()
      if (nrow(other_table_data) != 0){
        for (j in 1:nrow(other_table_data)){
          newlist = list()
          newlist$table = other_table_data$table[j]
          if (other_table_data$mode[j] == "backward"){
            newlist$ids = relevant_ids[, other_table_data$origin[j]]
          } else {
            newlist$ids = relevant_ids[, other_table_data$ids[j]]
          }
          newlist$mode = other_table_data$mode[j]
          newlist$origin_id = other_table_data$origin[j]
          query = c(query, list(newlist))
        }
      }
      # Increase max_i
      max_i = max_i + 1
      
      print(paste("Query", i, "done"))
    }
  }
  
  return(id_list)
}

# 
# library(DBI)
# library(RSQLite)
# library(dplyr)
# conn = DBI::dbConnect(RSQLite::SQLite(), "initial_db.db")
# table = "study_table"
# ids = c(2,3)
# 
# test = return_connected_ids(conn, table, ids)
