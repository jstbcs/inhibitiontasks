return_connected_ids <- function(conn, table, ids){
  # TODO: testing
  
  id_list = list()
  done = c()
  finished = c("publication", "study", "condition", "dataset_overview", "data",
               "group_table", "within", "task")

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
      for (j in 1:length(done)){
        finished_ids[j] = return_id_name_from_table(done[j])
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
      backward_tables = backward_tables[!backward_tables %in% done]
      backward_ids = c()
      if (length(backward_tables) != 0){
        for (j in 1:length(backward_tables)){
          backward_ids[j] = return_id_name_from_table(backward_tables[j])
        }
      }

      
      other_tables = unique(c(forward_tables, backward_tables))
      other_ids = unique(c(forward_ids, backward_ids))
      mode = c(rep("forward", length(forward_ids)), rep("backward", length(backward_ids)))
      origin = id_name
      
      if (length(other_tables) != 0){
        other_table_data = data.frame(
          other_tables,
          other_ids,
          mode,
          origin
        )
      }
      
      # TODO: Not i+j, but find next free spot
      if (length(other_tables != 0)){
        for (j in 1:nrow(other_table_data)){
          newlist = list()
          newlist$table = other_table_data$other_tables[j]
          if (other_table_data$mode[j] == "backward"){
            newlist$ids = relevant_ids[, other_table_data$origin[j]]
          } else {
            newlist$ids = relevant_ids[, other_table_data$other_ids[j]]
          }
          newlist$mode = other_table_data$mode[j]
          newlist$origin_id = other_table_data$origin[j]
          query = c(query, list(newlist))
        }
      }
      max_i = max_i + 1
      print(paste("Max_i within for loop:", max_i))
      print(paste("Query", i, "done"))
      print(done)
    }
    print(paste("Max_i outside:", max_i))
  }
  
  return(id_list)
}

return_id_name_from_table <- function(table){
  if (table == "data"){
    name = "observation_id"
  } else if (table == "dataset_overview"){
    name = "dataset_id"
  } else if(table == "group_table") {
    name = "group_id"
  } else{
    name = paste0(table, "_id")
  }
  return(name)
}

return_table_name_from_id <- function(id_name){
  if (id_name == "observation_id"){
    name = "data"
  } else if (id_name == "dataset_id"){
    name = "dataset_overview"
  } else if (id_name == "group_id"){
    name = "group_table"
  } else {
    name = stringr::str_remove(id_name, "_id$")
  }
}

library(DBI)
library(RSQLite)
library(dplyr)
conn = DBI::dbConnect(RSQLite::SQLite(), "pilot.db")
table = "study"
ids = c(1, 2)

test = return_connected_ids(conn, table, ids)
