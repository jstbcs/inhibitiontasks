# These functions should take a vector of database-ids and a info_table
# Then they should properly replace the ids in the dataset

obtain_keys <- function(info_table, method){
  id_name = paste0(method, "_id")
  key_name = paste0(method, "_name")
  
  if (!id_name %in% names(info_table)){
    stop("Remember to add the proper db ids to the table first")
  }
  
  keys = info_table[, c(key_name, id_name)]

  if (is.character(keys[, key_name])){
    keys[, key_name] = parse_number(keys[, key_name])
    
  }
  
  return(keys)
}

replace_id_keys_in_data <- function(data, keys, method){
  id_name = paste0(method, "_id")
  
  # Keys has colums: within_name and within_id, we want within, within_id
  colnames(keys) = c(method, id_name)
  
  
  data = data %>% 
    dplyr::left_join(., keys) %>% 
    select(-{{method}}) 
  return(data)
}
