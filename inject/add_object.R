# These functions take an object and add the db ids to the respective tables
# Need to find the IDs to make sure that tables on different levels can get them

# Add object on data level
add_data <- function(conn, entry_data, study_id, group_keys){
  # Add task and get the task id
  task_id = find_next_free_id(conn, "task_table")
  entry_data$task_table$task_id = task_id
  entry_data$dataset_table$task_id = task_id
  task_table = entry_data$task_table
  add_table(conn, task_table, "task_table")
  
  # Get dataset id
  dataset_id = find_next_free_id(conn, "dataset_table")
  
  entry_data$dataset_table$dataset_id = dataset_id
  entry_data$dataset_table$study_id = study_id
  entry_data$dataset_table$task_id = task_id
  
  entry_data$within_table$dataset_id = dataset_id
  entry_data$condition_table$dataset_id = dataset_id
  
  entry_data$observation_table$dataset_id = dataset_id
  
  # Add within
  within_id = find_next_free_id(conn, "within_table")
  for (row in 1:nrow(entry_data$within_table)){
    entry_data$within_table$within_id[row] = within_id
    within_id = within_id + 1
  }
  
  within_keys = obtain_keys(entry_data$within_table,
                            "within")
  
  # Add condition
  condition_id = find_next_free_id(conn, "condition_table")
  for (row in 1:nrow(entry_data$condition_table)){
    entry_data$condition_table$condition_id[row] = condition_id
    condition_id = condition_id + 1
  }
  
  condition_keys = obtain_keys(entry_data$condition_table,
                               "condition")
  
  
  # Sometimes condition, between, or within might be NA, replace those
  if (all(is.na(entry_data$observation_table$between))){
    entry_data$observation_table$between = 1
  }
  if (all(is.na(entry_data$observation_table$within))){
    entry_data$observation_table$within = 1
  }
  if (all(is.na(entry_data$observation_table$condition))){
    entry_data$observation_table$condition = 1
  }
  
  # Replace group, within, condition in data
  entry_data$observation_table = entry_data$observation_table %>% 
    replace_id_keys_in_data(., group_keys, "between") %>% 
    replace_id_keys_in_data(., within_keys, "within") %>% 
    replace_id_keys_in_data(., condition_keys, "condition")
  
  # Add all tables
  dataset_table = entry_data$dataset_table
  add_table(conn, dataset_table, "dataset_table")
  
  within = entry_data$within_table
  add_table(conn, within, "within_table")
  
  condition = entry_data$condition_table
  add_table(conn, condition, "condition_table")
  
  observation = as.data.frame(entry_data$observation_table)
  
  add_table(conn, observation, "observation_table")
}

add_study <- function(conn, study_add, pub_id){
  study_id = find_next_free_id(conn, "study_table")
  # Add study id to study_info and group_info
  study_add$study_table$study_id = study_id
  study_add$between_table$study_id = study_id
  
  # Also add the publication id
  study_add$study_table$publication_id = pub_id
  
  # Then add the study table
  study_table = study_add$study_table
  add_table(conn, study_table, "study_table")
  
  between_id = find_next_free_id(conn, "between_table")
  
  # This adds the global group-id to the group_id table
  for (row in 1:nrow(study_add$between_table)){
    study_add$between_table$between_id[row] = between_id
    between_id = between_id + 1
  }
  
  between_keys = obtain_keys(study_add$between_table,
                           "between")
  
  # Then add the group table
  between_table = study_add$between_table
  
  add_table(conn, between_table, "between_table")
  
  # Now moving to dataset
  data_names = which_elements_match(names(study_add), regex_matches_data_names)
  
  for (data_element in data_names){
    add_data(
      conn,
      study_add[[data_element]],
      study_id,
      between_keys
    )
  }
  
}


# Publication
add_object <- function(conn, object){
  pub_names = which_elements_match(names(object), regex_matches_publication_names)
  
  for (publication in pub_names){
    pub_code = object[[publication]]$publication_table$publication_code
    if (does_publication_code_exist(conn, pub_code) == TRUE){
      stop("This publication code already exists. Please use the append_db function to add to a specific publication.")
    }
    
    # Find and add pub id
    pub_id = find_next_free_id(conn, "publication_table")

    pub_info = object[[publication]]$publication_table
    
    # Then add that to db
    add_table(conn, pub_info, "publication_table")
    
    # Now for study names
    study_names = which_elements_match(names(object[[publication]]), regex_matches_study_names)
    
    for (study in study_names){
      add_study(
        conn,
        object[[publication]][[study]],
        pub_id
      )
    }
  }
}

# Informative Error messages: check publication code
# If publication is already present. Pull study and dataset ids
# Then add_function with publication code

# function to delete publication, study, or dataset
