# These functions take an object and add the db ids to the respective tables
# Need to find the IDs to make sure that tables on different levels can get them

# Add object on data level
add_data <- function(conn, entry_data, study_id, group_keys){
  # Add task and get the task id
  task_id = find_next_free_id(conn, "task")
  entry_data$task$task_id = task_id
  entry_data$overview$task_id = task_id
  entry_data$task$task_name = entry_data$task$task
  task = entry_data$task
  add_table(conn, task, "task")
  
  # Get dataset id
  dataset_id = find_next_free_id(conn, "dataset_overview")
  
  entry_data$overview$dataset_id = dataset_id
  entry_data$overview$study_id = study_id
  entry_data$overview$task_id = task_id
  
  entry_data$within$dataset_id = dataset_id
  entry_data$condition$dataset_id = dataset_id
  
  entry_data$data$dataset_id = dataset_id
  
  # Add within
  within_id = find_next_free_id(conn, "within")
  for (row in 1:nrow(entry_data$within)){
    entry_data$within$within_id[row] = within_id
    within_id = within_id + 1
  }
  
  within_keys = obtain_keys(entry_data$within,
                            "within")
  
  # Add condition
  condition_id = find_next_free_id(conn, "condition")
  for (row in 1:nrow(entry_data$condition)){
    entry_data$condition$condition_id[row] = condition_id
    condition_id = condition_id + 1
  }
  
  condition_keys = obtain_keys(entry_data$condition,
                               "condition")
  
  
  # Replace group, within, condition in data
  entry_data$data = entry_data$data %>% 
    replace_id_keys_in_data(., group_keys, "group") %>% 
    replace_id_keys_in_data(., within_keys, "within") %>% 
    replace_id_keys_in_data(., condition_keys, "condition")
  
  # Add all tables
  dataset_overview = entry_data$overview
  add_table(conn, dataset_overview, "dataset_overview")
  
  within = entry_data$within
  add_table(conn, within, "within")
  
  condition = entry_data$condition
  add_table(conn, condition, "condition")
  
  raw_data = entry_data$data
  add_table(conn, raw_data, "data")
}

add_study <- function(conn, study, pub_id){
  study_id = find_next_free_id(conn, "study")
  # Add study id to study_info and group_info
  study$study_info$study_id = study_id
  study$group_info$study_id = study_id
  
  # Also add the publication id
  study$study_info$publication_id = pub_id
  
  # Then add the study table
  study_info = study$study_info
  add_table(conn, study_info, "study")
  
  group_id = find_next_free_id(conn, "group_table")
  
  # This adds the global group-id to the group_id table
  for (row in 1:nrow(study$group_info)){
    study$group_info$group_id[row] = group_id
    group_id = group_id + 1
  }
  
  group_keys = obtain_keys(study$group_info,
                           "group")
  
  # Then add the group table
  group_info = study$group_info
  
  add_table(conn, group_info, "group_table")
  
  # Now moving to dataset
  data_names = which_elements_match(names(study), regex_matches_data_names)
  
  for (data in data_names){
    add_data(
      conn,
      study[[data]],
      study_id,
      group_keys
    )
  }
  
}


# Publication
add_object <- function(conn, object){
  pub_names = which_elements_match(names(object), regex_matches_publication_names)
  
  for (publication in pub_names){
    pub_code = object[[publication]]$publication_info$publication_code
    # if (does_publication_code_exist(conn, pub_code) == TRUE){
    #   stop("This publication code already exists. Please use the append_db function to add to a specific publication.")
    # }
    
    # Find and add pub id
    pub_id = find_next_free_id(conn, "publication")

    pub_info = object[[publication]]$publication_info
    
    # Then add that to db
    add_table(conn, pub_info, "publication")
    
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
