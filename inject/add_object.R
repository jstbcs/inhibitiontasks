# These functions take an object and add the db ids to the respective tables
# Need to find the IDs to make sure that tables on different levels can get them

# Publication
add_object <- function(conn, object){
  pub_names = which_elements_match(names(object), regex_matches_publication_names)
  
  for (publication in pub_names){
    pub_code = object[[publication]]$publication_info$publication_code
    # if (does_publication_code_exist(conn, pub_code) == TRUE){
    #   stop("Implement this. Cross check all other existances with the db. Or just go ahead and add nonetheless.
    #      But how do you prevent duplicates in this case?")
    # }
    
    # Find and add pub id
    pub_id = find_next_free_id(conn, "publication")

    pub_info = object[[publication]]$publication_info
    
    # Then add that to db
    add_table(conn, pub_info, "publication")
    
    # Now for study names
    study_names = which_elements_match(names(object[[publication]]), regex_matches_study_names)
    
    for (study in study_names){
      study_id = find_next_free_id(conn, "study")
      # Add study id to study_info and group_info
      object[[publication]][[study]]$study_info$study_id = study_id
      object[[publication]][[study]]$group_info$study_id = study_id
      
      # Also add the publication id
      object[[publication]][[study]]$study_info$publication_id = pub_id
      
      # Then add the study table
      study_info = object[[publication]][[study]]$study_info
      add_table(conn, study_info, "study")
      
      group_id = find_next_free_id(conn, "group_table")
      
      # This adds the global group-id to the group_id table
      for (row in 1:nrow(object[[publication]][[study]]$group_info)){
        object[[publication]][[study]]$group_info$group_id[row] = group_id
        group_id = group_id + 1
      }
      
      group_keys = obtain_keys(object[[publication]][[study]]$group_info,
                               "group")

      # Then add the group table
      group_info = object[[publication]][[study]]$group_info
      
      add_table(conn, group_info, "group_table")
      
      # Now moving to dataset
      data_names = which_elements_match(names(object[[publication]][[study]]), regex_matches_data_names)
      
      for (data in data_names){
        # Add task and get the task id
        task_id = find_next_free_id(conn, "task")
        object[[publication]][[study]][[data]]$task_info$task_id = task_id
        object[[publication]][[study]][[data]]$overview$task_id = task_id
        
        task = object[[publication]][[study]][[data]]$task_info
        add_table(conn, task, "task")
        
        # Get dataset id
        dataset_id = find_next_free_id(conn, "dataset_overview")
        
        object[[publication]][[study]][[data]]$overview$dataset_id = dataset_id
        object[[publication]][[study]][[data]]$overview$study_id = study_id
        object[[publication]][[study]][[data]]$overview$task_id = task_id
        
        object[[publication]][[study]][[data]]$within$dataset_id = dataset_id
        object[[publication]][[study]][[data]]$condition$dataset_id = dataset_id

        object[[publication]][[study]][[data]]$data$dataset_id = dataset_id

        # Add within
        within_id = find_next_free_id(conn, "within")
        for (row in 1:nrow(object[[publication]][[study]][[data]]$within)){
          object[[publication]][[study]][[data]]$within$within_id[row] = within_id
          within_id = within_id + 1
        }
        
        within_keys = obtain_keys(object[[publication]][[study]][[data]]$within,
                                  "within")
        
        # Add condition
        condition_id = find_next_free_id(conn, "condition")
        for (row in 1:nrow(object[[publication]][[study]][[data]]$condition)){
          object[[publication]][[study]][[data]]$condition$condition_id[row] = condition_id
          condition_id = condition_id + 1
        }
        
        condition_keys = obtain_keys(object[[publication]][[study]][[data]]$condition,
                                     "condition")
        

        # Replace group, within, condition in data
        object[[publication]][[study]][[data]]$data = object[[publication]][[study]][[data]]$data %>% 
          replace_id_keys_in_data(., group_keys, "group") %>% 
          replace_id_keys_in_data(., within_keys, "within") %>% 
          replace_id_keys_in_data(., condition, "condition")
        
        # Add all tables
        dataset_overview = object[[publication]][[study]][[data]]$overview
        add_table(conn, dataset_overview, "dataset_overview")
        
        within = object[[publication]][[study]][[data]]$within
        add_table(conn, within, "within")
        
        condition = object[[publication]][[study]][[data]]$condition
        add_table(conn, condition, "condition")
        
        raw_data = object[[publication]][[study]][[data]]$data
        add_table(conn, raw_data, "data")
      }
    }
  }
}

# Informative Error messages: check publication code
# If publication is already present. Pull study and dataset ids
# Then add_function with publication code

# function to delete publication, study, or dataset
