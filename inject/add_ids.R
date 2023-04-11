# These functions take an object and add the db ids to the respective tables
# Need to find the IDs to make sure that tables on different levels can get them

# Publication
add_ids <- function(conn, object){
  pub_names = which_elements_match(names(object), regex_matches_publication_names)
  
  for (publication in pub_names){
    pub_code = object[[publication]]$publication_info$publication_code
    if (does_publication_code_exist(conn, pub_code) == TRUE){
      stop("Implement this. Cross check all other existances with the db. Or just go ahead and add nonetheless.
         But how do you prevent duplicates in this case?")
    }
    
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
      
      group_id = find_next_free_id(conn, "group")
      
      # This adds the global group-id to the group_id table
      for (row in seq_along(object[[publication]][[study]]$group_info)){
        object[[publication]][[study]]$group_info$id[row] = group_id
        group_id = group_id + 1
      }
      
      # TODO: Replace the group-ids in the dataset with the correct group-ids
      
      # Then add the group table
      group_info = object[[publication]][[study]]$group_info
      
      # Now moving to dataset
      data_names = which_elements_match(names(object[[publication]][[study]]), regex_matches_data_names)
      
      for (data in data_names){
        # Add task and get the task id
        # TODO:
        
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
        for (row in seq_along(object[[publication]][[study]][[data]]$within)){
          object[[publication]][[study]][[data]]$within$within_id[row] = within_id
          within_id = within_id + 1
        }
        
        # TODO: Replace within-id in dataset
        
        # Add condition
        condition_id = find_next_free_id(conn, "condition")
        for (row in seq_along(object[[publication]][[study]][[data]]$condition)){
          object[[publication]][[study]][[data]]$condition$condition_id[row] = condition_id
          condition_id = condition_id + 1
        }
        
        # TODO: Do correct within and group id in condition
        
        # TODO: Replace group, within, condition in data
        
        # Add all tables
        dataset_overview = object[[publication]][[study]][[data]]$overview
        add_table(conn, dataset_overview, "dataset_overview")
        
        within = object[[publication]][[study]][[data]]$within
        add_table(conn, within, "within")
        
        condition = object[[publication]][[study]][[data]]$condition
        add_table(conn, condition, "condition")
        
        data = object[[publication]][[study]][[data]]$data
        add_table(conn, data, "data")
      }
    }
  }
 
  
  return(object_with_ids)
}