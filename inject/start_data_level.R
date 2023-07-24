# create task_table, within_table, and dataset_table for data level
start_data_level <- function(pub, entry){
  
  # IF JUST ONE STUDY 
  if(entry$Number.of.studies == 1){
    # if just one 
    for(i in 1:entry$Number.of.inhibition.tasks){
      
      # create data list --------------------
      pub[[2]]$data <- list()
      names(pub[[2]])[i+2] <- paste("data", i, sep ="")
      
      # create task_table -------------------
      # get relevant column names
      if(entry$Number.of.inhibition.tasks == 1){  # if only one task in study
        task_name_value <- "Inhibition.task.type...STUDY.1"
        task_desc_name <- "Task.description...STUDY.1"
      } else {    # if more than one task in study 
        task_name_value <- paste("Inhibition.task.type...task.", i, sep="")
        task_desc_name <- paste("Task.description...task.", i, sep="")
      }
      
      task_desc_value <- ifelse(task_desc_name %in% colnames(entry),
                                entry[1, task_desc_name] , 
                                NA)
      
      pub[[2]][[i+2]]$task_table<- data.frame(
        task_name = entry[1, task_name_value],
        task_description = task_desc_value
      )
      
      if(entry[1, task_name_value] == "Other"){
        alternative_task_name <- ifelse(entry$Number.of.inhibition.tasks == 1, 
                                        paste("Alternative.task.type...STUDY.1"),
                                        paste("Alternative.task.type...task.", i, sep="")) #TODO: change
        
        alternative_name_value <- paste("Other:", entry[1, alternative_task_name])
        
        pub[[2]][[i+2]][[1]]$task_name <- alternative_name_value
      }
      
      # create within_table    ---------------
      # reference correct column
      if(entry$Number.of.inhibition.tasks == 1){  # if only one task in study
        within_manipulation_name <- "Within.subject.manipulation...STUDY.1"
      } else {    # if more than one task in study 
        within_manipulation_name <- paste("Within.subject.manipulation...task.", i, sep="")
      }
      
      # check if there was a within manipulation
      if(entry[1, within_manipulation_name] == "No"){
        
        pub[[2]][[i+2]]$within_table <- data.frame(
          within_name = NA, 
          within_description = "No within-subject manipulation"
        )
        
        # if within manipulation:
      } else if(entry[1, within_manipulation_name] == "Yes"){
        
        # set up data frame with first condition
        if(entry$Number.of.inhibition.tasks == 1){  # if only one task in study
          within_name <- "Within.value.of.condition.1"
          within_descr_name <- "Within.description..condition.1"
        } else {    # if more than one task in study 
          within_name <- paste("Within.value.of.condition.1...task.", i, sep="")
          within_descr_name <- paste("Within.description.condition.1...task.", i, sep="")
        }
        
        pub[[2]][[i+2]]$within_table <- data.frame(
          within_name = entry[1, within_name], 
          within_description = entry[1, within_descr_name]
        )
        
        # then loop over all remaining within conditions and append them to the df
        if(entry$Number.of.inhibition.tasks == 1){ # reference column name
          number_withincon_name <- "Number.of.within.conditions...STUDY.1"
        } else {
          number_withincon_name <- paste("Number.of.within.conditions...task.", i, sep="")
        }
        
        for(j in 2:entry[1, number_withincon_name]){
          if(entry$Number.of.inhibition.tasks == 1){ # reference column name
            within_name <- paste("Within.value.of.condition.", j, sep="")
            within_descr_name <- paste("Within.description..condition.", j, sep="")
          } else {
            within_name <- paste("Within.value.of.condition.", j, "...task.", i, sep="")
            within_descr_name <- paste("Within.description.condition.", j, "...task.", i, sep="")
          }
          
          pub[[2]][[i+2]][[2]][j,1] <-  entry[1, within_name]
          pub[[2]][[i+2]][[2]][j,2] <-  entry[1, within_descr_name]
        }
      }
      
      # create dataset_table -----------------
      # relevant column names
      if(entry$Number.of.inhibition.tasks == 1){
        data_excl_name <- "Data.exclusion.criteria...STUDY.1"
        fix_cross_name <- "Fixation.point...STUDY.1"
        time_limit_name <- "Time.limit.STUDY.1"
      } else {
        data_excl_name <- paste("Data.exclusion.criteria...task.", i, sep="")
        fix_cross_name <- paste("Fixation.point...task.", i, sep="")
        time_limit_name <- paste("Time.limit...task.", i, sep="")
      }
      
      # get input, else put NA 
      data_excl_value <- ifelse(data_excl_name %in% colnames(entry), 
                                entry[1, data_excl_name], 
                                NA)
      fix_cross_value <- ifelse(fix_cross_name %in% colnames(entry), 
                                entry[1, fix_cross_name], 
                                NA)
      time_limit_value <- ifelse(time_limit_name %in% colnames(entry),
                                 entry[1, time_limit_name],
                                 NA)
      
      # insert into dataset_table 
      pub[[2]][[i+2]]$dataset_table <- data.frame(
        data_excl = data_excl_value, 
        n_participants = NA, # computed later
        n_blocks = NA, # computed later
        n_trials = NA, # computed later
        neutral_trials= NA, # computed later
        fixation_cross = fix_cross_value,
        time_limit = time_limit_value
      )
      
    }
   
  # FOR ENTRIES WITH MORE THAN 1 STUDY     
  } else if (entry$Number.of.studies > 1){
    
    for(i in 1:entry$Number.of.studies){
      
      n_inhibition_tasks <- entry[1, paste("Number.of.inhibition.tasks...STUDY.",i, sep="")]
      
      # for each task in study i
      for(j in 1:n_inhibition_tasks){
        
        # create data list 
        pub[[i+1]]$data <- list()
        names(pub[[i+1]])[j+2] <- paste("data", j, sep ="")
        
        # create task_table -------------------
        
        # get relevant column names depending on n_tasks
        if(i == 1){  # column names for first study 
          task_name_value <- ifelse(n_inhibition_tasks == 1, 
                                    "Inhibition.task.type...STUDY.1",
                                    paste("Inhibition.task.type...STUDY.", i, "...task.", j, sep = ""))
          task_desc_name <-  ifelse(n_inhibition_tasks == 1, 
                                    "Task.description...STUDY.1",
                                    paste("Task.description...STUDY.", i, "...task.", j, sep = ""))
        } else {    # column names for studies 2-5
          task_name_value <- ifelse(n_inhibition_tasks == 1,
                                    paste("Inhibition.task.type..Study.", i, sep=""),
                                    paste("Inhibition.task.type..Study.", i, "...task.", j, sep=""))
          task_desc_name <- ifelse(n_inhibition_tasks == 1,
                                   paste("Task.description...STUDY.", i, sep=""),
                                   paste("Task.description...STUDY.", i, "...task.", j, sep=""))
        }
        
        # insert NA if no info task description submitted
        task_desc_value <- ifelse(task_desc_name %in% colnames(entry),
                                  entry[1, task_desc_name] , 
                                  NA)
        
        # fill in task_table
        pub[[i+1]][[j+2]]$task_table<- data.frame(
          task_name = entry[1, task_name_value],
          task_description = task_desc_value
        )
        
        # If task type is other, add info to task_name
        if(pub[[i+1]][[j+2]][[1]]$task_name == "Other"){
          if(i == 1){
            # TODO: test 
            alternative_task_name <- ifelse(n_inhibition_tasks == 1, 
                                            "Alternative.task.type...STUDY.1",
                                            paste("Alternative.task.type...Study.1...task.",j,sep=""))
            alternative_name_value <- paste("Other:", entry[1, alternative_task_name])
          } else{
            alternative_task_name <- ifelse(n_inhibition_tasks == 1, 
                                            paste("Alternative.task.type...study.",i,sep=""),
                                            paste("Alternative.task.type...Study.",i,"...task.",j,sep=""))
            alternative_name_value <- paste("Other:", entry[1, alternative_task_name])
          }
          
          
          pub[[i+1]][[j+2]][[1]]$task_name <- alternative_name_value
        }
        
        # create within_table -----------------
        # reference correct column
        if(n_inhibition_tasks == 1){  # if only one task in study
          within_manipulation_name <- paste("Within.subject.manipulation...STUDY.", i, sep = "")
        } else {    # if more than one task in study 
          within_manipulation_name <- paste("Within.subject.manipulation...STUDY.", i, "...task.", j, sep="")
        }
        
        # check if there was a within manipulation
        if(entry[1, within_manipulation_name] == "No"){  # if not
          
          pub[[i+1]][[j+2]]$within_table <- data.frame(
            within_name = NA, 
            within_description = "No within-subject manipulation"
          )
          
          # if within manipulation:
        } else if(entry[1, within_manipulation_name] == "Yes"){
          
          # set up data frame with first condition
          # reference correct column names
          if(i == 1){ # column names of study 1
            # TODO: test
            within_name <- ifelse(n_inhibition_tasks == 1,
                                  "Within.value.of.condition.1",
                                  paste("Within.value.of.condition.1...STUDY.",i,"...task.",j, sep=""))
            within_descr_name <- ifelse(n_inhibition_tasks == 1, 
                                        "Within.description..condition.1",
                                        paste("Within.description.condition.1...STUDY.",i, "...task.", j, sep=""))
          } else{ # column names of studies 2-5
            within_name <- ifelse(n_inhibition_tasks == 1,
                                  paste("Within.value.of.condition.1...STUDY.",i, sep=""),
                                  paste("Within.value.of.condition.1...STUDY.",i,"...task.",j, sep=""))
            within_descr_name <- ifelse(n_inhibition_tasks == 1, 
                                        paste("Within.description.condition.1...STUDY.",i, sep=""),
                                        paste("Within.description.condition.1..STUDY.",i, "...task.", j, sep=""))
          }
          
          
          pub[[i+1]][[j+2]]$within_table <- data.frame(
            within_name = entry[1, within_name], 
            within_description = entry[1, within_descr_name]
          )
          
          # then loop over all remaining within conditions and append them to the df
          if(n_inhibition_tasks == 1){ # reference column name
            number_withincon_name <- paste("Number.of.within.conditions...STUDY.", i, sep="")
          } else {
            number_withincon_name <- paste("Number.of.within.conditions...STUDY.", i, "...task.", j, sep="")
          }
          
          for(k in 2:entry[1, number_withincon_name]){
            # again, make sure to reference the right column names
            if(n_inhibition_tasks == 1){ # column names if just one task
              within_name <- ifelse(i == 1,
                                    paste("Within.value.of.condition.", k, sep=""),
                                    paste("Within.value.of.condition.", k, "...STUDY.", i, sep=""))
              within_descr_name <- ifelse(i == 1,
                                          paste("Within.description..condition.", k, sep=""),
                                          paste("Within.description.condition.", k, "...STUDY.", i, sep=""))
            } else{  # column names when several tasks
              within_name <- paste("Within.value.of.condition.", k, "...STUDY.", i, "...task.", j, sep="")
              within_descr_name <- paste("Within.description.condition.", k, "..STUDY.", i, "...task.", j, sep="")
            }
            
            pub[[i+1]][[j+2]][[2]][k,1] <-  entry[1, within_name]
            pub[[i+1]][[j+2]][[2]][k,2] <-  entry[1, within_descr_name]
       
          }
        }
        
        # create dataset_table -----------------
        # relevant column names
        if(n_inhibition_tasks == 1){
          data_excl_name <- paste("Data.exclusion.criteria...STUDY.", i, sep="")
          fix_cross_name <- paste("Fixation.point...STUDY.", i, sep="")
          time_limit_name <- paste("Time.limit.STUDY.1", i, sep="")
        } else {
          data_excl_name <- paste("Data.exclusion.criteria....STUDY.", i, "...task.", j, sep="")
          fix_cross_name <- paste("Fixation.point...STUDY.", i, "...task.", j, sep="")
          time_limit_name <- paste("Time.limit...STUDY.", i, "...task.", j, sep = "")
        }
        
        # get input, else put NA 
        data_excl_value <- ifelse(data_excl_name %in% colnames(entry), 
                                  entry[1, data_excl_name], 
                                  NA)
        fix_cross_value <- ifelse(fix_cross_name %in% colnames(entry), 
                                  entry[1, fix_cross_name], 
                                  NA)
        time_limit_value <- ifelse(time_limit_name %in% colnames(entry),
                                   entry[1, time_limit_name],
                                   NA)
        
        # insert into dataset_table 
        pub[[i+1]][[j+2]]$dataset_table <- data.frame(
          data_excl = data_excl_value, 
          n_participants = NA, # computed later
          n_blocks = NA, # computed later
          n_trials = NA, # computed later
          neutral_trials= NA, # computed later
          fixation_cross = fix_cross_value,
          time_limit = time_limit_value
        )
      }
    }
  }
  return(pub)
}