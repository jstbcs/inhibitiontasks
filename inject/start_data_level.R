# create task_table, within_table, and dataset_table for data level
start_data_level <- function(pub, entry, n_studies = 1){
  
  # IF JUST ONE STUDY 
  if(n_studies == 1){
    # if just one 
    for(i in 1:entry$Number.of.inhibition.tasks){
      
      # create task_table -------------------
      # get relevant column names
      if(entry$Number.of.inhibition.tasks == 1){  # if only one task in study
        task_name_value <- "Inhibition.task.type...STUDY.1"
        task_desc_name <- "Inhibition.task.type...STUDY.1"
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
          
          pub[[2]][[i+2]]$within_table$within_name[j] <- entry[1, within_name]
          pub[[2]][[i+2]]$within_table$within_description[j] <- entry[1, within_descr_name]
        }
      }
      
      # create dataset_table -----------------
      # relevant column names
      if(entry$Number.of.inhibition.tasks == 1){
        data_excl_name <- "Data.exclusion.criteria...STUDY.1"
        fix_cross_name <- "Fixation.point...STUDY.1"
        #time_limit_name <- paste()
      } else {
        data_excl_name <- paste("Data.exclusion.criteria...task.", i, sep="")
        fix_cross_name <- paste("Fixation.point...task.", i, sep="")
        #time_limit_name <- paste()
      }
      
      # get input, else put NA 
      data_excl_value <- ifelse(data_excl_name %in% colnames(entry), 
                                entry[1, data_excl_name], 
                                NA)
      fix_cross_value <- ifelse(fix_cross_name %in% colnames(entry), 
                                entry[1, fix_cross_name], 
                                NA)
      #time_limit_value <- ifelse()
      
      # insert into dataset_table 
      pub[[2]][[i+2]]$dataset_table <- data.frame(
        data_excl = data_excl_value, 
        n_participants = NA, # computed later
        n_blocks = NA, # computed later
        n_trials = NA, # computed later
        neutral_trials= NA, # computed later
        fixation_cross = fix_cross_value
        # time_limit = NA
      )
      
    }
   
  # FOR ENTRIES WITH MORE THAN 1 STUDY     
  } else if (n_studies > 1){
    
    for(i in 1:entry$Number.of.studies){
      n_inhibition_tasks <- paste("Number.of.inhibition.tasks...STUDY.",i, sep="")
      
      # for each task in study i
      for(j in 1:n_inhibition_tasks){
        
        # create task_table -------------------
        # get relevant column names depending on n_tasks
        if(n_inhibition_tasks == 1){  # if only one task in study
          task_name_value <- paste("Inhibition.task.type...STUDY.", i, sep = "")
          task_desc_name <-  paste("Inhibition.task.type...STUDY.", i, sep = "")
        } else {    # if more than one task in study 
          task_name_value <- paste("Inhibition.task.type...STUDY.", i, "...task.", j, sep="")
          task_desc_name <- paste("Task.description...STUDY.", i, "...task.", j, sep="")
        }
        
        task_desc_value <- ifelse(task_desc_name %in% colnames(entry),
                                  entry[1, task_desc_name] , 
                                  NA)
        
        pub[[i+1]][[j+2]]$task_table<- data.frame(
          task_name = entry[1, task_name_value],
          task_description = task_desc_value
        )
        
        # create within_table -----------------
        # reference correct column
        if(n_inhibition_tasks == 1){  # if only one task in study
          within_manipulation_name <- paste("Within.subject.manipulation...STUDY.", i)
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
          within_name <- paste("Within.value.of.condition.1...STUDY.",i,"...task.",j, sep="")
          within_descr_name <- paste("Within.description.condition.1...STUDY.",i, "...task.", j, sep="")
          
          pub[[i+1]][[j+2]]$within_table <- data.frame(
            within_name = entry[1, within_name], 
            within_description = entry[1, within_descr_name]
          )
          
          # then loop over all remaining within conditions and append them to the df
          if(n_inhibition_tasks == 1){ # reference column name
            number_withincon_name <- paste("Number.of.within.conditions...STUDY.", i, sep="")
          } else {
            number_withincon_name <- paste("Number.of.within.conditions...STUDY.", i, "..task.", j, sep="")
          }
          
          for(k in 2:entry[1, number_withincon_name]){
            # again, make sure to reference the right column names
            if(n_inhibition_tasks == 1){ # column names if just one task
              within_name <- paste("Within.value.of.condition.", k, sep="")
              within_descr_name <- paste("Within.description..condition.", k, sep="")
            } else{  # column names when several tasks
              within_name <- paste("Within.value.of.condition.", k, "...STUDY.", i, "...task.", j, sep="")
              within_descr_name <- paste("Within.description.condition.", k, "...STUDY.", i, "...task.", j, sep="")
            }
            
            pub[[i+1]][[j+2]]$within_table$within_name[k] <- entry[1, within_name]
            pub[[i+1]][[j+2]]$within_table$within_description[k] <- entry[1, within_descr_name]
          }
        }
        
        # create dataset_table -----------------
        # relevant column names
        if(n_inhibition_tasks == 1){
          data_excl_name <- paste("Data.exclusion.criteria...STUDY.", i, sep="")
          fix_cross_name <- paste("Fixation.point...STUDY.", i, sep="")
          #time_limit_name <- paste()
        } else {
          data_excl_name <- paste("Data.exclusion.criteria....STUDY.", i, "...task.", j, sep="")
          fix_cross_name <- paste("Fixation.point...STUDY.", i, "...task.", j, sep="")
          #time_limit_name <- paste()
        }
        
        # get input, else put NA 
        data_excl_value <- ifelse(data_excl_name %in% colnames(entry), 
                                  entry[1, data_excl_name], 
                                  NA)
        fix_cross_value <- ifelse(fix_cross_name %in% colnames(entry), 
                                  entry[1, fix_cross_name], 
                                  NA)
        #time_limit_value <- ifelse()
        
        # insert into dataset_table 
        pub[[i+1]][[j+2]]$dataset_table <- data.frame(
          data_excl = data_excl_value, 
          n_participants = NA, # computed later
          n_blocks = NA, # computed later
          n_trials = NA, # computed later
          neutral_trials= NA, # computed later
          fixation_cross = fix_cross_value
          # time_limit = NA
        )
      }
    }
  }
  return(pub)
}