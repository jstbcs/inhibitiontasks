# create study level 

library(dplyr)

create_study_level <- function(pub, entry){
  # STUDY LEVEL IF NUMBER OF STUDIES = 1 --#
  if(entry$Number.of.studies == 1){
    
    # CREATE STUDY LEVEL LIST
    pub[[2]] <- list()
    names(pub)[2] <- "study1"
    
    # FILL study_table
    pub[[2]]$study_table <- data.frame(
      n_groups = entry$Number.of.groups,
      n_tasks = entry$Number.of.tasks, 
      comment = entry$Description
    )
    
    # FILL between_table 
    # if only 1 group: 
    if(pub[[2]]$study_table$n_groups == 1){
      
      # get info, otherwise put NA 
      mean_age_value <- ifelse("Mean.age" %in% colnames(entry), 
                               entry$Mean.age,
                               NA)
      percentage_fem_value <- ifelse("Percentage.female" %in% colnames(entry), 
                                     entry$Percentage.female,
                                     NA)
      group_description_value <- ifelse("Sample.description" %in% colnames(entry),
                                        entry$Sample.description, 
                                        "no within manipulation")
      
      # insert into between_table 
      pub[[2]]$between_table <- data.frame(
        between_name = 1,
        mean_age = mean_age_value,
        pecentage_fem = percentage_fem_value,
        n_members = NA,
        group_description = group_description_value
      )
      
      # if several groups  
    } else if (pub[[2]]$study_table$n_groups > 1){
      
      # get needed info of first group 
      mean_age_value <- ifelse("Mean.age.group.1" %in% colnames(entry), 
                               entry$Mean.age.group.1,
                               NA)
      percentage_fem_value <- ifelse("Percentage.female.group.1" %in% colnames(entry), 
                                     entry$Percentage.female.group.1,
                                     NA)
      group_description_value <- ifelse("Sample.description.of.group.1" %in% colnames(entry),
                                        entry$Sample.description.of.group.1, 
                                        NA)
      
      # intialize between_table with first group 
      pub[[2]]$between_table <- data.frame(
        between_name = 1,
        mean_age = mean_age_value,
        pecentage_female = percentage_fem_value,
        n_members = NA,
        group_description = group_description_value
      )
      
      # append one row for each following group 
      for(j in 1:pub[[2]]$study_table$n_groups){
        
        # get needed needed info 
        between_number <- paste("Between.value.of.group.", j, sep ="")
        mean_age_name <- paste("Mean.age.group.", j, sep = "")
        percentage_fem_name <- paste("Percentage.female.group.", j, sep =)
        group_description_name <- paste("Sample.description.of.group.", j, sep = "")
        
        mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                                 entry[1, mean_age_name], 
                                 NA)
        percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                       entry[1, percentage_fem_name],
                                       NA)
        group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                          entry[1, group_description_name] , 
                                          NA)
        
        # add entries 
        pub[[2]]$between_table$between_name[j] <- between_number
        pub[[2]]$between_table$mean_age[j] <- mean_age_value
        pub[[2]]$between_table$percentage_female[j] <- percentage_fem_value
        pub[[2]]$between_table$n_members[j] <- NA
        pub[[2]]$between_table$group_description[j] <- group_description_value
      }
    }
    
    # add empty data list for each task 
    for(i in 1:pub[[2]]$study_table$n_tasks){
      
      pub[[2]]$data <- list()
      names(pub[[2]])[i+2] <- paste("data", i, sep ="")
    }
    
  # STUDY LEVEL IF NUMBER OF STUDIES > 1 ------------------- #
    # TODO: test this
  } else if (entry$Number.of.studies > 1){
  
  
    # loop through each study
    for(i in 1:entry$Number.of.studies){
      
      # CREATE STUDY LEVEL LIST -----
      
      pub[[i+1]] <- list()
      names(pub)[i+1] <- paste("study", i, sep = "")
      
      # FILL study_table---------------
      
      # get required info 
      n_tasks_name <- paste("Number.of.tasks..STUDY.", i, sep = "")
      n_groups_name <- paste("Number.of.groups..STUDY.", i, sep = "")
      comment_name <- paste("Description.STUDY.", i, sep = "")
      
      # insert into study_table 
      pub[[i+1]]$study_table <- data.frame(
        n_groups = entry[, n_tasks_name], 
        n_tasks = entry[, n_groups_name],
        comment = entry[, comment_name]
      )
      
      # FILL between_table ------------
      
      # if 1 group in respective study 
      if(pub[[i+1]]$study_table$n_groups == 1){
        
        # get info, otherwise put NA
        mean_age_name <- paste("Mean.age..STUDY.", i, ".", sep = "")
        percentage_fem_name <- paste("Percentage.female..STUDY.", i, ".", sep = "")
        group_description_name <- paste("Sample.description..STUDY.", i, ".", sep = "")
        
        mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                                 entry[1, mean_age_name], 
                                 NA)
        percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                       entry[1, percentage_fem_name],
                                       NA)
        group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                          entry[1, group_description_name] , 
                                          NA)
        
        # fill in between_table
        pub[[i+1]]$between_table <- data.frame(
          between_name =  1,
          mean_age = mean_age_value,
          pecentage_female = percentage_fem_value,
          n_members = NA,
          group_description = group_description_value
        )
        
        # if several groups in respective study  
      } else if(pub[[i+1]]$study_table$n_groups > 1){
        
        # get needed info of first group
        between_number <- paste("Between.value.of.group.1..STUDY.",i, ".", sep = "")
        mean_age_name <- paste("Mean.age...group.1..STUDY.", i, ".", sep = "")
        percentage_fem_name <- paste("Percentage.female...group.1..STUDY.", i, ".", sep = "" )
        group_description_name <- paste("Sample.description.of.group.1...STUDY.", i, ".", sep = "")
        
        mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                                 entry[1, mean_age_name], 
                                 NA)
        percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                       entry[1, percentage_fem_name],
                                       NA)
        group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                          entry[1, group_description_name] , 
                                          NA)
        
        # initialize between_table with first group 
        pub[[i+1]]$between_table <- data.frame(
          between_name =  between_number,
          mean_age = mean_age_value,
          pecentage_female = percentage_fem_value,
          n_members = NA,
          group_description = group_description_value
        )
        
        # append one row for each following group 
        for(j in 1:pub[[i+1]]$study_table$n_groups){
          
          # get needed info of group j in study i 
          between_number <- paste("Between.value.of.group.", j, "..STUDY.",i, ".", sep = "")
          mean_age_name <- paste("Mean.age...group.", j, "..STUDY.", i, ".", sep = "")
          percentage_fem_name <- paste("Percentage.female...group.", j, "..STUDY.", i, sep = "" )
          group_description_name <- paste("Sample.description.of.group.", j, "...STUDY.", i, ".", sep = "")
          
          mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                                   entry[1, mean_age_name], 
                                   NA)
          percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                         entry[1, percentage_fem_name],
                                         NA)
          group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                            entry[1, group_description_name] , 
                                            NA)
          
          # add entry
          pub[[i+1]]$between_table$between_name[j] <- between_number
          pub[[i+1]]$between_table$mean_age[j] <- mean_age_value
          pub[[i+1]]$between_table$percentage_female[j] <- percentage_fem_value
          pub[[i+1]]$between_table$n_members[j] <- NA
          pub[[i+1]]$between_table$group_description[j] <- group_description_value
          
        }
      }
      
      # add empty data list for each task in study i
      for(k in 1:pub[[i+1]]$study_table$n_tasks){
        
        pub[[2]]$data <- list()
        names(pub[[2]])[k+2] <- paste("data", k, sep ="")
      }
    }
  }
  
  return(pub)
}