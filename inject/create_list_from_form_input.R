# Test read in online form data 
library(dplyr)
source("./inject/compute_automatic_info.R")
source("./inject/create_publication_level.R")
source("./inject/create_study_level.R")
source("./inject/start_data_level.R")
source("./inject/source_testing_scripts.R")

# STEP 1: Manual work --------------------------------------------------------#

# 1.1 download entry from wordpress and read in as csv
#entry <- read.csv("C:/Users/Michael/Downloads/inhibition-data-base-2023-06-07(3).csv")
entry <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/inhibition-data-base-2023-07-05.csv")

# 1.2 If several entries appeared on that day: Extract entry of interest
# entry <- entry[1, ]

# 1.3 download the actual data file(s)
  # finding non-empty "Upload.data" entries
upload_columns <- which(grepl("Upload", colnames(entry)))
download_links <- entry %>%
  select(upload_columns[which(!is.na(entry[1, upload_columns]))])
  # based on the download_links data frame download all data sets by entering the
  # link in your browser and reading it into R (see manual for naming conventions)

# 1.4 create the publication code 
pub_code <- "tang_2022_dual"  # see naming conventions in manual 


# 1.5 delete all non-used columns 
not_all_na <- function(x) any(!is.na(x) & x!="")
entry <- entry %>%
  select(where(not_all_na))


# TODO: STEP 2: SANITY CHECKS ------------------------------------------------------# 

# 2.1 are deleted columns in line with the number of studies? 


# 2.2 are deleted columns in line with the number of groups?


# 2.3 are deleted columns in line with the number of tasks?


# STEP 3: AUTOMATICALLY CREATE LIST OBJECT -------------------------------------#

# 3.1 CREATE PUBLICATION LEVEL 
pub <- create_pub_level(entry)

# 3.2 ADD STUDY LEVEL
pub <- create_study_level(pub, entry)

# 3.3 START DATA LEVEL 
# creates task_table, within_table, and dataset_table 
pub <- start_data_level(pub, entry, n_studies = 3) # Note: adjust number of studies 

# 3.4 MANUALLY COMPLETE DATA LEVEL 
# NOTE. before running the following loops, make sure to load all datasets 
# and name them according to conventions (see manual)

# RUN THIS IF JUST ONE STUDY WAS SUBMITTED 
for(i in 1:entry$Number.of.inhibition.tasks){
  
  # create observations_table  -----------------
  if(entry$Number.of.inhibition.tasks == 1){
    observations_name <- "raw_data_study1"
  } else {
    observations_name <- paste("raw_data_study1_task",i, sep="")
  }
  
  pub[[2]][[i+2]]$observations_table <- eval(parse(text = observations_name))
  
  # add condition column to dataset 
  # (unique combinations of between and within column values)
  pub[[2]][[i+2]]$observations_table <- code_condition(pub[[2]][[i+2]]$observations_table)
  
  
  # Compute automatic info for dataset_table --------------
  
  # create data frame without trial blocks 
  df_test <- remove_practice(pub[[2]][[i+2]]$observations_table)
  # get info for dataset_table
  pub[[2]][[i+2]]$dataset_table$n_participants <- get_n(df_test)
  pub[[2]][[i+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
  pub[[2]][[i+2]]$dataset_table$n_trials <- get_n_trials(df_test)
  pub[[2]][[i+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
  
  # create condition_table --------------------------
  
  # initiate with first condition
  # get required info
  df_condition1 <- filter_condition(df_test, cond = 1) # get data of condition 1
  perc_congr <- get_perc_congr(df_condition1)
  perc_neutral <- get_perc_neut(df_condition1)
  mean_obs_pp <- get_mean_obs_pp(df_condition1)
  n_obs <- get_n_obs(df_condition1)
  
  # fill table
  pub[[2]][[i+2]]$condition_table <- data.frame(
    condition_name = 1, 
    percentage_congruent = perc_congr, 
    percentage_neutral = perc_neutral, 
    mean_obs_per_participant = mean_obs_pp,
    n_obs = n_obs
  )
  
  # if more than 1 condition: fill in following ones
  if(length(unique(df_test$condition)) > 1){
    for(k in 2:length(unique(df_test$condition))){
      # get required info
      df_condition <- filter_condition(df_test, cond = k) # get data of condition k
      perc_congr <- get_perc_congr(df_condition)
      perc_neutral <- get_perc_neut(df_condition)
      mean_obs_pp <- get_mean_obs_pp(df_condition)
      n_obs <- get_n_obs(df_condition)
      
      # append to condition_table
      pub[[2]][[i+2]]$condition_table[k, ] <- c(k, perc_congr, perc_neutral, 
                                                mean_obs_pp, n_obs)
    }
  }
}

# RUN THIS IF SEVERAL STUDIES WERE SUBMITTED AT ONCE
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
    
    # create observations_table ------------------------
    if(n_inhibition_tasks == 1){
      observations_name <- paste("raw_data_study",i, sep="")
    } else {
      observations_name <- paste("raw_data_study",i,"_task",j, sep="")
    }
    
    pub[[i+1]][[j+2]]$observations_table <- eval(parse(text = observations_name))
    
    # add condition column to dataset 
    # (unique combinations of between and within column values)
    pub[[i+1]][[j+2]]$observations_table <- code_condition(pub[[i+1]][[j+2]]$observations_table)
    
    # Compute automatic info for dataset_table --------------
    
    # create data frame without trial blocks 
    df_test <- remove_practice(pub[[i+1]][[j+2]]$observations_table)
    # get info for dataset_table
    pub[[i+1]][[j+2]]$dataset_table$n_participants <- get_n(df_test)
    pub[[i+1]][[j+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
    pub[[i+1]][[j+2]]$dataset_table$n_trials <- get_n_trials(df_test)
    pub[[i+1]][[j+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
    
    # create condition_table --------------------------
    
    # initiate with first condition
    # get required info
    df_condition1 <- filter_condition(df_test, cond = 1) # get data of condition 1
    perc_congr <- get_perc_congr(df_condition1)
    perc_neutral <- get_perc_neut(df_condition1)
    mean_obs_pp <- get_mean_obs_pp(df_condition1)
    n_obs <- get_n_obs(df_condition1)
    
    # fill table
    pub[[i+1]][[j+2]]$condition_table <- data.frame(
      condition_name = 1, 
      percentage_congruent = perc_congr, 
      percentage_neutral = perc_neutral, 
      mean_obs_per_participant = mean_obs_pp,
      n_obs = n_obs
    )
    
    # if more than 1 condition: fill in following ones
    if(length(unique(df_test$condition)) > 1){
      for(n in 2:length(unique(df_test$condition))){
        # get required info
        df_condition <- filter_condition(df_test, cond = n) # get data of condition k
        perc_congr <- get_perc_congr(df_condition)
        perc_neutral <- get_perc_neut(df_condition)
        mean_obs_pp <- get_mean_obs_pp(df_condition)
        n_obs <- get_n_obs(df_condition)
        
        # append to condition_table
        pub[[2]][[i+2]]$condition_table[n, ] <- c(n, perc_congr, perc_neutral, 
                                                  mean_obs_pp, n_obs)
      }
    }
    
  }
}


# STEP 4: AUTOMATIC CHECKS --------------------------------------------------#
























# ORIGINAL CODE ------------------------------
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
  
  # create observations_table  -----------------
  if(entry$Number.of.inhibition.tasks == 1){
    observations_name <- "raw_data_study1"
  } else {
    observations_name <- paste("raw_data_study1_task",i, sep="")
  }
  
  pub[[2]][[i+2]]$observations_table <- eval(parse(text = observations_name))
  
  # add condition column to dataset 
  # (unique combinations of between and within column values)
  pub[[2]][[i+2]]$observations_table <- code_condition(pub[[2]][[i+2]]$observations_table)
  
  
  # Compute automatic info for dataset_table --------------
  
  # create data frame without trial blocks 
  df_test <- remove_practice(pub[[2]][[i+2]]$observations_table)
  # get info for dataset_table
  pub[[2]][[i+2]]$dataset_table$n_participants <- get_n(df_test)
  pub[[2]][[i+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
  pub[[2]][[i+2]]$dataset_table$n_trials <- get_n_trials(df_test)
  pub[[2]][[i+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
  
  # create condition_table --------------------------
  
  # initiate with first condition
  # get required info
  df_condition1 <- filter_condition(df_test, cond = 1) # get data of condition 1
  perc_congr <- get_perc_congr(df_condition1)
  perc_neutral <- get_perc_neut(df_condition1)
  mean_obs_pp <- get_mean_obs_pp(df_condition1)
  n_obs <- get_n_obs(df_condition1)
  
  # fill table
  pub[[2]][[i+2]]$condition_table <- data.frame(
    condition_name = 1, 
    percentage_congruent = perc_congr, 
    percentage_neutral = perc_neutral, 
    mean_obs_per_participant = mean_obs_pp,
    n_obs = n_obs
  )
  
  # if more than 1 condition: fill in following ones
  if(length(unique(df_test$condition)) > 1){
    for(k in 2:length(unique(df_test$condition))){
      # get required info
      df_condition <- filter_condition(df_test, cond = k) # get data of condition k
      perc_congr <- get_perc_congr(df_condition)
      perc_neutral <- get_perc_neut(df_condition)
      mean_obs_pp <- get_mean_obs_pp(df_condition)
      n_obs <- get_n_obs(df_condition)
      
      # append to condition_table
      pub[[2]][[i+2]]$condition_table[k, ] <- c(k, perc_congr, perc_neutral, 
                                                mean_obs_pp, n_obs)
    }
  }
}

# 3.4 DATA LEVEL IF >1 STUDIES

# for each study
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
    
    # create observations_table ------------------------
    if(n_inhibition_tasks == 1){
      observations_name <- paste("raw_data_study",i, sep="")
    } else {
      observations_name <- paste("raw_data_study",i,"_task",j, sep="")
    }
    
    pub[[i+1]][[j+2]]$observations_table <- eval(parse(text = observations_name))
    
    # add condition column to dataset 
    # (unique combinations of between and within column values)
    pub[[i+1]][[j+2]]$observations_table <- code_condition(pub[[i+1]][[j+2]]$observations_table)
    
    # Compute automatic info for dataset_table --------------
    
    # create data frame without trial blocks 
    df_test <- remove_practice(pub[[i+1]][[j+2]]$observations_table)
    # get info for dataset_table
    pub[[i+1]][[j+2]]$dataset_table$n_participants <- get_n(df_test)
    pub[[i+1]][[j+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
    pub[[i+1]][[j+2]]$dataset_table$n_trials <- get_n_trials(df_test)
    pub[[i+1]][[j+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
    
    # create condition_table --------------------------
    
    # initiate with first condition
    # get required info
    df_condition1 <- filter_condition(df_test, cond = 1) # get data of condition 1
    perc_congr <- get_perc_congr(df_condition1)
    perc_neutral <- get_perc_neut(df_condition1)
    mean_obs_pp <- get_mean_obs_pp(df_condition1)
    n_obs <- get_n_obs(df_condition1)
    
    # fill table
    pub[[i+1]][[j+2]]$condition_table <- data.frame(
      condition_name = 1, 
      percentage_congruent = perc_congr, 
      percentage_neutral = perc_neutral, 
      mean_obs_per_participant = mean_obs_pp,
      n_obs = n_obs
    )
    
    # if more than 1 condition: fill in following ones
    if(length(unique(df_test$condition)) > 1){
      for(n in 2:length(unique(df_test$condition))){
        # get required info
        df_condition <- filter_condition(df_test, cond = n) # get data of condition k
        perc_congr <- get_perc_congr(df_condition)
        perc_neutral <- get_perc_neut(df_condition)
        mean_obs_pp <- get_mean_obs_pp(df_condition)
        n_obs <- get_n_obs(df_condition)
        
        # append to condition_table
        pub[[2]][[i+2]]$condition_table[n, ] <- c(n, perc_congr, perc_neutral, 
                                                  mean_obs_pp, n_obs)
      }
    }
    
  }
}
  
  
  
  
  
