# Test read in online form data 
library(dplyr)
source("./inject/compute_automatic_info.R")

# STEP 1: Manual work --------------------------------------------------------#

# 1.1 download entry from wordpress and read in as csv
#entry <- read.csv("C:/Users/Michael/Downloads/inhibition-data-base-2023-06-07(3).csv")
entry <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/inhibition-data-base-2023-06-28.csv")

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

pub <- list()

# 3.1 PUBLICATION LEVEL: fill with relevant data, otherwise NA --#
pub$publication_table <- data.frame(
  authors = ifelse("Authors" %in% names(entry), entry$Authors, NA),
  conducted = ifelse("Year" %in% names(entry), entry$Year, NA), 
  added = Sys.Date(), 
  country = ifelse("Country" %in% names(entry), entry$Country, NA), 
  contact = ifelse("Email.for.contact" %in% names(entry), entry$Email.for.contact, NA),
  keywords = ifelse("Keywords" %in% names(entry), entry$Keywords, NA),
  APA_reference = entry$APA.reference, 
  publication_code = pub_code
)

# 3.2 STUDY LEVEL IF NUMBER OF STUDIES = 1 --#
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
}

# 3.2 STUDY LEVEL IF NUMBER OF STUDIES > 1 --- #
# TODO: test this
if(entry$Number.of.studies > 1){
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



# 3.3 DATA LEVEL IF 1 STUDY

# for each inhibition task 
for(i in 1:entry$Number.of.inhibition.tasks){
  
  # TODO: same thing but for column names when just one task in only study
  
  # create task_table ----------
  # get relevant column names
  task_name_value <- paste("Inhibition.task.type...task.", i, sep="")
  task_desc_name <- paste("Task.description...task.", i, sep="")
  
  task_desc_value <- ifelse(task_desc_name %in% colnames(entry),
                            entry[1, task_desc_name] , 
                            NA)
  
  pub[[2]][[i+2]]$task_table<- data.frame(
    task_name = entry[1, task_name_value],
    task_description = task_desc_value
  )
  
  # create within_table    --------
  # if within no manipulation for task i: just fill in details
  within_manipulation_name <- paste("Within.subject.manipulation...task.", i, sep="")
  
  if(entry[1, within_manipulation_name] == "No"){

    pub[[2]][[i+2]]$within_table <- data.frame(
      within_name = NA, 
      within_description = "No within-subject manipulation"
    )
    
  # if within manipulation:
  } else if(entry[1, within_manipulation_name] == "Yes"){
    
    # set up data frame with first condition
    within_name <- paste("Within.value.of.condition.1...task.", i, sep="")
    within_descr_name <- paste("Within.description.condition.1...task.", i, sep="")
    
    pub[[2]][[i+2]]$within_table <- data.frame(
      within_name = entry[1, within_name], 
      within_description = entry[1, within_descr_name]
    )
    
    # then loop over all remaining conditions and append them to the df
    number_withincon_name <- paste("Number.of.within.conditions...task.", i, sep="")
    
    for(j in 2:entry[1, number_withincon_name]){
      within_name <- paste("Within.value.of.condition.", j, "...task.", i, sep="")
      within_descr_name <- paste("Within.description.condition.", j, "...task.", i, sep="")
      
      pub[[2]][[i+2]]$within_table$within_name[j] <- entry[1, within_name]
      pub[[2]][[i+2]]$within_table$within_description[j] <- entry[1, within_descr_name]
    }
  }
  
  # create dataset_table -----------------
  # relevant column names 
  data_excl_name <- paste("Data.exclusion.criteria...task.", i, sep="")
  fix_cross_name <- paste("Fixation.point...task.", i, sep="")
  #time_limit_name <- paste()
  
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
  observations_name <- paste("raw_data_study1_task",i, sep="")
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
  # for each task in study i
  for(j in 1:entry$Number.of.tasks){
    
    # create task_table 
  
    # create within_table 
    
    # create dataset_table 
    
    # create observations_table 
  }
  
  
  
}
# we need info about how many data frames a study has 
# loop over each data frame
# add overview and fill in the blanks 
# add task info 
# add within_info 
# add raw data frame --> code condition column based on group and within!
  
  
  
  
  
  
