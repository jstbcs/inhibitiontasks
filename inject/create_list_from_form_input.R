# Test read in online form data 
library(dplyr)
library(xlsx)
source("./inject/compute_automatic_info.R")
source("./inject/create_publication_level.R")
source("./inject/create_study_level.R")
source("./inject/start_data_level.R")
source("./inject/source_testing_scripts.R")

# STEP 1: Manual work --------------------------------------------------------#

# 1.1 download entry from wordpress and read in as csv
#entry <- read.csv("C:/Users/Michael/Downloads/inhibition-data-base-2023-06-07(3).csv")
entry <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/inhibition-data-base-2023-07-05.csv")
# or 
# entry <- read.xlsx("~filepath")

# 1.2 If several entries appeared on that day: Extract entry of interest
# entry <- entry[1, ]

# 1.3 download the actual data file(s)
  # finding non-empty "Upload.data" entries
upload_columns <- which(grepl("Upload", colnames(entry)))
download_links <- entry %>%
  select(upload_columns[which(!is.na(entry[1, upload_columns]))])
  # based on the download_links data frame download all data sets by entering the
  # link in your browser and reading it into R (see manual for naming conventions)
processed_data_study1 <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/Online_form/test_observation_tables/dataset1.csv")
processed_data_study1 <- processed_data_study1[,2:10]
processed_data_study2_task1 <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/Online_form/test_observation_tables/dataset2.csv")
processed_data_study2_task1 <- processed_data_study2_task1[,2:10]
processed_data_study2_task2 <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/Online_form/test_observation_tables/dataset4.csv")
processed_data_study2_task2 <- processed_data_study2_task2[,2:10]
processed_data_study3_task1 <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/Online_form/test_observation_tables/dataset49.csv")
processed_data_study3_task1 <- processed_data_study3_task1[,2:10]
processed_data_study3_task2 <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/Online_form/test_observation_tables/dataset42.csv")
processed_data_study3_task2 <- processed_data_study3_task2[,2:10]

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
    observations_name <- "processed_data_study1"
  } else {
    observations_name <- paste("processed_data_study1_task",i, sep="")
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
  n_inhibition_tasks <- entry[1, n_inhibition_tasks]
  
  # for each task in study i
  for(j in 1:n_inhibition_tasks){
    
    # create observations_table ------------------------
    if(n_inhibition_tasks == 1){
      observations_name <- paste("processed_data_study",i, sep="")
    } else {
      observations_name <- paste("processed_data_study",i,"_task",j, sep="")
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
        pub[[i+1]][[j+2]]$condition_table[n, ] <- c(n, perc_congr, perc_neutral, 
                                                  mean_obs_pp, n_obs)
      }
    }
    
  }
}


# STEP 4: AUTOMATIC CHECKS --------------------------------------------------#




















  
  
  
  
  
