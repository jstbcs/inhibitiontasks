# CREATE NESTED LIST OBJECT TO READ IN PRATTE ET AL DATA INTO DB ###############

# NOTE: when running all scripts in "\add_data" to replicate/ reconstruct the 
# first 40 datasets added to the db, we recommend first running "reformat_datasets.R"
# to have all raw datasets loaded when constructing the nested list objects 

source("./inject/compute_automatic_info.R")
source("./Create_db/add_data/scripts_creating_list_objects/00_create_publication_study_level.R")

# Load required info from excel file -------------------------------------------
publication_df <- readxl::read_excel("./add_data/Book.xlsx", "publication_table", range = "A1:I2") 
study_df <- readxl::read_excel("./add_data/Book.xlsx", "study_table", range = "A1:D3")
study_df$n_data <- c(2,2) # encode number of data sets per study (by hand)
group_df <- readxl::read_excel("./add_data/Book.xlsx", "group_table", range = "A1:G3")
task_df <- readxl::read_excel("./add_data/Book.xlsx", "task", range = "A1:D5")
dataset_df <- readxl::read_excel("./add_data/Book.xlsx", "dataset_overview_table", range = "A1:K5")
within_df <- readxl::read_excel("./add_data/Book.xlsx", "within_table", range = "A1:D5")
condition_df <- readxl::read_excel("./add_data/Book.xlsx", "condition_descriptives", range = "A1:F5")

# NOTE: read in dataset2, dataset3, dataset8, and dataset9 from "reformat_datasets.R"

# create publication and study level -------------------------------------------
pub <- list_study_level(publication_df, study_df, group_df)

# create data level ------------------------------------------------------------ 
data_added <- 0 # keep track of datasets already added
for(i in 1:nrow(study_df)){ # within each study
  
  for(k in 1:study_df$n_data[i]){ # loop over each dataset
    # CREATE DATA LIST
    pub[[i+1]][[k+2]] <- list()
    names(pub[[i+1]])[k+2] <-paste("data", k, sep = "")
    
    # assign respective raw data as observation_table and add condition column
    df <- eval(parse(text = dataset_df$`dataset in R`[k + data_added]))
    pub[[i+1]][[k+2]]$observation_table <- code_condition(df)
    
    # add dataset_table
    pub[[i+1]][[k+2]]$dataset_table <- data.frame(
      data_excl = dataset_df$data_excl[k + data_added],
      n_participants = dataset_df$n_participants[k + data_added], 
      n_blocks = dataset_df$n_blocks[k + data_added], 
      n_trials = dataset_df$n_trials[k + data_added], 
      neutral_trials = dataset_df$neutral_trials[k + data_added], 
      fixation_cross = dataset_df$fixaction_cross[k + data_added], 
      time_limit = dataset_df$time_limit[k + data_added], 
      github = dataset_df$github[k + data_added], 
      comment = NA
    )
    
    # add within_table
    pub[[i+1]][[k+2]]$within_table <- data.frame(
      within_name = within_df$within_id[k + data_added], 
      within_description = within_df$within_desciption[k + data_added]
    )
    
    # add task_table
    pub[[i+1]][[k+2]]$task_table <- data.frame(
      task_name = task_df$task[k + data_added], 
      task_description = task_df$task_description[k + data_added]
    )
    
    # add condition table
    # create df containing observations of respective condition only
    df_test <- remove_practice(pub[[i+1]][[k+2]]$observation_table) # remove practice trials
    df_cond <- filter_condition(df_test, cond = 1)  # filter by condition
    
    pub[[i+1]][[k+2]]$condition_table <- data.frame(
      condition_name = 1, 
      percentage_congruent = get_perc_congr(df_cond), 
      percentage_neutral = get_perc_neut(df_cond), 
      mean_obs_per_participant = get_mean_obs_pp(df_cond), 
      n_obs = get_n_obs(df_cond)
    )
    
    # if more than 1 condition: add rows for each condition
    if(length(unique(pub[[i+1]][[k+2]]$observation_table$condition)) > 1){
      for(condition in 2:length(unique(pub[[i+1]][[k+2]]$observation_table$condition))){
        # create df containing observations of respective condition only
        df_t <- remove_practice(pub[[i+1]][[k+2]]$observation_table) # remove practice trials
        df_con <- filter_condition(df_t, cond = condition)  # filter by condition
        
        # calculate info
        perc_congr <- get_perc_congr(df_con)
        perc_neut <- get_perc_neut(df_con)
        mean_obs_pp <- get_mean_obs_pp(df_con)
        n_obs <- get_n_obs(df_con)
        
        # extend condition table
        pub[[i+1]][[k+2]]$condition_table[condition, ] <- c(condition, 
                                                            perc_congr, perc_neut, 
                                                            mean_obs_pp, n_obs)
      }
    }
    
  }
  data_added <- data_added + study_df$n_data[i] # keep track of datasets added
}


# save list object -------------------------------------------------------------
saveRDS(pub, file="pratte_list.RData")
