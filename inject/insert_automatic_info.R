# ASSIGNING AUTOMATICALLY COMPUTED INFORMATION TO COLUMNS IN NESTED LIST ELEMENT

source("./inject/compute_automatic_info.R")
source("./inject/helper_functions.R")

add_missing_info <- function(entry_list) {
  # loop over each publication in entry_list object 
  for(i in 1:length(names(entry_list))) {
    # count number of studies in publication i (regrex patterns)
    n_studies <- sum(str_detect(names(entry_list[[i]]), regex_matches_study_names))
    
    # loop over each study 
    for(j in 1:n_studies) {
      # count number of data lists in study
      n_data <- sum(str_detect(names(entry_list[[i]][[j+1]]), regex_matches_data_names))
      
      # loop over each data list in study 
      for(k in 1:n_data) { 
        # code condition column for data frame 
        #entry_list[[i]][[j+1]][[k+2]]$observation_table$condition <- 
        
        # remove practice trials 
        df_test <- remove_practice(entry_list[[i]][[j+1]][[k+2]]$observation_table)
        
        # fill in missing info for dataset_table
        entry_list[[i]][[j+1]][[k+2]]$dataset_table$n_participants <- get_n(df_test)
        entry_list[[i]][[j+1]][[k+2]]$dataset_table$n_blocks <- get_n_blocks(df_test)
        entry_list[[i]][[j+1]][[k+2]]$dataset_table$n_trials <- get_n_trials(df_test)
        entry_list[[i]][[j+1]][[k+2]]$dataset_table$neutral_trials <- get_neutral_trials(df_test)
        
        # count number of unique conditions 
        n_conditions <- sum(unique(entry_list[[i]][[j+1]][[k+2]]$observation_table$condition))
        
        # loop over each condition
        for(cond in 1:n_conditions){
          # create data frame containing only observations of respective condition
          df_cond <- filter_condition(entry_list[[i]][[j+1]][[k+2]]$observation_table, cond = cond)
          
          # fill in missing info for condition_table
          entry_list[[i]][[j+1]][[k+2]]$condition_table$percentage_congruent[cond] <- get_per_congr(df_cond)
          entry_list[[i]][[j+1]][[k+2]]$condition_table$percentage_neutral[cond] <- get_perc_neutral(df_cond)
          entry_list[[i]][[j+1]][[k+2]]$condition_table$mean_obs_per_participant[cond] <- get_mean_obs(df_cond)
          entry_list[[i]][[j+1]][[k+2]]$condition_table$n_obs[cond] <- get_n_obs(df_cond)
          
        }
      }
    }
  }
}






