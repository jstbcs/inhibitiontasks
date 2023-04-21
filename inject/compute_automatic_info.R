# COMPUTING MISSING INFO FOR DATA BASE #--------------------------------------#

# Info computed in this script ---
# - for observation_table: 
#   - condition column
# - for dataset_table: 
#  - n_participants 
#  - n_blocks 
#  - n_trials 
#  - neutral_trials
# - for condition_table: 
#   - percentage_congr
#   - percentage_neut
#   - mean_obs_pp 
#   - n_obs


library(dplyr)

# For observation_table -----------------

# code condition column based on combination of within and between column
code_condition <- function(df){
  # create overview of conditions
  conditions <- df %>%
    count(within, between) %>%
    select(within, between) %>%
    mutate(condition = 1:nrow(.))
  
  # add respective condition value to observation
  data <- df %>% 
    left_join(conditions, by = join_by(between, within))
  
  return(data)
}
  
# For dataset_table --------------------------

# create data frame without trial blocks 
remove_practice <- function(df) {
  df_test <- df[df$block != -999, ]
  return(df_test)
}


get_n <- function(df_test){
  n <- length(unique(df_test$subject))
  return(n)
}

get_n_blocks <- function(df_test){
  n_blocks <- length(unique(df_test$block))
  return(n_blocks)
}

get_n_trials <- function(df_test){
  n_trials <- length(unique(df$trial))
  return(n_trials)
}

get_neutral_trials <- function(df_test){
  return(ifelse(3 %in% df_test$congruency, 1, 0))
}

# For condition_table -------------------

# filter data frame by condition
filter_condition <- function(df_test, cond = 1) {
  # inputs:
  # - df_test: data frame (without practice trials)
  # - cond: condition_id to filter by 
  
  df_cond <- df_test[df_test$condition == cond, ]
  return(df_cond)
}


# percentage congruent
get_perc_congr <- function(df_cond){
  perc_congr <- sum(df$congruency == 1) / length(df$congruency)
  return(round(perc_congr),2)
}

# percentage neutral
get_perc_neut <- function(df_cond){
  perc_neut <- sum(df$congruency == 3) / length(df$congruency)
  return(round(perc_neut),2)  
}

# mean_obs_pp
get_mean_obs_pp <- function(df_cond){
  mean_obs <- df_cond %>% 
    group_by(subject) %>%
    summarise(N = n()) %>%
    summarise(mean(N))
  
  return(round(mean_obs$`mean(N)`, 0))
}

# n_obs
get_n_obs <- function(df_cond){
  n_obs <- nrow(df_cond)
  
  return(n_obs)
}

