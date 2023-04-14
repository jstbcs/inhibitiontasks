# COMPUTING MISSING INFO FOR DATA BASE #--------------------------------------#

# Overview: Info computed in this script ---
# - for overview_info: 
#  - n_participants 
#  - n_blocks 
#  - n_trials 
#  - neutral_trials
# - for conditions: 
#   - percentage_congr
#   - percentage_neut
#   - mean_obs_pp 
#   - n_obs


library(dplyr)


# For overview_info --------------------------

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
  return(ifelse(3 %in% df_test$congr, 1, 0))
}


# For condition_descriptives -------------------

# TODO: function to split data frame (df_test!) based on condition column
# e.g. dataset6_test <- dataset6[dataset6$within == 1, ] # change this 


# percentage congruent
get_perc_congr <- function(df_cond){
  perc_congr <- sum(df$congr == 1) / length(df$congr)
  return(round(perc_congr),2)
}

# percentage neutral
get_perc_neut <- function(df_cond){
  perc_neut <- sum(df$congr == 3) / length(df$congr)
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

