# ASSIGNING AUTOMATICALLY COMPUTED INFORMATION TO COLUMNS IN NESTED LIST ELEMENT

source("./inject/compute_automatic_info.R")

add_missing_info <- function(list) {
  
}

# loop over studies; 
  # loop over each data element: 
    # remove practice trials: 
    # df_test <- remove_practice(df)
    # for overview_info: 
      # overview_info$n_participants <- get_n(df_test)
      # overview_info$n_blocks <- get_n_blocks(df_test)
      # overview_info$n_trials <- get_n_trials(df_test)
      # overview_info$neutral_trials <- get_neutral_trials(df_test)
    # for condition_info: loop over each value of condition in df
      # create seperate df containing only respective condition (df_cond)
      # condition_info$percentage_congruent <- get_per_congr(df_cond)
      # condition_info$percentage_neutral <- get_perc_neutral(df_cond)
      # condition_info$mean_obs_pp <- get_mean_obs(df_cond)
      # condition_info$n_obs <- get_n_obs(df_cond)





  
# data_entry <- list(
#   # top level
#   publication = list(
#     # publication level
#     study1 = list(
#       # study level
#       study_info,
#       group_info,
#       data1 = list(
#         # Data level
#         task = task_info,
#         overview = dataset_overview_info,
#         data = data_table,
#         within = within_info,
#         condition  = condition_descr_info
#       ),
#       data2 = list(
#         task = task_name,
#         overview = dataset_overview,
#         data = data_table,
#         within = within_description,
#         condition = condition_descr_info
#       )
#     ),
#     study2 = list(
#       study_info,
#       group_info,
#       data1 = list(
#         task = task_name,
#         overview = dataset_overview,
#         data = data_table,
#         within = within_description,
#         condition  = condition_descr_info
#       )
#     )
#   )
# )