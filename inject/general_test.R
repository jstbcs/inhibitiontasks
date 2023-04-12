# GENERAL FUNCTION TO TEST STRUCTURE OF INSERTED DATA 

source("./inject/helper_functions.R")
source("./inject/test_publication.R")
source("./inject/test_study.R")
source("./inject/test_data.R")
library(tidyverse)


# this function takes nested list element 'entry_list' and checks entire structure

check_overall_structure <- function(entry_list){
  # check publication level 
  check_publication_level_structure(entry_list$publication)
  
  # loop over each study element and test structure on study level
  for(study in publication) {
    check_study_level_structure(entry_list$study)
    
    # loop over each data element within each study element
    for(i in study) {
      # if element is data element
      if(str_detect(names(study$i), regex_matches_data_names)){
        check_data_level_structure(entry_list$study$i)
      }
    }
  }
}




# create test element ---
## publication
publication <- list()
publication$study1 <- list()
publication$study2 <- list()

## study 1 (has data1 and data2)
publication$study1$study_info <- data.frame(n_groups = 1, n_tasks = 2, comment = "bla")
publication$study1$group_info <- data.frame(mean_age = 1, percentage_female = 2, 
                                            n_participants = 100, group_description = "bla")
### study 1 data 1
publication$study1$data1$task_info <- data.frame(task = "stroop", task_description = "bla")
publication$study1$data1$dataset_overview_info <- data.frame(data_exclusion = 1, 
                                                       fixation_cross = 2, 
                                                       time_limit = 3, 
                                                       github = "https", 
                                                       comment = "bla")
publication$study1$data1$within_info <- data.frame(within_id = 2, within_description = "bla")
publication$study1$data1$condition_info <- data.frame(percentage_congruent = 0.5, percentage_neutral = 0,
                                                n_obs = 7000, mean_obs_per_participant = 70)
publication$study1$data1$data <- dataset35 # from clean_and_reformat_new_data

### study 1 data 2
publication$study1$data2$task_info <- data.frame(task = "stroop", task_description = "bla")
publication$study1$data2$dataset_overview_info <- data.frame(data_exclusion = 1, 
                                                             fixation_cross = 2, 
                                                             time_limit = 3, 
                                                             github = "https", 
                                                             comment = "bla")
publication$study1$data2$within_info <- data.frame(within_id = 2, within_description = "bla")
publication$study1$data2$condition_info <- data.frame(percentage_congruent = 0.5, percentage_neutral = 0,
                                                      n_obs = 7000, mean_obs_per_participant = 70)
publication$study1$data2$data <- dataset42 # from clean_and_reformat_new_data

## study 2 (has data 1)
publication$study2$study_info <- data.frame(n_groups = 1, n_tasks = 2, comment = "bla")
publication$study2$group_info <- data.frame(mean_age = 1, percentage_female = 2, 
                                            n_participants = 100, group_description = "bla")
### study 2 data 1 
publication$study2$data1$task_info <- data.frame(task = "stroop", task_description = "bla")
publication$study2$data1$dataset_overview_info <- data.frame(data_exclusion = 1, 
                                                             fixation_cross = 2, 
                                                             time_limit = 3, 
                                                             github = "https", 
                                                             comment = "bla")
publication$study2$data1$within_info <- data.frame(within_id = 2, within_description = "bla")
publication$study2$data1$condition_info <- data.frame(percentage_congruent = 0.5, percentage_neutral = 0,
                                                      n_obs = 7000, mean_obs_per_participant = 70)
publication$study2$data1$data <- dataset43 # from clean_and_reformat_new_data


# ------------- test function
check_overall_structure(publication)











