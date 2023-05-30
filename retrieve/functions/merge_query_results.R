merge_query_results <- function(query_results){
  # join the proper tables, return data frame
  data_raw = query_results$observation_table
  
  data_joined = data_raw %>% 
    left_join(., query_results$between_table) %>% 
    left_join(., query_results$within_table) %>% 
    left_join(., query_results$condition_table) %>% 
    left_join(., query_results$dataset_table) %>% 
    rename(comment_dataset = comment) %>% 
    left_join(., query_results$study_table, by = "study_id") %>% 
    rename(comment_study = comment) %>% 
    left_join(., query_results$publication_table, by = "publication_id") %>% 
    left_join(., query_results$task_table, by = "task_id")
    
  return(data_joined)
  # Maybe add functionality to compress this here
}
