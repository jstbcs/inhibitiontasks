# FUNCTIONS TO CREATE NESTED LIST OBJECTS TO INSERT INITIAL DATA INTO DB ######

# Note: This function creates the publictaion and study level of nested list 
# objects of first 40 data sets which were inserted into the data base
# It can and be re-used to replicate/ reconstruct these data sets 
source("/inject/compute_automatic_info.R")

list_study_level <- function(publication_df,
                               study_df,
                               group_df){
  
  # create nested list object ---------------------------
  pub <- list()
  
  # PUBLICATION LEVEL-------------------------------------
  pub$publication_table <- data.frame(
    authors = publication_df$authors, 
    conducted = publication_df$conducted, 
    added = Sys.Date(), 
    country = publication_df$country, 
    contact = publication_df$contact, 
    keywords = publication_df$keywords, 
    apa_reference = publication_df$`APA-reference`, 
    publication_code = publication_df$publication_code 
  )
  
  # STUDY LEVEL --------------------------------------------
  recorded_groups <- 0 # keep track of how many groups are added already
  
  for(i in 1:nrow(study_df)){
    # CREATE A STUDY LIST
    pub[[i+1]] <- list()
    names(pub)[i+1] <-paste("study", i, sep = "")
    
    # study_table
    pub[[i+1]]$study_table <- data.frame(
      n_groups = study_df$n_groups[i], 
      n_tasks = study_df$n_tasks[i],
      comment = study_df$comment[i]
    )
    
    # group_info table
    pub[[i+1]]$between_table <- data.frame(
      between_name = 1,
      mean_age = group_df$mean_age[1 + recorded_groups],
      percentage_female = group_df$percentage_female[1 + recorded_groups],
      n_members = group_df$n_members[1 + recorded_groups],
      group_description = group_df$group_description[1 + recorded_groups]
    )
    
    # if study has 1 group and no description given: automatically fill in description
    if(pub[[i+1]]$study_table$n_groups == 1 && is.na(group_df$group_description[i])){
      pub[[i+1]]$between_table$group_description <- "all participants in same group"
    }
    
    # if study has > 1 group: add info in additional rows
    if(study_df$n_groups[i] > 1){
      
      # loop over each group and add respective info
      for(j in 2:study_df$n_groups[i]){
        pub[[i+1]]$between_table[j,] <- c(group_df$between_id[j + recorded_groups], 
                                          group_df$mean_age[j + recorded_groups],
                                          group_df$percentage_female[j + recorded_groups],
                                          group_df$n_members[j + recorded_groups],
                                          group_df$group_description[j + recorded_groups])
      }
    }
    recorded_groups <- recorded_groups + nrow(pub[[i+1]]$between_table)
  }
  return(pub)
}



