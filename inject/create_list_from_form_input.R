# Test read in online form data 
library(dplyr)

entry <- read.csv("C:/Users/Michael/Downloads/inhibition-data-base-2023-06-07.csv")

# 1. delete all non-used columns (with some sanity checks)
not_all_na <- function(x) any(!is.na(x))
entry <- entry %>%
  select(where(not_all_na))
# TODO: add sanity checks 
#   - deleted columns in line with N_studies, N groups and N tasks?
#   - required info present?

# 2. Manual work -------------------------------------------------------------#
#   - create publication code 
pub_code <- "tang_2022_dual"
# TODO: (see if function could do this)
#   - search and fill in missing relevant data 

# 2. create list object  ------------------------------------------------------#

# PUBLICATION LEVEL ---------#
pub <- list()

pub$publication_table <- data.frame(
  authors = entry$Authors, 
  conducted = entry$Year, 
  added = Sys.Date(), 
  country = entry$Country, 
  contact = entry$Email.for.contact, 
  keywords = entry$Keywords, 
  APA_reference = entry$APA.reference, 
  publication_code = pub_code
)

# STUDY LEVEL ---------------#
# for each study: 
for (i in 1:entry$Number.of.studies){
  # CREATE A STUDY LIST
  pub[[i+1]] <- list()
  names(pub)[i+1] <-paste("study", i, sep = "")
  
  # FILL WITH RESPECTIVE group_info, study_info AND data list
  
  # specify object names (not very elegant; we can change this later)
  # if only one study: object names are "Number.of.groups", "Number.of.tasks" (i.e., without numbers)
  if(entry$Number.of.studies == 1){
    n_tasks_name <-   
    
  }
  
  
  
  
  
  n_tasks_name <- paste("study", i, "_n_tasks", sep = "")
  n_groups_name <- paste("study", i, "_n_groups", sep = "")
  comment_name <- paste("study", i, "_comment", sep = "")
  group_number_name <- paste("study", i, "_group1_number", sep = "") # default for group 1
  group_descr_name <- paste("study", i, "_group1_descr", sep = "")
  group_meanage_name <- paste("study", i, "_group1_meanage", sep = "")
  group_percfem_name <- paste("study", i, "_group1_percfem", sep = "")
  group_members_name <- paste("study", i, "_group1_members", sep = "")
  
  
  # study_info table
  pub[[i+1]]$study_table <- data.frame(
    n_groups = eval(parse(text = n_groups_name)), 
    n_tasks = eval(parse(text = n_tasks_name)),
    comment = eval(parse(text = comment_name))
  )
  
  
  # group_info table 
  pub[[i+1]]$group_table <- data.frame(
    group =  eval(parse(text = group_number_name)),
    mean_age = eval(parse(text = group_meanage_name)),
    pecentage_female = eval(parse(text = group_percfem_name)),
    n_members = eval(parse(text = group_members_name)),
    group_description = eval(parse(text = group_descr_name))
  )
  
  # if just one group and no description given: automatically fill in description
  if(pub[[i+1]]$study_table$n_groups == 1 && is.na(eval(parse(text = group_descr_name)))){
    pub[[i+1]]$group_table$group_description <- "all participants in same group"
  }
  
  # if more than one group: add info in additional rows
  if(pub[[i+1]]$study_table$n_groups > 1) {
    for(j in 2:pub[[i+1]]$study_table$n_groups) {
      # update object names of respective group j
      current_group_number_name <- paste("study", i, "_group",j, "_number", sep = "") # default for group 1
      current_group_descr_name <- paste("study", i, "_group", j, "_descr", sep = "")
      current_group_meanage_name <- paste("study", i, "_group", j, "_meanage", sep = "")
      current_group_percfem_name <- paste("study", i, "_group", j, "_percfem", sep = "")
      current_group_members_name <- paste("study", i, "_group", j, "_members", sep = "")
      
      # add info to respective row in group_info
      pub[[i+1]]$group_table$group[j] <- eval(parse(text = current_group_number_name))
      pub[[i+1]]$group_table$mean_age[j] <- eval(parse(text = current_group_meanage_name))
      pub[[i+1]]$group_table$percentage_female[j] <- eval(parse(text = current_percfem_number_name))
      pub[[i+1]]$group_table$n_members[j] <- eval(parse(text = current_group_members_name))
      pub[[i+1]]$group_table$group_description[j] <- eval(parse(text = current_group_descr_name))
    }
    
  }
  
  # add empty data list 
  pub[[i+1]]$data <- list()
  
}   # end study level
