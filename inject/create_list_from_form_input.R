# Test read in online form data 
library(dplyr)

# STEP 1: Manual work --------------------------------------------------------#

# download entry from wordpress and read in as csv
entry <- read.csv("C:/Users/Michael/Downloads/inhibition-data-base-2023-06-07.csv")

# download the actual data file(s)
download_link <- entry$Upload.data
download_link 
# TODO: add links if more than 1 study

# 1. delete all non-used columns (with some sanity checks)
not_all_na <- function(x) any(!is.na(x))
entry <- entry %>%
  select(where(not_all_na))
# TODO: add sanity checks 
#   - deleted columns in line with N_studies, N groups and N tasks?
#   - required info present?

# 2. Manual work -------------------------------------------------------------#
#   - create publication code 
pub_code <- "tang_2022_dual" # TODO: (see if function could do this)
#   - search and fill in missing relevant data 
#   - download data files

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

# CREATE STUDY LEVEL IF ONLY 1 STUDY
if(entry$Number.of.studies == 1){
  # CREATE A STUDY LIST 
  pub[[2]] <- list()
  names(pub)[2] <- "study1"
  
  # FILL WITH RESPECTIVE study_info
  
  # retrieve respective info about study of current loop
  n_tasks_value <- entry$Number.of.tasks
  n_groups_value <- entry$Number.of.groups
  comment_value <- entry$Description
  
  # fill study_info table
  pub[[2]]$study_table <- data.frame(
    n_groups = n_tasks_value, 
    n_tasks = n_groups_value,
    comment = comment_value
  )
  
  # FILL WITH group_info
  # if only 1 group in study: 
  if(n_groups_value == 1){
    
    # either retrieve or compute needed info 
    mean_age_value <- ifelse("Mean.age" %in% colnames(entry), 
                             entry$Mean.age,
                             NA)
    percentage_female_value <- ifelse("Percentage.female" %in% colnames(entry), 
                                      entry$Percentage.female,
                                      NA)
    # TODO: n_members <- function_to_compute_n_members
    group_description_value <- ifelse("Sample.description" %in% colnames(entry), 
                                entry$Sample.description,
                                NA)
    
    # insert in group_info table 
    pub[[2]]$group_table <- data.frame(
      group = 1,
      mean_age = mean_age_value,
      pecentage_female = percentage_female_value,
     # n_members = n_members_value,
      group_description = group_description_value
    )
    
    # if more than 1 group
  } else {
    
    # either retrieve or compute needed info 
    #TODO: continue here
    
    # create group table with first group
    pub[[2]]$group_table <- data.frame(
      group = 1,
      mean_age = mean_age_value,
      pecentage_female = percentage_female_value,
      # n_members = n_members_value,
      group_description = group_description_value
    )
    
    
    
    # loop over 
    for(j in 2:n_groups_value)
    
    
    
  }
  
  
  
}







# STUDY LEVEL ---------------#
for(i in 1:entry$Number.of.studies){
  # CREATE A STUDY LIST 
  pub[[i+1]] <- list()
  names(pub)[i+1] <-paste("study", i, sep = "")
  
  # FILL WITH RESPECTIVE study_info
  
  # retrieve respective info about study of current loop
    # if only 1 study: names contain no numbers 
    if(entry$Number.of.studies == 1){
      n_tasks_value <- entry$Number.of.tasks
      n_groups_value <- entry$Number.of.groups
      comment_value <- entry$Description
    } else {
    # if more than 1 study: 
      n_tasks_name <- paste("Number.of.tasks..STUDY.", i, sep = "")
      n_tasks_value <- entry[, n_tasks_name]
      n_groups_name <- paste("Number.of.groups..STUDY.", i, sep = "")
      n_groups_value <- entry[, n_groups_name]
      comment_name <- paste("Description.STUDY.", i, sep = "")
      comment_value <- entry[, comment_name]
    }
  
  # fill study_info table
  pub[[i+1]]$study_table <- data.frame(
    n_groups = n_tasks_value, 
    n_tasks = n_groups_value,
    comment = comment_value
  )
  
  # FILL WITH group_info 
  
  # retrieve info about groups of study in current loop
  if(pub[[i+1]]$study_table$n_groups == 1){
    
  }
  
  # create group_info table 
  pub[[i+1]]$group_table <- data.frame(
    group =  eval(parse(text = group_number_name)),
    mean_age = eval(parse(text = group_meanage_name)),
    pecentage_female = eval(parse(text = group_percfem_name)),
    n_members = eval(parse(text = group_members_name)),
    group_description = eval(parse(text = group_descr_name))
  )
  
}
  
  
  
  
  
  
  
  
