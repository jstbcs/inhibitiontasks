# Test read in online form data 
library(dplyr)

# STEP 1: Manual work --------------------------------------------------------#

# 1.1 download entry from wordpress and read in as csv
#entry <- read.csv("C:/Users/Michael/Downloads/inhibition-data-base-2023-06-07(3).csv")
entry <- read.csv("C:/Users/Michael/OneDrive - UvA/RA_Mathematical_Psychology/inhibition-data-base-2023-06-28.csv")

# 1.2 If several entries appeared on that day: Extract entry of interest
# entry <- entry[1, ]

# 1.3 download the actual data file(s)
  # finding non-empty "Upload.data" entries
upload_columns <- which(grepl("Upload", colnames(entry)))
download_links <- entry %>%
  select(upload_columns[which(!is.na(entry[1, upload_columns]))])
  # based on the download_links data frame download all data sets by entering the
  # link in your browser and reading it into R (see manual for naming conventions)

# 1.4 create the publication code 
pub_code <- "tang_2022_dual"  # see naming conventions in manual 


# 1.5 delete all non-used columns 
not_all_na <- function(x) any(!is.na(x) & x!="")
entry <- entry %>%
  select(where(not_all_na))


# TODO: STEP 2: SANITY CHECKS ------------------------------------------------------# 

# 2.1 are deleted columns in line with the number of studies? 


# 2.2 are deleted columns in line with the number of groups?


# 2.3 are deleted columns in line with the number of tasks?


# STEP 3: AUTOMATICALLY CREATE LIST OBJECT -------------------------------------#

pub <- list()

# 3.1 PUBLICATION LEVEL: fill with relevant data, otherwise NA --#
pub$publication_table <- data.frame(
  authors = ifelse("Authors" %in% names(entry), entry$Authors, NA),
  conducted = ifelse("Year" %in% names(entry), entry$Year, NA), 
  added = Sys.Date(), 
  country = ifelse("Country" %in% names(entry), entry$Country, NA), 
  contact = ifelse("Email.for.contact" %in% names(entry), entry$Email.for.contact, NA),
  keywords = ifelse("Keywords" %in% names(entry), entry$Keywords, NA),
  APA_reference = entry$APA.reference, 
  publication_code = pub_code
)

# 3.2 STUDY LEVEL IF NUMBER OF STUDIES = 1 --#
if(entry$Number.of.studies == 1){
  
  # CREATE STUDY LEVEL LIST
  pub[[2]] <- list()
  names(pub)[2] <- "study1"
  
  # FILL study_table
  pub[[2]]$study_table <- data.frame(
    n_groups = entry$Number.of.groups,
    n_tasks = entry$Number.of.tasks, 
    comment = entry$Description
  )
  
  # FILL between_table 
    # if only 1 group: 
    if(pub[[2]]$study_table$n_groups == 1){
      
      # get info, otherwise put NA 
      mean_age_value <- ifelse("Mean.age" %in% colnames(entry), 
                               entry$Mean.age,
                               NA)
      percentage_fem_value <- ifelse("Percentage.female" %in% colnames(entry), 
                                        entry$Percentage.female,
                                        NA)
      group_description_value <- ifelse("Sample.description" %in% colnames(entry),
                                        entry$Sample.description, 
                                        "no within manipulation")
      
      # insert into between_table 
      pub[[2]]$between_table <- data.frame(
        between_name = 1,
        mean_age = mean_age_value,
        pecentage_fem = percentage_fem_value,
        n_members = NA,
        group_description = group_description_value
      )
      
    # if several groups  
    } else if (pub[[2]]$study_table$n_groups > 1){
      
      # get needed info of first group 
      mean_age_value <- ifelse("Mean.age.group.1" %in% colnames(entry), 
                               entry$Mean.age.group.1,
                               NA)
      percentage_fem_value <- ifelse("Percentage.female.group.1" %in% colnames(entry), 
                                     entry$Percentage.female.group.1,
                                     NA)
      group_description_value <- ifelse("Sample.description.of.group.1" %in% colnames(entry),
                                        entry$Sample.description.of.group.1, 
                                        NA)
      
      # intialize between_table with first group 
      pub[[2]]$between_table <- data.frame(
        between_name = 1,
        mean_age = mean_age_value,
        pecentage_female = percentage_fem_value,
        n_members = NA,
        group_description = group_description_value
      )
      
      # append one row for each following group 
      for(j in 1:pub[[2]]$study_table$n_groups){
        
        # get needed needed info 
        between_number <- paste("Between.value.of.group.", j, sep ="")
        mean_age_name <- paste("Mean.age.group.", j, sep = "")
        percentage_fem_name <- paste("Percentage.female.group.", j, sep =)
        group_description_name <- paste("Sample.description.of.group.", j, sep = "")
        
        mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                                 entry[1, mean_age_name], 
                                 NA)
        percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                       entry[1, percentage_fem_name],
                                       NA)
        group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                          entry[1, group_description_name] , 
                                          NA)
        
        # add entries 
        pub[[2]]$between_table$between_name[j] <- between_number
        pub[[2]]$between_table$mean_age[j] <- mean_age_value
        pub[[2]]$between_table$percentage_female[j] <- percentage_fem_value
        pub[[2]]$between_table$n_members[j] <- NA
        pub[[2]]$between_table$group_description[j] <- group_description_value
      }
    }
}

# 3.2 STUDY LEVEL IF NUMBER OF STUDIES > 1 --- #

if(entry$Number.of.studies > 1){
  # loop through each study
  for(i in 1:entry$Number.of.studies){
    
    # CREATE STUDY LEVEL LIST -----
    
    pub[[i+1]] <- list()
    names(pub)[i+1] <- paste("study", i, sep = "")
    
    # FILL study_table---------------
    
    # get required info 
    n_tasks_name <- paste("Number.of.tasks..STUDY.", i, sep = "")
    n_groups_name <- paste("Number.of.groups..STUDY.", i, sep = "")
    comment_name <- paste("Description.STUDY.", i, sep = "")
    
    # insert into study_table 
    pub[[i+1]]$study_table <- data.frame(
      n_groups = entry[, n_tasks_name], 
      n_tasks = entry[, n_groups_name],
      comment = entry[, comment_name]
    )
    
    # FILL between_table ------------
    
    # if 1 group in respective study 
    if(pub[[i+1]]$study_table$n_groups == 1){
      
      # get info, otherwise put NA
      mean_age_name <- paste("Mean.age..STUDY.", i, ".", sep = "")
      percentage_fem_name <- paste("Percentage.female..STUDY.", i, ".", sep = "")
      group_description_name <- paste("Sample.description..STUDY.", i, ".", sep = "")
      
      mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                               entry[1, mean_age_name], 
                               NA)
      percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                     entry[1, percentage_fem_name],
                                     NA)
      group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                        entry[1, group_description_name] , 
                                        NA)
      
      # fill in between_table
      pub[[i+1]]$between_table <- data.frame(
        between_name =  1,
        mean_age = mean_age_value,
        pecentage_female = percentage_fem_value,
        n_members = NA,
        group_description = group_description_value
      )
      
    # if several groups in respective study  
    } else if(pub[[i+1]]$study_table$n_groups > 1){
      
      # get needed info of first group
      between_number <- paste("Between.value.of.group.1..STUDY.",i, ".", sep = "")
      mean_age_name <- paste("Mean.age...group.1..STUDY.", i, ".", sep = "")
      percentage_fem_name <- paste("Percentage.female...group.1..STUDY.", i, ".", sep = "" )
      group_description_name <- paste("Sample.description.of.group.1...STUDY.", i, ".", sep = "")
      
      mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                               entry[1, mean_age_name], 
                               NA)
      percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                     entry[1, percentage_fem_name],
                                     NA)
      group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                        entry[1, group_description_name] , 
                                        NA)
      
      # initialize between_table with first group 
      pub[[i+1]]$between_table <- data.frame(
        between_name =  between_number,
        mean_age = mean_age_value,
        pecentage_female = percentage_fem_value,
        n_members = NA,
        group_description = group_description_value
      )
      
      # append one row for each following group 
      for(j in 1:pub[[i+1]]$study_table$n_groups){
        
        # get needed info of group j in study i 
        between_number <- paste("Between.value.of.group.", j, "..STUDY.",i, ".", sep = "")
        mean_age_name <- paste("Mean.age...group.", j, "..STUDY.", i, ".", sep = "")
        percentage_fem_name <- paste("Percentage.female...group.", j, "..STUDY.", i, sep = "" )
        group_description_name <- paste("Sample.description.of.group.", j, "...STUDY.", i, ".", sep = "")
        
        mean_age_value <- ifelse(mean_age_name %in% colnames(entry), 
                                 entry[1, mean_age_name], 
                                 NA)
        percentage_fem_value <- ifelse(percentage_fem_name %in% colnames(entry), 
                                       entry[1, percentage_fem_name],
                                       NA)
        group_description_value <- ifelse(group_description_name %in% colnames(entry),
                                          entry[1, group_description_name] , 
                                          NA)
        
        # add entry
        pub[[i+1]]$between_table$between_name[j] <- between_number
        pub[[i+1]]$between_table$mean_age[j] <- mean_age_value
        pub[[i+1]]$between_table$percentage_female[j] <- percentage_fem_value
        pub[[i+1]]$between_table$n_members[j] <- NA
        pub[[i+1]]$between_table$group_description[j] <- group_description_value
        
      }
    }
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
  
  
  
  
  
  
  
  
