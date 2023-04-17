# Step 1: User gives inputs (here: example of Pratte data) ---------------------
# - dataset (df)
study1_task1_df <- dataset8
study1_task2_df <- dataset2
study2_task1_df <- dataset9
study2_task2_df <- dataset3
# - infos about publication
authors <- "Michael S. Pratte, Jeffrey N. Rouder, Richard D. Morey, Chuning Feng"
conducted <- 2010
country <- "US"
contact <- "xy@gmail.com"
keywords <- "congruency effect, stroop task, simon effect, stroop effect, simon task"
APA_ref <- "blabla"
pub_code <- "pratte_2010_exploring"
# - number of studies 
n_studies <- 2 # specifies how many studies the publication consists of (recorded automatically)
# - study comment
study1_comment <- "Task battery with simon task (identifying color of red or greeen squares presented on right/ left side) followed by stroop task (identifying color of color words)"
study2_comment <- "Task battery with alternating blocks of stroop task (identify location of word; either left or right) and simon task (identify semantic meaning of target word; either LEFT or RIGHT). Target words where LEFT and RIGHT and appeared on either right or left side of fixation poin"
# - n_tasks (study level)
study1_n_tasks <- 2
study2_n_tasks <- 2
# - n_groups (study level)
study1_n_groups <- 1
study2_n_groups <- 1
# - task type (data level)
study1_task1_task <- "simon"
study1_task2_task <- "stroop"
study2_task1_task <- "simon"
study2_task2_task <- "stroop"
# - task description (data level)
study1_task1_description <- "Participants had to identify the color of squares (red/green) appearing either right or left of the fixation point. 'green'' responses were given by pressing key with right hand; 'red' with left hand"
study1_task2_description <- "Classical color stroop task with manual response (pressing keys) rather than verbal response"
study2_task1_description <- "Participants had to identify semantic meaning of target words (LEFT and RIGHT) which appeared on left or right side of the fixation point; responses were given by pressing left and right key respectively; "
study2_task2_description <- "Spatial stroop task where participants had to identify the location of target word presentation (left or right); irrelevant stimulus was the semantic meaning of target word (left or right); responses were given by pressing left or right key "


# - optional: within description of each within number

# - optional: group number, group description, mean age, percentage female & n members of each group
study1_group1_number <- 1
study2_group1_number <- 1
study1_group1_descr <- NA
study2_group1_descr <- NA
study1_group1_meanage <- NA
study2_group1_meanage <- NA
study1_group1_percfem <- NA
study2_group1_percfem <- NA
study1_group1_members <- 38
study2_group1_members <- 38

# - optional: data exclusion (data level)
study1_task1_data_excl <- "no info"
study1_task2_data_excl <- "no info"
study2_task1_data_excl <- "no info"
study2_task2_data_excl <- "no info"
# - optional: fixaction_cross (data level)
study1_task1_fix_cross <- "cross"
study1_task2_fix_cross <- "cross"
study2_task1_fix_cross <- "cross"
study2_task2_fix_cross <- "cross"
# - optional: time_limit (data level)	
study1_task1_time_limit <- "none"
study1_task2_time_limit <- "none"
study2_task1_time_limit <- "none"
study2_task2_time_limit <- "none"
# - optional: github (data level)
study1_task1_github <- "https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi2.dat.txt"
study1_task2_github <- "https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi2.dat.txt"
study2_task1_github <- "https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi7.dat.txt"
study2_task2_github <- "https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi7.dat.txt"




# Step 2: We create nested list objects -----------------------------------------

# PUBLICATION LEVEL  --------------------
pub <- list()

pub$publication_info <- data.frame(
  authors = authors, 
  conducted = conducted, 
  added = Sys.Date(), 
  country = country, 
  contact = contact, 
  keywords = keywords, 
  APA_reference = APA_ref, 
  publication_code = pub_code
)

# STUDY LEVEL --------------------------------------------
# for each study: 
for (i in 1:n_studies){
  # CREATE A STUDY LIST
  pub[[i+1]] <- list()
  names(pub)[i+1] <-paste("study", i, sep = "")
  
  # FILL WITH RESPECTIVE group_info, study_info AND data list
  
  # specify object names (not very elegant; we can change this later)
  n_tasks_name <- paste("study", i, "_n_tasks", sep = "")
  n_groups_name <- paste("study", i, "_n_groups", sep = "")
  comment_name <- paste("study", i, "_comment", sep = "")
  group_number_name <- paste("study", i, "_group1_number", sep = "") # default for group 1
  group_descr_name <- paste("study", i, "_group1_descr", sep = "")
  group_meanage_name <- paste("study", i, "_group1_meanage", sep = "")
  group_percfem_name <- paste("study", i, "_group1_percfem", sep = "")
  group_members_name <- paste("study", i, "_group1_members", sep = "")
  
  
  # study_info table
  pub[[i+1]]$study_info <- data.frame(
    n_groups = eval(parse(text = n_groups_name)), 
    n_tasks = eval(parse(text = n_tasks_name)),
    comment = eval(parse(text = comment_name))
  )

  
  # group_info table 
  pub[[i+1]]$group_info <- data.frame(
    group =  eval(parse(text = group_number_name)),
    mean_age = eval(parse(text = group_meanage_name)),
    pecentage_female = eval(parse(text = group_percfem_name)),
    n_members = eval(parse(text = group_members_name)),
    group_description = eval(parse(text = group_descr_name))
  )
  
  # if just one group and no description given: automatically fill in description
  if(pub[[i+1]]$study_info$n_groups == 1 && is.na(eval(parse(text = group_descr_name)))){
    pub[[i+1]]$group_info$group_description <- "all participants in same group"
  }
  
  # if more than one group: add info in additional rows
  if(pub[[i+1]]$study_info$n_groups > 1) {
    for(j in 2:pub[[i+1]]$study_info$n_groups) {
      # update object names of respective group j
      current_group_number_name <- paste("study", i, "_group",j, "_number", sep = "") # default for group 1
      current_group_descr_name <- paste("study", i, "_group", j, "_descr", sep = "")
      current_group_meanage_name <- paste("study", i, "_group", j, "_meanage", sep = "")
      current_group_percfem_name <- paste("study", i, "_group", j, "_percfem", sep = "")
      current_group_members_name <- paste("study", i, "_group", j, "_members", sep = "")
      
      # add info to respective row in group_info
      pub[[i+1]]$group_info$group[j] <- eval(parse(text = current_group_number_name))
      pub[[i+1]]$group_info$mean_age[j] <- eval(parse(text = current_group_meanage_name))
      pub[[i+1]]$group_info$percentage_female[j] <- eval(parse(text = current_percfem_number_name))
      pub[[i+1]]$group_info$n_members[j] <- eval(parse(text = current_group_members_name))
      pub[[i+1]]$group_info$group_description[j] <- eval(parse(text = current_group_descr_name))
    }
    
  }

  # add empty data list 
  pub[[i+1]]$data <- list()
  
}   # end study level

# DATA LEVEL --------------------------------------------
# note: insert this into the study loop 

# we need info about how many data frames a study has 
# loop over each data frame
# add overview and fill in the blanks 
# add task info 
# add within_info 
# add raw data frame --> code condition column based on group and within!
















# > here we also already code the condition column in the dataset (based on
# within und group column)

