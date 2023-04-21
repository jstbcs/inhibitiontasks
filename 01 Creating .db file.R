# Load packages
library(dplyr)
library(tidyr)

########## Read in and format the datasets ########## 

# Dataset 1 (Von Bastian et al.) Stroop task 
dataset1 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/vonbastian_2015_evidence/LEF_stroop.csv", sep = ";") %>%
  mutate(congruency = ifelse(congruency == "congruent", 1, ifelse(congruency == "incongruent", 2, ifelse(congruency == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         datasetid = 1,
         subject = as.factor(ID),
         between = NA,
         within = NA,
         block = 1,
         rt = RT/1000) # rt data in seconds
ntrial <- length(dataset1 [dataset1$ID == dataset1$ID[1], 1])
nsub <- length(unique(dataset1$ID))
dataset1$trial <- rep(1:ntrial, nsub) # add subject and trial numbers
dataset1 <- dataset1 %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  

# Dataset 2 (Pratte et al.); color stroop
dataset2 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi2.dat.txt", sep = " ")
colnames(dataset2) <- c("exp", "subject", "blk", "trial", "color", "distract", "cond", "resp", "accuracy", "rt", "errorTotal", "unused")
dataset2 <- dataset2 %>% filter(exp == 1) %>% # keep Stroop task data
  mutate(datasetid = 2,
         block = blk+1,
         trial = trial+1,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         between = NA,
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 3 (Pratte et al.); spatial Stroop
dataset3 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi7.dat.txt", sep = " ")
colnames(dataset3) <- c("subject","blk","blktype","trial","word","location","cond","resp","accuracy","rt","errorTotal", "unused")
dataset3 <- dataset3 %>% filter(blktype == 1) %>% # keep Stroop task data
  mutate(datasetid = 3,
         block = (blk+2)/2,
         trial = trial,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         between = NA, 
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 4 (Rey-Mermet et al.); numStroop
dataset4 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/mermet_2018_should/numStroop.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset4 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset4 <- left_join(dataset4, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 4,
         subject =  sub - 100, 
         subject = as.factor(subject),
         accuracy = acc,
         between = ageGroup,        
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 5 (Rey-Mermet et al.); colStroop
dataset5 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/mermet_2018_should/colStroop.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset5 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset5 <- left_join(dataset5, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congr),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 5,
         subject = as.factor(sub - 100),
         accuracy = acc,
         between = ageGroup,
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 6 (Hedge et al.); Stroop task study 1
study <- 1:2
idx <- list(matrix(c(rep(rep(c(1:5, 7:16, 18:36, 38:50), each = 2),2), # no data of participants 6, 17 and 37 (no second session)
                     rep(c(1,2), 94), rep(c("Stroop", "Flanker"), each = 94)), ncol = 3),
            matrix(c(rep(rep(c(1:27, 29:55, 57:62), each = 2),2),  # no data of participants 28 and 56 (no second session)
                     rep(c(1,2), 120), rep(c("Stroop", "Flanker"), each = 120)), ncol = 3))
urls <- list(vector(), vector())
hedge <- list(list(), list())
for(i in 1:length(study)){
  for(j in 1:nrow(idx[[i]])){
    urls[[i]][j] <- paste("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/hedge_2018_reliability/Study", paste(study[i]), "-",
                          idx[[i]][j,3], "/Study", paste(study[i]), "_P", idx[[i]][j,1], idx[[i]][j,3], idx[[i]][j,2], ".csv", sep = "")
    hedge[[i]][[j]] <- read.csv(urls[[i]][j]) %>% mutate(subject = paste(idx[[i]][j,1]),
                                                         session = paste(idx[[i]][j,2]),
                                                         study = paste(study[i]))
    colnames(hedge[[i]][[j]]) <- c("block", "trial", "direction", "cond", "accuracy", "rt", "participant", "session", "study")
  }
}

# combine data of study 1
hedge_data1 <- bind_rows(hedge[[1]]) %>%
  mutate(congruency = ifelse(cond == 0, 1, ifelse(cond == 2, 2, ifelse(cond == 1, 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(session == 1, block, block + 5),
         between = NA,
         within = as.numeric(session),
         subject = as.factor(as.numeric(study)*100 + as.numeric(participant))) # add subject numbers
dataset6 <- hedge_data1 %>% filter(direction == 0) %>% # keep Stroop task data of study 1
  mutate(datasetid = 6) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 7 (Von Bastian et al.); simon task 
dataset7 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/vonbastian_2015_evidence/LEF_simon.csv", sep = ";") %>%
  mutate(congruency = ifelse(congruency == "congruent", 1, ifelse(congruency == "incongruent", 2, ifelse(congruency == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         datasetid = 7,
         block = 1,
         subject = as.factor(ID),
         between = NA, 
         within = NA,
         rt = RT/1000) # rt data in seconds
ntrial <- length(dataset7[dataset7$ID == dataset7$ID[1], 1])
nsub <- length(unique(dataset7$ID))
dataset7$trial <- rep(1:ntrial, nsub) # add subject and trial numbers
dataset7 <- dataset7 %>% select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 8 (Pratte et al.); classic simon task
dataset8 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi2.dat.txt", sep = " ")
colnames(dataset8) <- c("exp", "subject", "blk", "trial", "color", "distract", "cond", "resp", "accuracy", "rt", "errorTotal", "unused")
dataset8 <- dataset8 %>% filter(exp == 0) %>% # keep classic Simon task data
  mutate(datasetid = 8,
         block = blk+1,
         trial = trial,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         group = NA, 
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 9 (Pratte et al.); lateral simon task
dataset9 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/pratte_2010_exploring/allsi7.dat.txt", sep = " ")
colnames(dataset9) <- c("subject","blk","blktype","trial","word","location","cond","resp","accuracy","rt","errorTotal", "unused")
dataset9 <- dataset9 %>% filter(blktype == 0) %>% # keep lateral Simon task data
  mutate(datasetid = 9,
         block = (blk+1)/2,
         trial = trial+1,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         group = NA, 
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 10 (Von Bastian et al.); flanker task
dataset10 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/vonbastian_2015_evidence/LEF_flanker.csv", sep = ";") %>%
  mutate(congruency = ifelse(congruency == "congruent", 1, ifelse(congruency == "incongruent", 2, ifelse(congruency == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         datasetid = 10,
         block = 1,
         subject = as.factor(ID),
         between = NA,
         within = NA,
         rt = RT/1000) # rt data in seconds
ntrial <- length(dataset10[dataset10$ID == dataset10$ID[1], 1])
nsub <- length(unique(dataset10$ID))
dataset10$trial <- rep(1:ntrial, nsub) # add subject and trial numbers
dataset10 <- dataset10 %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  

# Young: 18-24, old: 65-75
# Dataset 11 (Rey-Mermet et al.); arrow flanker task
dataset11 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/mermet_2018_should/arrowFlanker.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset11 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset11 <- left_join(dataset11, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 11,
         subject = as.factor(sub - 100),
         accuracy = acc,
         between = ageGroup,   
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 12 (Rey-Mermet et al.); letter flanker task
dataset12 <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/mermet_2018_should/letFlanker.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset12 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset12 <- left_join(dataset12, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 12,
         subject = as.factor(sub - 100),
         accuracy = acc,
         between = ageGroup,   
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  


# Dataset 13 (Hedge et al.); flanker of study 1
dataset13 <- hedge_data1 %>% filter(direction != 0) %>% # keep flanker task data of study 1
  mutate(datasetid = 13) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)
 

# Dataset 14-34 (Many Labs studies from https://osf.io/n8xa7/)
manylabs <- read.csv("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/ebersole_2016_many/StroopCleanSet.csv")
for(i in 1:21){
  assign(paste("dataset", i+13, sep = ""), 
         manylabs %>% filter(study_name == unique(manylabs$study_name)[i]) %>% 
           mutate(datasetid = i+13, 
                  subject = as.factor(session_id), 
                  block = block_number,
                  trial = trial_number+1, 
                  congruency = ifelse(congruent == "Congruent", 1, ifelse(congruent == "Incongruent", 2, NA)), 
                  congruency = as.factor(congruency),
                  accuracy = trial_error,
                  between = NA,
                  within = NA,
                  rt = trial_latency/1000) %>% 
           select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) %>%
           group_by(subject) %>%
           arrange(trial, .by_group = TRUE) %>%
           ungroup())
}

# Additionally: Hedge data of study 2
hedge_data2 <- bind_rows(hedge[[2]]) %>% # combine data of study2
  mutate(congruency = ifelse(cond == 0, 1, ifelse(cond == 2, 2, ifelse(cond == 1, 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(session == 1, block, block + 5),
         between = NA,
         within = as.numeric(session),
         subject = as.factor(as.numeric(study)*100 + as.numeric(participant))) # add subject numbers

dataset49 <- hedge_data2 %>% filter(direction == 0) %>% # keep Stroop task data of study 2
  mutate(datasetid = 49) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

dataset50 <- hedge_data2 %>% filter(direction != 0) %>% # keep flanker task data of study 2
  mutate(datasetid = 50) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)



# Add new datasets here:


############ Calculate mean effects #############

# List with all datasets
datasetlist <- list(dataset1, dataset2, dataset3, dataset4, dataset5, 
                    dataset6, dataset7, dataset8, dataset9, dataset10, 
                    dataset11, dataset12, dataset13, dataset14, dataset15, 
                    dataset16, dataset17, dataset18, dataset19, dataset20, 
                    dataset21, dataset22, dataset23, dataset24, dataset25, 
                    dataset26, dataset27, dataset28, dataset29, dataset30, 
                    dataset31, dataset32, dataset33, dataset34)

meaneffect <- vector()
sdeffect <- vector()
for(i in 1:34){
  data <- datasetlist[[i]] %>%
    filter(incl == 1) %>%
    group_by(subject, cond) %>%
    summarize(mean = mean(rt)) %>%
    ungroup() %>% 
    select(subject, cond, mean) %>% 
    group_by(subject) %>% 
    pivot_wider(names_from = cond, values_from = mean) %>%
    rename(cond1 = `1`,
           cond2 = `2`) %>%
    mutate(effect = (cond2-cond1)*1000)
  meaneffect[i] <- round(mean(data$effect, na.rm = TRUE))
  sdeffect[i] <- round(sd(data$effect, na.rm = TRUE))
}

########## Create dataset, study and task table ########## 

# Dataset table
dataset <- data.frame(datasetid = 1:34,
                      studyid = c(1,2,2,3,3,4,1,2,2,1,3,3,4,rep(5,21)),
                      taskid = c(rep(1, 6), rep(2, 3), rep(3, 4), rep(1, 21)),
                      keywords = c(c("number stroop, stroop, stroop task, stroop effect, stroop number, inhibition, inhibition task"),
                                   c("color stroop, stroop, stroop task, stroop effect, stroop color, inhibition, inhibition task"),
                                   c("stroop, sidedness, stroop task, stroop effect, inhibition, inhibition task"),
                                   c("number stroop, stroop, stroop task, stroop effect, stroop number, inhibition, inhibition task"),
                                   c("color stroop, stroop, stroop task, stroop effect, stroop color, inhibition, inhibition task"),
                                   c("color stroop, stroop, stroop task, stroop effect, stroop color, inhibition, inhibition task"),
                                   c("simon, simon task, simon effect, inhibition, inhibition task"),
                                   c("simon, simon task, simon effect, inhibition, inhibition task"),
                                   c("simon, sidedness, simon task, simon effect, inhibition, inhibition task"),
                                   c("letter flanker, flanker, flanker task, flanker effect, flanker letter, letters, inhibition, inhibition task"),
                                   c("arrow flanker, flanker, flanker task, flanker effect, flanker arrow, arrows, inhibition, inhibition task"),
                                   c("letter flanker, flanker, flanker task, flanker effect, flanker letter, letters, inhibition, inhibition task"),
                                   c("arrow flanker, flanker task, flanker effect, flanker arrow, arrows, inhibition, inhibition task"),
                                   rep(c("color stroop, stroop, stroop task, stroop effect, stroop color, inhibition, inhibition task"), 21)),
                      data_exclusions = c("No data exclusions.",
                                          "No data exclusions.",
                                          "No data exclusions.",
                                          "Participant 164 is excluded because of experimenter error and participants 368 and 402 because they did not return for the second part of the experiment.",
                                          "Participant 132 is excluded because of experimenter error and participant 429 because they did not return for the second part of the experiment.",
                                          "Participants 6, 17 and 37 of Study 1 and participants 28 and 56 of Study 2 are excluded because they did not return for the second part of the experiment.",
                                          "No data exclusions.",
                                          "No data exclusions.",
                                          "No data exclusions.",
                                          "No data exclusions.",
                                          "Participant 132 is excluded because of experimenter error and participant 429 because they did not return for the second part of the experiment.",
                                          "Participant 164 is excluded because of experimenter error and participants 368 and 402 because they did not return for the second part of the experiment.",
                                          "Participants 6, 17 and 37 of Study 1 and participants 28 and 56 of Study 2 are excluded because they did not return for the second part of the experiment.",
                                          rep("No data exclusions.", 21)),
                      codebook = c(
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response, 97 and 99 = no response within 2 sec)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response, 99 = no response within 2 sec)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number (first digit indicates study number)
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
"Columns: 
  1. Dataset id 
  2. Subject number (first digit indicates study number)
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent, 3 = neutral)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)",
rep(
"Columns: 
  1. Dataset id 
  2. Subject number
  3. Trial number
  4. Congruency (1 = congruent, 2 = incongruent)
  5. Accuracy (0 = inaccurate response, 1 = accurate response)
  6. Response time in seconds
  7. Age group (1 = young, 3 = old)
  8. Included in analyses (0 = excluded, 1 = included)", 21)),
                      n_participants = sapply(datasetlist, function(x) length(unique(x$subject))),
                      n_blocks = sapply(datasetlist, function(x) max(as.numeric(x$block, na.rm = TRUE))),
                      n_trials = sapply(datasetlist, function(x) round(nrow(x)/length(unique(x$subject)))),
                      mean_effect = meaneffect,
                      sd_effect = sdeffect,
                      neutral_trials = c("Yes", "Yes", "No", "Yes", "Yes", "Yes", "No", "No", "No",
                                         "Yes", "Yes", "Yes", "Yes", rep("No", 21)),
                      percentage_incongruent = sapply(datasetlist, function(x) round(length(which(x$cond == 2))/nrow(x), 2)*100),
                      feedback = c(NA, "None", "Throughout the experiment", rep("After each trial", 2),
                                   "After each block", NA, "None", "Throughout the experiment", NA,
                                   rep("After each trial", 2), "After each block", rep(NA, 21)),
                      fixation_cross = c(NA, rep("700 ms", 2), rep("500 ms", 2), "None", "250 ms", rep("700 ms", 2), NA,
                                         rep("500 ms", 2), "None", rep(NA, 21)),
                      time_limit = c(rep("None", 3), rep("2000 ms", 2), rep("None", 5), rep("2000 ms", 2), rep("None", 22)),
                      mean_age = c(24.2, NA, NA, rep(47.5, 2), 19.5, 24.2, NA, NA, 24.2, rep(47.5, 2), 19.5, 18.69,
                                   19.05, 18.65, 18.68, 20.88, 20.47, 19.41, 19.58, 19.33, 19.45, 18.79, 18.75,
                                   18.86, 20.05, 35.11, 19.31, 18.73, 18.75, 20.07, 19.66, 19.70),
                      percentage_female = c(61.2, NA, NA, 49.2, 49.0, 86.6, 61.2, NA, NA, 61.2, 49.0, 49.0, 86.6, 
                                            45.9, 74.2, 69.9, 62.4, 77.5, 73.9, 78.3, 71.7, 78.0, 65.8, 55.2, 78.0, 
                                            76.2, 74.1, 48.2, 65.3, 62.1, 81.0, 71.4, 68.1, 68.3),
                      percentage_male = c(36.4, NA, NA, 39.3, 39.2, 13.4, 36.4, NA, NA, 36.4, 39.2, 39.2, 13.4,
                                          52.9, 25.8, 30.1, 37.6, 21.3, 25.2, 21.4, 28.3, 21.2, 32.9, 43.8, 20.3,
                                          23.8, 25.9, 51.1, 34.7, 37.9, 17.9, 28.6, 31.5, 31.7))

# Study table
study <- data.frame(studyid = 1:5,
                    authors = c("Von Bastian, Souza, & Gade",
                                "Pratte, Rouder, Morey, & Feng",
                                "Rey-Mermet, Gade, & Oberauer",
                                "Hedge, Powell, & Sumner",
                                "Cebersole et al. (Many Labs study)"),
                    conducted = c(2015, 2010, 2018, 2018, 2016), 
                    added = c(rep(as.Date("2020-3-1"), 4), as.Date("2020-3-19")),
                    contact = c("claudia.vonbastian@colorado.edu", 
                                "prattems@gmail.com",
                                "alodie.rey-mermet@ku.de",
                                "hedgec@cardiff.ac.uk",
                                "cebersole@virginia.edu"),
                    county = c("Switzerland",
                               "United States",
                               "Switzerland",
                               "United Kingdom",
                               "United States/Canada/Online"))

# Task table
task <- data.frame(taskid = 1:3, 
                   task = c("Stroop task", "Simon task", "Flanker task"),
                   task_description = c(
"Participants are asked to report the ink color of the word that is being presented. In congruent trials, the ink colors are congruent to the meaning of the words and in inconguent trials, these aspects are in conflict. The Stroop effect implies that in general, people are quicker in identifying the color of color words in the congruent condition as opposed to the color words in the incongruent condition.",
"Participants are asked to press a specific button on the left side of their keyboard when a green circle appears and a on the right side when a red circle appears. In the congruent condition, the green circle appears on the left of the screen and the red circle on right side of the screen, whereas in the incongruent condition, the side associated with the presented color is in contrast with the side on which the circle is represented.",
"Participants are asked to press a specific button when a specific character is presented (e.g., a button on the left side of the keyboard when a vowel appears and a button on the right side if a consonant appears). Distracting characters are placed around the presented character (these distracting characters are always identical). In congruent conditions the target and distracting characters match and are both are vowels or consonants (e.g., “AAAEAAA”), whereas in incongruent conditions the target and distracting characters mismatch (e.g., “SSSESSS”)."))


########## Create database ########## 

library("RSQLite")
conn <- dbConnect(RSQLite::SQLite(), "inhibitiontasks.db")
dbWriteTable(conn, "study", study)
dbWriteTable(conn, "dataset", dataset)
dbWriteTable(conn, "task", task)
list <- paste(rep("dataset", 34), 1:34, sep = "") # list with datasets as strings
for(i in 1:34){
  dbWriteTable(conn, paste(list[i]), get(list[i]))
}
dbListTables(conn) # check all tables
dbDisconnect(conn)

# To add new datasets, use dbWriteTable(conn, "datasetX", datasetX)
# Remember to also update the dataset, study and task tables

