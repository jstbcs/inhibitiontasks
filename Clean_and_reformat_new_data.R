## Adding new data to Inhibition Task Open Data Base 
#
# Record of Revisions
#
# Date                                     Descriptions of Change
# ====          ================           ======================
# 10-02-2023    Madlen Hoffstadt      Read in whitehead, 2020 (Exp 2 data) 
# 13-02-2023    Madlen Hoffstadt      Read in Snijder et al., 2022 & start Chetverikov, 2017
# 14-02-2023    Sven Lesche           Read in Exp3 data of Whitehead, 2020
# 20-02-2023    Madlen Hoffstadt      Implement structural changes (add group and within, remove incl)
# 22-02-2023    Madlen Hoffstadt      Added Stahl et al. (2014)
# 24-02-2023    Madlen Hoffstadt      Adjustments to Chetverikov et al. data
# 04-03-2023    Madlen Hoffstadt      Recoded practice blocks and warm-up trials
# 06-03-2023    Madlen Hoffstadt      Added Whitehead Experiment1 (datasets 46-48)
# 17-03-2023    Madlen Hoffstadt      Changed name of cond column to congr and point to files on our repo


library(dplyr)
library(data.table)


########## Overview of datasets ####################################
# - Whitehead et al.(2020):     dataset 35 - 40 and dataset 46 - 48
# - Snijder et al. (2022):      dataset 41
# - Chetverikov et al. (2017):  dataset 42
# - Stahl et al. (2014):        dataset 43 - 45
#####################################


########## Overview over variables of each data frame  ##########
# - datasetid (numeric): constant for all rows
# - subject (factor)
# - block (numeric): starting at 1 (-999 for practice blocks)
# - trial: trial number for each subject in each block
# - congr (factor): 1 -> congruent; 2 -> incongruent; 3 -> neutral 
# - accuracy (numeric): 0 or 1
# - group
# - within
# - rt (numeric): in seconds
##########################################


# Dataset 35 (Whitehead et al., 2020; FlankerExp2)
dataset35 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/FlankerExp2.csv") %>%
  mutate(
    datasetid = 35,
    congr = ifelse(Congruency == 0, 2, Congruency),
    congr = as.factor(congr),
    subject = as.factor(Subject - 100),
    block = BlockNum,
    group = NA,
    within = NA,
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 36 (Whitehead et al., 2020; FlankerExp3)
dataset36 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/FlankerExp3.csv") %>% 
  mutate(
    datasetid = 36,
    congr = ifelse(Congruency == 0, 2, Congruency),
    congr = as.factor(congr),
    subject = factor(Subject - 100),
    group = NA,
    within = NA,
    block = ifelse(PracExp == "Exp", 1, -999), # Todo: Check in original paper
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC
  ) %>% 
  group_by(subject) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 37 (Whitehead et al., 2020; SimonExp2)
dataset37 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/SimonExp2.csv") %>%
  mutate(
    datasetid = 37,
    congr = ifelse(Congruency == 0, 2, Congruency),
    congr = as.factor(congr),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA,
    block = BlockNum, 
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject, block) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 38 (Whitehead et al., 2020; SimonExp 3)
dataset38 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/SimonExp3.csv") %>%
  mutate(
    datasetid = 38,
    congr = ifelse(Congruency == 0, 2, Congruency),
    congr = as.factor(congr),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA,
    block = ifelse(PracExp == "Exp", 1, -999), # Todo: Check in original paper
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 39 (Whitehead et al., 2020; StroopExp 2)
dataset39 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/StroopExp2.csv") %>% 
  mutate(
    datasetid = 39, 
    congr = ifelse(Congruency == 0, 2, Congruency),
    congr = as.factor(congr),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA, 
    block = BlockNum, 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt)


# Dataset 40 (Whitehead et al., 2020; StroopExp 3)
dataset40 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/StroopExp3.csv") %>% 
  mutate(
    datasetid = 40,
    congr = ifelse(Congruency == 0, 2, Congruency),
    congr = as.factor(congr),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA, 
    block = ifelse(PracExp == "Exp", 1, -999), # Todo: Check in original paper 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% # group by block if existed
  mutate(trial = row_number()) %>% 
  ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 41 (Snijder et al., 2022); data online at https://osf.io/evuhg
dataset41 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/tang_2022_dual/destroop-raw.csv") %>%
  mutate(
    datasetid = 41,
    # create subject variable starting at 1
    subject = rep(seq_along(rle(ID)$lengths), times = rle(ID)$lengths),
    subject = as.factor(subject),
    # block = ,
    trial = trialNum,
    group = NA, 
    within = NA,   # baseline/ reactive/ proactive condition
    congr = ifelse(grepl("incon", trialCode), 2, 1),
    accuracy = ACC,
    rt = RT / 1000)  %>%
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt)


# Dataset 42 (Chetverikov et al., 2017); data online at https://osf.io/7rb48
dataset42 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/chetverikov_2017_blame/flanker_data.csv") %>%
  mutate(
    datasetid = 42,
    subject = as.factor(uid),
    block = lapply(blockf, function(i) as.numeric(strsplit(i, " ")[[1]][2])),
    trial = trialN + 1, 
    group = NA, # NOTE: change later; this is agegroup + add gender?
    # max age is 31
    within = NA, 
    congr = ifelse(grepl("Incompatible", compf), 2, 1), 
    accuracy = corr) %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt)

# Dataset 43: Stahl et al. (2014): Stroop task 
dataset43 <- read.delim("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/stahl_2014_behavioral/stroop.dat", header = FALSE, sep = " ") %>%
  select(-V14) 
colnames(dataset43) <- c("subj", "subj_code", "date", "time", "block", "trial_no", 
                        "trial_type", "condition", "color", "word", "exp_resp", 
                        "latency", "error")
dataset43 <- dataset43 %>%
  mutate(
    datasetid = 43,
    subject = rep(seq_along(rle(subj)$lengths), times = rle(subj)$lengths),
    subject = as.factor(subject),
    block = ifelse(grepl("tst", block), 
                   apply(dataset43["block"], 1, function(i) as.numeric(strsplit(i, "tst")[[1]][2])),
                   ifelse(grepl("mix", block),
                          -999,
                          block)),
    congr = ifelse(condition == "con" | condition == "ident", 1, 
                       ifelse(condition == "incon", 2, 3)),
    congr = as.factor(congr),
    accuracy = error,
    group = NA, 
    within = NA,
    rt = latency / 1000) %>%
  group_by(subject, block) %>% # change trial number to include warm-ups
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt)


# Dataset 44: Stahl et al. (2014); Simon task
dataset44 <- read.delim("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/stahl_2014_behavioral/simon.dat", header = FALSE, sep = " ") 
colnames(dataset44) <- c("subj", "subjcode", "date", "time", "part", 
                         "trial_no", "trial_type", "response", "latency")
dataset44 <- dataset44 %>%
  mutate(
    datasetid = 44, 
    subject = rep(seq_along(rle(subj)$lengths), times = rle(subj)$lengths),
    subject = as.factor(subject),
    # block = part?,
    # trial = trial_no?,
    # cond = trial_type? 
    # accuracy missing,  
    group = NA, 
    within = NA, 
    rt = latency / 1000
  )


# Dataset 45: Stahl et al. (2014); Flanker task
dataset45 <- read.delim("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/stahl_2014_behavioral/flanker.dat", header = FALSE, sep= " ") %>%
  select(-V14)
colnames(dataset45) <- c("subj", "subjcode", "date", "time", "block", 
                            "trial_no", "trial_type", "condition", "targ",
                            "dist", "exp_resp", "latency", "err")
dataset45 <- dataset45 %>%
  mutate(
    datasetid = 45, 
    subject = rep(seq_along(rle(subj)$lengths), times = rle(subj)$lengths),
    subject = as.factor(subject), 
    block = ifelse(grepl("tst", block), 
                   apply(dataset45["block"], 1, function(i) as.numeric(strsplit(i, "tst")[[1]][2])),
                   block),  # extract block number
    block = ifelse(grepl("mix", block), 
                   -999,
                   block),   # encode practice block
    congr = ifelse(condition == "congr" | condition == "ident", 1, 
                  ifelse(condition == "incon", 2, 3)),
    congr = as.factor(congr),
    accuracy = err,
    group = NA,
    within = NA, # code match/ mismatch of target & distractor (i.e., identical)?
    rt = latency / 1000
  ) %>%
  group_by(subject, block) %>% # change trial number to include warm-ups
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt)


# Dataset 46: Whitehead et al. (2020): Simon task from Experiment1
dataset46 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideSimon.RT != "NA") %>%  # choose simon task entries
  mutate(datasetid = 46, 
         congr = ifelse(Congruency == 0, 2, Congruency),
         congr = as.factor(congr),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         group = NA, 
         within = NA,
         rt = StimSlideSimon.RT / 1000,
         accuracy = StimSlideSimon.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 47: Whitehead et al. (2020): Flanker task from Experiment1
dataset47 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideFlanker.RT != "NA") %>%  # choose Flanker task entries
  mutate(datasetid = 47, 
         congr = ifelse(Congruency == 0, 2, Congruency),
         congr = as.factor(congr),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         group = NA, 
         within = NA,
         rt = StimSlideFlanker.RT / 1000,
         accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 


# Dataset 48: Whitehead et al. (2020): Stroop task from Experiment1
dataset48 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideStroop.RT != "NA") %>%  # choose Stroop task entries
  mutate(datasetid = 48, 
         congr = ifelse(Congruency == 0, 2, Congruency),
         congr = as.factor(congr),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         group = NA, 
         within = NA,
         rt = StimSlideStroop.RT / 1000,
         accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congr, group, within, accuracy, rt) 







