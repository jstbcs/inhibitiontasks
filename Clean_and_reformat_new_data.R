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

library(dplyr)
library(data.table)


########## Overview of datasets ########## 
# - Whitehead et al.(2020):     dataset 35 - 40
# - Snijder et al. (2022):      dataset 41
# - Chetverikov et al. (2017):  dataset 42
# - Stahl et al. (2014):        dataset 43 - 45
#########################


########## Overview over variables of each data frame  ##########
# - datasetid (numeric): constant for all rows
# - subject (factor)
# - block (numeric): starting at 1
# - trial: trial number for each subject in each block
# - cond (factor): 1 -> congruent; 2 -> incongruent; 3 -> neutral 
# - accuracy (numeric): 0 or 1
# - group
# - within
# - rt (numeric): in seconds
# - incl (numeric): variable indicating whether to include in analysis (1) or not (0)
#     - exclude first 5 trials; practice and warm-up trials
#     - exclude inaccurate responses (accuracy 0)
#     - exclude neutral condition 
#     - exclude very fast/ slow reaction times ( < .20 or > 2)
##########################################



# Dataset 35 (Whitehead et al., 2020; FlankerExp2)
dataset35 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/FlankerExp2.csv") %>%
  mutate(
    datasetid = 35,
    cond = ifelse(Congruency == 0, 2, Congruency),
    cond = as.factor(cond),
    subject = as.factor(Subject - 100),
    block = BlockNum,
    group = NA,
    within = NA,
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>% mutate(trial = row_number()) %>% ungroup() %>% # code trial number
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt) 


# Dataset 36 (Whitehead et al., 2020; FlankerExp3)
dataset36 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/FlankerExp3.csv") %>% 
  mutate(
    datasetid = 36,
    cond = ifelse(Congruency == 0, 2, Congruency),
    cond = as.factor(cond),
    subject = factor(Subject - 100),
    group = NA,
    within = NA,
    block = 1, # Todo: Check in original paper
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC
  ) %>% 
  group_by(subject) %>% 
  mutate(
    trial = ifelse(PracExp == "Exp", row_number(), "practice")
  ) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt) 


# Dataset 37 (Whitehead et al., 2020; SimonExp2)
dataset37 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp2.csv") %>%
  mutate(
    datasetid = 37,
    cond = ifelse(Congruency == 0, 2, Congruency),
    cond = as.factor(cond),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA,
    block = BlockNum, # Note: check in original paper whether just 1 block
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject, block) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>% 
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt) 


# Dataset 38 (Whitehead et al., 2020; SimonExp 3)
dataset38 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp3.csv") %>%
  mutate(
    datasetid = 38,
    cond = ifelse(Congruency == 0, 2, Congruency),
    cond = as.factor(cond),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA,
    block = 1, # Note: check in original paper whether just 1 block
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject) %>% mutate(trial = ifelse(PracExp == "Exp", row_number(), "practice")) %>% ungroup() %>% 
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt) 


# Dataset 39 (Whitehead et al., 2020; StroopExp 2)
dataset39 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/StroopExp2.csv") %>% 
  mutate(
    datasetid = 39, 
    cond = ifelse(Congruency == 0, 2, Congruency),
    cond = as.factor(cond),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA, 
    block = BlockNum, 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% mutate(trial = row_number()) %>% ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt)


# Dataset 40 (Whitehead et al., 2020; StroopExp 3)
dataset40 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/StroopExp3.csv") %>% 
  mutate(
    datasetid = 40,
    cond = ifelse(Congruency == 0, 2, Congruency),
    cond = as.factor(cond),
    subject = as.factor(Subject - 100),
    group = NA, 
    within = NA, 
    block = 1, 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject) %>% # group by block if existed
  mutate(trial = ifelse(PracExp == "Prac", "practice", row_number())) %>% 
  ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt) 


# Dataset 41 (Snijder et al., 2022); data online at https://osf.io/evuhg
dataset41 <- data.table::fread("destroop-raw.csv") %>%
  mutate(
    datasetid = 41,
    # create subject variable starting at 1
    subject = rep(seq_along(rle(ID)$lengths), times = rle(ID)$lengths),
    subject = as.factor(subject),
    #block = ,
    trial = trialNum,
    group = NA, 
    within = NA,   # baseline/ reactive/ proactive condition
    cond = ifelse(grepl("incon", trialCode), 2, 1),
    accuracy = ACC,
    rt = RT / 1000)  %>%
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt)


# Dataset 42 (Chetverikov et al., 2017); data online at https://osf.io/7rb48
dataset42 <- data.table::fread("flanker_data.csv") %>%
  mutate(
    datasetid = 42,
    subject = as.factor(uid),
    block = lapply(blockf, function(i) as.numeric(strsplit(i, " ")[[1]][2])),
    trial = trialN + 1, 
    group = ifelse(age < 35, 1, 3), # NOTE: change later; this is agegroup + add gender?
    within = NA, 
    cond = ifelse(grepl("Incompatible", compf), 2, 1), 
    accuracy = corr) %>% 
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt)

  

# Dataset 43: Stahl et al. (2014): Stroop task 
dataset43 <- read.delim("stroop.dat", header = FALSE, sep = " ") %>%
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
                   block),
    trial = trial_no,
    cond = ifelse(condition == "con" | condition == "ident", 1, 
                       ifelse(condition == "incon", 2, 3)),
    accuracy = error,
    group = NA, 
    within = NA,
    rt = latency / 1000) %>%
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt)


# Dataset 44: Stahl et al. (2014); Simon task
dataset44 <- read.delim("simon.dat", header = FALSE, sep = " ") 
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
dataset45 <- read.delim("flanker.dat", header = FALSE, sep= " ") %>%
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
                   block),
    trial = trial_no,
    cond = ifelse(condition == "congr" | condition == "ident", 1, 
                  ifelse(condition == "incon", 2, 3)),
    accuracy = err,
    group = NA,
    within = NA, # code match/ mismatch of target & distractor (i.e., identical)?
    rt = latency / 1000
  ) %>%
  select(datasetid, subject, block, trial, cond, group, within, accuracy, rt)







