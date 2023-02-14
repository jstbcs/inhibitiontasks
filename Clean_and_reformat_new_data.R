## Adding new data to Inhibition Task Open Data Base 
#
# Record of Revisions
#
# Date                                     Descriptions of Change
# ====          ================           ======================
# 10-02-2023    Madlen Hoffstadt      Read in whitehead, 2020 (Exp 2 data) 
# 13-02-2023    Madlen Hoffstadt      Read in Snijder et al., 2022 & Chetverikov, 2017
# 14-02-2023    Sven Lesche           Read in Exp3 data of Whitehead, 2020

library(dplyr)


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
# - agegroup (numeric): 1 -> young, 3  -> old
# - rt (numeric): in seconds!
# - incl (numeric): variable indicating whether to include in analysis (1) or not (0)
#     - exclude first 5 trials; practice and warm-up trials
#     - exclude inaccurate responses (accuracy 0)
#     - exclude neutral condition 
#     - exclude very fast/ slow reaction times ( < .20 or > 2)
##########################################



# Dataset 35 (Whitehead et al., 2020; FlankerExp2)
dataset35 <- read.csv("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/FlankerExp2.csv") %>%
  mutate(cond = as.factor(Congruency),
         datasetid = 35,   # Note: change later to make id assignment agile in SQL
         subject = as.factor(Subject - 100),
         block = BlockNum,
         agegroup = 1,  # Note: check in original paper from 2018
         rt = StimSlideFlanker.RT / 1000,
         accuracy = StimSlideFlanker.ACC) %>%
  filter(!is.na(cond) & !is.na(rt)) %>% # remove practice and failed trials 
  group_by(subject, block) %>% mutate(trial = row_number()) %>% ungroup() %>% # code trial number
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt) %>%
  # add column that indicates whether to include row in analysis
  mutate(incl = ifelse(trial %in% 1:5 | #  exclude first 5 trials
                         accuracy == 0 | accuracy == 97 | accuracy == 99 | # inaccurate responses
                         rt < .2 | rt > 2, 0, 1))  # outlier reaction times



# Dataset 36 (Whitehead et al., 2020; FlankerExp3)
dataset36 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/FlankerExp3.csv") %>% 
  mutate(
    datasetid = 36,
    cond = factor(Congruency),
    subject = factor(Subject - 100),
    agegroup = 1, # TODO: CHeck in original paper
    block = 1, # TODO: Check in original paper
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC
  ) %>% 
  filter(!is.na(cond) & !is.na(rt)) %>% 
  group_by(subject) %>% 
  mutate(
    trial = ifelse(PracExp == "Exp", row_number(), "practice")
  ) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt) %>% 
  mutate(incl = ifelse(
      trial %in% 1:5 |
      trial == "practice" |
      accuracy == 0 |
      rt < 0.2 |
      rt > 2,
    0,
    1
  ))


# Dataset 37 (Whitehead et al., 2020; SimonExp2)
dataset37 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp2.csv") %>%
  mutate(cond = as.factor(Congruency),
         datasetid = 37,
         subject = as.factor(Subject - 100),
         agegroup = 1, # Note: check in original paper
         block = BlockNum, # Note: check in original paper whether just 1 block
         rt = StimSlideSimon.RT / 1000,
         accuracy = StimSlideSimon.ACC) %>%
  filter(!is.na(cond) & !is.na(rt)) %>%   # remove failed trials
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject, block) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>% 
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt) %>%
  # add column to indicate whether to include row in analysis
  mutate(incl = ifelse(trial %in% 1:5 | trial == "practice" | # exclude first 5 and practice trials
                         accuracy == 0 | # inaccurate responses
                         rt < .2 | rt > 2, 0, 1)) # outlier response times


# Dataset 38 (Whitehead et al., 2020; SimonExp 3)
dataset38 <- read.csv("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp3.csv") %>%
  mutate(cond = as.factor(Congruency),
         datasetid = 38,
         subject = as.factor(Subject - 100),
         agegroup = 1, # Note: check in original paper
         block = 1, # Note: check in original paper whether just 1 block
         rt = StimSlideSimon.RT / 1000,
         accuracy = StimSlideSimon.ACC) %>%
  filter(!is.na(cond) & !is.na(rt)) %>%   # remove failed trials
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject) %>% mutate(trial = ifelse(PracExp == "Exp", row_number(), "practice")) %>% ungroup() %>% 
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt) %>%
  # add column to indicate whether to include row in analysis
  mutate(incl = ifelse(trial %in% 1:5 | trial == "practice" | # exclude first 5 and practice trials
                         accuracy == 0 | # inaccurate responses
                         rt < .2 | rt > 2, 0, 1)) # outlier response times



# Dataset 39 (Whitehead et al., 2020; StroopExp 2)
dataset39 <- read.csv("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/StroopExp2.csv") %>% 
  mutate(cond = as.factor(Congruency),
         datasetid = 39, 
         subject = as.factor(Subject - 100),
         agegroup = 1, 
         block = BlockNum, 
         rt = StimSlideStroop.RT / 1000,
         accuracy = StimSlideStroop.ACC) %>%
  filter(!is.na(cond) & !is.na(rt)) %>%   # remove failed trials
  group_by(subject, block) %>% mutate(trial = row_number()) %>% ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt) %>%
  # add column to indicate whether to include row in analysis
  mutate(incl = ifelse(trial %in% 1:5 |  # exclude first 5 trials
                         accuracy == 0 | # exclude inaccurate responses
                         rt < .2 | rt > 2, 0, 1)) # exclude outlier response times
  

# Dataset 40 (Whitehead et al., 2020; StroopExp 3)
dataset40 <- data.table::fread("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/StroopExp3.csv") %>% 
  mutate(cond = as.factor(Congruency),
         datasetid = 40, 
         subject = as.factor(Subject - 100),
         agegroup = 1, 
         block = 1, 
         rt = StimSlideStroop.RT / 1000,
         accuracy = StimSlideStroop.ACC) %>%
  filter(!is.na(cond) & !is.na(rt)) %>%   # remove failed trials
  group_by(subject) %>% # group by block if existed
  mutate(trial = ifelse(PracExp == "Prac", "practice", row_number())) %>% 
  ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt) %>%
  # add column to indicate whether to include row in analysis
  mutate(incl = ifelse(trial %in% 1:5 |  # exclude first 5 trials
                         accuracy == 0 | # exclude inaccurate responses
                         rt < .2 | rt > 2, 0, 1))


# Dataset 41 (Snijder et al., 2022); data online at https://osf.io/evuhg
dataset41 <- read.csv("destroop-raw.csv") %>%
  mutate(datasetid = 41,
         # create subject variable starting at 1
         subject = rep(seq_along(rle(ID)$lengths), times = rle(ID)$lengths),
         subject = as.factor(subject),
         # block = ,
         # trial = ,
         cond = ifelse(grepl("incon", trialCode), 0, 1),
         accuracy = ACC, 
         # agegroup = ,
         rt = RT / 1000) %>%
  select(datasetid, subject, cond, accuracy, rt) %>% # add agegroup, trial & block
 # Add incl



# Dataset 42 (Chetverikov et al., 2017); data online at https://osf.io/7rb48
dataset42 <- read.csv("flanker_data.csv") %>%
  mutate(datasetid = 42,
         subject = as.factor(uid),
         block = apply(dataset42["blockf"], 1, function(i) as.numeric(strsplit(i, " ")[[1]][2])),
         trial = trialN + 1, # Note: check if trialN 0 is practice trial
         cond = ifelse(grepl("Incompatible", compf), 0, 1), # Note: check concept of compatibility
         accuracy = corr, 
         agegroup = ifelse(age < 35, 1, 3)) %>% # cutoff for age?
  mutate(incl = ifelse(trial %in% 1:5 | # exclude first 5 trials
                         accuracy == 0 | # exclude inaccurate responses
                         missing == 1 | is.na(rt) | # exclude missing responses
                         rt < .2 | rt > 2, 0, 1)) %>% # exclude outlier response times
  select(datasetid, subject, block, trial, cond, accuracy, agegroup, rt, incl)
  


# Dataset 43: Stahl et al. (2014); Stroop task 
dataset43 <- read.delim("stroop.dat", header = FALSE, sep = " ")

# v10 = target meaning 
# v09 = probe meaning 
# v08 = condition 
#     - neutral (target is QQQQ)
#     - congruent (target color = target meaning)
#     - incongruent (target color != target meaning)
# v 05 = block --> exclude uebung?

