## Adding new data to Inhibition Task Open Data Base 
#
# Record of Revisions
#
# Date                                     Descriptions of Change
# ====          ================           ======================
# 10-02-2023    Madlen Hoffstadt      Read in whitehead, 2020 (Exp 2 data) & Snijder


library(dplyr)
library(osfr)


########## Overview of datasets ########## 
# - Whitehead et al.(2020): datset 35 - 39
# - Snijer et al. (2022)
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


# Dataset 37 (Whitehead et al., 2020; SimonExp2)
dataset37 <- read.csv("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/Whitehead2020/SimonExp3.csv") %>%
  mutate(cond = as.factor(Congruency),
         datasetid = 37,
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


# Dataset 38 (Whitehead et al., 2020; SimonExp 3)



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
  








