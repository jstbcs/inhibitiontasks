## Adding new data to Inhibition Task Open Data Base 
#
# Record of Revisions
#
# Date                                     Descriptions of Change
# ====          ================           ======================
# 10-02-2023    Madlen Hoffstadt          Read in whitehead, 2020 (Flanker, Exp 2)


########## Overview of datasets ########## 
# - Whitehead, 2020 

library(dplyr)


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
  










