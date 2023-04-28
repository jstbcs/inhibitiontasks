# PREPARE RAW DATA FOR INHIBITION DATA BASE 
library(dplyr)
library(data.table)

########## Overview of datasets #################################### 
# - von Basitian et al.:        dataset1, dataset7, dataset10
# - Pratte et al. :             dataset2, dataset3, dataset8, dataset9
# - Rey-Mermet et al.:          dataset4, dataset5, dataset11, dataset12 
# - Hedge et al.:               dataset6, dataset13, dataset49, dataset50
# - Many labs data:             dataset14 - dataset34 
# - Whitehead et al.(2020):     dataset 35 - 40, dataset 46 - 48
# - Tang et al. (2022):         dataset 41
# - Chetverikov et al. (2017):  dataset 42
# - Stahl et al. (2014):        dataset 43 - 45

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
         congruency = as.factor(congruency),
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
         between = NA, 
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
         between = NA, 
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


# Dataset 35 (Whitehead et al., 2020; FlankerExp2)
dataset35 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/FlankerExp2.csv") %>%
  mutate(
    datasetid = 35,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    block = BlockNum,
    between = NA,
    within = NA,
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 36 (Whitehead et al., 2020; FlankerExp3)
dataset36 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/FlankerExp3.csv") %>% 
  mutate(
    datasetid = 36,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = factor(Subject - 100),
    between = NA,
    within = NA,
    block = ifelse(PracExp == "Exp", 1, -999), # Todo: Check in original paper
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC
  ) %>% 
  group_by(subject) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 37 (Whitehead et al., 2020; SimonExp2)
dataset37 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/SimonExp2.csv") %>%
  mutate(
    datasetid = 37,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA,
    block = BlockNum, 
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject, block) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 38 (Whitehead et al., 2020; SimonExp 3)
dataset38 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/SimonExp3.csv") %>%
  mutate(
    datasetid = 38,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA,
    block = ifelse(PracExp == "Exp", 1, -999), # Todo: Check in original paper
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 39 (Whitehead et al., 2020; StroopExp 2)
dataset39 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/StroopExp2.csv") %>% 
  mutate(
    datasetid = 39, 
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA, 
    block = BlockNum, 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 40 (Whitehead et al., 2020; StroopExp 3)
dataset40 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/StroopExp3.csv") %>% 
  mutate(
    datasetid = 40,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA, 
    block = ifelse(PracExp == "Exp", 1, -999), # Todo: Check in original paper 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% # group by block if existed
  mutate(trial = row_number()) %>% 
  ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 41 (Snijder et al., 2022); data online at https://osf.io/evuhg
dataset41 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/tang_2022_dual/destroop-raw.csv")  %>% 
  mutate(itemType = ifelse(.$itemType == 2, 
                           "PC50",
                           ifelse(.$itemType == 1 & .$session != "baseline",
                                  "MI",
                                  "MC"
                           )
  )
  ) %>% 
  mutate(
    datasetid = 41,
    # create subject variable starting at 1
    subject = rep(seq_along(rle(ID)$lengths), times = rle(ID)$lengths),
    subject = as.factor(subject),
    block = case_when(
      phase == "test" & session == "baseline"  ~ 1,
      phase == "test" & session == "reactive"  ~ 2,
      phase == "test" & session == "proactive" ~ 3,
      phase == "retest" & session == "baseline"  ~ 4,
      phase == "retest" & session == "reactive"  ~ 5,
      phase == "retest" & session == "proactive" ~ 6),
    between = NA, 
    within = factor(interaction(phase, session, itemType)),   # baseline/ reactive/ proactive condition and test setting
    within = case_when(
      within == "test.baseline.MC" ~ 1, 
      within == "test.baseline.PC50" ~ 2, 
      within == "test.reactive.MC" ~ 3, 
      within == "test.reactice.PC50" ~ 4,
      within == "test.reactive.MI" ~ 5, 
      within == "test.proactive.PC50" ~ 6, 
      within == "test.proactive.MI" ~ 7, 
      within == "restest.baseline.MC" ~ 8, 
      within == "restest.baseline.PC50" ~ 9, 
      within == "retest.reactive.MC" ~ 10, 
      within == "retest.reactive.PC50" ~ 11, 
      within == "retest.reactive.MI" ~ 12, 
      within == "retest.proactive.PC50" ~ 13, 
      within == "retest.proactive.MI" ~ 14
    ),
    congruency = ifelse(grepl("incon", trialCode), 2, 1),
    accuracy = ACC,
    rt = RT / 1000)  %>%
  group_by(subject, block) %>%   # adding within each subject and within condition trial number
  mutate(
    trial = row_number()
  ) %>%
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 42 (Chetverikov et al., 2017); data online at https://osf.io/7rb48
dataset42 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/chetverikov_2017_blame/flanker_data.csv") 
dataset42 <- dataset42 %>% 
  mutate(
    datasetid = 42,
    subject = as.factor(uid),
    block = sapply(blockf, function(i) as.numeric(strsplit(i, " ")[[1]][2])),
    trial = trialN + 1, 
    between = NA, 
    within = NA, 
    congruency = ifelse(grepl("Incompatible", compf), 2, 1), 
    accuracy = corr) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

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
                   ifelse(grepl("mix", block) | grepl("ueb_", block),
                          -999,
                          block)),
    congruency = ifelse(condition == "con" | condition == "ident", 1, 
                        ifelse(condition == "incon", 2, 3)),
    congruency = as.factor(congruency),
    accuracy = error,
    between = NA, 
    within = NA,
    rt = latency / 1000) %>%
  group_by(subject, block) %>% # change trial number to include warm-ups
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


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
    # congruency = trial_type? 
    # accuracy missing,  
    between = NA, 
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
    block = ifelse(grepl("mix", block)  | grepl("ueb_", block), 
                   -999,
                   block),   # encode practice block
    congruency = ifelse(condition == "congr" | condition == "ident", 1, 
                        ifelse(condition == "incon", 2, 3)),
    congruency = as.factor(congruency),
    accuracy = err,
    between = NA,
    within = NA, # code match/ mismatch of target & distractor (i.e., identical)?
    rt = latency / 1000
  ) %>%
  group_by(subject, block) %>% # change trial number to include warm-ups
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 46: Whitehead et al. (2020): Simon task from Experiment1
dataset46 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideSimon.RT != "NA") %>%  # choose simon task entries
  mutate(datasetid = 46, 
         congruency = ifelse(Congruency == 0, 2, Congruency),
         congruency = as.factor(congruency),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         between = NA, 
         within = NA,
         rt = StimSlideSimon.RT / 1000,
         accuracy = StimSlideSimon.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 47: Whitehead et al. (2020): Flanker task from Experiment1
dataset47 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideFlanker.RT != "NA") %>%  # choose Flanker task entries
  mutate(datasetid = 47, 
         congruency = ifelse(Congruency == 0, 2, Congruency),
         congruency = as.factor(congruency),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         between = NA, 
         within = NA,
         rt = StimSlideFlanker.RT / 1000,
         accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 48: Whitehead et al. (2020): Stroop task from Experiment1
dataset48 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/inhibitiontasks/adding-new-data/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideStroop.RT != "NA") %>%  # choose Stroop task entries
  mutate(datasetid = 48, 
         congruency = ifelse(Congruency == 0, 2, Congruency),
         congruency = as.factor(congruency),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         between = NA, 
         within = NA,
         rt = StimSlideStroop.RT / 1000,
         accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset49 & 50: Hedge data of study 2
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



