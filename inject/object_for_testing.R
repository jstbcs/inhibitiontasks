publication_info_1 = data.frame(
  authors = "One and two",
  added = "2018",
  publication_code = "one2018ptest"
)

study_info_1 = data.frame(
  n_groups = "3",
  n_tasks = "2",
  comment = "Sven was here"
)

study_info_2 = data.frame(
  n_groups = "1",
  n_tasks = "1",
  comments = "Sven wasn't here"
)

group_info_1 = data.frame(
  group = 1:3,
  mean_age =c(20, 30, 40),
  n_participants = c(100, 100, 200),
  group_description = c("stink", "stink", "okay")
)

group_info_2 = data.frame(
  group = 1,
  mean_age = c(45),
  n_participants = c(400),
  group_description = c("hey")
)

data_overview_1 = data.frame(
  data_excl = c("Did this"),
  n_participants = 400,
  n_blocks = 12,
  n_trials = 250,
  neutral_trials = 1, 
  fixation_cross = 1
)

data_overview_2 = data.frame(
  data_excl = "None",
  n_participants = 500,
  n_blocks = 5,
  n_trials = 100,
  neutral_trials = 0,
  fixation_cross = 1
)

within = data.frame(
  within = 1:3,
  within_description = c("test", "retest", "messing about")
)

condition = data.frame(
  condition = 1:6,
  n_obs = 201:206
)
task_info = data.frame(
  task = "stroop"
)

data = data.frame(
  rt = 1:200,
  acc = rep(1, 200),
  congr = rep(c(0,1), 100),
  block = rep(1:10, 20),
  trial = 1:200,
  group = 1,
  within = 1,
  condition =1 
)

object = list(
  publication1 = list(
    publication_info = publication_info_1,
    study1 = list(
      study_info = study_info_1,
      group_info = group_info_1,
      data1 = list(
        task = task_info,
        overview = data_overview_1,
        within = within,
        condition = condition,
        data = data
      ),
      data2 = list(
        task = task_info,
        overview = data_overview_1,
        within = within,
        condition = condition,
        data = data
      )
    ),
    study2 = list(
      study_info = study_info_2,
      group_info = group_info_2,
      data1 = list(
        task = task_info,
        overview = data_overview_1,
        within = within,
        condition = condition,
        data = data
      )
    )
  ),
  publication2 = list(
    publication_info = publication_info_1,
    study1 = list(
      study_info = study_info_1,
      group_info = group_info_1,
      data1 = list(
        task = task_info,
        overview = data_overview_1,
        within = within,
        condition = condition,
        data = data
      ),
      data2 = list(
        task = task_info,
        overview = data_overview_1,
        within = within,
        condition = condition,
        data = data
      )
    ),
    study2 = list(
      study_info = study_info_2,
      group_info = group_info_2,
      data1 = list(
        task = task_info,
        overview = data_overview_1,
        within = within,
        condition = condition,
        data = data
      )
    )
  )
)
