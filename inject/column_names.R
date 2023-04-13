# Column names for the tables
publication_info_columns <- c(
  "authors",
  "conducted",
  "added",
  "country",
  "contact",
  "keywords",
  "apa_reference",
  "publication_code"
)

study_info_columns <- c(
  "n_groups",
  "n_tasks",
  "comment",
  "study_id",
  "publication_id"
)

data_overview_info_columns <- c(
  "data_excl",
  "codebook",
  "n_participants",
  "n_blocks",
  "n_trials",
  "neutral_trials",
  "fixation_cross", 
  "time_limit",
  "filename_github_repo",
  "study_id",
  "dataset_id"
)

within_info_columns <- c(
  "within_description",
  "dataset_id",
  "within_id"
)

group_info_columns <- c(
  "mean_age",
  "percentage_female",
  "n_members",
  "group_description",
  "study_id",
  "group_id"
)

task_info_columns <- c(
  "task",
  "task_id",
  "task_description"
)

condition_descriptives_info_columns <- c(
  "percentage_congruent",
  "percentage_neutral",
  "dataset_id",
  "group_id",
  "within_id",
  "condition_id",
  "n_obs",
  "mean_obs_per_participant"
)

data_columns <- c(
  "subject",
  "block", 
  "trial",
  "congruency",
  "accruacy",
  "rt",
  "dataset_id",
  "group_id",
  "within_id",
  "condition_id"
)

task_columns <- c(
  "task_id",
  "task_name",
  "task_description"
)

column_names_db <- list(
  publication = publication_info_columns,
  study = study_info_columns,
  dataset_overview = data_overview_info_columns,
  within = within_info_columns,
  group_table = group_info_columns,
  condition = condition_descriptives_info_columns,
  data = data_columns,
  task = task_columns
)
