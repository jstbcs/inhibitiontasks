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
  "study_id"
)

within_info_columns <- c(
  "within_description",
  "dataset_id"
)

group_info_columns <- c(
  "mean_age",
  "percentage_female",
  "n_members",
  "group_description",
  "study_id"
)

task_info_columns <- c(
  "task"
)

condition_descriptives_info_columns <- c(
  "percentage_congr",
  "percentage_neutral",
  "n_observations",
  # "n_trials",
  "n_blocks",
  "dataset_id",
  "group_id",
  "within_id",
  "n_obs"
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

column_names_db <- list(
  publication = publication_info_columns,
  study = study_info_columns,
  dataset_overview = data_overview_info_columns,
  within = within_info_columns,
  group_table = group_info_columns,
  condition = condition_descriptives_info_columns,
  data = data_columns
)
