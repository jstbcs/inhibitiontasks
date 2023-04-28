# Column names for the tables
# TODO: Replace all functions with reference to this setup file.
publication_table_columns <- c(
  "publication_id",
  "authors",
  "conducted",
  "added",
  "country",
  "contact",
  "keywords",
  "apa_reference",
  "publication_code"
) 

publication_table_mandatory <- c(1, 0, 0, 0, 0, 0, 0, 1, 1)

study_table_columns <- c(
  "study_id",
  "publication_id",
  "n_groups",
  "n_tasks",
  "comment"
)

study_table_mandatory <- c(1, 1, 1, 1, 1)

dataset_table_columns <- c(
  "study_id",
  "dataset_id",
  "task_id",
  "data_excl",
  "codebook",
  "n_participants",
  "n_blocks",
  "n_trials",
  "neutral_trials",
  "fixation_cross", 
  "time_limit",
  "github",
  "comment"
  
)

dataset_table_mandatory <- c(0, 0, 0, 0,
                             0, 0, 0, 0,
                             0, 0, 0, 0, 0)

within_table_columns <- c(
  "within_id",
  "dataset_id",
  "within_description"
)

within_table_mandatory <- c(1, 1, 1)

between_table_columns <- c(
  "between_id",
  "study_id",
  "mean_age",
  "percentage_female",
  "n_members",
  "group_description"
)

between_table_mandatory <- c(1, 1, 0, 0, 1, 1)

task_table_columns <- c(
  "task_id",
  "task_name",
  "task_description"
)

task_table_mandatory <- c(1, 1, 0)

condition_table_columns <- c(
  "condition_id",
  "dataset_id",
  "between_id",
  "within_id",
  "percentage_congruent",
  "percentage_neutral",
  "n_obs",
  "mean_obs_per_participant"
)

condition_table_mandatory <- c(1, 1, 1, 1, 
                               1, 1, 1, 1)

observation_table_columns <- c(
  "observation_id",
  "dataset_id",
  "subject",
  "block", 
  "trial",
  "between_id",
  "within_id",
  "condition_id",
  "congruency",
  "accruacy",
  "rt"
)

observation_table_mandatory <- c(1, 1, 1, 1,
                          1, 1, 1, 1, 
                          1, 1, 1)

table_info_db <- list(
  publication_table = data.frame(column = publication_table_columns, 
                           mandatory = publication_table_mandatory),
  study_table = data.frame(column = study_table_columns,
                     mandatory = study_table_mandatory),
  dataset_table = data.frame(column = dataset_table_columns,
                             mandatory = dataset_table_mandatory),
  within_table = data.frame(column = within_table_columns,
                            mandatory = within_table_mandatory),
  between_table = data.frame(column = between_table_columns,
                             mandatory = between_table_mandatory),
  condition_table = data.frame(column = condition_table_columns,
                               mandatory = condition_table_mandatory),
  observation_table = data.frame(column = observation_table_columns,
                          mandatory = observation_table_mandatory),
  task_table = data.frame(column = task_table_columns,
                          mandatory = task_table_mandatory)
)

rm(
  publication_table_columns,
  publication_table_mandatory,
  study_table_columns,
  study_table_mandatory,
  dataset_table_columns,
  dataset_table_mandatory,
  within_table_columns,
  within_table_mandatory,
  between_table_columns,
  between_table_mandatory,
  condition_table_columns,
  condition_table_mandatory,
  observation_table_columns,
  observation_table_mandatory,
  task_table_columns,
  task_table_mandatory
)

