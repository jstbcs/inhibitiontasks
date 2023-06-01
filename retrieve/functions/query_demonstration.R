library(DBI)
library(RSQLite)
library(dplyr)
conn = DBI::dbConnect(RSQLite::SQLite(), "initial_db.db")
source("./retrieve/functions/source_query_scripts.R")
arguments = list()

arguments %<>% 
  add_argument(
    ., 
    conn = conn,
    variable = "accuracy",
    operator = "equal",
    values = c(1)
  ) %>% 
  add_argument(
    .,
    conn = conn,
    variable = "n_participants",
    manual = TRUE,
    statement = "SELECT dataset_id FROM dataset_table WHERE n_participants > 500"
  ) %>% 
  add_argument(
    .,
    conn = conn,
    variable = "percentage_female",
    operator = "between",
    values = c(0.2, 0.8)
  ) %>% 
  add_argument(
    ., 
    conn = conn,
    variable = "task_name",
    operator = "equal",
    values = c("stroop", "flanker")
  ) %>% 
  add_argument(
    .,
    conn = conn,
    variable = "n_tasks",
    operator = "greater",
    values = c(4)
  ) 

test = query_db(
  conn,
  arguments
)

test_df = merge_query_results(test)

