library(DBI)
library(RSQLite)
library(dplyr)
conn = DBI::dbConnect(RSQLite::SQLite(), "initial_db.db")

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
    variable = "percentage_female",
    operator = "between",
    values = c(0.2, 0.8)
  ) %>% 
  add_argument(
    ., 
    conn = conn,
    variable = "task_name",
    operator = "equal",
    values = "stroop"
  ) %>% 
  add_argument(
    .,
    conn = conn,
    variable = "task_name",
    operator = "equal",
    values = "flanker"
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
  arguments,
  argument_relation = c(1, 2, 3, 3, 4)
)
