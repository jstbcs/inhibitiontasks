# Library Calls
library(dplyr)
library(dbplyr)
library(DBI)
library(RSQLite)
library(lubridate)

# Initialize sqlite db
con <-  dbConnect(RSQLite::SQLite(), "pilot.db")


