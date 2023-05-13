# Library
library(dplyr)
library(RSQLite)
library(DBI)

# Getting info on lists
data_folder_path = "./Create_db/add_data/"

list_files = list.files(data_folder_path, "(list).*(RData)", full.names = TRUE)

lists = vector(mode = "list", length = length(list_files))

for (i in seq_along(list_files)){
  lists[[i]] = readRDS(list_files[i])
}

names(lists) = paste0("publication", seq_along(list_files))

# Now checking all structures

# Sourcing scripts
source("./inject/source_testing_scripts.R")

# Testing list items
check_overall_structure(lists)

# Source adding scripts
source("./inject/source_adding_scripts.R")

# Adding lists
db_conn = DBI::dbConnect(RSQLite::SQLite(), "testing.db")

add_object(db_conn, lists)
