# Library
library(dplyr)
library(RSQLite)
library(DBI)

# Getting info on lists -------
# NOTE: adjust this code to select only those list objects you want to newly add to the db
data_folder_path = "./Create_db/add_data/"

list_files = list.files(data_folder_path, "(list).*(RData)", full.names = TRUE)

lists = vector(mode = "list", length = length(list_files))

for (i in seq_along(list_files)){
  lists[[i]] = readRDS(list_files[i])
}

names(lists) = paste0("publication", seq_along(list_files))


# Adding lists ------

# Source adding scripts
source("./inject/source_adding_scripts.R")


db_conn = DBI::dbConnect(RSQLite::SQLite(), "inhibitiontasks.db")

add_object(db_conn, lists)
