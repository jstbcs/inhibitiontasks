# inhibitiontasks data base 2023

This repo contains all code required for creating and maintaining the SQLite data base storing inhibition.

## Create_db
This folder contains scripts to create the initial database in R (using the DBI package) in `create_tables.Rmd`, as well as an add_data subfolder. It stores all list objects required to read the first 40 datasets into the data base and the R script `adding_list_to_db.R` which inserts them into the data base. 
The datasets themselves were formatted in the `reformat_datasets.R` script. Note that we created nested a nested list object for each publication in the `scripts_creating_list_objects` subfolder. The nested list structure looks as follows: 
**
It allows us to systematically read in data, making sure that all primary and foreign keys are correctly assigned. 
The nested list objects themselves can be found in `add_data` as well. 

## data
Holds all raw data sets included in the data base. The maintainer of this project should make sure that datasets newly added to the db are also strored in this folder on github. 

## helper_functions
Stores scripts for functions both used to insert data into the data base, as well as retrieve it. 

## inject 
Contains all scripts used in the process of adding datasets to the data base. 

## retrieve
Scripts in this folder serve as the base of our R package which can be used to retrieve data from the db, based on selected criteria. 
