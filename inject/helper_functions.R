# Helper functions to automatically check inserted data 

# Library calls
library(stringr)
library(dbplyr)

# Regex patterns
regex_matches_publication_code <- "^[a-zA-Z]+_[12][0-9][0-9][0-9]_[a-zA-Z]+$"
regex_matches_publication_names <- "^publication([1-9][0-9]?)$"
regex_matches_study_names <- "^study([1-9][0-9]?)$"
regex_matches_data_names <- "^data([1-9][0-9]?)$"

# Require user input to confirm something
# Returns the pressed key
require_warning_input <- function(message){
  utils::menu(
    choices = c("Yes, I want to continue anyways", "No. That is not what I want"),
    title = paste0(message)
  )
}

# Stop function if warning input is 'STOP'
continue_after_warning <- function(message){
  answer = require_warning_input(message)
  if (answer == 2)
  {
    stop("Process cancelled")
  }
}

# Loop check vector of column names in data frame
# Returns TRUE/FALSE if all column names are in that object
do_elements_exist <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
    if (!exists(colnames[i], object))
    {
      warning(paste("Column", colnames[i], "not present in data specified"))
    } 
  }
  return(all(vec))
}

# Informs user about columns not present in object
# Requires input to continue
confirm_columns_not_specified <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
  }
  if (all(vec) == FALSE)
  {
    message = paste(
      "Caution, you have not specified the columns: \n",
      paste(colnames[which(vec == FALSE)], collapse = "; "),
      "\ndo you want to continue adding the data anyway? \nNULL will be entered into the database in columns not specified"
    )
    continue_after_warning(message)
  }
}



# Helper stopping functions that make sure object is specified at the right depth
stop_if_not_top_level <- function(object){
  if (do_elements_exist(c("publication"), object) == FALSE)
  {
    stop("This function takes the overall list as input. \nMake sure the object passed to it is on highest level and not a data-sublist object")
  }
}

stop_if_not_publication_level <- function(object){
  if (!all(str_detect(colnames(object), regex_matches_study_names))){
    stop("This function takes a publication-level object")
  }
}

stop_if_not_study_level <- function(object){
  if(do_elements_exist(c("study_info", "group_info", "data1"), object) == FALSE)
  {
    stop("This function takes a study-level object")
  }
}

stop_if_not_data_level <- function(object){
  if (do_elements_exist(c("task", "overview", "data", "within"), object) == FALSE)
  {
    stop("This function takes a data-level object")
  }
}

stop_if_names_duplicated <- function(names){
  if (any(duplicated(names)))
  {
    error_name = names[which(duplicated(names))]
    error_message = paste(
      "Element-name:",
      error_name,
      "is not unique.",
      "Elements in object must be uniquely named."
    )
    stop(error_message)
  }
}

# Returns elements of colnames that exist in specified object
which_elements_exist <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
  }
  return(colnames[which(vec == TRUE)])
}

# Create SQL insertion query, not used in code right now. Returns insert query
write_sql_insert <- function(table, columns){
  n_cols = length(columns)
  insert = paste0(
    "INSERT INTO ",
    table,
    " (",
    paste(columns, collapse = ", "),
    ") ",
    "VALUES (",
    paste(rep("?", n_cols), collapse = ", "),
    ");"
  )
  return(insert)
}

# Return only elements that match a code
which_elements_match <- function(vector, regex){
  clean = vector[stringr::str_detect(vector, regex)]
  return(clean)
}


