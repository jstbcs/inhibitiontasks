# This is a library of functions used for structure testing and injecting new data 
# into the database
# Depends on: stringr, dbplyr, DBI, RSQLite

# Regex patterns
regex_matches_study_code <- "^[a-zA-Z]+_[12][0-9][0-9][0-9]_[a-zA-Z]+$"


# Require user input to confirm something
require_warning_input <- function(message){
  utils::menu(
    choices = c("Yes, I want to continue anyways", "No. That is not what I want"),
    title = paste0(message)
  )
}

continue_after_warning <- function(message){
  answer = require_warning_input(message)
  if (answer == 2)
  {
    stop("Process cancelled")
  } else
  {
    return(TRUE)
  }
}

# Loop check vector of column names in data frame
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

# requires user input
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
  else 
  {
    return(TRUE)
  }
}

which_element_wrong_study <- function(object){
  names = names(object)
  length = length(object)
  no_study = names[which(names != "study")]
  if (!"study" %in% names)
  {
    stop("Object needs to have a 'study' element")
  } 
  else if (!all(stringr::str_detect(names, "^data_[0-9]+$|study")))
  { # TODO: Maybe doesnt matter, but should give warning nonetheless
    error_name = names[which(stringr::str_detect(names, "^data_[0-9]+$|study", negate = TRUE))]
    error_message = paste(
      "Element-name:",
      error_name,
      "invalid.",
      "Elements can only be named 'study' or 'data_[NUMBER]"
    )
    stop(error_message)
  } 
  else if (!any(stringr::str_detect(no_study, "^data_[0-9]+$")))
  { 
    error_name = names
    error_message = paste(
      "Object must contain at least one data element named 'data_[NUMBER].",
      "Current names:",
      error_name
    )
    stop(error_message) 
  } 
  else if (any(duplicated(names)))
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
# Object entries - this is for checking the list name, if they contain proper structure
check_object_elements <- function(object){
  names = names(object)
  length = length(object)
  no_study = names[which(names != "study")]
  if(!all(names == c("study", paste0("data_", 1:(length-1)))))
  {
    which_element_wrong_study(object)
  }
  else
  {
    return(TRUE)
  }
}

# check study info structure
check_study_info_structure <- function(object){
  if(is.data.frame(object$study) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  else if (nrow(object$study) != 1){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 
  else if (do_elements_exist(c("study_code", "authors"), object$study) == FALSE){
    stop("Need to have variables: study_code, authors present")
  }
  else if (confirm_columns_not_specified(c("added", "conducted", "country", "contact"), object$study) == FALSE)
  {
    stop("Process cancelled")
  }
  else if (stringr::str_detect(get_study_code(object),
                               regex_matches_study_code,
                               negate = TRUE))
  {#does study code match regex pattern
    stop(paste("Study code:",
               get_study_code(object),
               "is not valid. Study codes should follow the principle AUTHOR_YEAR_FIRSTWORD"))
  }
  else {
    return(TRUE)
  }
}

# Make two: Check raw data, check overview structure

# Check data list structure
check_data_structure <- function(object){
  # Check if all other (not study) objects are lists containing two 1 items
  for (i in 2:length(object)){
    if(inherits(object[[i]], "list") == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    } else if(length(object[[i]]) != 2)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    } else if (do_elements_exist(c("data", "overview"), object[[i]]) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    } else if(is.data.frame(object[[i]]$data) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Raw data must be a data frame"))
    } else if(is.data.frame(object[[i]]$overview) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Overview must be a data frame"))
    } else if (do_elements_exist(c("subject", "trial", "accuracy", "rt", "block", "congruency", "age_group"), object[[i]]$data) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Raw data must have correct columns specified"))
    } else if (do_elements_exist(c("task_name", "keywords"), object[[i]]$overview) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Overview must have correct columns specified"))
    } 
    else if (confirm_columns_not_specified(c("mean_age", "percentage_male", "country", "contact"), object[[i]]$overview) == FALSE)
    {# TODO: Finish this, clean else conditions
      stop("Process cancelled")
    }  
    else
    {
      return(TRUE)
    }
  }
}

# Check object structure
check_object_structure <- function(object){
  if (inherits(object, "list") == FALSE)
  {
    stop("Object not a list")
  } 
  check_object_elements(object)
  check_study_info_structure(object)
  check_data_structure(object)
}


# Returns vector of study code specified in object
get_study_code <- function(object){
  study_code = c()
  study_code = object$study$study_code
  return(study_code)
}

# Checks to see if a given study_code exists in study-lookup table
does_study_id_exist <- function(conn, object){
  code = get_study_code(object)
  study_table = tbl(conn, "study")
  study_id = study_table %>% 
    filter(study_code == code) %>% 
    pull(study_id)
  length = length(study_id)
  if (length == 0){
    return(FALSE)
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This study code was already found twice in the database. Please investigate what went wrong here.")
  }
}

# Helper function, which columns exist?
# Returns elements of colnames that exist in specified object
which_elements_exist <- function(colnames, object){
  vec = c()
  for (i in seq_along(colnames))
  {
    vec[i] = exists(colnames[i], object)
  }
  return(colnames[which(vec == TRUE)])
}

# Create SQL insertion query
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
# Add a study variable to database
add_study_info <- function(conn, object){ # add more study variables here
  study_columns = c("study_code", "authors", "conducted", "added", "country", "contact")
  insert_study_info = object$study[which_elements_exist(study_columns, object$study)]
  return(
    dbWriteTable(
      conn = conn,
      name = "study",
      value = insert_study_info,
      append = TRUE
    )
  )
}

# Find study id of table for a given code
find_study_id <- function(conn, object){
  code = get_study_code(object)
  study_table = tbl(conn, "study")
  study_id = study_table %>% 
    filter(study_code == code) %>% 
    pull(study_id)
  return(study_id)
}

# Finds the study id in table of that code
return_study_id <- function(conn, object){
  if (does_study_id_exist(conn, object) == FALSE) 
  {
    continue_after_warning("This study code does not currently exist. Want to add it to the study-table?")
    # TODO: Make person confirm this? Show them the info that's added
    # do this via utils::menu
    # choices are Yes/No
    # title should output the current study info and maybe a representation of how it would look
    # after the study info is added
    add_study_info(conn, object)
    find_study_id(conn, object)
  } else 
  {
    find_study_id(conn, object)
  }
}

# Returns vector of task names found in inject object
get_task_names <- function(object){
  if (do_elements_exist(c("data", "overview"), object) == FALSE)
  {
    stop("This function takes the sub-lists: data_NUMBER as input.")
  }
  task_names = c()
  for (i in 2:length(object)){
    task_names[i - 1] = object$overview$task_name
  }
  return(task_names)
}
# Checks to see if a given task_name exists in task-lookup table
does_task_id_exist <- function(conn, object){
  names = get_task_names(object)
  task_table = tbl(conn, "task")
  vec = c()
  for (i in seq_along(names))
  {
    temp_task_name = names[i]
    task_id = task_table %>% 
      filter(task_name == temp_task_name) %>% 
      pull(task_id)
    length = length(task_id)
    if (length == 0){
      vec[i] = FALSE
      stop(paste("Task Name:", temp_task_name, "not found in database"))
    } else if (length == 1){
      vec[i] = TRUE
    } else {
      stop("This task name was already found twice in the database. Please investigate what went wrong here.")
    }
  }
  return(all(vec))
}

# Option to add new task?

# Find task id of table for a given code, returns data_frame of 
find_task_id <- function(conn, object){
  names = get_task_names(object)
  task_table = tbl(conn, "task")
  df = data.frame(task_id = c(), task_name = c())
  for (i in seq_along(names))
  {
    temp_task_name = names[i]
    task_id = task_table %>% 
      filter(task_name == temp_task_name) %>% 
      select(task_id, task_name) %>% 
      as_tibble()
    df = rbind(df, task_id)
  }
  return(df)
}

# Finds the task id in table of that code
return_task_id <- function(conn, object){
  does_task_id_exist(conn, object)
  find_task_id(conn, object)
}


# TODO: Add 'overwrite' option to enable overwriting existing data easily