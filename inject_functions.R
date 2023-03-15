# This is a library of functions used for structure testing and injecting new data 
# into the database
# Depends on: stringr, dbplyr, DBI, RSQLite, utils
library(stringr)

# Regex patterns
regex_matches_publication_code <- "^[a-zA-Z]+_[12][0-9][0-9][0-9]_[a-zA-Z]+$"
regex_matches_study_names <- "^study([1-9][0-9]?)$"

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

# This function checks whether element on study level contains:
# 1. "study_info"
# 2. "group_info"
# 3. at least one data[NUMBER] element, e.g. data1
which_element_wrong_study <- function(object){
  names = names(object)
  length = length(object)
  no_study = names[which(names != "study")]
  if (!"study_info" %in% names)
  {
    stop("Object needs to have a 'study_info' element")
  } 
  if (!"group_info" %in% names)
  {
    stop("Object needs to have a 'group_info' element")
  }
  
  if (!all(stringr::str_detect(names, "^data_[0-9]+$|study_info|group_info")))
  {
    error_name = names[which(stringr::str_detect(names, "^data_[0-9]+$|study", negate = TRUE))]
    error_message = paste(
      "Element-name:",
      error_name,
      "invalid.",
      "Elements can only be named 'study' or 'data_[NUMBER]"
    )
    stop(error_message)
  } 
  if (!any(stringr::str_detect(no_study, "^data_[0-9]+$")))
  { 
    error_name = names
    error_message = paste(
      "Object must contain at least one data element named 'data_[NUMBER].",
      "Current names:",
      error_name
    )
    stop(error_message) 
  } 
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

# Helper stopping functions that make sure object is specified at the right depth
stop_if_not_top_level <- function(object){
  if (do_elements_exist(c("publication"), object) == FALSE)
  {
    stop("This function takes the overall list as input. \nMake sure the object passed to it is on highest level and not a data-sublist object")
  }
}

stop_if_not_study_level <- function(object){
  if (!all(str_detect(colnames(object), regex_matches_study_names))){
    stop("This function takes the study-specific info as input. \nMake sure the object is passed to it on study-level.")
  }
}

stop_if_not_data_level <- function(object){
  if (do_elements_exist(c("data", "overview"), object) == FALSE)
  {
    stop("This function takes the sub-lists: data_NUMBER as input. \nMake sure the object passed to it is on data-level and not the grand object")
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
}

# check study info structure
check_study_info_structure <- function(object){
  if(is.data.frame(object$study) == FALSE)
  {# check study info is data frame
    stop("Study-Info is not a dataframe")
  } 
  if (nrow(object$study) != 1){# check the number of rows in that dataframe, should be 1
    stop("Study-Info contains more than one row")
  } 
  if (do_elements_exist(c("study_code", "authors"), object$study) == FALSE){
    stop("Need to have variables: study_code, authors present")
  }
  
  confirm_columns_not_specified(c("added", "conducted", "country", "contact"), object$study)
  
  if (stringr::str_detect(get_study_code(object),
                               regex_matches_publication_code,
                               negate = TRUE))
  {#does study code match regex pattern
    stop(paste("Study code:",
               get_study_code(object),
               "is not valid. Study codes should follow the principle AUTHOR_YEAR_FIRSTWORD"))
  }
}

# Make two: Check raw data, check overview structure

# Check data list structue
check_data_structure <- function(object){
  # Check if all other (not study) objects are lists containing two 1 items
  overview_names = c("task_name", "keywords", "data_exclusions", "codebook", 
                     "n_participants", "n_blocks", "n_trials", 
                     "mean_effect", "sd_effect", "neutral_trials",
                     "percentage_incongruent", "feedback",
                     "fixation_cross", "time_limit",
                     "mean_age", "percentage_female", "percentage_male"
                     )
  for (i in 2:length(object)){
    if(inherits(object[[i]], "list") == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    }
    if(length(object[[i]]) != 2)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    }
    if (do_elements_exist(c("data", "overview"), object[[i]]) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Data entry in object must be a list of two elements (data, overview)"))
    } 
    if(is.data.frame(object[[i]]$data) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Raw data must be a data frame"))
    } 
    if(is.data.frame(object[[i]]$overview) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Overview must be a data frame"))
    } 
    if (do_elements_exist(c("subject", "trial", "accuracy", "rt", "block", "congruency", "age_group"), object[[i]]$data) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Raw data must have correct columns specified"))
    } 
    if (do_elements_exist(c("task_name", "keywords"), object[[i]]$overview) == FALSE)
    {
      stop(paste0("Error in structure of data_", i - 1, ":", " Overview must have correct columns specified"))
    } 
    confirm_columns_not_specified(overview_names, object[[i]]$overview)
  } # TODO: Talk about which elemens have to exist in overview, and which are optional
  # Right now all columns in $data are required, but only task_name and keywords for overview
}

# Check object structure, parent function
check_object_structure <- function(object){
  stop_if_not_top_level(object)
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
  stop_if_not_top_level(object) # defined below
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

# Add a study variable to database
add_study_info <- function(conn, object){ # add more study variables here
  stop_if_not_top_level(object)
  study_columns = c("study_code", "authors", "conducted", "added", "country", "contact")
  insert_study_info = object$study[which_elements_exist(study_columns, object$study)]
  dbWriteTable(
    conn = conn,
    name = "study",
    value = insert_study_info,
    append = TRUE
  )
}

# Find study id of table for a given code
find_study_id <- function(conn, object){
  stop_if_not_top_level(object)
  code = get_study_code(object)
  study_table = tbl(conn, "study")
  study_id = study_table %>% 
    filter(study_code == code) %>% 
    pull(study_id)
  return(study_id)
}

# Finds the study id in table of that code
return_study_id <- function(conn, object){
  stop_if_not_top_level(object)
  if (does_study_id_exist(conn, object) == FALSE) 
  {
    continue_after_warning("This study code does not currently exist. Want to add it to the study-table?")
    add_study_info(conn, object)
    find_study_id(conn, object)
  } else 
  {
    find_study_id(conn, object)
  }
}

# Returns vector of task names found in inject object
get_task_names <- function(object){
  stop_if_not_data_level(object)
  task_names = c()
  for (i in 2:length(object)){
    task_names[i - 1] = object$overview$task_name
  }
  return(task_names)
}
# Checks to see if a given task_name exists in task-lookup table
# Returns TRUE/FALSE if task ID exsits
does_task_id_exist <- function(conn, object){
  name = get_task_names(object)
  task_table = tbl(conn, "task")
  task_id = task_table %>% 
    filter(task_name == name) %>% 
    pull(task_id)
  length = length(task_id)
  if (length == 0){
    return(FALSE)
    stop(paste("Task Name:", name, "not found in database"))
  } else if (length == 1){
    return(TRUE)
  } else {
    stop("This task name was already found twice in the database. Please investigate what went wrong here.")
  }
}

# Option to add new task?
# TODO

# Find task id of table for a given code, returns data_frame of 
find_task_id <- function(conn, object){
  name = get_task_names(object)
  if (length(name) != 1)
  {
    error_message = paste0(
      "get_task_names returned more than 1 output, suggesting more than one entry in .$overview$task_name. Please go investigate"
    )
    stop(error_message)
  }
  task_table = tbl(conn, "task")
  task_id = task_table %>% 
    filter(task_name == name) %>% 
    pull(task_id)
  return(task_id)
}

# Finds the task id in table of that code
return_task_id <- function(conn, object){
  if (does_task_id_exist(conn, object) == FALSE)
  {
    stop("This task was not found in our task-database. Please considers adding it")
  }
  find_task_id(conn, object)
}

# Returns the next free elements in overview table as data_ids that are assigned
return_next_free_data_id <- function(conn){
  overview = tbl(conn, "data_overview")
  last_id = overview %>% 
    summarize(max = max(data_id)) %>% 
    pull(max)
  if (is.na(last_id)){
    last_id = 0
  }
  next_free = last_id + 1
  return(next_free)
}

add_data_id <- function(conn, object){
  stop_if_not_data_level(object)
  data_id = return_next_free_data_id(conn) - 1 # because we want the id of the last added overview
  object$data$data_id = data_id
  return(object)
}

# add the returned task id and study id to overview column
add_study_id_overview <- function(conn, object){
  stop_if_not_top_level(object)
  study_id = return_study_id(conn, object)
  names = names(object)[which(names(object) != "study")]
  for (i in names)
  {
    object[[i]]$overview$study_id = study_id
  }
  return(object)
}

add_task_id_overview <- function(conn, object){
  stop_if_not_data_level(object)
  task_id = return_task_id(conn, object)
  object$overview$task_id = task_id
  return(object)
}

prepare_object_ids <- function(conn, object){
  stop_if_not_top_level(object)
  n_data = length(object) - 1
  names = paste0("data_", 1:n_data)
  study_add = add_study_id_overview(conn, object)
  for (i in names)
  {
    study_add[[i]] = add_task_id_overview(conn, study_add[[i]])
  }
  return(study_add)
}

add_overview_table <- function(conn, object){
  stop_if_not_data_level(object)
  if (do_elements_exist(c("study_id", "task_id"), object$overview) == FALSE)
  {
    stop("Variables 'study_id' and 'task_id' need to be present in overview")
  }
  overview_columns = c("study_id", "task_id", "keywords", "data_exclusions", "codebook", 
                     "n_participants", "n_blocks", "n_trials", 
                     "mean_effect", "sd_effect", "neutral_trials",
                     "percentage_incongruent", "feedback",
                     "fixation_cross", "time_limit",
                     "mean_age", "percentage_female", "percentage_male"
  )
  overview_inject = object$overview[which_elements_exist(overview_columns, object$overview)]
  dbWriteTable(
    conn = conn,
    name = "data_overview",
    value = overview_inject,
    append = TRUE
  )
}

add_raw_data_table <- function(conn, object){
  stop_if_not_data_level(object)
  if (do_elements_exist("data_id", object$data) == FALSE)
  {
    stop("Variable 'data_id' needs to be present in data")
  }
  data_columns = c("data_id", "rt", "accuracy", "congruency",
                   "subject", "block", "trial", "age_group")
  data_inject = object$data[which_elements_exist(data_columns, object$data)]
  dbWriteTable(
    conn = conn,
    name = "data",
    value = data_inject,
    append = TRUE
  )
}

add_object_to_database <- function(conn, object){
  stop_if_not_top_level(object)
  
  n_data = length(object) - 1
  
  data_names = paste0("data_", 1:n_data)
  
  check_object_structure(object)
  
  prep = prepare_object_ids(conn, object)
  
  for (i in data_names)
  {
    free_data_id = return_next_free_data_id(conn)
    add_overview_table(conn, prep[[i]])
    
    if (free_data_id != return_next_free_data_id(conn) - 1)
    {
      stop("Looks like there was no overview row added. Check the SQL Database")
    }
    prep[[i]] = add_data_id(conn, prep[[i]])
    add_raw_data_table(conn, prep[[i]])
  }
}

# TODO: Add 'overwrite' option to enable overwriting existing data easily
# TODO: Have unique constraints for data_overview so that something won't get added twice even after executing same command
