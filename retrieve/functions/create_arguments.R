# These functions serve to create arguments

add_argument <- function(list, conn, variable, operator, values){
  list[[length(list) + 1]] = make_valid_sql(conn, variable, operator, values)
  return(list)
}


make_valid_sql <- function(conn, variable, operator, values){
  # First find out which table the variable is from
  table = find_relevant_tables(conn, variable)
  id_name = return_id_name_from_table(table)
  
  # Then turn this into a statement
  # Check validity of operator
  check_operator(operator, values)
  
  # if values is a char, surround with ''
  if (all(is.character(values))){
    values = paste0("'", values, "'")
  }
  
  if (operator == "greater"){
    sql_statement = paste(
      "SELECT",
      id_name, 
      "FROM",
      table,
      " ",
      "WHERE",
      variable, 
      ">",
      values[1]
    )
  }
  
  if (operator == "less"){
    sql_statement = paste(
      "SELECT",
      id_name, 
      "FROM",
      table,
      "WHERE",
      variable, 
      "<",
      values[1]
    )
  }
  
  if (operator == "equal"){
    sql_statement = paste(
      "SELECT",
      id_name, 
      "FROM",
      table,
      "WHERE",
      variable, 
      "=",
      values[1]
    )
  }
  
  if (operator == "between"){
    sql_statement = paste(
      "SELECT",
      id_name, 
      "FROM",
      table,
      "WHERE",
      variable, 
      ">",
      values[1],
      "AND",
      variable,
      "<",
      values[2]
    )
  }
  return(sql_statement)
}

check_operator <- function(operator, values){
  valid_operators = c("less", "greater", "between", "equal")
  if (!operator %in% valid_operators){
    msg = paste0("Operator can only take the following values: ", valid_operators)
    stop(msg)
  }
  
  if (length(values) > 2){
    msg = "Please only provide a maximum of 2 values"
    stop(msg)
  }
  
  if (length(values) == 2 & (values[1] >= values[2])){
    msg = "When providing two values, the first must be smaller than the second"
    stop(msg)
  }
  
  if (operator == "between" & length(values) != 2){
    msg = "When using the 'between' operator, please provide two values"
    stop(msg)
  }
  
  if (operator != "between" & length(values) != 1){
    msg = "When suing 'less', 'greater' or 'equal', provide only one value"
    stop(msg)
  }
  
}
