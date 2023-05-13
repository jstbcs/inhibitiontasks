which_table_is_queried <- function(sql_argument){
  table = stringr::str_extract(sql_argument, "(?<=FROM\\s)\\w+")
  return(table)
}
