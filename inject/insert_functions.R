# Functions to insert things into the db
# Column names
add_table <- function(conn, table, type){
  # conn is connection
  # Table is data
  # Type is the table name in db
  # Depending on the type, read out the column names from name list
  
  # Only read possible cols
  possible_cols = table_info_db[[type]]
  
  insert = table[which_elements_exist(possible_cols, table)]
  
  dbWriteTable(
    conn = conn,
    name = type,
    value = insert,
    append = TRUE
  )
  
  print(paste("Added to", type, "table"))
}
