# Functions to insert things into the db
# Column names
add_table <- function(conn, table, type){
  # Depending on the type, read out the column names from name list
  possible_cols = column_names_db[[type]]
  
  insert = table[which_elements_exist(possible_cols, table)]
  
  dbWriteTable(
    conn = conn,
    name = type,
    value = insert,
    append = TRUE
  )
  
  print(paste("Added to", type, "table"))
}
