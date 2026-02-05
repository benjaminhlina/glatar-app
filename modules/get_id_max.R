

id_max <- function(table_name, id_col) {
  result <- DBI::dbGetQuery(
    con,
    glue::glue("SELECT COALESCE(MAX({id_col}), 0) AS max_id FROM {table_name}")
  )
  selected_id_max <- result$max_id
  return(selected_id_max)
}
