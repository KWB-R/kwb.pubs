#' Read Endnote DB Tables
#'
#' @param path full path to Endnote database ("sdb.eni")
#'
#' @return list with all tables contained in Endnote Databse
#' @export
#' @importFrom DBI dbConnect dbListTables dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom stats setNames
read_endnote_db <- function(path) {

  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  table_names <- DBI::dbListTables(con)

  contents <- lapply(stats::setNames(nm = table_names), DBI::dbReadTable, con = con)

  DBI::dbDisconnect(con)

  contents
}
