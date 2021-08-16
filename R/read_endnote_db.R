#' Read Endnote DB Tables
#'
#' @param path full path to Endnote database ("sdb.eni")
#'
#' @return list with all tables contained in Endnote Databse
#' @export
#' @importFrom DBI dbConnect dbListTables dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom stats setNames
read_endnote_db <- function(path)
{
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))

  lapply(
    X = stats::setNames(nm = DBI::dbListTables(con)),
    FUN = DBI::dbReadTable,
    con = con
  )
}
