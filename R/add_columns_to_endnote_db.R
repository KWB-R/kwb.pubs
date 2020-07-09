#' add_columns_to_endnote_db
#'
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [read_endnote_db])
#'
#' @return Endnote table with added columns using [add_publishdate_to_endnote_db]
#' and [add_kwb_style_to_reports]
#' @export

add_columns_to_endnote_db <- function(endnote_db_refs) {
  add_publishdate_to_endnote_db(endnote_db_refs) %>%
  add_kwb_style_to_reports()
}
