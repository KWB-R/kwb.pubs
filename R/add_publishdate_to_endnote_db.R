#' add_publishdate_to_endnote_db
#'
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [read_endnote_db])
#'
#' @return Endnotes "refs" table with added columns "publish_datetime",
#' "publishDate" (required for hugo-academic) and "date-cleaned"
#' @export
#' @importFrom lubridate with_tz ymd
#' @importFrom tibble as_tibble
#' @importFrom dplyr if_else mutate
#' @importFrom stringr str_detect str_trim
#' @examples
#' \dontrun{
#' endnote_db <- read_endnote_db(path = "../../dms/2020-07-08/KWB-documents_20191205.Data/sdb/sdb.eni")
#' en_db_with_publishdates <- add_publishdate_to_endnote_db(endnote_db$refs)
#' }
#'
add_publishdate_to_endnote_db <- function(endnote_db_refs) {
    endnote_db_refs %>%
    tibble::as_tibble() %>%
    ## + 3600s (fix as Endnote exports are otherwise 1h before "real" export time)
    dplyr::mutate(publish_datetime = as.POSIXct(as.POSIXct("1970-01-01 00:00:00") + .data$record_last_updated + 3600, tz = "CEST") %>%
                    lubridate::with_tz(tzone = "UTC"),
                  publishDate = sprintf("%sT%sZ",
                                        format(.data$publish_datetime, "%Y-%m-%d"),
                                        format(.data$publish_datetime, "%H:%M:%S")),
                  date_cleaned = dplyr::if_else(!is.na(lubridate::ymd(.data$date)),
                                                as.character(lubridate::ymd(.data$date)),
                                                dplyr::if_else(stringr::str_detect(.data$year,
                                                                                   "[1-2][0-9][0-9][0-9]"),
                                                               sprintf("%s-01-01",
                                                                       stringr::str_trim(.data$year)),
                                                               "")))
}
