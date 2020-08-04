add_space_at_start_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf(" %s", stringr::str_trim(string)),
         "")
}

add_cursive_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf("*%s*", stringr::str_trim(string)),
         "")
}

add_dot_at_end_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf(" %s.", stringr::str_trim(string)),
         "")
}

add_dot_at_start_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf(". %s", stringr::str_trim(string)),
         "")
}
add_semicolon_at_start_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf(", %s", stringr::str_trim(string)),
         "")
}

add_in_at_start_if_not_empty <- function (string) {
ifelse(string != "",
       sprintf(" *In:* %s", stringr::str_trim(string)),
       "")
}

add_curvy_brackets_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf(" (%s)", stringr::str_trim(string)),
         "")
}

add_doubledot_at_start_if_not_empty <- function (string) {
  ifelse(string != "",
         sprintf(": %s", stringr::str_trim(string)),
         "")
}

add_p_at_start_if_not_empty <- function(string) {
  ifelse(string != "",
         sprintf(" p %s", stringr::str_trim(string)),
         "")
}

add_pages <- function(string) {
  ifelse(string != "",
         stringr::str_extract(string, "[0-9]{1,5}") ,
         "")
}

add_book_pages <- function(string) {
  add_pages(string) %>%
  add_p_at_start_if_not_empty()
}

add_journal_name <- function(string) {
add_cursive_if_not_empty(string) %>%
  add_space_at_start_if_not_empty()
}

add_volume <- function(string) {
  add_space_at_start_if_not_empty(string)
}

add_issue <- function(string) {
  add_curvy_brackets_if_not_empty(string)
}

add_conference_name <- function(string) {
add_in_at_start_if_not_empty(string)
}

# abbreviate_author ------------------------------------------------------------
abbreviate_author <- function(x) {
  last_first <- strsplit(x, "\\s*,\\s*")[[1L]]

  if (length(last_first) > 1L) {
    last_first[2L] <- shorten_first_name(last_first[2L])
  }

  paste(last_first, collapse = " ")
}

# shorten_first_name -----------------------------------------------------------
shorten_first_name <- function(x) {
  paste(sapply(strsplit(x, "\\s+")[[1]], shorten_dashed_name), collapse = " ")
}

# shorten_dashed_name ----------------------------------------------------------
shorten_dashed_name <- function(x) {
  paste(sapply(strsplit(x, "-")[[1]], dot_after_first_char), collapse = "-")
}

# dot_after_first_char ---------------------------------------------------------
dot_after_first_char <- function(x) {
  paste0(substr(x, 1L, 1L), ".")
}


clean_editors <- function(string) {

  abbreviated <- lapply(strsplit(string, "\r"), function(x) {
    sapply(x, abbreviate_author, USE.NAMES = FALSE)
  }) %>%
    sapply(paste, collapse = "\r")

  sapply(abbreviated, function(eds) {

    editors <- stringr::str_split(string = eds,
                                pattern = "\r")[[1]] %>%
    stringr::str_remove(",") %>%
    stringr::str_trim()

  n <- length(editors)

  eds_clean <- ""

  if (n == 1) {
  sprintf("%s %s", editors, "[ed.]")
  } else if (n > 1) {
  sprintf("%s & %s %s",
            stringr::str_c(editors[seq_len(n-1)], collapse = ", "),
            editors[n],
            "[eds.]")
  } else {
    ""
  }
  })

}

add_book_editors <- function(string) {
  clean_editors(string) %>%
  add_in_at_start_if_not_empty()
}

add_book_title <- function(string) {
  add_semicolon_at_start_if_not_empty(string)
}

add_book_series <- function(string) {
  add_semicolon_at_start_if_not_empty(string)
}


add_doi <- function(string) {
  ifelse(string != "",
         sprintf(" [%s](https://doi.org/%s)",
                 stringr::str_trim(string),
                 stringr::str_trim(string)),
         "")
}

replace_carriage_return_with_semicolon_and_space <- function(string) {
  gsub("\r", ", ", string)
}

add_publishers <- function(string) {
  replace_carriage_return_with_semicolon_and_space(string)
}

add_book_publishers <- function(string) {
  add_publishers(string) %>%
  add_dot_at_start_if_not_empty()
}

get_reference_type <- function (endnote_db_refs, id) {
  is_sel_ref_type <- endnote_db_refs$reference_type %in% id
  endnote_db_refs[is_sel_ref_type,]
}



add_kwb_style_to_books <- function(endnote_db_refs) {
  ### books (reference_type == 1, i.e. id == 1)
  books <- get_reference_type(endnote_db_refs, id = 1)
  books$publication <- sprintf(
    "%s%s%s%s%s",
    add_book_pages(books$pages),
    add_dot_at_start_if_not_empty(books$secondary_title),
    add_book_publishers(books$publisher),
    add_dot_at_start_if_not_empty(books$place_published),
    add_doi(books$electronic_resource_number)
  )
  books
}


add_kwb_style_to_book_sections <- function(endnote_db_refs) {
  ### book_sections (reference_type == 7, i.e. id == 7)
  book_sections <- get_reference_type(endnote_db_refs, id = 7)
  book_sections$publication <- sprintf(
    "%s%s%s%s%s%s",
    add_book_pages(book_sections$pages),
    add_book_editors(book_sections$secondary_author),
    add_book_title(book_sections$secondary_title),
    add_book_publishers(book_sections$publisher),
    add_dot_at_start_if_not_empty(book_sections$place_published),
    add_doi(book_sections$electronic_resource_number)
  )
  book_sections
}

add_kwb_style_to_theses <- function(endnote_db_refs) {
  ### thesis (reference_type == 2, i.e. id == 2)
  theses <- get_reference_type(endnote_db_refs, id = 2)
  theses$publication <- sprintf(
    "%s%s%s%s",
    add_dot_at_end_if_not_empty(theses$type_of_work),
    add_dot_at_end_if_not_empty(theses$secondary_title),
    add_space_at_start_if_not_empty(theses$publisher),
    add_doi(theses$electronic_resource_number)
  )
  theses
}

add_kwb_style_to_conferences <- function(endnote_db_refs) {
  ### conference_proceedings (reference_type == 3, i.e. id == 3)
  ### conference_paper (reference_type == 33, i.e. id == 33)
  conf <- get_reference_type(endnote_db_refs, id = c(3,33))
  conf$publication <- sprintf(
    "%s%s%s%s",
    add_book_pages(conf$pages),
    add_conference_name(conf$secondary_title),
    add_dot_at_start_if_not_empty(conf$place_published),
    add_dot_at_start_if_not_empty(conf$date),
    add_doi(conf$electronic_resource_number)
  )
  conf
}

add_kwb_style_to_journals <- function(endnote_db_refs) {

### journal_papers (reference_type == 0, i.e. id == 0)
papers <- get_reference_type(endnote_db_refs, id = 0)

papers$publication <- sprintf(
  "%s%s%s%s%s",
  add_journal_name(papers$secondary_title),
  add_volume(papers$volume),
  add_issue(papers$number),
  add_doubledot_at_start_if_not_empty(papers$pages),
  add_doi(papers$electronic_resource_number)
)
papers
}


add_kwb_style_to_reports <- function(endnote_db_refs) {

  ### reports (reference_type == 10, i.e. id == 10)
  reports <- get_reference_type(endnote_db_refs, id = 10)

  reports$publication <- sprintf(
    "%s%s",
    add_publishers(reports$publisher),
    add_doi(reports$electronic_resource_number)
  )
  reports
}

#' add_kwb_style
#'
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [read_endnote_db])
#'
#' @return Endnotes "refs" table with added columns "reference_type_name"
#' (translating "reference_type" to "own" classes) and "publication" (used for
#' hugo-academic)
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' endnote_db <- read_endnote_db(path = "../../dms/2020-07-08/KWB-documents_20191205.Data/sdb/sdb.eni")
#' kwb_style_list <- add_kwb_style(endnote_db$refs)
#' }
#'
add_kwb_style <- function(endnote_db_refs) {


  kwb_style_list <- list(books = add_kwb_style_to_books(endnote_db_refs),
      book_sections = add_kwb_style_to_book_sections(endnote_db_refs),
      conferences = add_kwb_style_to_conferences(endnote_db_refs),
      journals = add_kwb_style_to_journals(endnote_db_refs),
      reports = add_kwb_style_to_reports(endnote_db_refs),
      theses = add_kwb_style_to_theses(endnote_db_refs)
 )

  dplyr::bind_rows(kwb_style_list, .id = "reference_type_name")


}



if(FALSE) {

endnote_db <- read_endnote_db(path = "../../dms/2020-07-08/KWB-documents_20191205.Data/sdb/sdb.eni")
kwb_style_list <- add_kwb_style(endnote_db$refs)


}
