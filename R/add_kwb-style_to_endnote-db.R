fmts <- list(
  space_at_start = " %s",
  italics = "*%s*",
  dot_at_end = " %s.",
  dot_at_start = ". %s",
  comma_at_start = ", %s",
  in_at_start = " *In:* %s",
  in_parentheses = " (%s)",
  colon_at_start = ": %s",
  p_at_start = " p %s"
)

add_pages <- function(string) {
  ifelse(string != "",
         stringr::str_extract(string, "[0-9]{1,5}"),
         "")
}

add_book_pages <- function(string) {
  string %>%
    add_pages() %>%
    format_given(fmts$p_at_start)
}

add_journal_name <- function(string) {
  string %>%
    format_given(fmts$italics) %>%
    format_given(fmts$space_at_start)
}

add_volume <- function(string) {
  string %>%
    format_given(fmts$space_at_start)
}

add_issue <- function(string) {
  string %>%
    format_given(fmts$in_parentheses)
}

add_conference_name <- function(string) {
  string %>%
    format_given(fmts$in_at_start)
}

# abbreviate_author ------------------------------------------------------------
abbreviate_author <- function(x) {
  last_first <- split_one(x, "\\s*,\\s*")

  if (length(last_first) > 1L) {
    last_first[2L] <- shorten_first_name(last_first[2L])
  }

  space_collapsed(last_first)
}

# shorten_first_name -----------------------------------------------------------
shorten_first_name <- function(x) {
  space_collapsed(apply_to_split(x, "\\s+", shorten_dashed_name))
}

# shorten_dashed_name ----------------------------------------------------------
shorten_dashed_name <- function(x) {
  dash_collapsed(apply_to_split(x, "-", dot_after_first_char))
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
  string %>%
    clean_editors() %>%
    format_given(fmts$in_at_start)
}

add_book_title <- function(string) {
  string %>%
    format_given(fmts$comma_at_start)
}

add_book_series <- function(string) {
  string %>%
    format_given(fmts$comma_at_start)
}

add_doi <- function(string) {
  s <- stringr::str_trim(string)
  ifelse(s != "", sprintf(" [%s](https://doi.org/%s)", s, s), "")
}

replace_carriage_return_with_semicolon_and_space <- function(string) {
  gsub("\r", ", ", string)
}

add_publishers <- function(string) {
  replace_carriage_return_with_semicolon_and_space(string)
}

add_book_publishers <- function(string) {
  add_publishers(string) %>%
    format_given(fmts$dot_at_start)
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
    format_given(books$secondary_title, fmts$dot_at_start),
    add_book_publishers(books$publisher),
    format_given(books$place_published, fmts$dot_at_start),
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
    format_given(conf$place_published, fmts$dot_at_start),
    add_doi(book_sections$electronic_resource_number)
  )

  book_sections
}

add_kwb_style_to_theses <- function(endnote_db_refs) {

  ### thesis (reference_type == 2, i.e. id == 2)
  theses <- get_reference_type(endnote_db_refs, id = 2)

  theses$publication <- sprintf(
    "%s%s%s%s",
    format_given(theses$type_of_work, fmts$dot_at_end),
    format_given(theses$secondary_title, fmts$dot_at_end),
    format_given(theses$publisher, fmts$space_at_start),
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
    format_given(conf$place_published, fmts$dot_at_start),
    format_given(conf$date, fmts$dot_at_start),
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
    format_given(papers$pages, fmts$colon_at_start),
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

  kwb_style_list <- list(
    books = add_kwb_style_to_books(endnote_db_refs),
    book_sections = add_kwb_style_to_book_sections(endnote_db_refs),
    conferences = add_kwb_style_to_conferences(endnote_db_refs),
    journals = add_kwb_style_to_journals(endnote_db_refs),
    reports = add_kwb_style_to_reports(endnote_db_refs),
    theses = add_kwb_style_to_theses(endnote_db_refs)
  )

  dplyr::bind_rows(kwb_style_list, .id = "reference_type_name")
}

if (FALSE)
{
  endnote_db <- read_endnote_db(path = "../../dms/2020-07-08/KWB-documents_20191205.Data/sdb/sdb.eni")
  kwb_style_list <- add_kwb_style(endnote_db$refs)
}
