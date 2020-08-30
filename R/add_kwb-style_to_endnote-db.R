# fmts -------------------------------------------------------------------------
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

# add_pages --------------------------------------------------------------------
add_pages <- function(string) {
  ifelse(
    string != "",
    stringr::str_extract(string, "[0-9]{1,5}"),
    ""
  )
}

# add_book_pages ---------------------------------------------------------------
add_book_pages <- function(string) {
  string %>%
    add_pages() %>%
    format_given(fmts$p_at_start)
}

# add_journal_name -------------------------------------------------------------
add_journal_name <- function(string) {
  string %>%
    format_given(fmts$italics) %>%
    format_given(fmts$space_at_start)
}

# add_volume -------------------------------------------------------------------
add_volume <- function(string) {
  string %>%
    format_given(fmts$space_at_start)
}

# add_issue --------------------------------------------------------------------
add_issue <- function(string) {
  string %>%
    format_given(fmts$in_parentheses)
}

# add_conference_name ----------------------------------------------------------
add_conference_name <- function(string)
{
  string %>%
    format_given(fmts$in_at_start)
}

# abbreviate_author ------------------------------------------------------------
abbreviate_author <- function(x)
{
  last_first <- split_one(x, "\\s*,\\s*")

  if (length(last_first) > 1L) {
    last_first[2L] <- shorten_first_name(last_first[2L])
  }

  space_collapsed(last_first)
}

# shorten_first_name -----------------------------------------------------------
shorten_first_name <- function(x)
{
  space_collapsed(apply_to_split(x, "\\s+", shorten_dashed_name))
}

# shorten_dashed_name ----------------------------------------------------------
shorten_dashed_name <- function(x)
{
  dash_collapsed(apply_to_split(x, "-", dot_after_first_char))
}

# dot_after_first_char ---------------------------------------------------------
dot_after_first_char <- function(x)
{
  paste0(substr(x, 1L, 1L), ".")
}

# clean_editors ----------------------------------------------------------------
clean_editors <- function(string)
{
  abbreviated <- lapply(strsplit(string, "\r"), function(x) {
    sapply(x, abbreviate_author, USE.NAMES = FALSE)
  }) %>%
    sapply(paste, collapse = "\r")

  sapply(abbreviated, function(eds) {

    editors <- split_one(eds, "\r") %>%
      stringr::str_remove(",") %>%
      stringr::str_trim()

    n <- length(editors)

    if (n == 0L)
      return("")

    if (n == 1L)
      return(sprintf("%s %s", editors, "[ed.]"))

    # n > 1L
    sprintf(
      "%s & %s %s",
      stringr::str_c(editors[seq_len(n - 1L)], collapse = ", "),
      editors[n],
      "[eds.]"
    )
  })
}

# add_book_editors -------------------------------------------------------------
add_book_editors <- function(string)
{
  string %>%
    clean_editors() %>%
    format_given(fmts$in_at_start)
}

# add_book_title ---------------------------------------------------------------
add_book_title <- function(string)
{
  string %>%
    format_given(fmts$comma_at_start)
}

# add_book_series --------------------------------------------------------------
add_book_series <- function(string)
{
  string %>%
    format_given(fmts$comma_at_start)
}

# add_doi ----------------------------------------------------------------------
add_doi <- function(string)
{
  s <- stringr::str_trim(string)
  ifelse(s != "", sprintf(" [%s](https://doi.org/%s)", s, s), "")
}

# replace_carriage_return_with_semicolon_and_space -----------------------------
replace_carriage_return_with_semicolon_and_space <- function(string)
{
  gsub("\r", ", ", string)
}

# add_publishers ---------------------------------------------------------------
add_publishers <- function(string)
{
  replace_carriage_return_with_semicolon_and_space(string)
}

# add_book_publishers ----------------------------------------------------------
add_book_publishers <- function(string)
{
  add_publishers(string) %>%
    format_given(fmts$dot_at_start)
}

# get_reference_type -----------------------------------------------------------
get_reference_type <- function (endnote_db_refs, id)
{
  is_sel_ref_type <- endnote_db_refs$reference_type %in% id
  endnote_db_refs[is_sel_ref_type, ]
}

# reference_type_ids -----------------------------------------------------------
reference_type_ids <- list(
  book = 1L,
  book_section = 7L,
  conference = c(3L, 33L),
  journal = 0L,
  report = 10L,
  thesis = 2L
)

# add_kwb_style_to -------------------------------------------------------------
add_kwb_style_to <- function(
  endnote_db_refs,
  type = names(reference_type_ids)
)
{
  type <- match.arg(type)

  x <- get_reference_type(endnote_db_refs, id = reference_type_ids[[type]])

  x$publication <- if (type == "book") {

    sprintf(
      "%s%s%s%s%s",
      add_book_pages(x$pages),
      format_given(x$secondary_title, fmts$dot_at_start),
      add_book_publishers(x$publisher),
      format_given(x$place_published, fmts$dot_at_start),
      add_doi(x$electronic_resource_number)
    )

  } else if (type == "book_section") {

    sprintf(
      "%s%s%s%s%s%s",
      add_book_pages(x$pages),
      add_book_editors(x$secondary_author),
      add_book_title(x$secondary_title),
      add_book_publishers(x$publisher),
      format_given(x$place_published, fmts$dot_at_start),
      add_doi(x$electronic_resource_number)
    )

  } else if (type == "thesis") {

    sprintf(
      "%s%s%s%s",
      format_given(x$type_of_work, fmts$dot_at_end),
      format_given(x$secondary_title, fmts$dot_at_end),
      format_given(x$publisher, fmts$space_at_start),
      add_doi(x$electronic_resource_number)
    )

  } else if (type == "conference") {

    sprintf(
      "%s%s%s%s",
      add_book_pages(x$pages),
      add_conference_name(x$secondary_title),
      format_given(x$place_published, fmts$dot_at_start),
      format_given(x$date, fmts$dot_at_start),
      add_doi(x$electronic_resource_number)
    )

  } else if (type == "journal") {

    sprintf(
      "%s%s%s%s%s",
      add_journal_name(x$secondary_title),
      add_volume(x$volume),
      add_issue(x$number),
      format_given(x$pages, fmts$colon_at_start),
      add_doi(x$electronic_resource_number)
    )

  } else if (type == "report") {

    sprintf(
      "%s%s",
      add_publishers(x$publisher),
      add_doi(x$electronic_resource_number)
    )

  }

  x
}

# add_kwb_style ----------------------------------------------------------------

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
add_kwb_style <- function(endnote_db_refs)
{
  dplyr::bind_rows(.id = "reference_type_name", list(
    books         = add_kwb_style_to(endnote_db_refs, type = "book"),
    book_sections = add_kwb_style_to(endnote_db_refs, type = "book_section"),
    conferences   = add_kwb_style_to(endnote_db_refs, type = "conference"),
    journals      = add_kwb_style_to(endnote_db_refs, type = "journal"),
    reports       = add_kwb_style_to(endnote_db_refs, type = "report"),
    theses        = add_kwb_style_to(endnote_db_refs, type = "thesis")
  ))
}

# Test -------------------------------------------------------------------------
if (FALSE)
{
  endnote_db <- read_endnote_db(path = "../../dms/2020-07-08/KWB-documents_20191205.Data/sdb/sdb.eni")
  kwb_style_list <- add_kwb_style(endnote_db$refs)
}
