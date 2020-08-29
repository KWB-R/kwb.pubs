# apply_to_split ---------------------------------------------------------------
apply_to_split <- function(x, pattern, FUN)
{
  sapply(split_one(x, pattern), FUN)
}

# check_hugo_pub_dir -----------------------------------------------------------
check_hugo_pub_dir <- function(hugo_root_dir)
{
  path <- paste0(hugo_root_dir, "/content/publication")

  if (! dir.exists(path)) stop(
    sprintf("Hugo publication dir %s does not exist", path),
    call. = FALSE
  )

  path
}

# clean_lower ------------------------------------------------------------------
clean_lower <- function(x)
{
  x %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    space_to_single_dash() %>%
    replace_umlauts()
}

# dash_collapsed ---------------------------------------------------------------
dash_collapsed <- function(x)
{
  paste(x, collapse = "-")
}

# enclose_in_empty_strings -----------------------------------------------------
enclose_in_empty_strings <- function(...)
{
  c('', ..., '')
}

# filter_records ---------------------------------------------------------------
filter_records <- function(records, rec_ids)
{
  records[get_record_number(records) %in% rec_ids, ]
}

# format_given -----------------------------------------------------------------
format_given <- function(fmt, x)
{
  ifelse(x == "", "", sprintf(fmt, stringr::str_trim(x)))
}

# format_tag -------------------------------------------------------------------
#' @importFrom stringr str_replace_all
format_tag <- function(x)
{
  x %>%
    stringr::str_replace_all("\r", " ") %>%
    stringr::str_replace_all("\"", "\\\\\"")
}

# get_file_and_record ----------------------------------------------------------
get_file_and_record <- function(pub_dir, recs_in_pubs, rec_id, field, subject)
{
  pub_index_md <- get_pub_index_md_file(pub_dir, rec_id)

  if (is.null(pub_index_md)) {
    return(NULL)
  }

  record <- get_record_with(recs_in_pubs, rec_id, field, subject)

  if (is.null(record)) {
    return(NULL)
  }

  list(
    pub_index_md = pub_index_md,
    record = record
  )
}

# get_pattern ------------------------------------------------------------------
#' @importFrom kwb.utils selectElements
get_pattern <- function(type)
{
  kwb.utils::selectElements(elements = type, list(
    abstract_empty  = "abstract(\\s+)?=(\\s+)?\"(\\s+)?\"",
    abstract_filled = "abstract(\\s+)?=(\\s+)?\"\\w+",
    abstract_sep    = "\\+\\+\\+",
    project_empty   = "projects(\\s+)?:(\\s+)?\"(\\s+)?\"",
    project_filled  = "projects(\\s+)?:(\\s+)?\\[",
    project_sep     = "\\-\\-\\-"
  ))
}

# get_pub_dir_info -------------------------------------------------------------
get_pub_dir_info <- function(hugo_pub_dir)
{
  pub_id_pattern <- "[0-9]?[0-9]?[0-9]?[0-9]$"

  pub_dirs <- fs::dir_ls(hugo_pub_dir, type = "directory")
  pub_dir <- unique(stringr::str_remove(pub_dirs, pattern = pub_id_pattern))

  stopifnot(length(pub_dir) == 1L)

  rec_ids <- stringr::str_extract(pub_dirs, pattern = pub_id_pattern)
  rec_ids <- as.integer(rec_ids[! is.na(rec_ids)])

  list(
    pub_dir = pub_dir,
    rec_ids = rec_ids
  )
}

# get_pub_index_md_file --------------------------------------------------------
get_pub_index_md_file <- function(pub_dir, rec_id)
{
  file <- paste0(pub_dir, rec_id, "/index.md")

  if (! file.exists(file)) {

    message(paste(file, "is missing"))
    return(NULL)
  }

  file
}

# get_record_number ------------------------------------------------------------
get_record_number <- function(records)
{
  as.integer(records$rec_number)
}

# get_record_with --------------------------------------------------------------
get_record_with <- function(recs_in_pubs, rec_id, field, subject)
{
  stopifnot(length(rec_id) == 1L)

  record <- filter_records(recs_in_pubs, rec_id)

  if (is.na(record[[field]])) {

    message("no ", subject, "available")
    return(NULL)
  }

  record
}

# insert_after -----------------------------------------------------------------
insert_after <- function(x, pattern, what)
{
  i <- last_matching(pattern, x)

  c(x[1:(i - 1L)], what, x[i:length(x)])
}

# last_matching ----------------------------------------------------------------
last_matching <- function(pattern, x)
{
  max(grep(pattern, x))
}

# package_file -----------------------------------------------------------------
package_file <- function(...)
{
  system.file(..., package = "kwb.pubs")
}

# space_collapsed --------------------------------------------------------------
space_collapsed <- function(x)
{
  paste(x, collapse = " ")
}

# space_to_single_dash ---------------------------------------------------------
#' @importFrom stringr str_replace_all
space_to_single_dash <- function(x)
{
  stringr::str_replace_all(x, "\\s+", "-")
}

# space_to_single_space --------------------------------------------------------
#' @importFrom stringr str_replace_all
space_to_single_space <- function(x)
{
  stringr::str_replace_all(x, "\\s+", " ")
}

# split_at_comma_or_newline ----------------------------------------------------
#' @importFrom stringr str_split
split_at_comma_or_newline <- function(x)
{
  stringr::str_split(x, ",|\r")
}

# split_one --------------------------------------------------------------------
split_one <- function(x, pattern)
{
  stopifnot(length(x) == 1L)

  strsplit(x, pattern)[[1L]]
}

# trimmed_title ----------------------------------------------------------------
trimmed_title <- function(x)
{
  stringr::str_to_title(stringr::str_trim(x))
}
