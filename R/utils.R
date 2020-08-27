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

# get_pattern ------------------------------------------------------------------
#' @importFrom kwb.utils::selectElements
get_pattern <- function(type)
{
  kwb.utils::selectElements(elements = type, list(
    abstract_empty  = "abstract(\\s+)?=(\\s+)?\"(\\s+)?\"",
    abstract_filled = "abtract(\\s+)?=(\\s+)?\"\\w+",
    project_empty   = "projects(\\s+)?:(\\s+)?\"(\\s+)?\"",
    project_filled  = "projects(\\s+)?:(\\s+)?\\[",
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

# get_record_with --------------------------------------------------------------
get_record_with <- function(recs_in_pubs, rec_id, field, subject)
{
  record <- recs_in_pubs[recs_in_pubs$rec_number == rec_id, ]

  if (is.na(record[[field]])) {

    message("no ", subject, "available")
    return(NULL)
  }

  record
}
