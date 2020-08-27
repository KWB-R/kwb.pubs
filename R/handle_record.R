# handle_record_1 --------------------------------------------------------------
handle_record_1 <- function(rec_id, recs_in_pubs, pub_dir_info)
{
  print(sprintf("rec_id: %s", rec_id))

  if (is.null(file_and_record <- get_file_and_record(
    pub_dir = pub_dir_info$pub_dir,
    recs_in_pubs = recs_in_pubs,
    rec_id = rec_id,
    field = "abstract",
    subject = "abstract"
  )))
    return()

  abstract <- file_and_record$record$abstract %>%
    stringr::str_replace_all("\r", " ") %>%
    stringr::str_replace_all("\"", "\\\\\"") %>%
    sprintf(fmt = 'abstract = "%s"')

  file <- file_and_record$pub_index_md

  x <- readLines(file)

  is_empty <- grepl(get_pattern("abstract_empty"), x)
  is_filled <- grepl(get_pattern("abstract_filled"), x)

  if (sum(is_empty) == 1) {

    x[is_empty] <- abstract

  } else if (sum(is_filled) == 1 && overwrite) {

    x[is_filled] <- abstract

  } else {

    x <- insert_after(x, "\\+\\+\\+", abstract)
  }

  write_with_message_adding(x, file, subject = "abstract")
}

# handle_record_2 --------------------------------------------------------------
handle_record_2 <- function(rec_id, recs_in_pubs, pub_dir_info, col_project)
{
  print(sprintf("rec_id: %s", rec_id))

  if (is.null(file_and_record <- get_file_and_record(
    pub_dir = pub_dir_info$pub_dir,
    recs_in_pubs = recs_in_pubs,
    rec_id = rec_id,
    field = col_project,
    subject = "project metadata"
  )))
    return()

  file <- file_and_record$pub_index_md

  x <- readLines(file)

  project_names <- file_and_record$record[["project_names"]]

  is_empty <- grepl(get_pattern("project_empty"), x)
  is_filled <- grepl(get_pattern("project_filled"), x)

  if (sum(is_empty) == 1) {

    x[is_empty] <- project_names

  } else if (sum(is_filled) == 1 && overwrite) {

    x[is_filled] <- project_names

  } else {

    x <- insert_after(x, "\\-\\-\\-", project_names)
  }

  write_with_message_adding(x, file, subject = project_names)
}
