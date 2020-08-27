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

  write_abstract <- function(x) {
    print("Adding abstract...")
    writeLines(x, file, useBytes = TRUE)
  }

  dat <- readLines(file)

  is_empty <- grepl(get_pattern("abstract_empty"), dat)
  is_filled <- grepl(get_pattern("abstract_filled"), dat)

  if (sum(is_empty) == 1) {

    dat[is_empty] <- abstract
    write_abstract(x = dat)

  } else if (sum(is_filled) == 1 && overwrite) {

    dat[is_filled] <- abstract
    write_abstract(x = dat)

  } else {

    dat <- insert_after(dat, "\\+\\+\\+", abstract)
  }
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
  project_names <- file_and_record$record[["project_names"]]

  dat <- readLines(con = file)

  is_empty <- grepl(get_pattern("project_empty"), dat)
  is_filled <- grepl(get_pattern("project_filled"), dat)

  if (sum(is_empty) == 1) {

    dat[is_empty] <- project_names

  } else if (sum(is_filled) == 1 && overwrite) {

    dat[is_filled] <- project_names

  } else {

    dat <- insert_after(dat, pattern, project_names)
  }

  message(sprintf("Adding %s", project_names))
  writeLines(dat, file, useBytes = TRUE)
}
