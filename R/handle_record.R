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

  file <- file_and_record$pub_index_md

  dat <- readLines(file)

  is_empty <- grepl(get_pattern("abstract_empty"), dat)
  is_filled <- grepl(get_pattern("abstract_filled"), dat)

  clean_abstract <- file_and_record$record$abstract %>%
    stringr::str_replace_all("\r", " ") %>%
    stringr::str_replace_all("\"", "\\\\\"")

  if (sum(is_empty) == 1) {

    print("Adding abstract...")
    dat[is_empty] <- sprintf('abstract = "%s"', clean_abstract)
    writeLines(dat, file, useBytes = TRUE)

  } else if (sum(is_filled) == 1 && overwrite) {

    print("Adding abstract...")
    dat[is_filled] <- sprintf('abstract = "%s"', clean_abstract)
    writeLines(dat, file, useBytes = TRUE)

  } else {

    sep_idx <- last_matching("\\+\\+\\+", dat)
    before <- 1:(sep_idx-1)
    after <-  sep_idx:length(dat)
    dat <- c(
      dat[before],
      sprintf('abstract = "%s"', clean_abstract) ,
      dat[after]
    )
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

    sep_idx <- last_matching("\\-\\-\\-", dat)
    before <- 1:(sep_idx-1)
    after <-  sep_idx:length(dat)
    dat <- c(dat[before], project_names, dat[after])
  }

  message(sprintf("Adding %s", project_names))
  writeLines(dat, file, useBytes = TRUE)
}
