# handle_record_1 --------------------------------------------------------------
handle_record_1 <- function(rec_id, recs_in_pubs, pub_dir_info)
{
  print(sprintf("rec_id: %s", rec_id))

  if (is.null(pub_index_md <- get_pub_index_md_file(pub_dir_info$pub_dir))) {
    return()
  }

  sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == rec_id, ]

  if (is.na(sel_rec$abstract)) {
    message("no abstract available")
    return()
  }

  dat <- readLines(con = pub_index_md)

  abstract_is_empty <- grepl(pattern = "abstract(\\s+)?=(\\s+)?\"(\\s+)?\"", dat)
  abstract_is_filled <- grepl(pattern = "abtract(\\s+)?=(\\s+)?\"\\w+", dat)

  clean_abstract <- sel_rec$abstract %>%
    stringr::str_replace_all("\r", " ") %>%
    stringr::str_replace_all("\"", "\\\\\"")

  if (sum(abstract_is_empty) == 1) {

    print("Adding abstract...")
    dat[abstract_is_empty] <- sprintf('abstract = "%s"', clean_abstract)
    writeLines(dat, con = pub_index_md,useBytes = TRUE)

  } else if (sum(abstract_is_filled) == 1 && overwrite) {

    print("Adding abstract...")
    dat[abstract_is_filled] <- sprintf('abstract = "%s"', clean_abstract)
    writeLines(dat, con = pub_index_md,useBytes = TRUE)

  } else {

    sep_idx <- max(grep(pattern = "\\+\\+\\+", dat))
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

  if (is.null(pub_index_md <- get_pub_index_md_file(pub_dir_info$pub_dir))) {
    return()
  }

  sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == rec_id, ]

  if (is.na(sel_rec[[col_project]])) {
    message("no project metadata available")
    return()
  }

  project_names <- sel_rec[["project_names"]]

  dat <- readLines(con = pub_index_md)

  project_is_empty <- grepl(pattern = "projects(\\s+)?:(\\s+)?\"(\\s+)?\"", dat)
  project_is_filled <- grepl(pattern = "projects(\\s+)?:(\\s+)?\\[", dat)

  if (sum(project_is_empty) == 1) {

    dat[project_is_empty] <- project_names

  } else if (sum(project_is_filled) == 1 && overwrite) {

    dat[project_is_filled] <- project_names

  } else {

    sep_idx <- max(grep(pattern = "\\-\\-\\-", dat))
    before <- 1:(sep_idx-1)
    after <-  sep_idx:length(dat)
    dat <- c(dat[before], project_names, dat[after])
  }

  message(sprintf("Adding %s", project_names))
  writeLines(dat, con = pub_index_md,useBytes = TRUE)
}
