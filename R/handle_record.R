# handle_record_1 --------------------------------------------------------------
handle_record_1 <- function(rec_id, recs_in_pubs, pub_dir_info)
{
  print(sprintf("rec_id: %s", rec_id))

  sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == as.numeric(rec_id), ]

  pub_index_md <- sprintf("%s%s/index.md", pub_dir_info$pub_dir, rec_id)

  if (file.exists(pub_index_md)) {

    dat <- readLines(con = pub_index_md)

    abstract_empty_idx <- grepl(pattern = "abstract(\\s+)?=(\\s+)?\"(\\s+)?\"", dat)
    abstract_filled_idx <- grepl(pattern = "abtract(\\s+)?=(\\s+)?\"\\w+", dat)

    if (! is.na(sel_rec$abstract)) {
      clean_abstract <- sel_rec$abstract %>%
        stringr::str_replace_all("\r", " ") %>%
        stringr::str_replace_all("\"", "\\\\\"")
      if (sum(abstract_empty_idx) == 1) {
        print("Adding abstract...")
        dat[abstract_empty_idx] <- sprintf('abstract = "%s"', clean_abstract)
        writeLines(dat, con = pub_index_md,useBytes = TRUE)
      } else if (sum(abstract_filled_idx) == 1 && overwrite) {
        print("Adding abstract...")
        dat[abstract_filled_idx] <- sprintf('abstract = "%s"', clean_abstract)
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
    } else {
      message("no abstract available")
    }
  } else {
    message(sprintf("%s is missing", pub_index_md))
  }
}

# handle_record_2 --------------------------------------------------------------
handle_record_2 <- function(rec_id, recs_in_pubs, pub_dir_info)
{
  print(sprintf("rec_id: %s", rec_id))
  sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == as.numeric(rec_id), ]

  pub_index_md <- sprintf("%s%s/index.md", pub_dir_info$pub_dir, rec_id)

  if (file.exists(pub_index_md)) {

    dat <- readLines(con = pub_index_md)

    project_empty_idx <- grepl(pattern = "projects(\\s+)?:(\\s+)?\"(\\s+)?\"", dat)
    project_filled_idx <- grepl(pattern = "projects(\\s+)?:(\\s+)?\\[", dat)

    if(!is.na(sel_rec[[col_project]])) {
      if(sum(project_empty_idx) == 1) {
        dat[project_empty_idx] <-  sel_rec[["project_names"]]
      } else if (sum(project_filled_idx) == 1 && overwrite) {
        dat[project_filled_idx] <-  sel_rec[["project_names"]]

      } else {
        sep_idx <- max(grep(pattern = "\\-\\-\\-", dat))
        before <- 1:(sep_idx-1)
        after <-  sep_idx:length(dat)
        dat <- c(dat[before],  sel_rec[["project_names"]], dat[after])
      }
      message(sprintf("Adding %s",  sel_rec[["project_names"]]))
      writeLines(dat, con = pub_index_md,useBytes = TRUE)
    } else {
      message("no project metadata available")
    }
  } else {
    message(sprintf("%s is missing", pub_index_md))
  }
}
