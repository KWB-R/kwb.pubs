#' Add Projects to Hugo-Academic Publications
#'
#' @param endnote_df endnote_df as retrieved by kwb.endnote::create_references_df()
#' or kwb.endnote::clean_references_df()
#' @param overwrite should existing "projects" be overwritten (default: TRUE)
#' @param col_project name of column containing project id (default: "custom2")
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return add project tags to index.md
#' @export
#' @importFrom stringr str_split
#' @importFrom fs dir_ls
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' endnote_list <- kwb.endnote::create_endnote_list()
#' refs_df_clean <- kwb.endnote::clean_references_df(endnote_list)
#' add_projects_to_pub_index_md(endnote_df = refs_df_clean)
#' }
add_projects_to_pub_index_md <- function(endnote_df,
                                         overwrite = TRUE,
                                         col_project = "custom2",
                                         hugo_root_dir = ".") {


  hugo_pub_dir <- sprintf("%s/content/publication", hugo_root_dir)

  if(!dir.exists(hugo_pub_dir)) {
    msg <- sprintf("Hugo publication dir %s does not exist", hugo_pub_dir)
    stop(msg)
  }

  pub_dirs <- fs::dir_ls(hugo_pub_dir, type = "directory")
  pub_id_pattern <- "[0-9]?[0-9]?[0-9]?[0-9]$"
  pub_dir <- unique(stringr::str_remove(pub_dirs,
                                        pattern = pub_id_pattern))

  rec_ids <- stringr::str_extract(pub_dirs,
                                  pattern = pub_id_pattern)

  rec_ids <- rec_ids[!is.na(rec_ids)]

  recs_in_pubs <- endnote_df[endnote_df$rec_number %in% as.numeric(rec_ids),] %>%
    dplyr::mutate(project_names = stringr::str_split(.data[[col_project]],
                                                     ",|\r") %>%
    sapply(function(record) {
      sprintf('projects: [%s]',
              sprintf('"%s"', paste0(record, collapse = '", "')))}))


  for(rec_id in recs_in_pubs$rec_number) {

    print(sprintf("rec_id: %s", rec_id))
    sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == as.numeric(rec_id), ]


    pub_index_md <- sprintf("%s%s/index.md", pub_dir, rec_id)
    if(file.exists(pub_index_md)) {
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
}
