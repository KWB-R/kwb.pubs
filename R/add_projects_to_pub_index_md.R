#' Add Projects to Hugo-Academic Publications
#'
#' @param endnote_df endnote_df as retrieved by kwb.endnote::create_references_df()
#' or kwb.endnote::clean_references_df()
#' @param overwrite should existing "projects" be overwritten (default: TRUE)
#' @param col_project name of column containing project id (default: "custom2")
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return add project tags to index.md
#' @export
#' @importFrom fs dir_ls
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom kwb.utils stringList
#' @examples
#' \dontrun{
#' endnote_list <- kwb.endnote::create_endnote_list()
#' refs_df_clean <- kwb.endnote::clean_references_df(endnote_list)
#' add_projects_to_pub_index_md(endnote_df = refs_df_clean)
#' }
add_projects_to_pub_index_md <- function(
  endnote_df, overwrite = TRUE, col_project = "custom2", hugo_root_dir = "."
)
{
  pub_dir_info <- get_pub_dir_info(check_hugo_pub_dir(hugo_root_dir))

  recs_in_pubs <- filter_records(endnote_df, pub_dir_info$rec_ids) %>%
    dplyr::mutate(
      project_names = split_at_comma_or_newline(.data[[col_project]]) %>%
        sapply(function(record) sprintf(
          'projects: [%s]', kwb.utils::stringList(record, qchar = '"')
        ))
    )

  for (rec_id in get_record_number(recs_in_pubs)) {

    print(sprintf("rec_id: %s", rec_id))

    if (is.null(file_and_record <- get_file_and_record(
      pub_dir = pub_dir_info$pub_dir,
      recs_in_pubs = recs_in_pubs,
      rec_id = rec_id,
      field = col_project,
      subject = "project metadata"
    )))
      return()

    rewrite_md_file(
      file = file_and_record$pub_index_md,
      pattern_empty = get_pattern("project_empty"),
      pattern_filled = get_pattern("project_filled"),
      pattern_sep = get_pattern("project_sep"),
      content = file_and_record$record[["project_names"]],
      overwrite = overwrite
    )
  }
}
