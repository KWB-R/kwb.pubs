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

    handle_record_2(rec_id, recs_in_pubs, pub_dir_info, col_project)
  }
}
cat(kwb.utils::stringList(LETTERS,qchar = '"'))
