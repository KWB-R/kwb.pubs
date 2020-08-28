#' Add Abstracts to Hugo-Academic Publications
#'
#' @param endnote_df endnote_df as retrieved by kwb.endnote::create_references_df()
#' or kwb.endnote::clean_references_df()
#' @param overwrite should existing "projects" be overwritten (default: FALSE)
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return add abstracts to index.md
#' @export
#' @importFrom stringr str_replace str_extract str_replace_all
#' @importFrom fs dir_ls
#' @examples
#' \dontrun{
#' endnote_list <- kwb.endnote::create_endnote_list()
#' endnote_df <- kwb.endnote::clean_references_df(endnote_list)
#' add_abstracts_to_pub_index_md(endnote_df = endnote_df)
#' }
add_abstracts_to_pub_index_md <- function(
  endnote_df, overwrite = FALSE, hugo_root_dir = "."
)
{
  pub_dir_info <- get_pub_dir_info(check_hugo_pub_dir(hugo_root_dir))

  recs_in_pubs <- filter_records(endnote_df, pub_dir_info$rec_ids)

  for (rec_id in get_record_number(recs_in_pubs)) {

    handle_record_1(rec_id, recs_in_pubs, pub_dir_info)
  }
}
