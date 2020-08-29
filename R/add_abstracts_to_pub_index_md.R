#' Add Abstracts to Hugo-Academic Publications
#'
#' @param endnote_df endnote_df as retrieved by kwb.endnote::create_references_df()
#' or kwb.endnote::clean_references_df()
#' @param overwrite should existing "projects" be overwritten (default: FALSE)
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return add abstracts to index.md
#' @export
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

    print(sprintf("rec_id: %s", rec_id))

    file_and_record <- get_file_and_record(
      pub_dir = pub_dir_info$pub_dir,
      recs_in_pubs = recs_in_pubs,
      rec_id = rec_id,
      field = "abstract",
      subject = "abstract"
    )

    if (is.null(file_and_record))
      next

    abstract <- file_and_record$record$abstract %>%
      format_tag() %>%
      sprintf(fmt = 'abstract = "%s"')

    rewrite_md_file(
      file = file_and_record$pub_index_md,
      pattern_empty = get_pattern("abstract_empty"),
      pattern_filled = get_pattern("abstract_filled"),
      pattern_sep = get_pattern("abstract_sep"),
      content = abstract,
      overwrite = overwrite,
      subject = "abstract"
    )
  }
}
