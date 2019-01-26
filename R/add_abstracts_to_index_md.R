#' Add Abstracts to Hugo-Academic Publications
#'
#' @param hugo_pub_dir hugo-academic folder containing publications
#' (default: "./content/publication")
#' @param endnote_df endnote_df as retrieved by kwb.endnote::create_references_df()
#' or kwb.endnote::clean_references_df()
#' @return add abstracts to index.md
#' @export
#' @importFrom stringr str_replace str_extract str_replace_all
#' @importFrom fs dir_ls
#' @examples
#' \dontrun{
#' endnote_list <- kwb.endnote::create_endnote_list()
#' refs_df_clean <- kwb.endnote::clean_references_df(endnote_list)
#' add_abstracts_to_index_md(endnote_df = refs_df_clean)
#' }
add_abstracts_to_index_md <- function(endnote_df,
                                      hugo_pub_dir = "./content/publication") {


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

recs_in_pubs <- endnote_df[endnote_df$rec_number %in% as.numeric(rec_ids),]
for(rec_id in recs_in_pubs$rec_number) {

print(sprintf("rec_id: %s", rec_id))
sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == as.numeric(rec_id), ]


pub_index_md <- sprintf("%s%s/index.md", pub_dir, rec_id)
if(file.exists(pub_index_md)) {
dat <- readLines(con = pub_index_md)
abstract_idx <- grepl(pattern = "abstract = \"\"", dat)

if(!is.na(sel_rec$abstract) && sum(abstract_idx) == 1) {
  print("Adding abstract...")
  clean_abstract <- sel_rec$abstract %>%
    stringr::str_replace_all("\r", " ") %>%
    stringr::str_replace_all("\"", "\\\\\"")
  dat[abstract_idx] <- sprintf('abstract = "%s"', clean_abstract)
  writeLines(dat, con = pub_index_md,useBytes = TRUE)
} else {
  message("no abstract available")
  }
} else {
  message(sprintf("%s is missing", pub_index_md))
}
}
}
