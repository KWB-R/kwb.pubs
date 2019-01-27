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
add_abstracts_to_pub_index_md <- function(endnote_df,
                                          overwrite = FALSE,
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

recs_in_pubs <- endnote_df[endnote_df$rec_number %in% as.numeric(rec_ids),]
for(rec_id in recs_in_pubs$rec_number) {

print(sprintf("rec_id: %s", rec_id))
sel_rec <- recs_in_pubs[recs_in_pubs$rec_number == as.numeric(rec_id), ]


pub_index_md <- sprintf("%s%s/index.md", pub_dir, rec_id)
if(file.exists(pub_index_md)) {
dat <- readLines(con = pub_index_md)
abstract_empty_idx <- grepl(pattern = "abstract(\\s+)?=(\\s+)?\"(\\s+)?\"", dat)

abstract_filled_idx <- grepl(pattern = "abtract(\\s+)?=(\\s+)?\"\\w+", dat)



if(!is.na(sel_rec$abstract)) {
  clean_abstract <- sel_rec$abstract %>%
    stringr::str_replace_all("\r", " ") %>%
    stringr::str_replace_all("\"", "\\\\\"")
  if(sum(abstract_empty_idx) == 1) {
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
    dat <- c(dat[before],
             sprintf('abstract = "%s"', clean_abstract) ,
             dat[after])
  }
} else {
  message("no abstract available")
  }
} else {
  message(sprintf("%s is missing", pub_index_md))
}
}
}
