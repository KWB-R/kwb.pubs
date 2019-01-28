
#' @keywords internal
#' @noRd

template_author_md_default <- function() {

  system.file("templates/author.md", package = "kwb.pubs")
}


#' @keywords internal
#' @noRd
read_template_author_md <- function(path = template_author_md_default()) {
  readLines(path)
}





#' Create Author Social
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with social icons
#' @export
#' @importFrom dplyr select starts_with
create_author_social <- function(author_metadata) {

  social_metadata <- author_metadata %>%
    dplyr::select(dplyr::starts_with(match = "social_"))


  drop_na_or_empty <- !(is.na(social_metadata) | social_metadata == "")
  social_metadata <- social_metadata[, drop_na_or_empty]


  social_list <- lapply(names(social_metadata), function(x) {

   eval(call(sprintf('create_%s', x), social_metadata[,x]))})

unlist(social_list)
}

# create_author_education <- function() {
#
# }
#
# create_author_interests <- function() {
#
# }


#' Prepare Author "_index.md"
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param template_author_md default: template_author_md_default()
#' @return "_index.md" for author as text
#' @export
#' @importFrom stringr str_detect str_remove_all

prepare_author_index_md <- function(author_metadata,
                                   template_author_md = template_author_md_default()) {

  tags_pattern <- "<\\w+>"
  simple_tags <-  sprintf("<%s>", c("fullname", "bio_short", "bio_full"))


  template_md <- read_template_author_md(template_author_md)
  author_md <- template_md

  simple_tags_idx <- which(stringr::str_detect(string = template_md,
                                               pattern = paste0(simple_tags,
                                                                collapse = "|")))

  ## Change simple tags (one line values: "fullname", "bio_short", "bio_full")
  for(tag_idx in  simple_tags_idx) {
    tag_full <- template_md[tag_idx] %>%
      stringr::str_extract(tags_pattern)
    tag_name <- tag_full %>%
      stringr::str_remove_all("<|>")

   tag_value <- ifelse(is.na(author_metadata[[tag_name]]),
                          "",
                          sprintf('\"%s\"',
                                  author_metadata[[tag_name]] %>%
                                  stringr::str_replace_all("\r", " ") %>%
                                  stringr::str_replace_all("\"", "\\\\\"")))

      author_md[tag_idx] <- tag_value
   }


  social_tag_idx <- which(stringr::str_detect(string = template_md,
                                               pattern = "<social>"))


  c(author_md[1:(social_tag_idx-1)],
  create_author_social(author_metadata),
  author_md[(social_tag_idx+1):length(author_md)])


}


#' Add Author Index.md
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param overwrite default: FALSE
#' @param hugo_root_dir (default: ".")
#' @return creates author index.md for one author
#' @export
add_author_index_md <- function(author_metadata,
                                 overwrite = FALSE,
                                 hugo_root_dir = ".") {

  author_dir <- create_author_dir(author_dirname = author_metadata$dir_name,
                                  hugo_root_dir = hugo_root_dir)

  index_md <- file.path(author_dir, "_index.md")

  is_write <- !file.exists(index_md) || (overwrite && file.exists(index_md))

  if(is_write) {
   message(sprintf("Writting %s", index_md))
   index_txt <- prepare_author_index_md(author_metadata)
   writeLines(text = index_txt,
              con = index_md,
              useBytes = TRUE)
  } else {
    stop(sprintf("Author %s already existing.\n
                 Set 'overwrite = TRUE' if you want to overwrite it!",
                 index_md))
  }

}

#' Add Authors Index.md
#'
#' @param authors_metadata as retrieved by add_authors_metadata()
#' @param overwrite (default: FALSE)
#' @param hugo_root_dir (default: ".")
#' @return creates author index.md for all authors
#' @export
add_authors_index_md <- function(authors_metadata = add_authors_metadata(),
                                 overwrite = FALSE,
                                 hugo_root_dir = ".") {

  sapply(seq_len(nrow(authors_metadata)), FUN = function(idx) {
    add_author_index_md(author_metadata = authors_metadata[idx,],
                      overwrite,
                      hugo_root_dir)
  })

}
