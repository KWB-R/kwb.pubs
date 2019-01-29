#' Replace Author Umlauts in One Publication Index.md
#'
#' @param path path to index.md file
#' @param encoding  default: "UTF-8"
#' @return rewrites "index.md" file with umlauts but with "ae", "oe", "ue",
#' "Ae", "Oe", "Ue" where needed
#' @keywords internal
#' @noRd
#' @importFrom stringr str_detect
#'
replace_author_umlauts_in_pub_index_md <- function(path,
                                                   encoding = "UTF-8") {

pub_index_txt <- readLines(path,
                           encoding = encoding)

idx <- which(stringr::str_detect(pub_index_txt, pattern = "^author"))

if (idx > 0) {
  message(sprintf("Replacing German umlauts in '%s'", path))
  pub_index_txt[idx] <- replace_umlauts(pub_index_txt[idx])
}

writeLines(pub_index_txt,
           path,
           useBytes = TRUE)
}

#' Get Publication Index.md Paths
#'
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return vector with absolute paths with all publication "index.md" files
#' @export
#' @importFrom fs path_abs dir_ls

get_publication_index_md_paths <- function(hugo_root_dir = ".") {


  pub_dir <- fs::path_abs(hugo_root_dir, "content/publication")
  fs::dir_ls(pub_dir, recursive = TRUE, regexp = "/index.md$")

}


#' Replace Author Umlauts in Multiple Publication Index.md
#'
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @param encoding  default: "UTF-8"
#' @return rewrites "author" field in multiple "index.md" files with
#' umlauts but with "ae", "oe", "ue", "Ae", "Oe", "Ue" where needed
#' @export
#'
replace_authors_umlauts_in_pub_index_md <- function(hugo_root_dir = ".",
                                                    encoding = "UTF-8") {

  paths <- get_publication_index_md_paths(hugo_root_dir)

  sapply(paths, replace_author_umlauts_in_pub_index_md, encoding = encoding)
}
