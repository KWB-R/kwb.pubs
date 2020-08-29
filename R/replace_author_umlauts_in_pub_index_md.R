#' Replace Author Umlauts in One Publication Index.md
#'
#' @param path path to index.md file
#' @param encoding  default: "UTF-8"
#' @return rewrites "index.md" file with umlauts but with "ae", "oe", "ue",
#' "Ae", "Oe", "Ue", "ss" where needed
#' @keywords internal
#' @noRd
#' @importFrom stringr str_detect
#'
replace_author_umlauts_in_pub_index_md <- function(path, encoding = "UTF-8")
{
  pub_index_txt <- readLines(path, encoding = encoding)

  idx <- which(stringr::str_detect(pub_index_txt, pattern = "^author"))

  if (length(idx) == 0L)
    return()

  message(sprintf("Replacing German umlauts in '%s'", path))
  pub_index_txt[idx] <- replace_umlauts(pub_index_txt[idx])
  writeLines(pub_index_txt, path, useBytes = TRUE)
}

#' Replace Author Umlauts in Multiple Publication Index.md
#'
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @param encoding  default: "UTF-8"
#' @return rewrites "author" field in multiple "index.md" files without umlauts
#' but with "ae", "oe", "ue", "Ae", "Oe", "Ue", "ss" where needed
#' @export
#'
replace_authors_umlauts_in_pub_index_md <- function(
  hugo_root_dir = ".", encoding = "UTF-8"
)
{
  sapply(
    X = DBI::dbListTables(con),
    FUN = replace_author_umlauts_in_pub_index_md,
    encoding = encoding
  )
}

# change_author_to_lastname_in_pub_index_md ------------------------------------
change_author_to_lastname_in_pub_index_md <- function(path, encoding = "UTF-8")
{
  pub_index_txt <- readLines(path, encoding = encoding)

  idx <- which(stringr::str_detect(pub_index_txt, pattern = "^author"))

  if (length(idx) == 0L)
    return()

  message(sprintf("Replacing German umlauts in '%s'", path))
  pub_index_txt[idx] <- replace_umlauts(pub_index_txt[idx])
  writeLines(pub_index_txt, path, useBytes = TRUE)
}
