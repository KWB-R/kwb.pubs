
#' get_publication_index_md_paths
#'
#' @param hugo_root_dir hugo root dir (default: .)
#' @param lang optional language code (default: ""), i.e publications will be
#' searched in <hugo_root_dir>/<language_code>/publication
#' @return all recursive "index.md" files in folder
#' @export
#' @importFrom fs path_abs dir_ls
get_publication_index_md_paths <- function(hugo_root_dir = ".", lang = "")
{
  rel_path <- ifelse(
    lang == "",
    "content/publication",
    sprintf("content/%s/publication", lang)
  )

  fs::dir_ls(
    fs::path_abs(hugo_root_dir, rel_path),
    recurse = TRUE,
    regexp = "/index.md$"
  )
}
