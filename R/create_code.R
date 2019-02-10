#' Create Code
#'
#' @param repo_names character vector with code repo acryonms to create
#' (as retrieved by pkgmeta::get_github_repos())
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return create index.md for each project_name in subfolder "content/project"
#' @export
#' @importFrom withr with_dir
#' @importFrom fs path_abs
#' @importFrom blogdown hugo_cmd
#' @examples
#' \dontrun{
#' repo_topics <- pkgmeta::get_github_topics()
#' repo_names <- stringr::str_replace_all(stringr::str_remove_all(unique(repo_topics$full_name),
#' ".*/"), "\\.", "-")
#' create_code(repo_names = repo_names)
#' }
create_code <- function(repo_names,
                            hugo_root_dir = ".") {


  withr::with_dir(hugo_root_dir,
                  code = {
                    sapply(repo_names,
                           function(repo_name) {
                             message(sprintf("Creating code '%s' in hugo_dir = %s",
                                             repo_name,
                                             fs::path_abs(hugo_root_dir)))
                             cmd_proj <- sprintf('new  --kind project "code/%s"',
                                                 repo_name)
                             blogdown::hugo_cmd(cmd_proj)})})
}

