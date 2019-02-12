#' Create Code
#'
#' @param repo_topics data.frame as retrieved by pkgmeta::get_github_repos()
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return create index.md for each project_name in subfolder "content/project"
#' @export
#' @importFrom withr with_dir
#' @importFrom fs path_abs
#' @importFrom blogdown hugo_cmd
#' @examples
#' \dontrun{
#' repo_topics <- pkgmeta::get_github_topics()
#' create_code(repo_topics = repo_topics)
#' }
#'

create_code <- function(repo_topics,
                        hugo_root_dir = ".") {


  repo_fullnames <- unique(repo_topics$full_name)


  repo_names <- stringr::str_remove_all(repo_fullnames, ".*/")


  repo_names_without_dot <- repo_names %>%
    stringr::str_replace_all("\\.", "-")


  dir_repo_names_without_dot <- sprintf("code/%s", repo_names_without_dot)
  dir_repo_names <- sprintf("code/%s", repo_names)

  withr::with_dir(hugo_root_dir,
                  code = {
                    sapply(dir_repo_names_without_dot,
                           function(dir_name_without_dot) {
                             message(sprintf("Creating code '%s' in hugo_dir = %s",
                                             dir_name_without_dot,
                                             fs::path_abs(hugo_root_dir)))
                             cmd_proj <- sprintf('new  --kind project %s',
                                                 dir_name_without_dot)
                             blogdown::hugo_cmd(cmd_proj)})})


  is_dot <- which(stringr::str_detect(dir_repo_names, "\\."))
  if(length(is_dot) > 0) {
  withr::with_dir(hugo_root_dir,
                  code = {
                    sapply(is_dot, FUN = function(i) {
                      message(sprintf("Creating code '%s' in hugo_dir = %s",
                                      dir_repo_names[i],
                                      fs::path_abs(hugo_root_dir)))
                      fs::dir_copy(path = sprintf("content/%s",
                                                  dir_repo_names_without_dot[i]),
                                   new_path = sprintf("content/%s",
                                                      dir_repo_names[i])
                                   )
                      fs::dir_delete(sprintf("content/%s",dir_repo_names_without_dot[i]))
                    })})}


  withr::with_dir(hugo_root_dir,
                  code = {
                    sapply(seq_along(dir_repo_names), FUN = function(i) {
                      index_md_file <- fs::dir_ls(path = sprintf("content/%s",
                                                dir_repo_names[i]),
                                                recursive = TRUE,
                                                regexp = "\\.md$",
                                                type = "file")
                      if(fs::file_exists(index_md_file)) {
                      index_md <- readLines(index_md_file)

                      url_code_idx <- grepl(pattern = "^url_code", index_md)

                      index_md[url_code_idx] <- sprintf('url_code = "https://github.com/%s"',
                                                        repo_fullnames[i])

                      title_idx <- grepl(pattern = "^title", index_md)

                      index_md[title_idx] <- sprintf('title = "%s"',
                                                        repo_names[i])


                      tags_idx <- grepl(pattern = "^tags", index_md)

                      topics <- repo_topics$topics[repo_topics$full_name == repo_fullnames[i]]

                      if(length(topics) > 0) {

                      index_md[tags_idx] <- sprintf('tags = [%s]',
                                                    paste0(sprintf('\"%s\"',
                                                                   topics),
                                                           collapse = ", "))}


                      writeLines(index_md, index_md_file)

                    }})
                  })


}

