#' Helper function: get unique project names

#' @param project_names character vector with project acryonms to create
#' (as retrieved by kwb.endnote::clean_references_df()$custom2)
#' @export
#' @importFrom stringr str_split str_sort
get_unique_project_names <- function(project_names) {


  stringr::str_split(project_names, ",") %>%
    unlist() %>%
    unique() %>%
    stringr::str_sort(na_last = NA)


}

#' Create Projects
#'
#' @param project_names character vector with project acryonms to create
#' (as retrieved by kwb.endnote::clean_references_df()$custom2)
#' @param project_dir project directory in "content" subfolder (default: "project")
#' @param hugo_root_dir root dir of hugo-academic website (default: ".")
#' @return create index.md for each project_name in subfolder "content/project"
#' @export
#' @importFrom withr with_dir
#' @importFrom fs path_abs
#' @importFrom blogdown hugo_cmd
#' @examples
#' \dontrun{
#' endnote_list <- kwb.endnote::create_endnote_list()
#' endnote_df <- kwb.endnote::clean_references_df()
#' create_projects(project_names = endnote_df$custom2)
#' }
create_projects <- function(project_names,
                            project_dir = "project",
                            hugo_root_dir = ".") {


  unique_project_names <- get_unique_project_names(project_names)

  withr::with_dir(hugo_root_dir,
                  code = {
                    sapply(unique_project_names,
                           function(project_name) {
                             message(sprintf("Creating project '%s' in hugo_dir = %s",
                                             project_name,
                                             fs::path_abs(hugo_root_dir)))
                             cmd_proj <- sprintf("new  --kind project %s/%s",
                                                 project_dir,
                                                 project_name)
                             blogdown::hugo_cmd(cmd_proj)})})
}


