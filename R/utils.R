# check_hugo_pub_dir -----------------------------------------------------------
check_hugo_pub_dir <- function(hugo_root_dir)
{
  path <- paste0(hugo_root_dir, "/content/publication")

  if (! dir.exists(path)) stop(
    sprintf("Hugo publication dir %s does not exist", path),
    call. = FALSE
  )

  path
}
