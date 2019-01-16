# Set the name for your new package
package <- "kwb.pubs"

# Set the path to your new package
pkg_dir <- file.path(kwb.utils::get_homedir(), "Documents/RProjects", package)

usethis::proj_set(pkg_dir)

# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})

kwb.pkgbuild::use_pkg(pkg = list(name = "kwb.pubs",
                                 title = "Helper Functions For Generating Publications Website Based on Hugo Academic Theme" ,
                                 desc = "Helper Functions For Generating Publications Website Based on Hugo Academic Theme."))


### Make the repo "public" before using "autopkgdown"
kwb.pkgbuild::use_autopkgdown("kwb.pubs")
kwb.pkgbuild::add_gitlabci_to_ghpages("kwb.pubs", dest_dir = tempdir())
