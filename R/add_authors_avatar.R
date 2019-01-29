
#' @keywords internal
#' @noRd

config_authors_default <- function() {

  system.file("config/authors.txt", package = "kwb.pubs")

}


#' Get Authors Config
#'
#' @param path path to authors config (default: config_authors_default())
#' @return imports authors config as data.frame
#' @importFrom utils read.delim
#' @export

get_authors_config <- function(path = config_authors_default()) {

utils::read.delim(file = file(path, encoding = "UCS-2LE"),
                  stringsAsFactors = FALSE)

}


#' @keywords internal
#' @noRd
#' @importFrom stringr str_trim str_to_lower str_to_lower

construct_email <- function(firstname, lastname) {
  sprintf("%s.%s@kompetenz-wasser.de",
          firstname %>%
            stringr::str_trim() %>%
            stringr::str_to_lower() %>%
            replace_umlauts(),
          lastname %>%
            stringr::str_trim() %>%
            stringr::str_to_lower() %>%
            replace_umlauts())
}

#' @keywords internal
#' @noRd
#' @importFrom stringr str_to_lower str_to_lower

construct_fullname <- function(firstname, lastname) {
  sprintf("%s %s",
          firstname %>%
            stringr::str_trim() %>%
            stringr::str_to_title(),
          lastname %>%
            stringr::str_trim() %>%
            stringr::str_to_title())
}

#' @keywords internal
#' @noRd
#' @importFrom stringr str_split str_to_lower str_to_lower str_trim str_sub str_replace_all
#' @importFrom magrittr %>%

construct_dirname <- function(firstname,
                              lastname) {

  dir_firstname <- unlist(lapply(seq_along(firstname), function(idx) {
    tmp <- firstname[idx] %>%
    stringr::str_trim() %>%
    stringr::str_split("-|\\s+") %>%
    unlist() %>%
    stringr::str_sub(1, 1) %>%
    stringr::str_to_lower() %>%
    replace_umlauts()

    paste0(sprintf("%s.", tmp), collapse = "-")
}))


  dir_lastname <- lastname %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", "-") %>%
    stringr::str_to_lower() %>%
    replace_umlauts()

  sprintf("%s-%s", dir_firstname, dir_lastname)
}

#' Get Authors Metadata
#'
#' @param authors_config default: get_authors_config()
#' @return add authors metadata (email, dirname)
#' @export
add_authors_metadata <- function(authors_config = get_authors_config()) {

  email_idx <- which(is.na(authors_config$social_email))

  if(length(email_idx) > 1) {
    emails <- construct_email(authors_config$firstname[email_idx],
                              authors_config$lastname[email_idx])

    authors_config$social_email[email_idx] <- emails
  }

  authors_config$fullname <- construct_fullname(authors_config$firstname,
                                               authors_config$lastname)

  authors_config$dir_name <- construct_dirname(authors_config$firstname,
                                               authors_config$lastname)

  authors_config

}

#' Create Author Dir (if not existing)
#'
#' @keywords internal
#' @noRd
#' @importFrom fs dir_exists dir_create path_abs
#'
create_author_dir <- function(author_dirname,
                              hugo_root_dir = ".") {

  content_dir <- file.path(hugo_root_dir,"content")

  if(!fs::dir_exists(content_dir)) {
    stop(sprintf("No directory '%s' exists!", fs::path_abs(content_dir)))
  } else {
    author_dir <- sprintf("%s/author/%s",
                          content_dir,
                          author_dirname)

    if(!fs::dir_exists(author_dir)) {
      fs::dir_create(author_dir)
    } else {
      message(sprintf("Author directory '%s' already existing",
                      fs::path_abs(author_dir)))
    }
    author_dir
  }
}

#' Add Author Avatar
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param overwrite default: FALSE
#' @param hugo_root_dir (default: ".")
#' @param height passed to magickx::geometry_area (default: 250)
#' @param width passed to magickx::geometry_area (default: 300)
#' @param x_off  passed to magickx::geometry_area (default: 175)
#' @param y_off passed to magickx::geometry_area (default: 0)
#' @return add avatar for author
#' @export
#' @importFrom magick image_read image_crop image_write geometry_area
#' @importFrom fs dir_exists dir_create
add_author_avatar <- function(author_metadata,
                              overwrite = FALSE,
                              hugo_root_dir = ".",
                              width = 250,
                              height = 300,
                              x_off = 175,
                              y_off = 0) {


    author_dir <- create_author_dir(author_dirname = author_metadata$dir_name,
                                    hugo_root_dir = hugo_root_dir)

    author_avatar <- file.path(author_dir, "avatar.jpg")

    is_write <- !file.exists(author_avatar) || (overwrite && file.exists(author_avatar))

    if(is_write) {
    magick::image_read(author_metadata$avatar_source) %>%
    magick::image_crop(geometry = magick::geometry_area(width,
                                                        height,
                                                        x_off,
                                                        y_off)) %>%
    magick::image_write(author_avatar)
    } else {
      stop(sprintf("Author avatar %s already existing.\n
Set 'overwrite = TRUE' if you want to overwrite it!", author_avatar))
    }

}

#' Add Authors Avatars
#' @param authors_metadata as retrieved by add_authors_metadata()
#' @param overwrite (default: FALSE)
#' @param hugo_root_dir (default: ".")
#' @return creates author avatars for all authors
#' @export
add_authors_avatar <- function(authors_metadata = add_authors_metadata(),
                               overwrite = FALSE,
                               hugo_root_dir = ".") {

  sapply(seq_len(nrow(authors_metadata)), FUN = function(idx) {
    add_author_avatar(author_metadata = authors_metadata[idx,],
                      overwrite,
                      hugo_root_dir)
  })

}
