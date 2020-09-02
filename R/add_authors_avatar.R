#' @keywords internal
#' @noRd
config_authors_default <- function()
{
  package_file("config/authors.txt")
}

#' Get Authors Config
#'
#' @param path path to authors config (default: config_authors_default())
#' @return imports authors config as data.frame
#' @importFrom utils read.delim
#' @export
get_authors_config <- function(path = config_authors_default())
{
  utils::read.delim(
    file = file(path, encoding = "UCS-2LE"),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
#' @importFrom stringr str_trim str_to_lower str_to_lower
construct_email <- function(firstname, lastname)
{
  sprintf(
    "%s.%s@kompetenz-wasser.de",
    clean_lower(firstname),
    clean_lower(lastname)
  )
}

#' @keywords internal
#' @noRd
#' @importFrom stringr str_trim
construct_phonenumbers <- function(phonenumbers)
{
  unlist(lapply(phonenumbers, function(x) {

    x <- as.character(x)

    if (is.na(x))
      return(NA_character_)

    if (is_kwb_number <- nchar(stringr::str_trim(x)) == 3L)
      return(paste0("+493053653", x))

    if (has_countrycode <- stringr::str_detect(x, pattern = "\\+49|0049"))
      return(x)

    if (has_berlin_code <- stringr::str_detect(x, pattern = "^030"))
      return(paste0(
        "+49", stringr::str_replace(x, pattern = "^030", replacement = "30")
      ))

    paste0("+4930", x)
  }))
}

#' @keywords internal
#' @noRd
construct_fullname <- function(firstname, lastname)
{
  paste(trimmed_title(firstname), trimmed_title(lastname))
}

#' @keywords internal
#' @noRd
#' @importFrom stringr str_to_lower str_trim str_sub
#' @importFrom magrittr %>%
construct_dirname <- function(firstname, lastname)
{
  # dir_firstname <- unlist(lapply(firstname, function(x) {
  #   x %>%
  #     stringr::str_trim() %>%
  #     split_one("-|\\s+") %>%
  #     stringr::str_sub(1, 1) %>%
  #     stringr::str_to_lower() %>%
  #     replace_umlauts() %>%
  #     paste0(".") %>%
  #     dash_collapsed()
  # }))

  dir_lastname <- lastname %>%
    stringr::str_trim() %>%
    space_to_single_dash() %>%
    stringr::str_to_lower() %>%
    replace_umlauts()

  dir_lastname
}

#' @keywords internal
#' @noRd
#' @importFrom stringr str_trim str_sub
#' @importFrom magrittr %>%
construct_authorname <- function (firstname, lastname)
{
  firstname <- unlist(lapply(firstname, function(x) {
    x %>%
      stringr::str_trim() %>%
      split_one("-|\\s+") %>%
      stringr::str_sub(1, 1) %>%
      stringr::str_to_upper() %>%
      paste0(".", collapse = ifelse(grepl("-", x), "-", " "))
  }))

  lastname <- lastname %>%
    space_to_single_space() %>%
    trimmed_title()

  sprintf("%s, %s", lastname, firstname)
}

#' Get Authors Metadata
#'
#' @param authors_config default: get_authors_config()
#' @return add authors metadata (email, dirname)
#' @importFrom kwb.utils setColumns
#' @export
add_authors_metadata <- function(authors_config = get_authors_config())
{
  firstnames <- authors_config$firstname
  lastnames <- authors_config$lastname

  no_social <- is.na(authors_config$social_email)

  if (sum(no_social) > 1L)
    authors_config$social_email[no_social] <- construct_email(
      firstnames[no_social], lastnames[no_social]
    )

  kwb.utils::setColumns(
    authors_config,
    fullname = construct_fullname(firstnames, lastnames),
    author_name = construct_authorname(firstnames, lastnames),
    dir_name = construct_dirname(firstnames, lastnames),
    social_telephone = construct_phonenumbers(authors_config$social_telephone)
  )
}

#' Create Author Dir (if not existing)
#'
#' @keywords internal
#' @noRd
#' @importFrom fs dir_exists dir_create path_abs
#'
create_author_dir <- function(author_dirname, hugo_root_dir = ".")
{
  content_dir <- file.path(hugo_root_dir, "content")

  if (! fs::dir_exists(content_dir)) {
    stop(sprintf("Directory '%s' does not exist!", fs::path_abs(content_dir)))
  }

  author_dir <- sprintf("%s/authors/%s", content_dir, author_dirname)

  if (fs::dir_exists(author_dir)) {

    message(
      "Author directory '", fs::path_abs(author_dir), "' already existing"
    )

    return(author_dir)
  }

  fs::dir_create(author_dir)
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
add_author_avatar <- function(
  author_metadata,
  overwrite = FALSE,
  hugo_root_dir = ".",
  width = 250,
  height = 300,
  x_off = 175,
  y_off = 0
)
{
  author_dir <- create_author_dir(
    author_dirname = author_metadata$dir_name,
    hugo_root_dir = hugo_root_dir
  )

  author_avatar <- file.path(author_dir, "avatar.jpg")

  if (file.exists(author_avatar) && ! overwrite)
    stop(
      "Author avatar ", author_avatar, " already existing.\n",
      "Set 'overwrite = TRUE' if you want to overwrite it!"
    )

  magick::image_read(author_metadata$avatar_source) %>%
    magick::image_crop(
      geometry = magick::geometry_area(width, height, x_off, y_off)
    ) %>%
    magick::image_write(author_avatar)
}

#' Add Authors Avatars
#' @param authors_metadata as retrieved by add_authors_metadata()
#' @param overwrite (default: FALSE)
#' @param hugo_root_dir (default: ".")
#' @return creates author avatars for all authors
#' @export
add_authors_avatar <- function(
  authors_metadata = add_authors_metadata(),
  overwrite = FALSE,
  hugo_root_dir = "."
)
{
  sapply(seq_len(nrow(authors_metadata)), FUN = function(i) try(
    add_author_avatar(authors_metadata[i, ], overwrite, hugo_root_dir)
  ))
}
