#' @keywords internal
#' @noRd
template_author_md_default <- function()
{
  package_file("templates/author.md")
}

#' @keywords internal
#' @noRd
read_template_author_md <- function(path = template_author_md_default())
{
  readLines(path)
}

#' Create Author Social
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with social icons
#' @export
#' @importFrom dplyr select starts_with
#' @importFrom kwb.utils isNaOrEmpty
create_author_social <- function(author_metadata)
{
  social_metadata <- author_metadata %>%
    dplyr::select(dplyr::starts_with(match = "social_"))

  drop_na_or_empty <- ! kwb.utils::isNaOrEmpty(social_metadata)
  social_metadata <- social_metadata[, drop_na_or_empty, drop = FALSE]

  social_list <- lapply(names(social_metadata), function(x) {
    eval(call(sprintf('create_%s', x), social_metadata[, x]))
  })

  unlist(social_list)
}

#' Create Author Education
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with education
#' @export
#' @importFrom dplyr select starts_with mutate
#' @importFrom tidyr gather separate
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#'
create_author_education <- function(author_metadata) {

  start_tag <- "education"
  education_metadata <- author_metadata %>%
    dplyr::select(dplyr::starts_with(match = start_tag)) %>%
    tidyr::gather() %>%
    tidyr::separate(col = .data$key, c("id", "var")) %>%
    dplyr::mutate(id = as.numeric(stringr::str_remove(.data$id, start_tag))) %>%
    tidyr::spread(.data$var, .data$value)

  education_metadata[is.na(education_metadata)] <- ""
  drop_empty <- all(education_metadata[, 2:4] == "")

  if (any(drop_empty))
    education_metadata <- education_metadata[! drop_empty, ]

  n_edus <- nrow(education_metadata)

  if (n_edus == 0L)
    return("")

  unlist(lapply(seq_len(n_edus), function(i) {
    create_education(
      course =  education_metadata[["course"]][i],
      institution =  education_metadata[["institution"]][i],
      year =  education_metadata[["year"]][i]
    )
  }))
}

#' Create Author Interests
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with interests
#' @importFrom kwb.utils isNaOrEmpty
#' @export
create_author_interests <- function(author_metadata)
{
  interests <- author_metadata$interests

  if (kwb.utils::isNaOrEmpty(interests))
    return("")

  interests <- unlist(stringr::str_split(interests, ",(\\s+)?"))
  interests <- paste0("  ", interests, collapse = ",\n")
  interests <- unlist(stringr::str_split(interests, "\n"))

  enclose_in_empty_strings('interests = [', interests, ']')
}

#' Create Author User Groups
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with user_groups
#' @importFrom kwb.utils isNaOrEmpty
#' @export
create_author_user_groups <- function(author_metadata)
{
  value <- author_metadata[["user_groups"]]

  if (kwb.utils::isNaOrEmpty(value))
    return("")

  user_groups <- unlist(stringr::str_split(value, ",(\\s+)?"))
  user_groups <- kwb.utils::stringList(user_groups, qchar = '"')

  enclose_in_empty_strings(sprintf('user_groups = [%s]', user_groups))
}

#' Create Author Username
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with author username
#' @importFrom kwb.utils isNaOrEmpty
#' @export
create_username <- function(author_metadata)
{
  value <- author_metadata[["dir_name"]]

  if (kwb.utils::isNaOrEmpty(value))
    return("")

  enclose_in_empty_strings(sprintf('authors = ["%s"]', value))
}

#' @keywords internal
#' @noRd
add_complex_tag <- function(author_md, tag_pattern = "<social>", data)
{
  pos <- which(stringr::str_detect(author_md, tag_pattern))

  if (length(pos) == 0L) {

    message("No tag ", tag_pattern," to add found.")
    return(author_md)
  }

  c(
    author_md[1:(pos - 1L)],
    data,
    author_md[(pos + 1L):length(author_md)]
  )
}

#' Prepare Author "_index.md"
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param template_author_md default: template_author_md_default()
#' @return "_index.md" for author as text
#' @export
#' @importFrom stringr str_detect str_remove_all
#' @importFrom kwb.utils isNaOrEmpty
prepare_author_index_md <- function(
  author_metadata,
  template_author_md = template_author_md_default()
)
{
  tags_pattern <- "<\\w+>"
  simple_tags <- sprintf("<%s>", c("fullname", "role", "bio_short"))

  template_md <- read_template_author_md(template_author_md)
  author_md <- template_md

  simple_tags_idx <- which(
    stringr::str_detect(template_md, paste0(simple_tags, collapse = "|"))
  )

  ## Change simple tags (one line values: "fullname", "role", "bio_short")
  for (tag_idx in  simple_tags_idx) {

    tag_full <- stringr::str_extract(author_md[tag_idx], tags_pattern)
    tag_name <- stringr::str_remove_all(tag_full, "<|>")
    tag_value <- ifelse(
      is.na(author_metadata[[tag_name]]),
      "",
      sprintf(
        '%s\"%s\"',
        stringr::str_remove(author_md[tag_idx], tag_full),
        format_tag(author_metadata[[tag_name]])
      )
    )

    author_md[tag_idx] <- tag_value
  }

  ## Change "bio_full" tag
  bio_full_tag_idx <- which(stringr::str_detect(author_md, "<bio_full>"))

  tag_full <- stringr::str_extract(author_md[bio_full_tag_idx], tags_pattern)
  tag_name <- stringr::str_remove_all(tag_full, "<|>")

  author_md[bio_full_tag_idx] <- ifelse(
    kwb.utils::isNaOrEmpty(author_metadata[[tag_name]]),
    "",
    format_tag(author_metadata[[tag_name]])
  )

  tag_functions <- list(
    "<username>" = create_username,
    "<user_groups>" = create_author_user_groups,
    "<social>" = create_author_social,
    "<interests>" = create_author_interests,
    "<education>" = create_author_education
  )

  # Add tags of different types
  for (tag_pattern in names(tag_functions)) {

    author_md <- add_complex_tag(
      author_md = author_md,
      tag_pattern = tag_pattern,
      data = tag_functions[[tag_pattern]](author_metadata)
    )
  }

  author_md
}

#' Add Author Index.md
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param overwrite default: FALSE
#' @param hugo_root_dir (default: ".")
#' @return creates author index.md for one author
#' @export
add_author_index_md <- function(
  author_metadata, overwrite = FALSE, hugo_root_dir = "."
)
{
  author_dir <- create_author_dir(
    author_dirname = author_metadata$dir_name,
    hugo_root_dir = hugo_root_dir
  )

  index_md <- file.path(author_dir, "_index.md")

  if (file.exists(index_md) && ! overwrite)
    stop(
      "Author ", index_md, " already existing.\n",
      "Set 'overwrite = TRUE' if you want to overwrite it!",
    )

  message(sprintf("Writting %s", index_md))

  writeLines(
    text = prepare_author_index_md(author_metadata),
    con = index_md,
    useBytes = TRUE
  )
}

#' Add Authors Index.md
#'
#' @param authors_metadata as retrieved by add_authors_metadata()
#' @param overwrite (default: FALSE)
#' @param hugo_root_dir (default: ".")
#' @return creates author index.md for all authors
#' @export
add_authors_index_md <- function(
  authors_metadata = add_authors_metadata(),
  overwrite = FALSE,
  hugo_root_dir = "."
)
{
  sapply(seq_len(nrow(authors_metadata)), FUN = function(i) try(
    add_author_index_md(
      author_metadata = authors_metadata[i, ],
      overwrite,
      hugo_root_dir
    )
  ))
}
