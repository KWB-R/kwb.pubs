
#' @keywords internal
#' @noRd

template_author_md_default <- function() {

  system.file("templates/author.md", package = "kwb.pubs")
}


#' @keywords internal
#' @noRd
read_template_author_md <- function(path = template_author_md_default()) {
  readLines(path)
}





#' Create Author Social
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with social icons
#' @export
#' @importFrom dplyr select starts_with
create_author_social <- function(author_metadata) {

  social_metadata <- author_metadata %>%
    dplyr::select(dplyr::starts_with(match = "social_"))


  drop_na_or_empty <- !(is.na(social_metadata) | social_metadata == "")
  social_metadata <- social_metadata[, drop_na_or_empty, drop = FALSE]


  social_list <- lapply(names(social_metadata), function(x) {

   eval(call(sprintf('create_%s', x), social_metadata[,x]))})

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
  drop_empty <- all(education_metadata[,2:4]=="")


  if(any(drop_empty)) education_metadata <- education_metadata[!drop_empty, ]


  n_edus <- nrow(education_metadata)

  if(n_edus > 0) {

  edus_list <- lapply(seq_len(n_edus), function(idx) {
  create_education(course =  education_metadata[["course"]][idx],
                   institution =  education_metadata[["institution"]][idx],
                   year =  education_metadata[["year"]][idx])
      })

    unlist(edus_list)

    } else {
      ""
    }

}


#' Create Author Interests
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with interests
#' @export
create_author_interests <- function(author_metadata) {


  is_na_or_empty <- is.na(author_metadata$interests) | author_metadata$interests==""

  if(is_na_or_empty) {
    ""
  } else {
    interests <- unlist(author_metadata$interests %>%
             stringr::str_split(",(\\s+)?"))

    interests <- unlist(stringr::str_split(paste0(sprintf("  %s", interests),
                                                  collapse = ",\n"), "\n"))

    c('',
      'interests = [',
      interests,
      ']',
      ''
      )
  }


}


#' Create Author User Groups
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with user_groups
#' @export
create_author_user_groups <- function(author_metadata) {

  col_name <- "user_groups"

  is_na_or_empty <- is.na(author_metadata[,col_name]) | author_metadata[,col_name]==""

  if(is_na_or_empty) {
    ""
  } else {
    user_groups <- unlist(author_metadata[,col_name] %>%
                          stringr::str_split(",(\\s+)?"))

    user_groups <- paste0(sprintf('"%s"', user_groups), collapse = ", ")

    c('',
      sprintf('user_groups = [%s]', user_groups),
      ''
    )
  }


}

#' Create Author Username
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @return txt with author username
#' @export
create_username <- function(author_metadata) {

  col_name <- "dir_name"

  is_na_or_empty <- is.na(author_metadata[,col_name]) | author_metadata[,col_name]==""

  if(is_na_or_empty) {
    ""
  } else {

    c('',
      sprintf('authors = ["%s"]', author_metadata[,col_name]),
      ''
    )
  }


}

#' @keywords internal
#' @noRd
add_complex_tag <- function(author_md,
                    tag_pattern = "<social>",
                    data) {

  tag_idx <- which(stringr::str_detect(string = author_md,
                                       pattern = tag_pattern))

  if(length(tag_idx) > 0) {

  c(author_md[1:(tag_idx-1)],
    data,
    author_md[(tag_idx+1):length(author_md)])
  } else {
    message(sprintf("No tag %s to add found.", tag_pattern))
    author_md
  }

}

#' Prepare Author "_index.md"
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param template_author_md default: template_author_md_default()
#' @return "_index.md" for author as text
#' @export
#' @importFrom stringr str_detect str_remove_all

prepare_author_index_md <- function(author_metadata,
                                   template_author_md = template_author_md_default()) {

  tags_pattern <- "<\\w+>"
  simple_tags <-  sprintf("<%s>", c("fullname", "role", "bio_short"))


  template_md <- read_template_author_md(template_author_md)
  author_md <- template_md

  simple_tags_idx <- which(stringr::str_detect(string = template_md,
                                               pattern = paste0(simple_tags,
                                                                collapse = "|")))

  ## Change simple tags (one line values: "fullname", "role", "bio_short")
  for(tag_idx in  simple_tags_idx) {
    tag_full <-  author_md[tag_idx] %>%
      stringr::str_extract(tags_pattern)
    tag_name <- tag_full %>%
      stringr::str_remove_all("<|>")

   tag_value <- ifelse(is.na(author_metadata[[tag_name]]),
                          "",
                          sprintf('%s\"%s\"',
                                  stringr::str_remove(author_md[tag_idx],
                                                      tag_full),
                                  author_metadata[[tag_name]] %>%
                                  stringr::str_replace_all("\r", " ") %>%
                                  stringr::str_replace_all("\"", "\\\\\"")))

      author_md[tag_idx] <- tag_value
  }

  ## Change "bio_full" tag
  bio_full_tag_idx <- which(stringr::str_detect(string =  author_md,
                                               pattern = "<bio_full>"))

                            tag_full <- author_md[bio_full_tag_idx] %>%
                              stringr::str_extract(tags_pattern)
                            tag_name <- tag_full %>%
                              stringr::str_remove_all("<|>")

is_na_or_empty <- is.na(author_metadata[[tag_name]]) | author_metadata[[tag_name]]==""
                            tag_value <- ifelse(is_na_or_empty,
                                                "",
                                                author_metadata[[tag_name]] %>%
                                                  stringr::str_replace_all("\r", " ") %>%
                                                  stringr::str_replace_all("\"", "\\\\\""))

                            author_md[bio_full_tag_idx] <- tag_value



  ### Add username tag
  author_md <- add_complex_tag(author_md = author_md,
                               tag_pattern = "<username>",
                               data = create_username(author_metadata))
  ### Add user_groups tags
  author_md <- add_complex_tag(author_md = author_md,
                               tag_pattern = "<user_groups>",
                               data = create_author_user_groups(author_metadata))


  ### Add social tags

  author_md <- add_complex_tag(author_md = author_md,
                               tag_pattern = "<social>",
                               data = create_author_social(author_metadata))

  ### Add interests

  author_md <- add_complex_tag(author_md = author_md,
                               tag_pattern = "<interests>",
                               data = create_author_interests(author_metadata))

  ### Add education

  author_md <- add_complex_tag(author_md = author_md,
                               tag_pattern = "<education>",
                               data = create_author_education(author_metadata))

  author_md

}


#' Add Author Index.md
#'
#' @param author_metadata one record as retrieved by add_authors_metadata()
#' @param overwrite default: FALSE
#' @param hugo_root_dir (default: ".")
#' @return creates author index.md for one author
#' @export
add_author_index_md <- function(author_metadata,
                                 overwrite = FALSE,
                                 hugo_root_dir = ".") {

  author_dir <- create_author_dir(author_dirname = author_metadata$dir_name,
                                  hugo_root_dir = hugo_root_dir)

  index_md <- file.path(author_dir, "_index.md")

  is_write <- !file.exists(index_md) || (overwrite && file.exists(index_md))

  if(is_write) {
   message(sprintf("Writting %s", index_md))
   index_txt <- prepare_author_index_md(author_metadata)
   writeLines(text = index_txt,
              con = index_md,
              useBytes = TRUE)
  } else {
    stop(sprintf("Author %s already existing.\n
                 Set 'overwrite = TRUE' if you want to overwrite it!",
                 index_md))
  }

}

#' Add Authors Index.md
#'
#' @param authors_metadata as retrieved by add_authors_metadata()
#' @param overwrite (default: FALSE)
#' @param hugo_root_dir (default: ".")
#' @return creates author index.md for all authors
#' @export
add_authors_index_md <- function(authors_metadata = add_authors_metadata(),
                                 overwrite = FALSE,
                                 hugo_root_dir = ".") {

  sapply(seq_len(nrow(authors_metadata)), FUN = function(idx) {
    try(add_author_index_md(author_metadata = authors_metadata[idx,],
                      overwrite,
                      hugo_root_dir))
  })

}
