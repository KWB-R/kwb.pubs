# replace_md_key_with_endnote_db_vals_in_pub_index_md --------------------------

#' replace_md_key_with_endnote_db_vals_in_pub_index_md
#'
#' @keywords internal
#' @noRd
#' @importFrom stringr str_extract str_detect
#' @importFrom kwb.fakin read_lines
replace_md_key_with_endnote_db_vals_in_pub_index_md <- function(
  md_path, md_key, endnote_db_refs, endnote_db_col, file_encoding = "UTF-8",
  escape_with_double_quote = FALSE,
  dbg = TRUE
)
{
  id <- as.integer(stringr::str_extract(basename(dirname(md_path)), "[0-9]+"))

  ref <- endnote_db_refs[endnote_db_refs$id == id, ]

  if (nrow(ref) != 1L)
    return()

  pub_index_txt <- kwb.fakin::read_lines(md_path, fileEncoding = file_encoding)

  idx <- which(stringr::str_detect(pub_index_txt, sprintf("^%s:", md_key)))

  if (length(idx) == 0L)
    return()

  if (dbg)
    message(sprintf(
      "Replacing %s in '%s' with values of column '%s' in Endnote DB",
      md_key, md_path, endnote_db_col
    ))

  pub_index_txt[idx] <- sprintf('%s: %s', md_key,
                                ifelse(escape_with_double_quote,
                                       sprintf('"%s"',ref[[endnote_db_col]]),
                                       ref[[endnote_db_col]]))

  write_lines(pub_index_txt, md_path, file_encoding)
}

# replace_md_keys_with_endnote_db_vals_in_pub_index_md -------------------------

#' replace_md_keys_with_endnote_db_vals_in_pub_index_md
#'
#' @param md_paths paths to "publication" markdown files
#' @param md_key markdown key pattern to search for
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [read_endnote_db])
#' @param endnote_db_col column with values to be used for replacement
#' @param file_encoding default: "UTF-8
#' @param escape_with_double_quote should be double quotes added "<endnote-value>" (default: FALSE)
#' @param dbg default: TRUE
#'
#' @return replaces "md_key" line in markdown files based on values in column
#' defined with parameter "endnote_db_col"
#' @export

replace_md_keys_with_endnote_db_vals_in_pub_index_md <- function(
  md_paths, md_key, endnote_db_refs, endnote_db_col, file_encoding = "UTF-8",
  escape_with_double_quote = FALSE,
  dbg = TRUE
)
{
  sapply(md_paths, function(md_path) {
    replace_md_key_with_endnote_db_vals_in_pub_index_md(
      md_path, md_key, endnote_db_refs, endnote_db_col, file_encoding, escape_with_double_quote, dbg
    )
  })
}

# replace_dates_in_pub_index_md ------------------------------------------------

#' replace_dates_in_pub_index_md
#'
#' @param md_paths paths to "publication" markdown files
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [add_columns_to_endnote_db])
#' @param md_key markdown key pattern to search for (default: "date")
#' @param endnote_db_col column with values to be used for replacement (default:
#' "date_cleaned")
#' @param file_encoding default: "UTF-8
#' @param escape_with_double_quote should be double quotes added "<endnote-value>" (default: FALSE)
#' @param dbg default: TRUE
#'
#' @return replaces "date" in markdown files based on values in column
#' defined with parameter "endnote_db_col"
#' @export
replace_dates_in_pub_index_md <- function(
  md_paths, endnote_db_refs, md_key = "date", endnote_db_col = "date_cleaned",
  file_encoding = "UTF-8", escape_with_double_quote = FALSE, dbg = TRUE
)
{
  replace_md_keys_with_endnote_db_vals_in_pub_index_md(
    md_paths, md_key, endnote_db_refs, endnote_db_col, file_encoding, escape_with_double_quote, dbg
  )
}

# replace_publishdates_in_pub_index_md -----------------------------------------

#' replace_publishdates_in_pub_index_md
#'
#' @param md_paths paths to "publication" markdown files
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [add_columns_to_endnote_db])
#' @param md_key markdown key pattern to search for (default: "publishDate")
#' @param endnote_db_col column with values to be used for replacement (default:
#' "date_cleaned")
#' @param file_encoding default: "UTF-8
#' @param escape_with_double_quote should be double quotes added "<endnote-value>" (default: FALSE)
#' @param dbg default: TRUE
#' @return replaces "publishDate" in markdown files based on values in column
#' defined with parameter "endnote_db_col"
#' @export
replace_publishdates_in_pub_index_md <- function(
  md_paths, endnote_db_refs, md_key = "publishDate",
  endnote_db_col = "publishDate", file_encoding = "UTF-8", escape_with_double_quote = FALSE, dbg = TRUE
)
{
  replace_md_keys_with_endnote_db_vals_in_pub_index_md(
    md_paths, md_key, endnote_db_refs, endnote_db_col, file_encoding, escape_with_double_quote, dbg
  )
}

# replace_publications_in_pub_index_md -----------------------------------------

#' replace_publications_in_pub_index_md
#'
#' @param md_paths paths to "publication" markdown files
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [add_columns_to_endnote_db])
#' @param md_key markdown key pattern to search for (default: "publication")
#' @param endnote_db_col column with values to be used for replacement (default:
#' "publication")
#' @param file_encoding default: "UTF-8
#' @param escape_with_double_quote should be double quotes added "<endnote-value>" (default: FALSE)
#' @param dbg default: TRUE
#' @return replaces "publication" entry in markdown files based on values in column
#' defined with parameter "endnote_db_col"
#' @export
replace_publications_in_pub_index_md <- function(
  md_paths, endnote_db_refs, md_key = "publication",
  endnote_db_col = "publication", file_encoding = "UTF-8", escape_with_double_quote = FALSE, dbg = TRUE
)
{
  replace_md_keys_with_endnote_db_vals_in_pub_index_md(
    md_paths, md_key, endnote_db_refs, endnote_db_col, file_encoding, escape_with_double_quote, dbg
  )
}


# replace_abstracts_in_pub_index_md -----------------------------------------

#' replace_abstracts_in_pub_index_md
#'
#' @param md_paths paths to "publication" markdown files
#' @param endnote_db_refs table "refs" in Endnote DB (as retrieved by
#' [add_columns_to_endnote_db])
#' @param md_key markdown key pattern to search for (default: "abstract")
#' @param endnote_db_col column with values to be used for replacement (default:
#' "abstract")
#' @param file_encoding default: "UTF-8
#' @param escape_with_double_quote should be double quotes added "<endnote-value>" (default: TRUE)
#' @param dbg default: TRUE
#' @return replaces "publication" entry in markdown files based on values in column
#' defined with parameter "endnote_db_col"
#' @export
replace_abstracts_in_pub_index_md <- function(
  md_paths, endnote_db_refs, md_key = "abstract",
  endnote_db_col = "abstract", file_encoding = "UTF-8", escape_with_double_quote = TRUE, dbg = TRUE
)
{
  replace_md_keys_with_endnote_db_vals_in_pub_index_md(
    md_paths, md_key, endnote_db_refs, endnote_db_col, file_encoding, escape_with_double_quote, dbg
  )
}

