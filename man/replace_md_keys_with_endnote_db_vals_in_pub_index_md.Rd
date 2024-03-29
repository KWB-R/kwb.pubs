% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/replace_md_keys_with_endnote_db_vals_in_pub_index_md.R
\name{replace_md_keys_with_endnote_db_vals_in_pub_index_md}
\alias{replace_md_keys_with_endnote_db_vals_in_pub_index_md}
\title{replace_md_keys_with_endnote_db_vals_in_pub_index_md}
\usage{
replace_md_keys_with_endnote_db_vals_in_pub_index_md(
  md_paths,
  md_key,
  endnote_db_refs,
  endnote_db_col,
  file_encoding = "UTF-8",
  escape_with_double_quote = FALSE,
  dbg = TRUE
)
}
\arguments{
\item{md_paths}{paths to "publication" markdown files}

\item{md_key}{markdown key pattern to search for}

\item{endnote_db_refs}{table "refs" in Endnote DB (as retrieved by
[read_endnote_db])}

\item{endnote_db_col}{column with values to be used for replacement}

\item{file_encoding}{default: "UTF-8}

\item{escape_with_double_quote}{should be double quotes added "<endnote-value>" (default: FALSE)}

\item{dbg}{default: TRUE}
}
\value{
replaces "md_key" line in markdown files based on values in column
defined with parameter "endnote_db_col"
}
\description{
replace_md_keys_with_endnote_db_vals_in_pub_index_md
}
