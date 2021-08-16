# rewrite_md_file --------------------------------------------------------------
rewrite_md_file <- function(
  file, pattern_empty, pattern_filled, pattern_sep, content, overwrite = FALSE,
  subject = content
)
{
  x <- readLines(file)

  is_empty <- grepl(pattern_empty, x)
  is_filled <- grepl(pattern_filled, x)

  if (sum(is_empty) == 1L) {

    x[is_empty] <- content

  } else if (sum(is_filled) == 1 && overwrite) {

    x[is_filled] <- content

  } else {

    x <- insert_after(x, pattern_sep, content)
  }

  message(paste("Adding", subject))

  writeLines(x, file, useBytes = TRUE)
}
