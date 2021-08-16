#' Helper function: write Lines To File with File Encoding
#'
#' @param text text
#' @param file file
#' @param fileEncoding fileEncoding
#' @param ... additional arguments passed to [writeLines]
#'
#' @return writes file with "fileEncoding"
#' @export

write_lines <- function (text, file, fileEncoding = "", ...)
{
  if (is.character(file)) {

    con <- if (nzchar(fileEncoding)) {

      file(file, "wt", encoding = fileEncoding)

    } else {

      file(file, "wt")
    }

    on.exit(close(con))

  } else {

    con <- file
  }

  writeLines(text, con, ...)
}
