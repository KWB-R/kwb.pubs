#' Replace Umlauts
#'
#' @param string string for replacement
#' @return returns string without umlauts but with "ae", "oe", "ue",
#' "Ae", "Oe", "Ue", "ss" where needed
#' @export
#' @importFrom stringi stri_replace_all_fixed
#'
replace_umlauts <- function(string)
{
  stringi::stri_replace_all_fixed(
    string,
    c("\u00E4", "\u00F6", "\u00FC", "\u00C4", "\u00D6", "\u00DC", "\u00DF"),
    c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"),
    vectorize_all = FALSE
  )
}
