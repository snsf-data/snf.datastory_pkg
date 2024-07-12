#' Print numbers as characters with language specific format
#'
#' Function to format a given number with a specified language-specific
#' formatting (thousand separator and decimal separator)
#'
#' @param x The numeric to be transformed
#' @param lang Output format language, available is "en", "de", or "fr"
#'
#' @export
#'
#' @examples
#'  print_num(10000, "en")
#'  print_num(10000, "fr")
#'  print_num(10000, "de")
#'  print_num(pi, "en")
#'  print_num(pi, "fr")
#'  print_num(pi, "de")

print_num <- function(x, lang = "en") {

  chk::chk_string(lang)
  chk::chk_subset(lang, c("en", "de", "fr"))

  sapply(
    x,
    \(x) {
      x <- format(x, scientific = FALSE)
      if (x >= 10000) {
        if (lang == "en")
          prettyNum(x, big.mark = ",", decimal.mark = ".")
        else if (lang == "de")
          prettyNum(x, big.mark = "\U00A0", decimal.mark = ",")
        else if (lang == "fr")
          prettyNum(x, big.mark = "\U00A0", decimal.mark = ",")
      } else {
        if (lang == "en")
          prettyNum(x, big.mark = "", decimal.mark = ".")
        else if (lang == "de")
          prettyNum(x, big.mark = "", decimal.mark = ",")
        else if (lang == "fr")
          prettyNum(x, big.mark = "", decimal.mark = ",")
      }
    }
  )
}
