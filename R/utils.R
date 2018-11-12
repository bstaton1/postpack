#' Insert a brackets as regex version
#'
#' @param string a character vector
#' @export

ins_regex_bracket = function(string) {
  out = stringr::str_replace(string, "\\[", "\\\\[")
  out = stringr::str_replace(out, "\\]", "\\\\]")
  return(out)
}

#' Insert regex brackets
#'
#' @param string a character vector
#' @export

rm_regex_bracket = function(string) {
  out = stringr::str_replace(string, "\\\\\\[", "\\[")
  out = stringr::str_replace(out, "\\\\\\]", "\\]")
  return(out)
}
