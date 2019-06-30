#' Insert escapes on regex brackets
#'
#' @param string a character vector
#' @details searches the contents of a string for the occurrence of a
#'  square bracket or two, and inserts the necessary escapes for searching
#'  via regular expression
#' @note this is NOT a function users will generally use.

ins_regex_bracket = function(string) {
  out = stringr::str_replace(string, "\\[", "\\\\[")
  out = stringr::str_replace(out, "\\]", "\\\\]")
  return(out)
}

#' Remove escapes on regex brackets
#'
#' @param string a character vector
#' @details searches the contents of a string for the occurrence of a
#'  square bracket or two (that has been escaped), and removes the escaping
#'  that was necessary for regular expression matching
#' @note this is NOT a function users will generally use.

rm_regex_bracket = function(string) {
  out = stringr::str_replace(string, "\\\\\\[", "\\[")
  out = stringr::str_replace(out, "\\\\\\]", "\\]")
  return(out)
}

#' Insert the symbols to lock in a string for matching
#'
#' To ensure that a regular expression will match exactly,
#' it's necessary to specify so - which is what this function does.
#'
#' @param string a character vector on to which a carrot and dollar sign
#'   will be pasted to if not already present
#' @note this is NOT a function users will generally use.

ins_regex_lock = function(string) {
  if (!stringr::str_detect(string, "\\^")) out = paste("^", string, sep = "")
  if (!stringr::str_detect(out, "\\$")) out = paste(out, "$", sep = "")
  return(out)
}

#' Remove the symbols that lock in a string for matching
#'
#' To ensure that a regular expression will match exactly,
#' it's necessary to specify so - this function undoes the work of
#' \code{\link{ins_regex_lock}} - currently just for prettier printing of node names
#'
#' @param string a character vector on to which a carrot and dollar sign
#'   will be removed from if present
#' @note this is NOT a function users will generally use.

rm_regex_lock = function(string) {
  out = stringr::str_replace(string, "\\^", "")
  out = stringr::str_replace(out, "\\$", "")
  return(out)
}

#' Extract the base of a node name
#'
#' Removes square brackets, numbers, and commas that represent
#' the index of the node element in question. Returns just the
#' text portion. E.g, \code{"R\[1\]"} becomes \code{"R"}.
#'
#' @param p character vector containing node names
#' @note this is NOT a function users will generally use.

base_p = function(p) {
  stringr::str_replace(p, "\\[.+\\]", "")
}
