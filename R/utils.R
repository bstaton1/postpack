#' Insert escapes on regex brackets
#'
#' @param params a character vector
#' @details searches the contents of a string for the occurrence of a
#'  square bracket or two, and inserts the necessary escapes for searching
#'  via regular expression
#' @note this is NOT a function users will generally use.

ins_regex_bracket = function(params) {
  out = stringr::str_replace(params, "\\[", "\\\\[")
  out = stringr::str_replace(out, "\\]", "\\\\]")
  return(out)
}

#' Remove escapes on regex brackets
#'
#' @param params a character vector
#' @details searches the contents of a string for the occurrence of a
#'  square bracket or two (that has been escaped), and removes the escaping
#'  that was necessary for regular expression matching
#' @note this is NOT a function users will generally use.

rm_regex_bracket = function(params) {
  out = stringr::str_replace(params, "\\\\\\[", "\\[")
  out = stringr::str_replace(out, "\\\\\\]", "\\]")
  return(out)
}

#' Insert the symbols to lock in a string for matching
#'
#' To ensure that a regular expression will match exactly,
#' it's necessary to specify so - which is what this function does.
#'
#' @param params a character vector on to which a carrot and dollar sign
#'   will be pasted to if not already present
#' @note this is NOT a function users will generally use.

ins_regex_lock = function(params) {
  if (!stringr::str_detect(params, "\\^")) out = paste("^", params, sep = "")
  if (!stringr::str_detect(out, "\\$")) out = paste(out, "$", sep = "")
  return(out)
}

#' Remove the symbols that lock in a string for matching
#'
#' To ensure that a regular expression will match exactly,
#' it's necessary to specify so - this function undoes the work of
#' \code{\link{ins_regex_lock}} - currently just for prettier printing of node names
#'
#' @param params a character vector on to which a carrot and dollar sign
#'   will be removed from if present
#' @note this is NOT a function users will generally use.

rm_regex_lock = function(params) {
  out = stringr::str_replace(params, "\\^", "")
  out = stringr::str_replace(out, "\\$", "")
  return(out)
}

#' @title Extract the base node name of a parameter
#' @description Removes square brackets, numbers, and commas that represent
#'   the index of the node element in question. Returns just the node name.
#' @param params A character vector with length >= 1 containing node names.
#' @note This is NOT a function users will generally use.
#' @return A character vector with the same length as `params`, with no indices included.
#' @examples
#' postpack:::drop_index("a[1]")
#' postpack:::drop_index(c("a[1]", "a[2]"))
#' postpack:::drop_index(c("a[1,1,1,1,1]", "a[2,2,2,2,2]"))

drop_index = function(params) {
  stringr::str_replace(params, "\\[.+\\]", "")
}

#' Adds a title between two figures
#'
#' Used by \code{\link{diag_plots}} to place a common
#' title over top of two figures: one trace and one density
#' for a given node.
#'
#' @param text character vector length == 1. The text string to
#'   include as a title
#' @note this is NOT a function users will generally use.

mytitle = function(text) {
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  text(x = usr[1] - xdiff * 0.02, y = usr[4] + ydiff * 0.11,
       labels = text, font = 2, cex = 1.4, xpd = NA)
}

#' @title Extract chain and iteration IDs for each sample
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @return A matrix with columns`"CHAIN"` and `"ITER"`.
#' @note This is NOT a function users will generally use.

id_mat = function(post) {
  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # extract and return the sample identification information
  as.matrix(post, iters = T, chains = T)[,c("CHAIN", "ITER")]
}
