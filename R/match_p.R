#' Find matching node names
#'
#' Returns all the node names stored in an \code{mcmc.list} object
#' that match a provided string
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector with length >= 1. Passed to \code{stringr::str_detect},
#'   so can, and sometimes should, be a regular expression; see the examples.
#'   Duplicate matches found among different elements of \code{p} are discarded.
#' @param warn logical. Do you wish to be warned when duplicates and cases
#'   with more than one match per \code{p} element? Defaults to \code{FALSE}, and is provided for
#'   helping to diagnose problems with regex matching.#'
#' @return a character vector with all node names that match \code{p}.
#'   If no matches are found, it will return an error with
#'   the base node names found the \code{mcmc.list} to help the next try.
#' @details this function is called as one of the first steps in many of the more downstream
#'   functions in this package. It is thus fairly important to get used to
#'   how the regular expressions work, particularly
#'   with the \code{\link[stringr]{str_detect}} function.
#' @examples
#' \dontrun{
#' match_p(post, "alpha")
#' match_p(post, c("S", "R"))   # Elements of  S, S_msy, and Sigma_R, and R
#' match_p(post, c("R["))       # Elements of R and Sigma_R only
#' match_p(post, c("^R["))      # Elements of R
#' }
#'
#' @export

match_p = function(post, p, warn = F) {

  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # insert regex escapes for brackets if necessary
  p_regex = ifelse(!stringr::str_detect(p, "\\\\"), ins_regex_bracket(p), p)

  # extract all parameters in the post object
  all_p = get_nodes(post, type = "all")

  # extract the node names
  u_p = get_nodes(post, type = "unique")

  # determine which names match
  match_list = lapply(p_regex, function(x) {
    all_p[stringr::str_detect(all_p, x)]
  }); names(match_list) = p

  # get the base matches by element of p
  base_match_list = lapply(match_list, function(i) unique(base_p(i)))

  # number of unique base matches per element of p provided
  u_base_matches = unlist(lapply(base_match_list, function(i) length(i)))

  # stop if no matches were detected for any of the elements of p
  if (any(u_base_matches == 0)) {
    stop (
      paste(
        "\n  Supplied value(s) of p (",
        paste(p[u_base_matches == 0], collapse = ", "),
        ") did not have any matches in the nodes stored in post.\n  All elements of p must have at least one match.\n  The base names of all monitored nodes are:\n    ", paste(u_p, collapse = ", "), sep = "")
    )
  }

  # warn if two or more matches occurred for a single element of p
  if (warn & any(u_base_matches > 1)) {
    warning(
      paste(
        "\n  Supplied value(s) of p (",
        paste(p[u_base_matches > 1], collapse = ", "),
        ") matched more than one base in the nodes stored in post.\n  More nodes may be returned than expected.", sep = "")
    )
  }

  # warn if duplicates were found among elements of p
  match_vec = unname(unlist(match_list))
  if (any(duplicated(match_vec))) {
    if (warn) warning("Some matches were duplicated among elements of p.\n  All duplicates have been removed.")
    match_vec = match_vec[!duplicated(match_vec)]
  }

  # return the names of the exact nodes to extract
  match_vec
}
