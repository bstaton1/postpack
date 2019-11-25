#' Find matching node names
#'
#' Returns all the node names stored in an \code{mcmc.list} object
#' that match a provided string
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector with length >= 1. Passed to \code{stringr::str_detect},
#'   so can, and sometimes should, be a regular expression; see the examples.
#'   Duplicate matches found among different elements of \code{p} are discarded.
#' @param ubase logical. Do you wish to return only the unique bases (i.e., without indices listed)?
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
#' match_p(post, c("S", "R"))         # Elements of  S, S_msy, and Sigma_R, and R
#' match_p(post, c("R["))             # Elements of R and Sigma_R only
#' match_p(post, c("^R["))            # Elements of R
#' match_p(post, c("^R["), ubase = T) # Only the base node name: R
#' }
#'
#' @export

match_p = function(post, p, ubase = F) {

  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # insert regex escapes for brackets if necessary
  p_regex = ifelse(!stringr::str_detect(p, "\\\\"), ins_regex_bracket(p), p)

  # extract all parameters in the post object
  all_p = get_p(post, type = "all")

  # extract the node names
  u_p = get_p(post, type = "base")

  # determine which names match
  match_list = lapply(p_regex, function(x) {
    all_p[stringr::str_detect(all_p, x)]
  }); names(match_list) = p

  # get the base matches by element of p
  base_match_list = lapply(match_list, function(i) unique(base_p(i)))

  # number of unique base matches per element of p provided
  u_base_matches = unlist(lapply(base_match_list, function(i) length(i)))

  # the vector of matches
  match_vec = unname(unlist(match_list))

  # stop if no matches were detected for any of the elements of p
  if (any(u_base_matches == 0)) {
    stop (
      paste(
        "\n  Supplied value(s) of p (",
        StatonMisc::list_out(p[u_base_matches == 0], final = "and", wrap = '"'),
        ") did not have any matches in the nodes stored in post.\n  All elements of p must have at least one match.\n  The base names of all monitored nodes are:\n", StatonMisc::list_out(u_p, final = "and", wrap = '"', per_line = 4, indent = "    "), sep = "")
    )
  }

  # return the names of the exact nodes to extract
  if (!ubase) {
    return(unique(match_vec))
  } else {
    return(unique(base_p(match_vec)))
  }
}

