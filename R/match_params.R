#' Find matching node names
#'
#' Returns all the node names stored in an \code{mcmc.list} object
#' that match a provided string
#'
#' @param post an object of class \code{mcmc.list}
#' @param params a character vector with length >= 1. Passed to \code{stringr::str_detect},
#'   so can, and sometimes should, be a regular expression; see the examples.
#'   Duplicate matches found among different elements of \code{params} are discarded.
#' @param type a character vector with length == 1; only two options are accepted.
#'   Set to \code{type = "base_only"} if you wish to return only the unique node names (without indices).
#'   Set to \code{type = "base_index"} (the default) if you wish to return the node names with indices included.
#' @param auto_escape logical. \code{FALSE} will treat \code{"["} and \code{"]"}
#'   as regular expression syntax (unless explicitly escaped by user),
#'   \code{TRUE} will treat these symbols as plain text to be matched.
#'   It is generally recommended to keep this as \code{TRUE} (the default),
#'   unless you are performing complex regex searches that require the
#'   \code{"["} and \code{"]"} symbols to be special characters
#' @return a character vector with all node names that match \code{params}.
#'   If no matches are found, it will return an error with
#'   the base node names found the \code{mcmc.list} to help the next try.
#' @details this function is called as one of the first steps in many of the more downstream
#'   functions in this package. It is thus fairly important to get used to
#'   how the regular expressions work, particularly
#'   with the \code{\link[stringr]{str_detect}} function.
#' @examples
#' \dontrun{
#' match_params(post, "alpha")
#' match_params(post, c("S", "R"))         # Elements of  S, S_msy, and Sigma_R, and R
#' match_params(post, c("R["))             # Elements of R and Sigma_R only
#' match_params(post, c("^R["))            # Elements of R
#' match_params(post, c("^R["), ubase = T) # Only the base node name: R
#' }
#'
#' @export

match_params = function(post, params, type = "base_index", auto_escape = TRUE) {

  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # stop if type isn't one of "base_only" or "base_index"
  if (!(type %in% c("base_only", "base_index"))) {
    stop ("type must be one of 'base_only' or 'base_index'. See ?match_params for details")
  }

  # insert regex escapes for brackets if necessary
  regex_params = ifelse(!stringr::str_detect(params, "\\\\") & auto_escape, ins_regex_bracket(params), params)

  # extract all parameters in the post object
  all_params = get_params(post, type = "base_index")

  # extract the node names
  u_params = get_params(post, type = "base_only")

  # determine which names match
  match_list = lapply(regex_params, function(x) {
    all_params[stringr::str_detect(all_params, x)]
  }); names(match_list) = params

  # get the base matches by element of params
  base_match_list = lapply(match_list, function(i) unique(drop_index(i)))

  # number of unique base matches per element of p provided
  u_base_matches = unlist(lapply(base_match_list, function(i) length(i)))

  # the vector of matches
  match_vec = unname(unlist(match_list))

  # stop if no matches were detected for any of the elements of p
  if (any(u_base_matches == 0)) {
    stop (
      paste(
        "\n  Supplied value(s) of params (",
        list_out(params[u_base_matches == 0], final = "and", wrap = '"'),
        ") did not have any matches in the nodes stored in post.\n  All elements of params must have at least one match.\n  The base names of all monitored nodes are:\n", list_out(u_params, final = "and", wrap = '"', per_line = 4, indent = "    "), sep = "")
    )
  }

  # return the names of the exact nodes to extract
  if (type == "base_index") {
    return(unique(match_vec))
  } else {
    return(unique(drop_index(match_vec)))
  }
}
