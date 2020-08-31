#' @title Find matching node names
#' @description Returns all the node names stored in a [`mcmc.list`][coda::mcmc.list]
#'   object that match a provided string.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param params A character vector with length >= 1 specifying the nodes to match.
#'   Passed to [stringr::str_detect()] so is matched using regular expressions.
#' @param type A character vector with length == 1; only two options are accepted:
#'   * `type = "base_only"` to return only the unique node names (without indices).
#'   * `type = "base_index"` (the default) to return the node names with indices included.
#' @param auto_escape Automatically escape `"["` and `"]"` characters for pattern matching?
#'  `FALSE` will treat `"["` and `"]"` as special regular expression characters (unless explicitly escaped by user),
#'   `TRUE` will treat these symbols as plain text to be matched.
#'   It is generally recommended to keep this as `TRUE` (the default),
#'   unless you are performing complex regex searches that require the
#'   `"["` and `"]"` symbols to be special characters.
#' @return A character vector with all node names in `post` that match `params`, formatted as requested by `type`..
#'   If no matches are found, an error will be returned with
#'   the base node names found in `post` to help the next try.
#' @details This function is called as one of the first steps in many of the more downstream
#'   functions in this package. It is thus fairly important to get used to
#'   how the regular expressions work. This function can be used directly to hone in on the correct regular expression.
#'   See the examples below.
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # these produce same output b/c of regex pattern matching
#' match_params(cjs, params = c("b0", "b1"))
#' match_params(cjs, params = c("b"))
#'
#' # force a match to start with B
#' match_params(cjs, "^B")
#'
#' # force a match to end with 0
#' match_params(cjs, "0$")
#'
#' # use a wild card to get b0[3] and b1[3]
#' match_params(cjs, "b.[3]")
#'
#' # repeat a wild card
#' match_params(cjs, "s.+0")
#'
#' # turn off auto escape to use [] in regex syntax rather than matching them as text
#' match_params(cjs, params = "[:digit:]$", auto_escape = FALSE)
#'
#' # pass an empty string to match all (same as get_params)
#' match_params(cjs, "")
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
        ") did not have any matches in the nodes stored in post.\n  All elements of params must have at least one match.\n  The base names of all monitored nodes are:\n", list_out(u_params, final = "and", wrap = '"', per_line = 4, indent = "    "), sep = ""),
      call. = F)
  }

  # return the names of the exact nodes to extract
  if (type == "base_index") {
    return(unique(match_vec))
  } else {
    return(unique(drop_index(match_vec)))
  }
}
