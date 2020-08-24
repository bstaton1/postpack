#' Remove nodes from output
#'
#' Just like post_subset, but keep all nodes from \code{post} except nodes that match \code{params}
#'
#' @param post an object of class \code{mcmc.list}
#' @param params a character vector of with length >= 1 specifying the nodes to summarize.
#'   Passed to \code{\link{match_params}}, so can (and sometimes should) be a regular expression.
#' @param ask logical. Do you wish to be prompted prior to removing nodes?
#'   Defaults to \code{TRUE}
#' @param auto_escape logical. \code{FALSE} will treat \code{"["} and \code{"]"}
#'   as regular expression syntax (unless explicitly escaped by user),
#'   \code{TRUE} will treat these symbols as plain text to be matched.
#'   It is generally recommended to keep this as \code{TRUE} (the default),
#'   unless you are performing complex regex searches that require the
#'   \code{"["} and \code{"]"} symbols to be special characters
#' @return an object of class \code{mcmc.list}, identical in all ways to the original
#'   except that nodes matched by the \code{params} argument are removed. If the user
#'   responds "no" to the question when \code{ask = TRUE}, the object is returned unaltered.
#' @examples
#' \dontrun{
#' get_params(cjs)                # look at the nodes present
#' cjs2 = post_remove(cjs, "SIG") # remove nodes that match "SIG"
#' get_params(cjs2)               # look at the nodes present
#' }
#' @export

post_remove = function(post, params, ask = TRUE, auto_escape = TRUE) {

  # extract the names to discard
  discard_params = match_params(post, params, type = "base_index", auto_escape = auto_escape)

  # print message and ask user if they wish to proceed
  acceptable_yes = c("y", "yes")
  acceptable_no = c("n", "no")
  if (ask) {
    message("Node(s) found:")
    message(list_out(discard_params, final = "and", per_line = 5, indent = "  ", wrap = '"'))
    message("Do you wish to proceed with removing these nodes (Y/N)?")
    answer = readline(prompt = "")
    if (!(tolower(answer) %in% c(acceptable_yes, acceptable_no))) {
      stop ("invalid answer provided")
    }
  } else {
    answer = "y"
  }

  if (tolower(answer) %in% acceptable_yes) {
    # extract the names of all nodes
    all_params = get_params(post, type = "base_index")

    # get the names of the nodes to keep
    keep_params = all_params[!(all_params %in% discard_params)]

    # extract the iteration ids: chain and iteration numbers
    ids = id_mat(post)
    n_chains = length(unique(ids[,"CHAIN"]))
    n_iter = nrow(ids)/n_chains

    # extract the samples from the right nodes in each chain, place these into list elements
    post_sub_list = lapply(post, function(x) {
      mat = as.matrix(x[,keep_params], nrow = n_iter, ncol = length(keep_params))
      colnames(mat) = keep_params
      mat
    })

    # store the samples as a matrix
    post_sub_matx = NULL
    for (c in 1:n_chains) post_sub_matx = rbind(post_sub_matx, post_sub_list[[c]])
    post_sub = cbind(ids, post_sub_matx)

    # convert back to mcmc.list
    post_out = post_convert(post_sub)

    # print a message
    message("Node(s) discarded:")
    message(list_out(discard_params, final = "and", per_line = 5, indent = "  ", wrap = '"'))

    # return the output
    return(post_out)
  } else {
    # if replied no, tell user nothing happened and return the post object
    message("No action taken, original 'post' object returned\n")
    return(post)
  }
}
