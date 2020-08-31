#' @title Remove nodes from mcmc.list
#' @description Just like [post_subset()], but keep all nodes **except** those that match.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param params A vector of regular expressions specifying the nodes to match for removal.
#'   Accepts multi-element vectors to match more than one node at a time.
#'   See [match_params()] and `vignette("pattern-matching")` for more details.
#' @param ask Logical. Do you wish to be prompted prior to removing nodes?
#'   Defaults to `TRUE`.
#' @param auto_escape Automatically escape `"["` and `"]"` characters?
#'   See [match_params()] for details.
#' @return A [`mcmc.list`][coda::mcmc.list], identical in all ways to the original
#'   except that nodes matched by the `params` argument are removed. If the user
#'   responds "no" to the question when `ask = TRUE`, `post` is returned unaltered.
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # get names of all nodes
#' get_params(cjs)
#'
#' # remove the SIG nodes
#' new_cjs = suppressMessages(post_remove(cjs, "SIG", ask = FALSE))
#'
#' # get names of new output
#' get_params(new_cjs)
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
