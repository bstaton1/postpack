#' @title Extract samples from specific nodes
#' @description Subsets a smaller portion from a [`mcmc.list`][coda::mcmc.list] object
#'   corresponding only to the node(s) requested.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param params A vector of regular expressions specifying the nodes to match for subsetting.
#'   Accepts multi-element vectors to match more than one node at a time.
#'   See [match_params()] and `vignette("pattern-matching")` for more details.
#' @param matrix Logical. If `TRUE`, the output will be returned as a [`matrix`][base::matrix].
#'   Defaults to `FALSE`, in which case the [`mcmc.list`][coda::mcmc.list] class will be retained.
#' @param iters Logical. Do you wish to retain the iteration number of each sample if `matrix = TRUE`? Not used otherwise.
#'   Defaults to `FALSE`.
#' @param chains Logical. Do you wish to retain the chain number of each sample if `matrix = TRUE`? Not used otherwise.
#'   Defaults to `FALSE`.
#' @param auto_escape Automatically escape `"["` and `"]"` characters for pattern matching?
#'   See [match_params()] for details.
#' @return A [`mcmc.list`][coda::mcmc.list] or [`matrix`][base::matrix] object, depending on the
#'   value of the `matrix` argument. Object contains all nodes that match the `params` argument;
#'   an error will be returned if no matches are found.
#' @seealso [match_params()]
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # create mcmc.list with all nodes that contain "B0"
#' x1 = post_subset(cjs, "B0")
#'
#' # create mcmc.list with all nodes that contain "b" or "B"
#' x2 = post_subset(cjs, c("b", "B"))
#'
#' # perform the subset and return a matrix as output, while retaining the chain ID
#' x3 = post_subset(cjs, "B0", matrix = TRUE, chain = TRUE)
#' @export

post_subset = function(post, params, matrix = FALSE, iters = FALSE, chains = FALSE, auto_escape = TRUE) {

  # extract the names to keep
  keep_params = match_params(post, params, type = "base_index", auto_escape = auto_escape)

  # extract the iteration ids: chain and iteration numbers
  ids = id_mat(post)
  n_chains = length(unique(ids[,"CHAIN"]))
  n_iter = nrow(ids)/n_chains

  # extract the samples from the right nodes in each chain, place these into list elements
  post_sub_list = lapply(post, function(x) {
    mat = as.matrix(x[,keep_params], nrow = n_iter, ncol = length(keep_params))
    colnames(mat) = keep_params
    mat
    }
  )

  # store the samples as a matrix
  post_sub_matx = NULL
  for (c in 1:n_chains) post_sub_matx = rbind(post_sub_matx, post_sub_list[[c]])
  post_sub = cbind(ids, post_sub_matx)

  # if returning in matrix format, decide which columns to keep
  if (matrix) {
    keep_columns = keep_params
    if (iters) keep_columns = c("ITER", keep_columns)
    if (chains) keep_columns = c("CHAIN", keep_columns)
    post_out = as.matrix(post_sub[,keep_columns], nrow = nrow(ids), ncol = length(keep_columns))
    colnames(post_out) = keep_columns
  } else { # if returning in mcmc.list format, reformat to mcmc.list
    post_out = post_convert(post_sub)
  }

  # return the output
  return(post_out)
}
