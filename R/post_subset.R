#' Extract a subset of nodes from a \code{mcmc.list}
#'
#' Subsets a smaller portion from the joint posterior
#' corresponding only to the node(s) requested.
#'
#' @param post an object of class \code{mcmc.list}
#' @param params a character vector of with length >= 1 specifying the nodes to extract. Passed to \code{\link{match_params}},
#'   so can (and sometimes should) be a regular expression.
#' @param matrix logical. If \code{TRUE}, the subsetted output will be returned as a matrix.
#'   Defaults to \code{FALSE}, in which case the class \code{mcmc.list} will be retained.
#' @param iters logical. Do you wish to retain the iteration number if \code{matrix = TRUE}? Not used otherwise.
#'   Defaults to \code{FALSE}.
#' @param chains logical. Do you wish to retain the chain number if \code{matrix = TRUE}? Not used otherwise.
#'   Defaults to \code{FALSE}.
#' @param auto_escape logical. \code{FALSE} will treat \code{"["} and \code{"]"}
#'   as regular expression syntax (unless explicitly escaped by user),
#'   \code{TRUE} will treat these symbols as plain text to be matched.
#'   It is generally recommended to keep this as \code{TRUE} (the default),
#'   unless you are performing complex regex searches that require the
#'   \code{"["} and \code{"]"} symbols to be special characters
#' @seealso \code{\link{match_params}}
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
    post_out = matrix2mcmclist(post_sub)
  }

  # return the output
  return(post_out)
}
