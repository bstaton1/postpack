#' Extract a subset of nodes from a \code{mcmc.list}
#'
#' Subsets a smaller portion from the joint posterior
#' corresponding only to the node(s) requested.
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector of with length >= 1 specifying the nodes to extract. Passed to \code{\link{match_p}},
#'   so can (and sometimes should) be a regular expression.
#' @param matrix logical. If \code{TRUE}, the subsetted output will be returned as a matrix.
#'   Defaults to \code{FALSE}, in which case the class \code{mcmc.list} will be retained.
#' @param iters logical. Do you wish to retain the iteration number if \code{matrix = TRUE}? Not used otherwise.
#'   Defaults to \code{FALSE}.
#' @param chains logical. Do you wish to retain the chain number if \code{matrix = TRUE}? Not used otherwise.
#'   Defaults to \code{FALSE}.
#' @seealso \code{\link{match_p}}
#' @export

post_subset = function(post, p, matrix = FALSE, iters = F, chains = F) {

  # extract the names to keep
  keep = match_p(post, p)

  # extract them from each chain and coerce to mcmc.list
  post_out = coda::as.mcmc.list(lapply(post, function(x) x[,keep]))

  # if reformatting to matrix type, do so
  if (matrix) {
    post_out = as.matrix(post_out, iters = iters, chains = chains)
    # rename the column if there is only one element to keep
    if (length(keep) == 1) {
      colnames(post_out)[colnames(post_out) == "var1"] = keep
    }
  }

  # return the output
  return(post_out)
}
