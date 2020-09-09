#' @title Perform post-MCMC thinning
#' @description Removes iterations from each chain of a [`mcmc.list`][coda::mcmc.list]
#'   object at quasi-evenly spaced intervals. Post-MCMC thinning is useful for
#'   developing long-running post-processing code with a smaller but otherwise identical [`mcmc.list`][coda::mcmc.list].
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param keep_percent Proportion (between 0 and 1) of samples to keep from each chain.
#'   Setting `keep_percent = 0.2` will remove approximately 80 percent of the samples.
#' @param keep_iters Number of samples to keep from each chain.
#' @details The samples will be removed at as evenly spaced intervals
#'   as possible, however, this is not perfect. It is therefore recommended
#'   to use the full posterior for final post-processing calculations, but this should be fine for
#'   most development of long-running code.
#'
#'   If both `keep_percent` and `keep_iters` are supplied, an error will be returned requesting that only
#'   one be used.
#' @note Iteration numbers are reset after thinning the samples. So if running [post_dim()]
#'   on output passed through `post_thin()`, you cannot trust the burn-in or thinning counts.
#'   Again, this is not an issue for developing post-processing code.
#' @return A [`mcmc.list`][coda::mcmc.list] object, identical to `post`, but with fewer samples of each node.
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # take note of original dimensions
#' post_dim(cjs)
#'
#' # keep ~20% of the samples
#' cjs_thin1 = post_thin(cjs, keep_percent = 0.2)
#'
#' # note burn-in and thin intervals no longer correct!
#' # but desired outcome achieved - identical object but smaller
#' post_dim(cjs_thin1)
#'
#' # keep 30 samples per chain
#' cjs_thin2 = post_thin(cjs, keep_iters = 30)
#' post_dim(cjs_thin2)
#' @export

post_thin = function(post, keep_percent, keep_iters) {
  # extract the posterior dimensions
  post_dims = post_dim(post)

  # error check: make sure only one of keep_percent or keep_iters is specified
  if (!missing(keep_percent) & !missing(keep_iters)) {
    stop ("Only one of either keep_percent or keep_iters is accepted")
  }

  # determine sample indices to keep if thinning out a percent of samples
  if (!missing(keep_percent)) {
    keep = with(as.list(post_dims), {
      n_iters = saved/chains
      retain = ceiling(n_iters * keep_percent)
      seq(1, n_iters, by = n_iters/retain)
    })
  }

  # determine sample indices to keep if keeping a number of samples
  if (!missing(keep_iters)) {
    keep = with(as.list(post_dims), {
      n_iters = saved/chains
      if (keep_iters > n_iters) {
        stop("keep_iters must be less than the number of samples stored per chain")
      }
      unique(floor(seq(1, n_iters, length = keep_iters)))
    })
  }

  # convert mcmc.list to matrix
  post_mat = as.matrix(post, chains = TRUE, iters = TRUE)

  # extract unique iteration numbers of original object
  unique_iters = unique(post_mat[,"ITER"])

  # determine which rows to keep from the object
  keep_rows = post_mat[,"ITER"] %in% unique_iters[keep]

  # thin the object
  post_mat_thin = post_mat[keep_rows,]

  # rename the element sample numbers
  post_mat_thin[,"ITER"] = rep(1:length(keep), post_dims["chains"])

  # return a mcmc.list object
  post_convert(post_mat_thin)
}
