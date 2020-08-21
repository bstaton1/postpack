#' Thin a mcmc.list object
#'
#' Remove iterations from each chain of an
#' objective of class \code{mcmc.list} at quasi-evenly spaced intervals.
#' Thinning samples after running the model is useful for
#' developing long-running post-processing code.
#'
#' @param post an object of class \code{mcmc.list}
#' @param keep_percent numeric vector of length 1 and between the interval
#'   (0,1): what fraction of the samples do you wish to retain from each
#'   chain? Setting \code{keep_percent 0.2} will remove approximately 80\% of the samples.
#' @param keep_iters numeric vector of length 1 representing how many samples from each
#'   chain to retain.
#' @details The samples will be removed at as evenly spaced intervals
#'   as possible, however, this is not perfect. It is therefore recommended
#'   to use the full posterior for final output, but this should be fine for
#'   most development.
#' @note Iteration numbers are reset after thinning the samples, if running \code{\link{post_dim}}
#'   on output passed through \code{post_thin}, you cannot trust the burn-in or thinning counts.
#'   Again, this is not an issue for developing post-processing code, but is why you should perform final
#'   calculations using the full posterior output.
#'
#' @return an object of class \code{mcmc.list}
#'
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
  post_mat = as.matrix(post, chains = T, iters = T)

  # extract unique iteration numbers of original object
  unique_iters = unique(post_mat[,"ITER"])

  # determine which rows to keep from the object
  keep_rows = post_mat[,"ITER"] %in% unique_iters[keep]

  # thin the object
  post_mat_thin = post_mat[keep_rows,]

  # rename the element sample numbers
  post_mat_thin[,"ITER"] = rep(1:length(keep), post_dims["chains"])

  # return a mcmc.list object
  matrix2mcmclist(post_mat_thin)
}
