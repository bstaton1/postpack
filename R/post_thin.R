#' Thin a mcmc.list object
#'
#' Systematically remove iterations from each chain of an
#' objective of class \code{mcmc.list}. Thinning after running the model
#' is most often done during developement of post-processing code,
#' so the calculations take less time.
#'
#' @param post an object of class \code{mcmc.list}
#' @param thin_percent numeric vector of length 1 and between the interval
#'   (0,1): what fraction of the samples do you wish to remove from each
#'   chain? Defaults to \code{0.8}; i.e., removing 80\% of the samples.
#'
#' @details the samples will be removed at as evenly spaced intervals
#'   as possible, however, this is not perfect. It is therefore recommended
#'   to use the full posterior for final output, but this should be fine for
#'   most development. It can be improved by finding a better thinning rule:
#'   one that gives a consistent interval between all retained samples.
#'
#' @return an object of class \code{mcmc.list}
#'
#' @export

post_thin = function(post, thin_percent = 0.8) {
  post_dims = post_dim(post)

  keep = with(as.list(post_dims), {
    n_iters = saved/chains
    iters = 1:n_iters
    retain = ceiling(n_iters * (1 - thin_percent))
    seq(1, n_iters, by = n_iters/retain)
  })

  post_mat = as.matrix(post, chains = T, iters = T)
  unique_iters = unique(post_mat[,"ITER"])
  keep_rows = post_mat[,"ITER"] %in% unique_iters[keep]
  matrix2mcmclist(post_mat[keep_rows,])
}

## function not working ideally. the printing of iteration
  # numbers doesn't work quite right. The rownames are good
  # but
# tp = post_thin(post)
# length(tp)
# nrow((tp)[[1]])/nrow((post)[[1]])
# class(tp)
# attributes(tp)$mcpar[1] = min(iters)
# attributes(tp)$mcpar[2] = max(iters)
# attributes(tp)$mcpar[3] = unique(diff(unique(as.numeric(keep))))
