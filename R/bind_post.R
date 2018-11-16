#' Combine two mcmc.list objects
#'
#' Take two mcmc.list objects, possibly from the same or different models.
#' The two posterior objects must have equal chains and iterations per chain.
#'
#' @param post1 an object of class \code{mcmc.list}
#' @param post2 an object of class \code{mcmc.list}
#'
#' @details The two lists are coerced to matricies, \code{cbind()}-ed, and
#'   converted back to to an \code{mcmc.list} using \code{matrix2mcmclist}.
#'   The chains, thinning, and iterations, burn-in settings in the output object
#'   will remain as for \code{post1}.
#'
#' @export

bind_post = function(post1, post2) {
  require(coda)

  # error check for both mcmc lists
  if (!is.mcmc.list(post1) | !is.mcmc.list(post2)) {
    stop ("both post1 and post2 must be objects of class 'mcmc.list'")
  }

  # error check for same n_iter per chain
  if (nrow(post1[[1]]) != nrow(post2[[1]])) {
    stop ("post1 and post2 must have the same number of posterior samples saved per chain")
  }

  # error check for whether the number chains is different
  if (length(post1) != length(post2)) {
    stop ("post1 and post2 must have the same number of chains saved")
  }

  # use chains and iters from post1 only
  post1_m = as.matrix(post1, iters = T, chains = T)
  post2_m = as.matrix(post2, iters = F, chains = F)

  # cbind them
  post_m = cbind(post1_m, post2_m)

  # coerce back to mcmc.list
  matrix2mcmclist(post_m)
}
