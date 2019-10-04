#' Combine two mcmc.list objects
#'
#' Bind together two mcmc.list objects, possibly from the same or different models.
#' The two posterior objects must have equal chains and iterations per chain.
#'
#' @param post1 an object of class \code{mcmc.list}
#' @param post2 an object of class \code{mcmc.list}
#' @param dup_id a character vector of length == 1. If any
#'   node names are duplicated in post2, what should be appended to the
#'   end of the node names in the output? Defaults to \code{"_p2"}
#'
#' @details The two lists are coerced to matricies, \code{cbind()}-ed, and
#'   converted back to to an \code{mcmc.list} using \code{matrix2mcmclist}.
#'   The chains, thinning, and iterations, burn-in settings in the output object
#'   will remain as for \code{post1}.
#'
#' @export

post_bind = function(post1, post2, dup_id = "_p2") {

  # error check for both mcmc lists
  if (!coda::is.mcmc.list(post1) | !coda::is.mcmc.list(post2)) {
    stop ("both post1 and post2 must be objects of class 'mcmc.list'")
  }

  # error check for same n_iter per chain
  if (post_dim(post1, "post_burn") != post_dim(post2, "post_burn")) {
    stop ("post1 and post2 must have the same number of posterior samples saved per chain")
  }

  # error check for whether the number chains is different
  if (post_dim(post1, "chains") != post_dim(post2, "chains")) {
    stop ("post1 and post2 must have the same number of chains saved")
  }

  # use chains and iters from post1 only
  post1_m = as.matrix(post1, iters = T, chains = T)
  post2_m = as.matrix(post2, iters = F, chains = F)

  # cbind them
  post_m = cbind(post1_m, post2_m)

  # if any names are duplicated, append them for identification
  colnames(post_m) = ifelse(duplicated(colnames(post_m)), paste0(colnames(post_m), dup_id), colnames(post_m))

  # coerce back to mcmc.list
  matrix2mcmclist(post_m)
}
