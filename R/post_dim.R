#' Obtain MCMC dimensions from an mcmc.list
#'
#' Quickly query the number of burn-in samples, post-burnin, thinning,
#' number of chains, etc. from an object of class \code{mcmc.list}
#'
#' @param post an object of class \code{mcmc.list}
#'
#' @return vector with named elements
#' \itemize{
#'   \item \code{burn} - The burn-in period (per chain)
#'   \item \code{post_burn} - The post-burn-in period (per chain)
#'   \item \code{thin} - The thinning interval post-burn-in
#'   \item \code{chains} - The number of chains
#'   \item \code{saved} - The number of saved samples across all chains
#'   \item \code{nodes} - The number of nodes with MCMC samples
#' }
#'
#' @note if the \code{post} object was thinned after MCMC completed
#'   using \code{\link{post_thin}}, then these dimensions will be improperly calculated
#'   (currently).
#'
#' @export

post_dim = function(post) {

  # stop if not an mcmc.list object
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # coerce to matrix
  postm = as.matrix(post, iters = T, chains = T)
  iter = unique(postm[,"ITER"])
  chain = postm[,"CHAIN"]

  # calculate quantities
  n_thin = min(unique(diff(iter)))
  n_chains = max(chain)
  f_iter = min(iter)
  l_iter = max(iter)
  n_burn = unname(f_iter - n_thin)
  n_chain = unname(length(post))
  n_iter = unname(l_iter - f_iter + n_thin)
  n_save = unname((n_iter/n_thin * n_chain))

  # bundle output
  out = c(
    burn = n_burn,
    post_burn = n_iter,
    thin = n_thin,
    chains = n_chains,
    saved = n_save,
    nodes = ncol(postm[,-which(colnames(postm) %in% c("CHAIN", "ITER"))])
  )

  # return the output
  return(out)
}
