#' Quickly extract the chain and iteration IDs
#'
#' It is sometimes useful to be able to add these characteristics to a 
#' matrix of derived quantities, which then allows coercion to a \code{mcmc.list} 
#' object, which can then be binded (e.g., \code{\link{post_bind}}) to the 
#' more complete samples.
#' 
#' @param post an object of class \code{mcmc.list}
#'
#' @return a matrix with columns \code{"CHAIN"} and \code{"ITER"}
#' @export

id_mat = function(post) {
  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }
  
  # extract and return the sample identification information
  as.matrix(post, iters = T, chains = T)[,c("CHAIN", "ITER")]
}
