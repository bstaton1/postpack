#' Coerce a matrix to an mcmc.list
#'
#' Does as the name implies. Requires that the \code{"CHAIN"} and \code{"ITER"} are columns.
#' These can be obtained from the \code{filter_post()} function.
#'
#' @param post_mat a posterior model object, with samples in matrix format.
#'   The matrix can contain any number of nodes, but it must contain
#'   \code{"CHAIN"} and \code{"ITER"} are columns.
#'
#' @export

matrix2mcmclist = function(post_mat) {
  require(StatonMisc) # for %!in%

  if (!all(c("CHAIN", "ITER") %in% colnames(post_mat))) {
    stop ("the column names of post_mat must include 'CHAINS' and 'ITER'")
  }

  # extract the chain and iter of each sample
  chain = post_mat[,"CHAIN"]
  iters = post_mat[,"ITER"]
  p = colnames(post_mat)[which(colnames(post_mat) %!in% c("CHAIN", "ITER"))]

  # keep only parameters
  post_mat = post_mat[,p]
  if(class(post_mat) == "numeric") {
    post_mat = matrix(post_mat, ncol = 1)
  }
  # use lapply over chains to turn individual blocks of samples into list elements
    # the attributes are altered for consistency
  post_list = lapply(unique(chain), function(c) {
    # extract samples for this chain
    post_mat_sub = post_mat[chain == c,]
    if (class(post_mat_sub) == "numeric") {
      post_mat_sub = matrix(post_mat_sub, ncol = 1)
    }
    # set rownames equal to the iteration
    rownames(post_mat_sub) = unique(iters)
    colnames(post_mat_sub) = p
    # coerce to mcmc object
    post_mat_sub = coda::as.mcmc(post_mat_sub)
    # set the first and last iteration number and the thinning rate
    attributes(post_mat_sub)$mcpar[1] = min(iters)
    attributes(post_mat_sub)$mcpar[2] = max(iters)
    attributes(post_mat_sub)$mcpar[3] = unique(diff(unique(iters)))

    post_mat_sub
  })

  # coerce to an mcmc.list
  coda::as.mcmc.list(post_list)
}
