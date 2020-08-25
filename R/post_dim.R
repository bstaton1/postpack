#' @title Obtain MCMC dimensions from an mcmc.list
#' @description Quickly query the number of burn-in samples, post-burnin, thinning,
#'   number of chains, etc. from a [`mcmc.list`][coda::mcmc.list] object.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param types A character vector with length >= 1 and containing some of `"burn"`, `"post_burn"`,
#'   `"thin"`, `"chains"`, `"nodes"`. Defaults to `NULL`, in which case all of these are returned.
#' @return A numeric vector with named elements, which may contain:
#'   * `burn`: The burn-in period (per chain).
#'   * `post_burn`: The post-burn-in period (per chain).
#'   * `thin`: The thinning interval post-burn-in.
#'   * `chains`: The number of chains.
#'   * `saved`: The number of saved samples across all chains.
#'   * `params`: The number of nodes with MCMC samples.
#'
#'  All of these will be returned if `types = NULL`, a subset can be returned by
#'  specifying (for example) `types = c("burn", "thin")`.
#' @note If the `post` object was thinned after MCMC completed
#'   using [post_thin()], then the `"burn"` and `"thin"` dimensions will be improperly calculated.
#'   [post_thin()] performs post-MCMC thinning of [`mcmc.list`][coda::mcmc.list] objects,
#'   and is solely for developing long-running post-processing code, so this is okay.
#' @examples
#' # load example mcmc.list
#' data(cjs, package = "postpack")
#'
#' # get all relevant dimensions
#' post_dim(cjs)
#'
#' # get only the number of chains
#' post_dim(cjs, "chains")
#'
#' # get the thinning and burn-in intervals
#' post_dim(cjs, c("burn", "thin"))
#' @export

post_dim = function(post, types = NULL) {

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
    params = ncol(postm[,-which(colnames(postm) %in% c("CHAIN", "ITER"))])
  )

  # decide on which to return based on types argument
  # only eval if types isn't null
  if (!is.null(types)) {

    # if any of types are not in the names, return informative error
    if (!all(types %in% c(names(out)))) {
      stop ("types can only include some of: \n", list_out(names(out), final = "and", wrap = "'", indent = "  "))
    } else {# otherwise subset
      out = out[types]
      # unname if there is only one element returned
      if (length(out) == 1) out = unname(out)
    }
  }

  # return the output
  return(out)
}
