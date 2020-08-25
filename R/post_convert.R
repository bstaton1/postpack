#' @title Convert MCMC samples to mcmc.list format
#' @description Wrapper around several ways of converting objects to [`mcmc.list`][coda::mcmc.list] format,
#'   automated based on the input object class.
#' @param obj An object storing posterior samples from an MCMC algorithm.
#'   Accepted classes are [`list`][base::list], [`matrix`][base::matrix], [`stanfit`][rstan::stanfit], [`bugs`][R2WinBUGS::bugs], [`rjags`][R2jags::jags].
#' @details Accepted classes are produced by several packages, including but probably not limited to:
#'   * [`stanfit`][rstan::stanfit] objects are created by [rstan::stan()], which also provides [rstan::As.mcmc.list()] which is used here as well.
#'   * [`bugs`][R2WinBUGS::bugs] objects are created by [R2WinBUGS::bugs()] and [R2OpenBUGS::bugs()].
#'   * [`rjags`][R2jags::jags] objects are created by [R2jags::jags()].
#'   * [`list`][base::list] objects are created by [nimble::runMCMC()], 'MCMCpack' functions, or custom MCMC algorithms.
#'   * [`matrix`][base::matrix] objects are created by [post_subset()] and is often the format of posterior quantities derived from monitored nodes.
#'   * [`mcmc.list`][coda::mcmc.list] objects are created by [rjags::coda.samples()], [jagsUI::jags.basic()], and [jagsUI::jags()]`$samples`. If a [`mcmc.list`][coda::mcmc.list] object is passed to `obj`, an error will be returned telling the user this function is not necessary.
#'
#'   If you find that a critical class conversion is missing, please submit an [issue](https://github.com/bstaton1/postpack/issues) requesting its addition with a minimum working example of how it can be created.
#' @note
#'   * If samples are stored in a [`list`][base::list] object, the individual elements must be [`matrix`][base::matrix] or [`mcmc`][coda::mcmc] class, storing the samples (rows) across parameters (columns, with names) for each chain ([`list`][base::list] elements). If [`list`][base::list] elements are in [`matrix`][base::matrix] format, they will be coerced to [`mcmc`][coda::mcmc] format, and thinning, start, and end intervals may be inaccurate.
#'   * If samples are stored in a [`matrix`][base::matrix] object, rows should store samples and columns should store nodes. Multiple chains should be combined using [base::rbind()]. Two additional columns __must__ be present: `"CHAIN"` and `"ITER"`, which denote the MCMC chain and iteration numbers, respectively.
#' @return The same information as passed in the `obj` argument, but formatted as [`mcmc.list`][coda::mcmc.list] class.
#' @seealso [coda::as.mcmc.list()], [coda::as.mcmc()], [rstan::As.mcmc.list()]
#' @examples
#' ## EXAMPLE 1
#' # load example mcmc.list
#' data(cjs, package = "postpack")
#'
#' # take a subset from cjs as a matrix, retain chain and iter ids
#' cjs_sub = post_subset(cjs, "^B", matrix = TRUE, chains = TRUE, iters = TRUE)
#'
#' # convert back to mcmc.list
#' class(post_convert(cjs_sub))
#'
#' ## EXAMPLE 2: create mcmc.list from hypothetical MCMC samples; chains are list elements
#' # create hypothetical samples; can't use postpack on this - not an mcmc.list
#' samps = lapply(1:3, function(i) {
#'   m = matrix(rnorm(100), 20, 5)
#'   colnames(m) = paste0("param", 1:5)
#'   m
#' })
#'
#' # convert
#' samps_new = post_convert(samps)
#'
#' # can use postpack now
#' post_summ(samps_new, "param")
#'
#' ## EXAMPLE 3: create mcmc.list from hypothetical MCMC samples; chains rbind-ed matrices
#' # create samples
#' f = function() {
#'   m = matrix(rnorm(100), 20, 5)
#'   colnames(m) = paste0("param", 1:5)
#'   m
#' }
#' samps = rbind(f(), f(), f())
#'
#' # assign chain and iter IDs to each sample
#' samps = cbind(CHAIN = rep(1:3, each = 20), ITER = rep(1:20, 3), samps)
#'
#' # convert
#' samps_new = post_convert(samps)
#'
#' # can use postpack now
#' post_summ(samps_new, "param")
#' @export

post_convert = function(obj) {

  # determine the class of the supplied object
  obj_class = class(obj)[1]

  ### error handles ###
  # stop and return error if object is already an mcmc.list
  if (obj_class == "mcmc.list") {
    stop ("obj is already of class 'mcmc.list'")
  }

  # return error if it is not one of the required object types
  accepted_classes = c("matrix", "list", "stanfit", "bugs", "rjags")
  if (!(obj_class %in% accepted_classes)) {
    stop ("The class '", obj_class, "' is not accepted by this function.\n  Accepted classes are:\n    ", list_out(accepted_classes, wrap = "'"))
  }

  # return an error if it is a list object and the elements are not mcmc objects
  if (obj_class == "list") {
    if (!all(unlist(lapply(obj, function(x) class(x)[1])) %in% c("matrix", "mcmc"))) {
      stop ("If obj is of class 'list', then all list elements must be of class 'matrix' or 'mcmc'")
    }
  }

  # return an error if it is a matrix object and does not contain either CHAIN or ITER
  if (obj_class == "matrix" & !all(c("CHAIN", "ITER") %in% colnames(obj))) {
    stop ("obj is a matrix, but does not include 'CHAIN' and 'ITER' columns.\n  These are required for creating an mcmc.list object.\n  If you have another mcmc.list object from this MCMC run,\n  these columns can be obtained using postpack:::id_mat()")
  }

  ### convert to mcmc.list based on object class ###
  if (obj_class == "stanfit") {
    out_obj = rstan::As.mcmc.list(obj)
  }

  if (obj_class %in% c("bugs")) {
    out_obj = coda::as.mcmc.list(obj)
  }

  if (obj_class == "rjags") {
    out_obj = coda::as.mcmc(obj)
  }

  if (obj_class == "list") {
    if (all(unlist(lapply(obj, function(l) class(l)[1])) == "matrix")) {
      out_obj = coda::as.mcmc.list(lapply(obj, coda::as.mcmc))
    } else {
      out_obj = coda::as.mcmc.list(obj)
    }
  }

  if (obj_class == "matrix") {
    # extract the chain and iter of each sample
    chain = obj[,"CHAIN"]
    iters = obj[,"ITER"]
    params = colnames(obj)[which(!(colnames(obj) %in% c("CHAIN", "ITER")))]

    # keep only parameters, not the CHAIN and ITER columns
    obj = obj[,params]

    # if these steps would make it a vector, convert back to a one column matrix
    if ("numeric" %in% class(obj)) {
      obj = matrix(obj, ncol = 1)
    }

    # use lapply over chains to turn individual blocks of samples into list elements corresponding to chains
    # the attributes are altered for consistency
    out_obj = lapply(unique(chain), function(c) {
      # extract samples for this chain
      obj_sub = obj[chain == c,]
      if ("numeric" %in% class(obj_sub)) {
        obj_sub = matrix(obj_sub, ncol = 1)
      }
      # set rownames equal to the iteration
      rownames(obj_sub) = unique(iters)
      colnames(obj_sub) = params
      # coerce to mcmc object
      obj_sub = coda::as.mcmc(obj_sub)
      # set the first and last iteration number and the thinning rate
      attributes(obj_sub)$mcpar[1] = min(iters)
      attributes(obj_sub)$mcpar[2] = max(iters)
      attributes(obj_sub)$mcpar[3] = unique(diff(unique(iters)))

      obj_sub
    })

    # coerce to an mcmc.list
    out_obj = coda::as.mcmc.list(out_obj)
  }

  # return the output
  return(out_obj)
}
