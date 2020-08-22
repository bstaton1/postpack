#' Convert MCMC samples to mcmc.list format
#'
#' \code{postpack} requires objects in mcmc.list format.
#' This is a wrapper around several ways of converting objects to this format,
#' automated based on the input object class
#'
#' @param obj An R object storing posterior samples from an MCMC algorithm.
#'   Accepted classes are \code{list}, \code{stanfit}, \code{bugs}, \code{rjags}.
#' @return The same information as passed in the \code{obj} argument, but formatted as \code{mcmc.list} class
#' @details Accepted classes are produced by several packages, including but probably not limited to:
#'   \itemize{
#'     \item \code{stanfit} objects are created by \code{rstan::stan}, which also provides \code{\link[rstan]{As.mcmc.list()}} which is used here as well.
#'     \item \code{bugs} objects are created by \code{R2WinBUGS::bugs} and \code{R2OpenBUGS::bugs}
#'     \item \code{rjags} objects are created by \code{R2jags::jags}
#'     \item \code{list} objects are created by \code{nimble::runMCMC}, \code{MCMCpack}, or custom MCMC algorithms
#'     \item \code{mcmc.list} objects are created by \code{rjags::coda.samples}, \code{jagsUI::jags.basic}, and \code{jagsUI::jags()$samples}. If a \code{mcmc.list} object is passed to \code{post_convert}, an error will be returned telling the user this function is not necessary.
#'   }
#'   If you find that a critical class conversion is missing, please submit an issue to \url{https://github.com/bstaton1/postpack/issues} requesting its addition with a minimum working example of how it can be created.
#' @note If samples are stored in a \code{list} object, the individual elements must be
#'   \code{matrix} or \code{mcmc} class, storing the samples (rows) across parameters (columns, with names)
#'   for each chain (list elements). If list elements are in \code{matrix} format, they will be coerced to \code{mcmc} format, and thinning, start, and end intervals may be inaccurate.
#' @importFrom StatonMisc list_out
#' @seealso \code{\link[coda]{as.mcmc.list}}, \code{\link[coda]{as.mcmc}}, \code{\link[rstan]{As.mcmc.list}}
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
  accepted_classes = c("list", "stanfit", "bugs", "rjags")
  if (!(obj_class %in% accepted_classes)) {
    stop ("The class '", obj_class, "' is not accepted by this function.\n  Accepted classes are:\n    ", list_out(accepted_classes, wrap = "'"))
  }

  # return an error if it is a list object and the elements are not mcmc objects
  if (obj_class == "list") {
    if (!all(unlist(lapply(obj, function(x) class(x)[1])) %in% c("matrix", "mcmc"))) {
      stop ("If obj is of class 'list', then all list elements must be of class 'matrix' or 'mcmc'")
    }
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

  # return the output
  return(out_obj)
}
