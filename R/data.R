#' @title Example mcmc.list 1
#' @description An example of samples from a joint posterior distribution from a Cormack-Jolly-Seber model.
#'   **The specific context does not matter, this object is provided to show examples of 'postpack' functionality**.
#' @format A [`mcmc.list`][coda::mcmc.list] object.
#' @source Posterior samples generated from a model fitted to hypothetical data set.
#'   See `vignette("example-mcmclists")` on the context, model, and monitored parameters.
"cjs"

#' @title Example mcmc.list 2
#' @description An example of samples from a joint posterior distribution from a Cormack-Jolly-Seber model.
#'   **The specific context does not matter, this object is provided to show examples of 'postpack' functionality**.
#' @source This object stores samples from the same hypothetical example as for the [`cjs`] example object,
#'   with one small change to the model. The `rho` term that models correlation between slopes and intercepts
#'   was forced to be zero, rather than estimating it. Consult `vignette("example-mcmclists")` for more details.
#' @format A [`mcmc.list`][coda::mcmc.list] object.
"cjs_no_rho"
