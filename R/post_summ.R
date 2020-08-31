#' @title Obtain posterior summaries and diagnostics of specific nodes
#' @description Allows rapid calculation of summaries and diagnostics from **specific nodes**
#'  stored in [`mcmc.list`][coda::mcmc.list] objects.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param params A vector of regular expressions specifying the nodes to match for summarization.
#'   Accepts multi-element vectors to match more than one node at a time.
#'   See [match_params()] and `vignette("pattern-matching")` for more details.
#' @param probs Posterior quantiles to calculate. Passed to [stats::quantile()].
#'   Defaults to `probs = c(0.5, 0.025, 0.975)` (i.e., median and equal-tailed 95 percent credible interval).
#' @param digits Control rounding of summaries.
#'   Passed to [base::round()] and defaults to `NULL`, which produces no rounding.
#' @param Rhat Calculate the Rhat convergence diagnostic using [coda::gelman.diag()]?
#'   Fair warning: this can take a bit of time to run on many nodes/samples.
#' @param neff Calculate the number of effective MCMC samples using [coda::effectiveSize()]?
#'   Fair warning: this can take a bit of time to run on many nodes/samples.
#' @param mcse Calculate the Monte Carlo standard error for the posterior mean and reported quantiles
#'   using the [mcmcse::mcse()] and [mcmcse::mcse.q()] functions
#'   (batch means method with batch size automatically calculated)?
#'   Fair warning: this can take a bit of time to run on many nodes/samples.
#' @param by_chain Calculate posterior summaries for each chain
#'   rather than for the aggregate across chains? Defaults to `FALSE`.
#'   The arguments `Rhat`, `neff`, and `mcse` are ignored if `by_chain = TRUE`
#'   and a warning will be returned.
#' @param auto_escape Automatically escape `"["` and `"]"` characters for pattern matching?
#'   See [match_params()] for details.
#' @return A [`matrix`][base::matrix] object with summary statistics as rows and nodes as columns.
#'   If `by_chain = TRUE`, an [`array`][base::array] with chain-specific summaries as the third dimension is returned instead.
#' @seealso [match_params()], [coda::gelman.diag()], [coda::effectiveSize()], [mcmcse::mcse()], [mcmcse::mcse.q()]
#' @importFrom stats quantile sd
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # calculate posterior summaries for the "p" nodes
#' # ("p[1]" doesn't exist in model)
#' post_summ(cjs, "p")
#'
#' # do this by chain
#' post_summ(cjs, "p", by_chain = TRUE)
#'
#' # calculate Rhat and Neff diagnostic summaries as well
#' # multiple node names too
#' post_summ(cjs, c("b0", "p"), Rhat = TRUE, neff = TRUE)
#'
#' # calculate Monte Carlo SE for mean and quantiles, with rounding
#' post_summ(cjs, "p", mcse = TRUE, digits = 3)
#'
#' # summarize different quantiles: median and central 80%
#' post_summ(cjs, "p", probs = c(0.5, 0.1, 0.9))
#' @export

post_summ = function(post, params, digits = NULL, probs = c(0.5, 0.025, 0.975), Rhat = FALSE, neff = FALSE, mcse = FALSE, by_chain = F, auto_escape = TRUE) {

  # warn user that some arguments will be ignored if doing by chain
  if (any(c(Rhat, neff, mcse)) & by_chain) {
    warning("Rhat, neff, and mcmse will not be calculated by chain.\nSet by_chain = FALSE to see these summaries.")
  }

  # define a basic summary function
  summ = function(x, probs = c(0.5, 0.025, 0.975), digits = NULL) {
    out = c(mean = mean(x), sd = sd(x), quantile(x, probs))
    if (!is.null(digits)) out = round(out, digits)
    return(out)
  }

  # match the names of the nodes that will be extracted
  matched_params = match_params(post, params, type = "base_index", auto_escape = auto_escape)

  # subset the nodes corresponding to params
  post_sub = post_subset(post, params)

  # apply the summ function to calculate numerical summaries of each requested node
  if (!by_chain) {
    output = apply(as.matrix(post_sub), 2, function(x) {
      summ(x, probs = probs, digits = digits)
    })
  } else {
    output = lapply(post_sub, function(chain) {
      apply(chain, 2, function(x) {
        summ(x, probs = probs, digits = digits)
      })
    })

    # remove list format and make each chain summary an array slice
    output = abind::abind(output, along = 3)

    # add a more informative dimension name
    dimnames(output)[[3]] = paste0("chain", 1:length(post_sub))
  }

  # add the right node name if necessary
  if (length(matched_params) == 1) {
    colnames(output) = matched_params
  }

  # if requested, calculate Rhat
  if (Rhat & !by_chain) {
    Rhat = round(coda::gelman.diag(post_sub, autoburnin = F, multivariate = F)[[1]][,1], 3)
    output = rbind(
      output,
      Rhat = Rhat
    )
  }

  # if requested, calculate effective samples
  if (neff & !by_chain) {
    neff = round(coda::effectiveSize(post_sub))
    output = rbind(
      output,
      neff = neff
    )
  }

  # if requested, calculate MC error
  if (mcse & !by_chain) {
    # convert samples to matrix format
    post_sub_mat = as.matrix(post_sub)

    # calculate Monte Carlo SE of the mean
    se_mean = apply(post_sub_mat, 2, function(x) mcmcse::mcse(x)$se)
    if (!is.null(digits)) se_mean = round(se_mean, digits)

    # calculate Monte Carlo SE of the various quantiles that are returned
    se_q = NULL
    for (i in 1:length(probs)) {
      se_q = rbind(se_q, apply(post_sub_mat, 2, function(x) mcmcse::mcse.q(x, probs[i])$se))
    }
    if (!is.null(digits)) se_q = round(se_q, digits)
    rownames(se_q) = paste0("mcse_", probs * 100, "%")

    # add the standard errors to the output
    output = rbind(
      output,
      mcse_mean = se_mean,
      se_q
    )
  }

  # return the output
  return(output)
}
