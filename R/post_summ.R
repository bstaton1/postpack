#' Obtain a posterior summary of specific nodes
#'
#' Obtaining a summary of specific nodes is cumbersome from mcmc.list objects:
#' \code{summary(post)} if \code{post} is of class \code{mcmc.list} is clunky.
#' The central tendency and uncertainty measures are in different lists.
#' This function allows extractinging a summary of the posteriors
#' associated with requested nodes.
#'
#' @param post an object of class \code{mcmc.list}
#' @param params a character vector of with length >= 1 specifying the nodes to summarize.
#'   Passed to \code{\link{match_params}}, so can (and sometimes should) be a regular expression.
#' @param p_summ numeric vector with the posterior percentiles you wish to have summarized.
#'   Defaults to \code{p_summ = c(0.5, 0.025, 0.975)}.
#' @param rnd numeric vector controlling rounding of summaries.
#'   Passed to the \code{digits} argument of \code{\link[base]{round}}.
#'   Defaults to \code{NULL}, which produces no rounding.
#' @param Rhat logical. Do you wish to calculate the
#'   Rhat convergence diagnostic using \code{\link[coda]{gelman.diag}}?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#' @param ess logical. Do you wish to calculate the
#'   effective sample size using \code{\link[coda]{effectiveSize}}?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#' @param mcse logical. Do you wish to calculate the
#'   Monte Carlo standard error for the posterior mean and reported quantiles
#'   using the \code{\link[mcmcse]{mcse}} and \code{\link[mcmcse]{mcse.q}} functions
#'   (batch means method with batch size automatically calculated)?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#' @param by_chain logical. Do you wish to calculate posterior summaries for each chain,
#'   rather than for the aggregate across chains? Defaults to \code{FALSE}.
#'   The arguments \code{Rhat}, \code{ess}, and \code{mcse} are ignored if \code{by_chain = TRUE}
#'   and a warning will be returned
#' @seealso \code{\link{match_params}}, \code{\link[coda]{gelman.diag}},
#'   \code{\link[coda]{effectiveSize}}, \code{\link[mcmcse]{mcse}}, \code{\link[mcmcse]{mcse.q}}
#'
#'@export

post_summ = function(post, params, rnd = NULL, p_summ = c(0.5, 0.025, 0.975), Rhat = FALSE, ess = FALSE, mcse = FALSE, by_chain = F, auto_escape = TRUE) {

  # warn user that some arguments will be ignored if doing by chain
  if (any(c(Rhat, ess, mcse)) & by_chain) {
    warning("Rhat, ess, and mcmse will not be calculated by chain.\nSet by_chain = FALSE to see these summaries.")
  }

  # define a basic summary function
  summ = function(x, p_summ = c(0.5, 0.025, 0.975), rnd = NULL) {
    out = c(mean = mean(x), sd = sd(x), quantile(x, p_summ))
    if (!is.null(rnd)) out = round(out, rnd)
    return(out)
  }

  # match the names of the nodes that will be extracted
  matched_params = match_params(post, params, type = "base_index", auto_escape = auto_escape)

  # subset the nodes corresponding to params
  post_sub = post_subset(post, params)

  # apply the summ function to calculate numerical summaries of each requested node
  if (!by_chain) {
    output = apply(as.matrix(post_sub), 2, function(x) {
      summ(x, p_summ = p_summ, rnd = rnd)
    })
  } else {
    output = lapply(post_sub, function(chain) {
      apply(chain, 2, function(x) {
        summ(x, p_summ = p_summ, rnd = rnd)
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

  # if doing Rhat, calculate it
  if (Rhat & !by_chain) {
    Rhat = round(coda::gelman.diag(post_sub, autoburnin = F, multivariate = F)[[1]][,1], 3)
    output = rbind(
      output,
      Rhat = Rhat
    )
  }

  # if doing effective sample size, do so
  if (ess & !by_chain) {
    ess = round(coda::effectiveSize(post_sub))
    output = rbind(
      output,
      ess = ess
    )
  }

  # if doing MC error, do so
  if (mcse & !by_chain) {
    # convert samples to matrix format
    post_sub_mat = as.matrix(post_sub)

    # calculate Monte Carlo SE of the mean
    se_mean = apply(post_sub_mat, 2, function(x) mcmcse::mcse(x)$se)
    if (!is.null(rnd)) se_mean = round(se_mean, rnd)

    # calculate Monte Carlo SE of the various quantiles that are returned
    se_q = NULL
    for (i in 1:length(p_summ)) {
      se_q = rbind(se_q, apply(post_sub_mat, 2, function(x) mcmcse::mcse.q(x, p_summ[i])$se))
    }
    if (!is.null(rnd)) se_q = round(se_q, rnd)
    rownames(se_q) = paste0("mcse_", p_summ * 100, "%")

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
