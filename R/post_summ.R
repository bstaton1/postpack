#' Obtain a posterior summary of specific nodes
#'
#' Obtaining a summary of specific nodes is cumbersome from mcmc.list objects:
#' \code{summary(post)} if \code{post} is of class \code{mcmc.list} is clunky.
#' The central tendency and uncertainty measures are in different lists.
#' This function allows extractinging a summary of the posteriors
#' associated with requested nodes.
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector of with length >= 1 specifying the nodes to summarize.
#'   Passed to \code{\link{match_p}}, so can (and sometimes should) be a regular expression.
#' @param p_summ numeric vector passed to \code{\link[StatonMisc]{summ}}.
#'  The posterior percentiles you want to have summarized, Defaults to \code{p_summ = c(0.5, 0.025, 0.975)}.
#' @param rnd numeric vector passed to \code{\link[StatonMisc]{summ}}.
#'   The digits of \code{round}.
#' @param Rhat logical. Do you wish to calculate the
#'   Rhat convergence diagnostic using \code{\link[coda]{gelman.diag}}?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#' @param ess logical. Do you wish to calculate the
#'   effective sample size using \code{\link[coda]{effectiveSize}}?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#' @seealso \code{\link{match_p}}, \code{\link[StatonMisc]{summ}}, \code{\link[coda]{gelman.diag}},
#'   \code{\link[coda]{effectiveSize}}
#' @importFrom StatonMisc summ
#'
#'@export

post_summ = function(post, p, rnd = NULL, p_summ = c(0.5, 0.025, 0.975), Rhat = F, ess = F) {

  # match the names of the nodes that will be extracted
  p_match = match_p(post, p)

  # subset the nodes corresponding to p
  post_sub = post_subset(post, p)

  # apply the StatonMisc::summ function
  output = apply(as.matrix(post_sub), 2, function(x) {
    summ(x, p = p_summ, rnd = rnd)
  })

  # add the right node name if necessary
  if (length(p_match) == 1) {
    colnames(output) = p_match
  }

  # if doing Rhat, calculate it
  if (Rhat) {
    Rhat = round(coda::gelman.diag(post_sub, autoburnin = F, multivariate = F)[[1]][,1], 3)
    output = rbind(
      output,
      Rhat = Rhat
    )
  }

  # if doing effective sample size, do so
  if (ess) {
    ess = round(coda::effectiveSize(post_sub))
    output = rbind(
      output,
      ess = ess
    )
  }

  # return the output
  return(output)
}
