#' Obtain a posterior summary
#'
#' Oftentimes you want to obtain the summary of just one node at a time.
#' \code{summary(post)} if \code{post} is of class \code{mcmc.list} is clunky.
#' The central tendency and uncertainty measures are in different lists.
#' This function allows extractinging a summary of the posteriors associated with requested nodes.
#'
#' @param post an object of class \code{mcmc.list} from which to filter out the node
#'   identified by \code{p}
#' @param p a character vector of length 1. Passed to \code{stringr::str_detect()},
#'   so can (and perhaps should) be a regular expression.
#' @param p_summ numeric vector passed to \code{StatonMisc::summ()}.
#'  The posterior percentiles you want to see. Defaults to \code{p_summ = c(0.5, 0.025, 0.975)}.
#' @param prettify logical passed to \code{StatonMisc::summ()}.
#'   Do you wish to add commas to demarcate thousands points?
#'   Coerces numerics to characters.
#' @param rnd numeric vector passed to \code{StatonMisc::summ()}.
#'   The digits of \code{round()}.
#' @param bgr logical. Do you wish to calculate the
#'   effective sample size using \code{coda:effectiveSize(post)}?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#' @param ess logical. Do you wish to calculate the
#'   effective sample size using \code{coda:effectiveSize(post)}?
#'   Fair warning: this can take a bit of time to run on many nodes/samples
#'
#'@export

summ_post = function(post, p = NULL, p_summ = c(0.5, 0.025, 0.975), prettify = F, rnd = NULL, bgr = F, ess = F) {

  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("'post' is not an object of class 'mcmc.list'!")
  }

  # stop if p wasn't supplied
  if (is.null(p)) {
    stop("No nodes supplied to extract. Please specify 'p', please see ?post_extract")
  }

  # extract the nodes corresponding to p
  post_sub = filter_post(post, p)

  # apply the summary function
  output = apply(as.matrix(post_sub), 2, function(x) {
    summ(x,
         p = p_summ,
         rnd = rnd,
         prettify = prettify,
         )
  })

  # add the right node name if
  if (ncol(output) == 1) {
    colnames(output) = p
  }

  # if doing bgr (Rhat), do so
  if (bgr) {
    bgr = round(coda::gelman.diag(post_sub, multivariate = F)[[1]][,1], 3)
    output = rbind(
      output,
      bgr = bgr
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
