% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_convert.R
\name{post_convert}
\alias{post_convert}
\title{Convert MCMC samples to mcmc.list format}
\usage{
post_convert(obj)
}
\arguments{
\item{obj}{An object storing posterior samples from an MCMC algorithm.
Accepted classes are \code{\link[base:list]{list}}, \code{\link[base:matrix]{matrix}}, \code{\link[rstan:stanfit-class]{stanfit}}, \code{\link[R2WinBUGS:bugs]{bugs}}, \code{\link[R2jags:jags]{rjags}}.}
}
\value{
The same information as passed in the \code{obj} argument, but formatted as \code{\link[coda:mcmc.list]{mcmc.list}} class.
}
\description{
Wrapper around several ways of converting objects to \code{\link[coda:mcmc.list]{mcmc.list}} format,
automated based on the input object class.
}
\details{
Accepted classes are produced by several packages, including but probably not limited to:
\itemize{
\item \code{\link[rstan:stanfit-class]{stanfit}} objects are created by \code{\link[rstan:stan]{rstan::stan()}}, which also provides \code{\link[rstan:As.mcmc.list]{rstan::As.mcmc.list()}}. Rather than requiring users to have 'rstan' installed to use 'postpack', \code{post_convert()} will instruct users to use this function if supplied a \code{\link[rstan:stanfit-class]{stanfit}} object.
\item \code{\link[R2WinBUGS:bugs]{bugs}} objects are created by \code{\link[R2WinBUGS:bugs]{R2WinBUGS::bugs()}} and \code{\link[R2OpenBUGS:bugs]{R2OpenBUGS::bugs()}}.
\item \code{\link[R2jags:jags]{rjags}} objects are created by \code{\link[R2jags:jags]{R2jags::jags()}}.
\item \code{\link[base:list]{list}} objects are created by \code{\link[nimble:runMCMC]{nimble::runMCMC()}}, 'MCMCpack' functions, or custom MCMC algorithms.
\item \code{\link[base:matrix]{matrix}} objects are created by \code{\link[=post_subset]{post_subset()}} and is often the format of posterior quantities derived from monitored nodes.
\item \code{\link[coda:mcmc.list]{mcmc.list}} objects are created by \code{\link[rjags:coda.samples]{rjags::coda.samples()}}, \code{\link[jagsUI:jags.basic]{jagsUI::jags.basic()}}, and \code{\link[jagsUI:jags]{jagsUI::jags()}}\verb{$samples}. If a \code{\link[coda:mcmc.list]{mcmc.list}} object is passed to \code{obj}, an error will be returned telling the user this function is not necessary.
}

If you find that a critical class conversion is missing, please submit an \href{https://github.com/bstaton1/postpack/issues}{issue} requesting its addition with a minimum working example of how it can be created.
}
\note{
\itemize{
\item If samples are stored in a \code{\link[base:list]{list}} object, the individual elements must be \code{\link[base:matrix]{matrix}} or \code{\link[coda:mcmc]{mcmc}} class, storing the samples (rows) across parameters (columns, with names) for each chain (\code{\link[base:list]{list}} elements). If \code{\link[base:list]{list}} elements are in \code{\link[base:matrix]{matrix}} format, they will be coerced to \code{\link[coda:mcmc]{mcmc}} format, and thinning, start, and end intervals may be inaccurate.
\item If samples are stored in a \code{\link[base:matrix]{matrix}} object, rows should store samples and columns should store nodes. Multiple chains should be combined using \code{\link[base:cbind]{base::rbind()}}. Two additional columns \strong{must} be present: \code{"CHAIN"} and \code{"ITER"}, which denote the MCMC chain and iteration numbers, respectively.
}
}
\examples{
## EXAMPLE 1
# load example mcmc.list
data(cjs)

# take a subset from cjs as a matrix, retain chain and iter ids
cjs_sub = post_subset(cjs, "^B", matrix = TRUE, chains = TRUE, iters = TRUE)

# convert back to mcmc.list
class(post_convert(cjs_sub))

## EXAMPLE 2: create mcmc.list from hypothetical MCMC samples; chains are list elements
# create hypothetical samples; can't use postpack on this - not an mcmc.list
samps = lapply(1:3, function(i) {
  m = matrix(rnorm(100), 20, 5)
  colnames(m) = paste0("param", 1:5)
  m
})

# convert
samps_new = post_convert(samps)

# can use postpack now
post_summ(samps_new, "param")

## EXAMPLE 3: create mcmc.list from hypothetical MCMC samples; chains rbind-ed matrices
# create samples
f = function() {
  m = matrix(rnorm(100), 20, 5)
  colnames(m) = paste0("param", 1:5)
  m
}
samps = rbind(f(), f(), f())

# assign chain and iter IDs to each sample
samps = cbind(CHAIN = rep(1:3, each = 20), ITER = rep(1:20, 3), samps)

# convert
samps_new = post_convert(samps)

# can use postpack now
post_summ(samps_new, "param")
}
\seealso{
\code{\link[coda:mcmc.list]{coda::as.mcmc.list()}}, \code{\link[coda:mcmc]{coda::as.mcmc()}}
}
