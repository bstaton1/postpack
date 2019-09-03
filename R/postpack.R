#' postpack: Utilities for Dealing with Posterior Samples Stored in mcmc.lists
#'
#' This package contains many shortcuts for subsetting, summarizing, plotting, and diagnosing particular
#' nodes from an \code{mcmc.list} object. These objects are created from JAGS, BUGS, or STAN models,
#' yet the author has found the default summarization and diagnostic capabilities rather clunky
#' for plotting and subsetting particular portions of the posterior output. The attempt here is
#' to standardize all work with MCMC output into \code{mcmc.list} objects, and to provide the
#' infrastructure for a unified workflow. The functions in this package streamline the extraction,
#' summarization, and diagnostics of particular nodes monitored after model fitting. Subsetting is made
#' highly flexible by the incorporation of regular expression compatibility.
#'
#'@docType package
#'@name postpack
NULL
