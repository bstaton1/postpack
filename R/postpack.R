#' Utilities for Processing Posterior Samples Stored in mcmc.lists
#'
#' The aim of 'postpack' is to provide the infrastructure for a standardized workflow for [`mcmc.list`][coda::mcmc.list] objects.
#' These objects can be used to store output from models fitted with Bayesian inference using
#' JAGS, Win/OpenBUGS, NIMBLE, Stan, or even custom MCMC algorithms (see [post_convert()] for converting samples to
#' [`mcmc.list`][coda::mcmc.list] format). Although the 'coda' package provides some methods for these objects,
#' it is somewhat limited in easily performing post-processing tasks for **particular nodes**.
#' Models are ever increasing in their complexity and the number of tracked nodes, and oftentimes
#' a user may wish to summarize/diagnose sampling behavior for only a small subset of nodes at a time for a particular question or figure.
#' Thus, many 'postpack' functions support performing tasks on a subset of nodes, where the subset is specified with regular expressions.
#' The functions in this package streamline the extraction, summarization, and diagnostics of particular nodes monitored after model fitting.
#' Further, because there is rarely only ever one model under consideration, 'postpack' scales efficiently to perform the same tasks on output from multiple models
#' simultaneously, facilitating rapid assessment of model sensitivity to changes in assumptions.
#'
#'@docType package
#'@name postpack
NULL
