#' @title Create a trace plot for a single desired node
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param param A regular expression that matches a single element in the model.
#'   E.g., `"b0[1]"`, not `"b0"`. See [match_params()].
#' @param keep_percent A numeric vector of length == 1 and on the range (0,1].
#'   Percent of samples you'd like to keep for trace plotting and passed to [post_thin()].
#' @note If saving as a pdf file, these files can get very large with many samples and render slowly.
#'   The `keep_percent` argument is intended to help with this by thinning the chains at quasi-evenly spaced intervals.
#'   This is **not** a function users will generally use directly. Call [diag_plots()] instead.

trace_plot = function(post, param, keep_percent = 0) {

  # return error if param has length > 1
  if (length(param) > 1) stop ("param must have only one element, and it must match the desired node exactly")

  # lock in the search string. It must be exact for the rest of this code to work
  param = ins_regex_lock(param)

  # perform additional thinning if desired
  post = post_thin(post, keep_percent = keep_percent)

  # extract this node's samples
  post_sub = post_subset(post, param, matrix = TRUE, iters = TRUE, chains = TRUE)

  # get axis limits
  y_lim = range(post_sub[,3])

  # set up graphics device
  par(mar = c(1.5,0.5,2.5,2), tcl = -0.25,
      mgp = c(1.5,0.4,0), yaxs = "r")

  # create an empty plotting region
  plot(1,1, axes = FALSE, type = "n",
       xlim = range(post_sub[,"ITER"]),
       ylim = y_lim, xlab = "", ylab = "")

  # loop through chains plotting the trace of each
  sapply(unique(post_sub[,"CHAIN"]), function(c) {
    chain_sub = post_sub[post_sub[,"CHAIN"] == c,]
    lines(chain_sub[,3] ~ chain_sub[,"ITER"], col = c)
  })
  axis(side = 1); axis(side = 4)

  box()
}
