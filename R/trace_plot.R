#' Create a traceplot for a single desired node
#'
#' @param post an object of class \code{mcmc.list}
#' @param p_one a character vector of length 1.
#'   Should be a node reference to a single element in the model. E.g., \code{"pi[1]"}, not \code{"pi"}.
#' @param thin_percent a numeric vector of length one and scaled between 0 and 1.
#'   of samples you'd like to remove before traceplotting. If saving as a pdf,
#'   these files can get very large with many samples and render slowly.
#'   Samples are thinned systematically (i.e., at evenly-spaced intervals).
#'
#' @export

trace_plot = function(post, p_one, thin_percent = 0) {

  # return error if p_one has length > 1
  if (length(p_one) > 1) stop ("p_one must have only one element, and it must match the desired node exactly")

  # lock in the search string. It must be exact for the rest of this code to work
  p_one = ins_regex_lock(p_one)

  # perform additional thinning if desired
  post = post_thin(post, thin_percent = thin_percent)

  # extract this node's samples
  post_sub = post_subset(post, p_one, matrix = T, iters = T, chains = T)

  # get axis limits
  y_lim = range(post_sub[,3])

  # set up graphics device
  par(mar = c(1.5,0.5,2.5,2), tcl = -0.25,
      mgp = c(1.5,0.4,0), yaxs = "r")

  # create an empty plotting region
  plot(1,1, axes = F, type = "n",
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
