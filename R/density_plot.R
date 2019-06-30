#' Create a density for a single desired node
#'
#' @param post an object of class \code{mcmc.list}
#' @param p_one a character vector of length == 1. Should be a node reference
#'   to a single element in the model. E.g., \code{"pi[1]"}, not \code{"pi"}.
#' @export

density_plot = function(post, p_one) {
  # return error if p_one has length > 1
  if (length(p_one) > 1) stop ("p_one must have only one element, and it must match the desired node exactly")

  # lock in the search string. It must be exact for the rest of this code to work
  p_one = ins_regex_lock(p_one)

  # extract this node's samples
  post_sub = post_subset(post, p_one, matrix = T, iters = T, chains = T)

  # fit the KDE
  dens = density(post_sub[,3])

  # determine axis limits
  x_lim = range(dens$x)
  y_max = max(tapply(post_sub[,3], post_sub[,"CHAIN"], function(x) max(density(x)$y)))

  # set up device
  par(yaxs = "i", mar = c(1.5,2,2.5,0.5), tcl = -0.25,
      mgp = c(1.5,0.4,0))

  # create an empty plot
  plot(1,1,type = "n",
       xlim = x_lim,  xlab = "",
       ylim = c(0, y_max) * 1.05, ylab = "")

  # loop through chains plotting the distribution of each
  junk = sapply(unique(post_sub[,"CHAIN"]), function(c) {
    chain_sub = post_sub[post_sub[,"CHAIN"] == c,]
    dens = density(chain_sub[,3])
    lines(dens$y ~ dens$x, col = c)
  })
  box()
}


