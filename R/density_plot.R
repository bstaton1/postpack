#' Create a density for a single desired node
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector of length 1. Should be a node reference
#'   to a single element in the model. E.g., \code{"pi[1]"}, not \code{"pi"}.
#'   If escapes are not provided to square brakets, they will automatically be inserted.
#' @export

density_plot = function(post, p = NULL) {
  p = ifelse(!stringr::str_detect(p, "\\\\"), ins_regex_bracket(p), p)

  color = c("blue", "red", "green", "yellow", "pink")

  post_sub = filter_post(post, p, format = "matrix", iters = T, chains = T)
  if (ncol(post_sub) != 3) {
    stop ("more than one element from the model supplied!")
  }
  dens = density(post_sub[,3])
  x_lim = range(dens$x)
  y_max = max(tapply(post_sub[,3], post_sub[,"CHAIN"], function(x) max(density(x)$y)))

  par(yaxs = "i", mar = c(2.5,2,1.5,0.5), tcl = -0.25,
      mgp = c(1.5,0.4,0))
  plot(1,1,type = "n",
       xlim = x_lim,  xlab = "",
       ylim = c(0, y_max) * 1.05, ylab = "",
       main = paste("Density of", rm_regex_bracket(p)))
  junk = sapply(unique(post_sub[,"CHAIN"]), function(c) {
    chain_sub = post_sub[post_sub[,"CHAIN"] == c,]
    dens = density(chain_sub[,3])
    lines(dens$y ~ dens$x, col = color[c])
  })
  box()
}


