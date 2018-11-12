#' Create a traceplot for a single desired node
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector of length 1.
#'   Should be a node reference to a single element in the model. E.g., \code{"pi[1]"}, not \code{"pi"}.
#'   If escapes are not provided to square brakets, they will automatically be inserted.
#' @param thin_percent a numeric vector of length one and scaled between 0 and 1.
#'   of samples you'd like to remove before traceplotting. If saving as a pdf,
#'   these files can get very large with many samples and render slowly.
#'   Samples are thinned systematically (i.e., at evenly-spaced intervals).
#'
#' @export

trace_plot = function(post, p = NULL, thin_percent = 0) {

  p = ins_regex_bracket(p)

  color = c("blue", "red", "green", "yellow", "pink")

  post_sub = filter_post(post, p, format = "matrix", iters = T, chains = T)
  if (ncol(post_sub) != 3) {
    stop ("more than one element from the model supplied!")
  }

  y_lim = range(post_sub[,3])
  n_keep = (1 - thin_percent) * nrow(post_sub)/max(post_sub[,"CHAIN"])
  keep_iter = round(seq(min(post_sub[,"ITER"]), max(post_sub[,"ITER"]), length = n_keep))
  post_sub = post_sub[post_sub[,"ITER"] %in% keep_iter,]

  par(mar = c(2.5,0.5,1.5,2), tcl = -0.25,
      mgp = c(1.5,0.4,0), yaxs = "r")
  plot(1,1, axes = F, type = "n",
       xlim = range(post_sub[,"ITER"]),
       ylim = y_lim, xlab = "", ylab = "",
       main = paste("Trace of", rm_regex_bracket(p)))
  sapply(unique(post_sub[,"CHAIN"]), function(c) {
    chain_sub = post_sub[post_sub[,"CHAIN"] == c,]
    lines(chain_sub[,3] ~ chain_sub[,"ITER"], col = color[c])
  })
  axis(side = 1)
  axis(side = 4)
  # mtext(side = 4, "Value", line = par("mgp")[1],
  #       cex = par("cex.lab"))

  box()
}
