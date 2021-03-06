#' @title Create a density plot for a single desired node
#' @description Used by [diag_plots()], not intended to be called separately
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param param A regular expression that matches a single element in the model.
#'   E.g., `"b0[1]"`, not `"b0"`. See [match_params()].
#' @param show_diags Control when to display numerical diagnostic summaries on plots. Must be one of
#'   `"always"`, `"never"`, or `"if_poor_Rhat"`. `"if_poor_Rhat"` (the default)
#'   will display the Rhat and effective MCMC samples if the Rhat statistic
#'   is greater than 1.1.
#' @return A figure showing the posterior density, separated by chain.
#' @note This is **not** a function users will generally use directly. Call [diag_plots()] instead.

density_plot = function(post, param, show_diags = "if_poor_Rhat") {
  # return error if param has length > 1
  if (length(param) > 1) stop ("param must have only one element, and it must match the desired node exactly")

  # return error if show_diags is invalid
  valid_show_diags = c("always", "never", "if_poor_Rhat")
  if (!(show_diags %in% valid_show_diags)) {
    stop ("show_diags must be one of: ", list_out(valid_show_diags, final = "or", wrap = '"'))
  }

  # lock in the search string. It must be exact for the rest of this code to work
  param = ins_regex_lock(param)

  # extract this node's samples
  post_sub = post_subset(post, param, matrix = TRUE, iters = TRUE, chains = TRUE)

  # fit the KDE
  dens = suppressWarnings(density(post_sub[,3]))

  # determine axis limits
  x_lim = range(dens$x)
  y_max = max(tapply(post_sub[,3], post_sub[,"CHAIN"], function(x) suppressWarnings(max(density(x)$y))))

  # set up device
  # don't need on.exit(par(oldpar)) here: handled in diag_plots()
  # and this function is non-exported
  par(yaxs = "i", mar = c(1.5,2,2.5,0.5), tcl = -0.25,
      mgp = c(1.5,0.4,0))

  # create an empty plot
  plot(1,1,type = "n",
       xlim = x_lim,  xlab = "",
       ylim = c(0, y_max) * 1.05, ylab = "")

  # loop through chains plotting the distribution of each
  junk = sapply(unique(post_sub[,"CHAIN"]), function(c) {
    chain_sub = post_sub[post_sub[,"CHAIN"] == c,]
    dens = suppressWarnings(density(chain_sub[,3]))
    lines(dens$y ~ dens$x, col = c)
  })

  # calculate convergence diagnostics if requested
  if (show_diags %in% c("always", "if_poor_Rhat")) {
    diags = post_summ(post, param, Rhat = TRUE, neff = TRUE)[c("Rhat", "neff"),]
    if (!is.na(diags["Rhat"])) {
      if (diags["Rhat"] >= 1.1 | show_diags == "always") {
        Rhat_text = paste0("Rhat: ", diags["Rhat"])
        neff_text = paste0("Neff: ", prettyNum(diags["neff"], big.mark = ","))
        legend("topright", legend = c(Rhat_text, neff_text), bty = "n")
      }
    }
  }

  box()
}
