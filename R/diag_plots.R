#' @title Create MCMC diagnostic plots for nodes of interest
#' @description Allows quick visualization of posterior density and trace plots,
#'   **both** separated by chain, for the desired nodes of interest. Includes the
#'   ability to plot in the RStudio graphics device, an external device,
#'   or a PDF file. Further, with the auto settings, the dimensions of the
#'   plotting device scales to the job needed.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param params A character vector with length >= 1 specifying the nodes to plot.
#'   Passed to [match_params()] so is matched using regular expressions.
#' @param ext_device Logical. Do you wish to have an external device open to display the diagnostics?
#'   `FALSE` (the default) will plot in the active device (including RStudio window).
#'   `TRUE` will create a new graphics device.
#' @param show_diags A character vector of length == 1. Must be one of
#'   `"always"`, `"never"`, or `"if_poor_Rhat"`. `"if_poor_Rhat"` (the default)
#'   will display the Rhat and effective MCMC samples if the Rhat statistic
#'   is greater than 1.1.
#' @param layout A character vector with length == 1 specifying `"ROWSxCOLUMNS"` of parameter diagnostics.
#'   For example, `"4x1"` has 4 rows and 1 column of parameter diagnostics.
#'   Defaults to `"auto"`, which selects between the only accepted options of
#'   `"1x1"`, `"2x1"`, `"4x1"`, `"4x2"`, and `"5x3"`.
#' @param dims A character vector with length == 1 specifying the `"HEIGHTxWIDTH"` dimensions
#'   of the plotting device, in inches.
#'   For example, `"5x7"` would create a 5 inch tall and 7 inch wide plotting device.
#'   Defaults to `"auto"`, which selects the dimensions that look nice when `layout = "auto"`
#'   as well.
#' @param keep_percent A numeric vector of length == 1 and scaled between 0 and 1.
#'   Percent of samples you'd like to keep for trace plotting and passed to [post_thin()].
#' @param save Logical. Do you wish to save the diagnostic plots in a PDF file? If so,
#'   specify `file = "example.pdf"` as well. Defaults to `FALSE`.
#' @param file Character vector of length == 1. The file name, which
#'   must include the `".pdf"` extension. Saved to working directory by default,
#'   but can receive an absolute or relative file path as part of this argument.
#' @param auto_escape Logical. `FALSE` will treat `"["` and `"]"`
#'   as special regular expression characters (unless explicitly escaped by user),
#'   `TRUE` will treat these symbols as plain text to be matched.
#'   It is generally recommended to keep this as `TRUE` (the default),
#'   unless you are performing complex regex searches that require the
#'   `"["` and `"]"` symbols to be special characters.
#' @note If saving as a pdf, these files can get very large with many samples and render slowly.
#'   The `keep_percent` argument is intended to help with this by thinning the chains at quasi-evenly spaced intervals.
#' @seealso [match_params()], [density_plot()], [trace_plot()]
#' @return A multi-panel figure showing the posterior density and trace plots for requested nodes.
#'   The device in which it is placed depends on the argument values.
#' @importFrom grDevices dev.new dev.off pdf
#' @importFrom graphics axis box legend lines par plot
#' @importFrom stats density
#' @examples
#' if (interactive()) {
#'   #load example mcmc.list
#'   data(cjs)
#'
#'   # use current device
#'   diag_plots(cjs, "B0")
#'
#'   # use a new device
#'   diag_plots(cjs, "B0", ext_device = T)
#'
#'   # always show diagnostic summaries
#'   diag_plots(cjs, "B0", show_diags = "always")
#'
#'   # use a different layout (leaving it as "auto" is usually best)
#'   diag_plots(cjs, c("sig", "b"), layout = "5x3")
#'
#'   # save diagnostics for all nodes to a pdf file
#'   diag_plots(cjs, "", save = T, file = "diags.pdf")
#' }
#' @export

diag_plots = function(post, params, ext_device = FALSE, show_diags = "if_poor_Rhat", layout = "auto", dims = "auto", keep_percent = 1, save = FALSE, file = NULL, auto_escape = TRUE) {

  # the exact nodes to display. includes error checks for post and p being compatible.
  keep = match_params(post, params, type = "base_index", auto_escape = auto_escape); n = length(keep)

  # error handle for layout
  if (!(layout %in% c("auto", "1x1", "2x1", "4x1", "4x2", "5x3"))) {
    stop ("layout must be one of 'auto', '1x1', '2x1', '4x1', '4x2', or '5x3'")
  }

  # error handle for dims
  if (dims != "auto" & !stringr::str_detect(dims, "x")) {
    stop ("dimensions must be supplied as 'HxW', where H = height in inches and W = width in inches")
  }

  # saving-related error handles
  if (save & ext_device) stop(
    "You can't simultaneously save a PDF and open the plot in an external device. Set one to FALSE.")
  if (save & is.null(file)) stop("If saving a PDF, you must specify the filename")
  if (save) {
    if (tolower(substr(file, nchar(file) - 2, nchar(file))) != "pdf") {
      stop("filename must include the '.pdf' extension")
    }
  }

  # set the layout
  if (layout == "auto") {
    layout = ifelse(n == 1, "1x1",
                    ifelse(n < 4, "2x1",
                           ifelse(n < 8, "4x1",
                                  ifelse(8 <= n & n <= 16, "4x2", "5x3"))))
  }
  row_col = as.numeric(unlist(stringr::str_split(layout, "x")))
  n_per_page = prod(row_col)
  new_page = 1 + n_per_page * seq(1, 1000)

  # set the dimensions (in inches)
  if (dims == "auto") {
    dims = c("1x1" = "3.5x7", "2x1" = "6x7", "4x1" = "8x7", "4x2" = "8x7", "5x3" = "8.5x11")
    dims = dims[layout]
  }
  height_width = as.numeric(unlist(stringr::str_split(dims, "x")))

  if (ext_device) dev.new(height = height_width[1], width = height_width[2], units = "in", noRStudioGD = T)
  if (save) pdf(file, height = height_width[1], width = height_width[2])

  # set up the graphics device
  opar = par()
  opar = opar[-which(names(opar) %in% c("cin", "cra", "csi", "cxy", "din", "page", "pin"))]
  on.exit(par(opar))
  par(mfrow = c(row_col[1],row_col[2] * 2))

  junk = sapply(keep, function(i) {
    if (which(keep == i) %in% new_page & ext_device) {
      dev.new(height = height_width[1], width = height_width[2], units = "in", noRStudioGD = T)
      par(mfrow = c(row_col[1],row_col[2] * 2))
    }
    density_plot(post, i, show_diags)
    trace_plot(post, i, keep_percent = keep_percent)
    mytitle(i)
  })

  if (save) dev.off()
}
