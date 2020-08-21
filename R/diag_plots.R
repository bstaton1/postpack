#' Create MCMC diagnostic plots for nodes of interest
#'
#' Allows quick visualization of posterior density and traceplots,
#' BOTH separated by chain, for the desired nodes of interest. Includes the
#' ability to plot in the RStudio graphics device, an external device (OS-indepenent),
#' or a PDF file. Further, with the auto settings, the dimensions of the
#' plotting device scales to the job needed.
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector of with length >= 1 specifying the nodes to plot. Passed to \code{\link{match_p}},
#'   so can (and sometimes should) be a regular expression.
#' @param ext_device logical. Do you wish to have an external device open to display the diagnostics?
#'   \code{TRUE} will create a new plotting device using the OS-specific function.
#'   Defaults to \code{FALSE}. Requires the \code{\link[StatonMisc]{ext_device}} function.
#' @param show_diags a character vector of length == 1. Must be one of
#'   \code{"always"}, \code{"never"}, \code{"if_poor_Rhat"}. Defaults to
#'   \code{"if_poor_Rhat"}, which will display the Rhat and effective MCMC samples
#'   if the Rhat statistic is greater than 1.1
#' @param layout a character vector specifying \code{"ROWSxCOLUMNS"} of parameter diagnostics.
#'   For example, \code{"4x1"} has 4 rows and 1 column of parameter diagnostics.
#'   Defaults to \code{"auto"}, which selects between \code{"1x1"}, \code{"2x1"}, \code{"4x1"}, \code{"4x2"}, and \code{"5x3"}.
#' @param dims a character vector specifying the \code{"HEIGHTxWIDTH"} dimensions
#'   of the plotting device, in inches.
#'   For example, \code{"5x7"} would create a 5 inch tall and 7 inch wide plotting device.
#'   Defaults to \code{"auto"}, which selects the dimensions that look nice when \code{layout = "auto"}
#'   as well.
#' @param keep_percent a numeric vector of length one and scaled between 0 and 1.
#'   Percent of samples you'd like to keep for traceplotting and passed to \code{\link{post_thin}}.
#' @param save logical. Do you wish to save the diagnostic plots in a PDF file? If so,
#'   specify \code{file = "example.pdf"} as well. Defaults to \code{FALSE}.
#' @param file character vector of length 1. The file name, which
#'   must include the \code{".pdf"} extension. Saved to working directory by default,
#'   but can recieve an absolute or relative file path here as well
#' @param auto_escape logical. \code{FALSE} will treat \code{"["} and \code{"]"}
#'   as regular expression syntax (unless explicitly escaped by user),
#'   \code{TRUE} will treat these symbols as plain text to be matched.
#'   It is generally recommended to keep this as \code{TRUE} (the default),
#'   unless you are performing complex regex searches that require the
#'   \code{"["} and \code{"]"} symbols to be special characters
#' @note If saving as a pdf, these files can get very large with many samples and render slowly.
#'   The \code{keep_percent} argument is intended to help with this by thinning the chains at quasi-evenly spaced intervals.
#' @seealso \code{\link{match_p}}
#' @importFrom StatonMisc %!in%
#' @importFrom StatonMisc ext_device
#'
#' @export

diag_plots = function(post, p, ext_device = FALSE, show_diags = "if_poor_Rhat", layout = "auto", dims = "auto", keep_percent = 1, save = FALSE, file = NULL, auto_escape = TRUE) {

  # the exact nodes to display. includes error checks for post and p being compatible.
  keep = match_p(post, p, ubase = F, auto_escape = auto_escape); n = length(keep)

  # error handle for layout
  if (layout %!in% c("auto", "1x1", "2x1", "4x1", "4x2", "5x3")) {
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

  if (ext_device) ext_device(h = height_width[1], w = height_width[2])
  if (save) pdf(file, h = height_width[1], w = height_width[2])

  # set up the graphics device
  par(mfrow = c(row_col[1],row_col[2] * 2))

  junk = sapply(keep, function(i) {
    if (which(keep == i) %in% new_page & ext_device) {
      ext_device(h = height_width[1], w = height_width[2])
      par(mfrow = c(row_col[1],row_col[2] * 2))
    }
    density_plot(post, i, show_diags)
    trace_plot(post, i, keep_percent = keep_percent)
    mytitle(i)
  })

  if (save) dev.off()
}



