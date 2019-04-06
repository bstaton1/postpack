#' Create MCMC diagnostic plots for nodes of interest
#'
#' This function allows you to easily plot the density and traceplots,
#' BOTH separated by chain, for the desired nodes of interest. Includes the
#' ability to plot in the RStudio graphics device, an external device (OS-indepenent),
#' or a PDF file.
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector with length >= 1. Passed to \code{stringr::str_detect},
#'   so can, and perhaps should, be a regular expression.
#' @param layout a character vector specifying \code{"ROWSxCOLUMNS"} of parameter diagnostics.
#'   For example, \code{"4x1"} has 4 rows and 1 column of parameter diagnostics.
#'   Defaults to \code{"auto"}, which selects between \code{"1x1"}, \code{"4x1"}, and \code{"5x3"}.
#' @param dims a character vector specifying the \code{"HEIGHTxWIDTH"} dimensions
#'   of the plotting device, in inches.
#'   For example, \code{"5x7"} would create a 5 inch tall and 7 inch wide plotting device.
#'   Defaults to \code{"auto"}, which selects the dimensions that look nice when \code{layout = "auto"}
#'   as well.
#' @param ext_device logical. Do you wish to have an external device open to display the diagnostics?
#'   \code{TRUE} will create a new plotting device using the OS-specific function.
#'   Defaults to \code{FALSE}. Requires \code{StatonMisc}.
#' @param thin_percent if you wish to remove some fraction of the samples before traceplotting, do that here.
#'   The thinning will occur systematically, i.e., at evenly-spaced intervals.
#'   This is sometimes helpful when saving a pdf with many samples/nodes, which can render slowly.
#'   Defaults to \code{0}, i.e., no thinning.
#' @param save logical. Do you wish to save the diagnostic plots in a PDF file? If so,
#'   specify \code{file = "example.pdf"} as well. Defaults to \code{FALSE}.
#' @param file character vector of length 1. The file name, which
#'   must include the \code{".png"} extension. Saved to working directory by default,
#'   but can recieve an absolute or relative file path here as well
#'
#' @export

diag_plots = function(post, p = NULL, layout = "auto", dims = "auto",
                      ext_device = F, thin_percent = 0, save = F, file = NULL) {

  require(StatonMisc)

  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post is not an object of class 'mcmc.list'!")
  }

  # stop if p wasn't supplied
  if (is.null(p)) {
    stop("No nodes supplied to extract. Please specify 'p', please see ?post_extract")
  }
  p = ifelse(!stringr::str_detect(p, "\\\\"), ins_regex_bracket(p), p)

  # error handle for layout
  if (layout %!in% c("auto", "1x1", "4x1", "5x3")) {
    stop ("layout must be one of 'auto', '1x1', '4x1', or '5x3'")
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

  # extract the desired parameters from post
  all_nodes = get_nodes(post, type = "all")
  nodes = unlist(lapply(p, function(x) {
    all_nodes[stringr::str_detect(all_nodes, x)]
  }))
  n = length(nodes)

  # keep ^ and $ if supplied
  nodes = ifelse(stringr::str_detect(p, "\\^"), paste("^", nodes, sep = ""), nodes)
  nodes = ifelse(stringr::str_detect(p, "\\$"), paste(nodes, "$", sep = ""), nodes)

  # set the layout
  if (layout == "auto") {
    layout = ifelse(n == 1, "1x1",
                    ifelse(n <= 8, "4x1", "5x3"))
  }
  row_col = as.numeric(unlist(stringr::str_split(layout, "x")))
  n_per_page = prod(row_col)
  new_page = 1 + n_per_page * seq(1, 50)

  # set the dimensions (in inches)
  if (dims == "auto") {
    dims = c("1x1" = "3.5x7", "4x1" = "8x7","5x3" = "8.5x11")
    dims = dims[layout]
  }
  height_width = as.numeric(unlist(stringr::str_split(dims, "x")))

  if (ext_device) ext_device(h = height_width[1], w = height_width[2])
  if (save) pdf(file, h = height_width[1], w = height_width[2])

  # set up the graphics device
  par(mfrow = c(row_col[1],row_col[2] * 2))

  junk = sapply(nodes, function(i) {
    if (which(nodes == i) %in% new_page & ext_device) {
      ext_device(h = height_width[1], w = height_width[2])
      par(mfrow = c(row_col[1],row_col[2] * 2))
    }
    density_plot(post, i)
    trace_plot(post, i, thin_percent = thin_percent)
  })

  if (save) dev.off()
}
