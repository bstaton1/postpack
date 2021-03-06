% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diag_plots.R
\name{diag_plots}
\alias{diag_plots}
\title{Create MCMC diagnostic plots for nodes of interest}
\usage{
diag_plots(
  post,
  params,
  ext_device = FALSE,
  show_diags = "if_poor_Rhat",
  layout = "auto",
  dims = "auto",
  keep_percent = 1,
  save = FALSE,
  file = NULL,
  auto_escape = TRUE
)
}
\arguments{
\item{post}{A \code{\link[coda:mcmc.list]{mcmc.list}} object.}

\item{params}{A vector of regular expressions specifying the nodes to match for plotting.
Accepts multi-element vectors to match more than one node at a time.
See \code{\link[=match_params]{match_params()}} and \code{vignette("pattern-matching")} for more details.}

\item{ext_device}{Display plots in an external device rather than the active device?
\code{FALSE} (the default) will plot in the active device (including RStudio window).
\code{TRUE} will create a new graphics device.}

\item{show_diags}{Control when to display numerical diagnostic summaries on plots. Must be one of
\code{"always"}, \code{"never"}, or \code{"if_poor_Rhat"}. \code{"if_poor_Rhat"} (the default)
will display the Rhat and effective MCMC samples if the Rhat statistic
is greater than 1.1.}

\item{layout}{Control how parameter diagnostics are organized into \code{"ROWSxCOLUMNS"}.
For example, \code{layout = "4x1"} has 4 rows and 1 column of parameter diagnostics.
Defaults to \code{"auto"}, which selects between the only accepted options of
\code{"1x1"}, \code{"2x1"}, \code{"4x1"}, \code{"4x2"}, and \code{"5x3"}.}

\item{dims}{Control the dimensions of the graphics device using \code{"HEIGHTxWIDTH"} in inches.
For example, \code{"5x7"} would create a 5 inch tall and 7 inch wide plotting device.
Defaults to \code{"auto"}, which selects the dimensions that look nice when \code{layout = "auto"}
as well.}

\item{keep_percent}{Proportion (between 0 and 1) of samples to keep for trace plotting.
Passed to \code{\link[=post_thin]{post_thin()}}.}

\item{save}{Save the diagnostic plots in a PDF file? If so,
specify \code{file = "example.pdf"} as well. Defaults to \code{FALSE}.}

\item{file}{File name of a PDF file to save the plots to.
Must include the \code{".pdf"} extension. Saved to working directory by default,
but can receive an absolute or relative file path as part of this argument.}

\item{auto_escape}{Automatically escape \code{"["} and \code{"]"} characters for pattern matching?
See \code{\link[=match_params]{match_params()}} for details.}
}
\value{
A multi-panel figure showing the posterior density and trace plots for requested nodes.
The device in which it is placed depends on the argument values.
}
\description{
Allows quick visualization of posterior density and trace plots,
\strong{both} separated by chain, for the desired nodes of interest. Includes the
ability to plot in the RStudio graphics device, an external device,
or a PDF file. Further, with the auto settings, the dimensions of the
plotting device scales to the job needed.
}
\note{
If saving as a pdf, these files can get very large with many samples and render slowly.
The \code{keep_percent} argument is intended to help with this by thinning the chains at quasi-evenly spaced intervals.
}
\examples{
if (interactive()) {
  #load example mcmc.list
  data(cjs)

  # use current device
  diag_plots(cjs, "B0")

  # use a new device
  diag_plots(cjs, "B0", ext_device = TRUE)

  # always show diagnostic summaries
  diag_plots(cjs, "B0", show_diags = "always")

  # use a different layout (leaving it as "auto" is usually best)
  diag_plots(cjs, c("sig", "b"), layout = "5x3")

  # save diagnostics for all nodes to a pdf file
  diag_plots(cjs, "", save = TRUE, file = "diags.pdf")
}
}
\seealso{
\code{\link[=match_params]{match_params()}}, \code{\link[=density_plot]{density_plot()}}, \code{\link[=trace_plot]{trace_plot()}}
}
