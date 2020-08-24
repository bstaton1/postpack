#' Export BUGS model from function to file
#'
#' Performs the same basic function as \code{\link[R2OpenBUGS]{write.model}}
#'
#' @param fun a function object containing BUGS model code
#' @param file a character vector of length 1: the name of the file to write to
#'
#' @details This function performs the same basic function as \code{\link[R2OpenBUGS]{write.model}},
#'     but with slightly better output (scientific notation, spacing, etc.). The main reason it was created
#'     for use in this package was to remove the need for the \code{R2OpenBUGS} package, as this was
#'     the only function the author ever used from it.
#' @examples
#' if (interactive()) {
#'   # define some simple BUGS model as an R function
#'   # note the use of %_% to include a truncation
#'   mod = function() {
#'     # PRIORS
#'     mu ~ dnorm(0,0.001) %_% T(0,)
#'     sig ~ dunif(0,10)
#'     tau <- 1/sig^2
#'
#'     # LIKELIHOOD
#'     for (i in 1:n) {
#'       y[i] ~ dnorm(mu, tau)
#'     }
#'   }
#'
#'   # write model to a text file to be called by BUGS/JAGS
#'   write_model(mod, "model.txt")
#' }
#' @export

write_model = function(fun, file) {

  # error check
  if (!is.function(fun)) {
    stop("'fun' must be a function object")
  }

  # extract the body
  model_body = body(fun)
  model_body = as.character(model_body)
  model_body = model_body[-1]

  # remove all %_% instances
  model_body = stringr::str_replace_all(model_body, "%_%", "")

  # split on new lines
  model_body = unlist(stringr::str_split(model_body, "\n"))

  # reduce the number of spaces used for indenting: this is super hacky
  model_body = stringr::str_replace_all(model_body, stringr::fixed("    "), "  ")
  model_body = stringr::str_replace_all(model_body, stringr::fixed("      "), "    ")
  model_body = stringr::str_replace_all(model_body, stringr::fixed("        "), "      ")
  model_body = stringr::str_replace_all(model_body, stringr::fixed("          "), "        ")
  model_body = stringr::str_replace_all(model_body, stringr::fixed("            "), "          ")
  model_body = stringr::str_replace_all(model_body, stringr::fixed("              "), "            ")
  model_body = stringr::str_replace_all(model_body, stringr::fixed("                "), "              ")

  # add in a constant indent to all lines
  model_body = paste0("  ", model_body)

  # add the first and last lines
  model = c("model {", model_body, "}")

  # write the model to file
  writeLines(model, file)
}
