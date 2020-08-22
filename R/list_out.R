#' List vector elements in a nice format
#'
#' Oftentimes in my functions I like to list out the accepted arguments if
#' the user enters one that is not accepted. This function faciliates that.
#'
#' @param x a vector object, will be coerced to a character class
#' @param final character vector of length == 1: word that will separate the final element in the list from others.
#'   See the examples
#' @param per_line numeric vector of length == 1: number of elements printed per line. See the examples
#' @param wrap character vector of length == 1: optional character to wrap around each element,
#'   e.g., quotation marks
#' @param indent character vector of length == 1: anything you wish to place in front of the first
#'   element on each line. See the examples
#' @return a character vector of length == 1, ready to be passed to
#'   \code{\link[base]{stop}}, \code{\link[base]{warning}}, or \code{\link[base]{cat}}, for example.
#' @examples
#' list_out(x = letters[1:10])
#' cat(list_out(letters[1:26], wrap = "'", per_line = 4, final = "or", indent = "  "))

list_out = function(x, final = NULL, per_line = 1e6, wrap = NULL, indent = NULL) {

  # coerce x to a character
  x = as.character(x)

  # count the elements of x
  n = length(x)

  # insert the wrapping characters if requested
  if (!is.null(wrap)) x = paste0(wrap, x, wrap)

  if (n == 1) {
    out = paste0(indent, x)
  } else {
    if (n == 2) {
      out = paste0(indent, x[1], ifelse(!is.null(final), paste0(" ", final), ","), " ", x[2])
    } else {
      # which elements are on a new line?
      new_line = 1:n %% per_line == 0
      new_line[n] = F

      # which elements are the first on a new line?
      first_on_line = c(1, which(new_line) + 1)

      # add commas to all elements except the last one
      x[1:(n-1)] = paste0(x[1:(n-1)], ", ")

      # include the final word if requested
      if (!is.null(final)) x[n] = paste(final, x[n])

      # insert the line breaks
      x[new_line] = paste0(x[new_line], "\n")

      # insert the indents
      x[first_on_line] = paste0(indent, x[first_on_line])

      # combine into one element
      out = paste(x, collapse = "")
    }
  }

  # return the output
  return(out)
}
