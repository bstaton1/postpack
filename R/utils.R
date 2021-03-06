#' @title Insert escapes on regex brackets
#' @param params Node names.
#' @details Searches the contents of a string for the occurrence of a
#'  square bracket or two, and inserts the necessary escapes for pattern matching
#'  via regular expressions.
#' @note This is **not** a function users will generally use directly.
#' @return A character vector with all brackets escaped. For example,
#'   `"a[1]"` becomes `"a\\[1\\]"`

ins_regex_bracket = function(params) {
  out = stringr::str_replace(params, "\\[", "\\\\[")
  out = stringr::str_replace(out, "\\]", "\\\\]")
  return(out)
}

#' @title Remove escapes on regex brackets
#' @param params Node names.
#' @details Searches the contents of a string for the occurrence of a
#'  square bracket or two (that has been escaped), and removes the escaping
#'  that was necessary for matching via regular expressions.
#' @note This is **not** a function users will generally use directly.
#' @return A character vector with all brackets escaped. For example,
#'   `"a\\[1\\]"` becomes `"a[1]"`.

rm_regex_bracket = function(params) {
  out = stringr::str_replace(params, "\\\\\\[", "\\[")
  out = stringr::str_replace(out, "\\\\\\]", "\\]")
  return(out)
}

#' @title Insert the symbols to lock in a string for matching
#' @description To ensure that a regular expression will match exactly,
#'   it's necessary to specify so.
#' @param params Node names to paste a `^` and `$` (if not already present) to lock in the match.
#' @note This is **not** a function users will generally use directly.
#' @return A character vector with locking anchors inserted to force an exact match. For example,
#'   `"a\\[1\\]"` becomes `"^a\\[1\\]$"`.

ins_regex_lock = function(params) {
  if (!stringr::str_detect(params, "\\^")) out = paste("^", params, sep = "")
  if (!stringr::str_detect(out, "\\$")) out = paste(out, "$", sep = "")
  return(out)
}

#' @title Remove the symbols that lock in a string for matching
#' @description Undoes the work of [ins_regex_lock()].
#' @param params Node names to remove a `^` and `$` from (if present).
#' @note This is **not** a function users will generally use directly.
#' @return A character vector with locking anchors inserted to force an exact match. For example,
#'   `"^a\\[1\\]$"` becomes `"a\\[1\\]"`.

rm_regex_lock = function(params) {
  out = stringr::str_replace(params, "\\^", "")
  out = stringr::str_replace(out, "\\$", "")
  return(out)
}

#' @title Extract the base node name of a parameter
#' @description Removes square brackets, numbers, and commas that represent
#'   the index of the node element in question. Returns just the node name.
#' @param params Node names.
#' @note This is **not** a function users will generally use directly.
#' @return A character vector with the same length as `params`, with no indices included.
#'   For example, `"a[1]"` becomes `"a"`.

drop_index = function(params) {
  stringr::str_replace(params, "\\[.+\\]", "")
}

#' @title Add a title between two figures
#' @description Used by [diag_plots()] to place a common
#'   title over top of two figures: one density and one trace
#'   for a given node.
#' @param text The text string to include as a centered title over two adjacent plots.
#' @note This is **not** a function users will generally use directly.

mytitle = function(text) {
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  text(x = usr[1] - xdiff * 0.02, y = usr[4] + ydiff * 0.11,
       labels = text, font = 2, cex = 1.4, xpd = NA)
}

#' @title Extract chain and iteration IDs for each sample
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @note This is **not** a function users will generally use directly.
#' @return A matrix with columns `"CHAIN"` and `"ITER"`.

id_mat = function(post) {
  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # extract and return the sample identification information
  as.matrix(post, iters = TRUE, chains = TRUE)[,c("CHAIN", "ITER")]
}

#' @title List vector elements in a nice format
#' @description Converts a vector into a comma-separated list for use in sentences (error messages, warnings, etc.).
#' @param x A vector, will be coerced to a character.
#' @param final Word that will separate the final element in the list from others.
#'   See the examples.
#' @param per_line Number of elements printed per line. See the examples.
#' @param wrap Optional character to wrap around each element, e.g., quotation marks.
#' @param indent Optional string to place in front of the first
#'   element on each line. See the examples.
#' @return A character vector with length == 1; ready to be passed to
#'   [base::stop()], [base::warning()], or [base::cat()], to provide a useful message.

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
      new_line[n] = FALSE

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
