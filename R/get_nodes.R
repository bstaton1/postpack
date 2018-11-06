#' Get the names of saved nodes
#'
#' Sometimes it's helpful to know what nodes are
#' stored in an object of class \code{mcmc.list}.
#' That's what this function does.
#'
#' @param post an object of class \code{mcmc.list}
#' @param type character vector of length 1. Currently accepted options are
#'   \code{"unique"} and \code{"all"}. See below.
#'
#' @return if \code{type = "unique"}, then the unique node names (i.e., without indices)
#'   will be returned. If \code{type = "all"}, then all node names, including the indices,
#'   will be returned.
#'
#'@export

get_nodes = function(post, type = "unique") {
  require(StatonMisc) # for %!in%

  # error check for type acceptance
  if (type %!in% c("unique", "all")) {
    stop("type must be either 'unique' or 'all'")
  }

  # if only viewing the unique node names
  if(type == "unique") {
    output = unique(stringr::str_replace(colnames(post[[1]]), "\\[.+\\]", ""))
  }

  # if viewing all nodes
  if (type == "all") {
    output = colnames(post[[1]])
  }

  # return the output
  return(output)
}
