#' Get the names of saved nodes
#'
#' Sometimes it's helpful to know what nodes are
#' stored in an object of class \code{mcmc.list}.
#' That's what this function does.
#'
#' @param post an object of class \code{mcmc.list}
#' @param type character vector of length == 1. Currently accepted options are
#'   \code{"unique"} and \code{"all"}. See below.
#'
#' @return if \code{type = "unique"}, then the unique node names (i.e., without indices)
#'   will be returned. If \code{type = "all"}, then all node names, including the indices,
#'   will be returned.
#'
#' @seealso \code{\link{base_p}}
#'
#'@export

get_nodes = function(post, type = "unique") {

  # error check for type acceptance
  if (type != "unique" & type != "all") {
    stop("type must be either 'unique' or 'all'")
  }

  # error check for class acceptance
  if (!coda::is.mcmc.list(post)) {
    stop("post must be an object of class 'mcmc.list'")
  }

  # extract all names
  all_p = colnames(post[[1]])

  ## return the appropriate output
  # if only viewing the unique node names
  if(type == "unique") unique(base_p(all_p))

  # if viewing all nodes
  if (type == "all") all_p
}
