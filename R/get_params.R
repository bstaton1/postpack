#' Get the names of saved nodes
#'
#' Sometimes it's helpful to know what nodes are
#' stored in an object of class \code{mcmc.list}.
#' That's what this function does.
#'
#' @param post an object of class \code{mcmc.list}
#' @param type a character vector with length == 1; only two options are accepted.
#'   Set to \code{type = "base_only"} (the default) if you wish to return only the unique node names (without indices).
#'   Set to \code{type = "base_index"}  if you wish to return the node names with indices included.
#' @return character vector with all node names stored in the object \code{post}, formatted as requested by \code{type}
#' @examples
#'  # load example mcmc.list
#' data(cjs, package = "postpack")
#'
#' # get only node names, no indices (default)
#' get_params(cjs, type = "base_only")
#'
#' # get indices too, where applicable
#' get_params(cjs, type = "base_index")
#'@export

get_params = function(post, type = "base_only") {

  # stop if post isn't mcmc.list
  if (!coda::is.mcmc.list(post)) {
    stop ("post must be an object of class 'mcmc.list'")
  }

  # stop if type isn't one of "base_only" or "base_index"
  if (!(type %in% c("base_only", "base_index"))) {
    stop ("type must be one of 'base_only' or 'base_index'. See ?get_params for details")
  }

  # extract all names
  all_params = colnames(post[[1]])

  ## return the appropriate output
  if (type == "base_only") return(unique(drop_index(all_params))) else return(all_params)
}
