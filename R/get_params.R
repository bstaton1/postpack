#' @title Obtain the names of all nodes
#' @description Returns the names of all quantities stored in
#'   a [`mcmc.list`][coda::mcmc.list] object.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param type Format of returned matches; only two options are accepted:
#'   * `type = "base_only"` (the default) to return only the unique node names (without indices).
#'   * `type = "base_index"` to return the node names with indices included.
#' @return A character vector with all node names stored in the `post` object, formatted as requested by `type`.
#' @examples
#'  # load example mcmc.list
#' data(cjs)
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
