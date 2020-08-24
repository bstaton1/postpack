#' Convert a vector to the array structure used in model
#'
#' This function unvectorizes summarized output and creates an array
#' of the appropriate dimensions based on the element names.
#'
#' @param v a vector with names indicating its elements' appropriate placement in a new array.
#'   See the details for more about what this means.
#'
#' @details For example, you have a AxB matrix \code{m} in your JAGS model, and you would like to
#'   create an object in R that stores the posterior means in the same location as
#'   in the JAGS model. If A and B are small, it is simple to create this matrix "by-hand".
#'   However, if there are also dimensions C, D, and E, it becomes much more difficult.
#'   Up to 10 dimensions are currently supported.
#'
#' @examples
#' \dontrun{
#' array_format(post_summ(post, "^m[")["mean",])
#' }
#'
#'@export

array_format = function(v) {
  # stop if v doesn't have names
  if (is.null(names(v))) {
    stop("'v' must have names from an mcmc.list or matrix")
  }

  #the node names
  all_p = names(v)

  # extract the base name
  base = unique(drop_index(all_p))
  if (length(base) > 1) stop(paste0("more than one base name provided: ", paste(base, collapse = ", ")))

  # extract the indices
  inds = stringr::str_extract(all_p, "\\[.+\\]")

  # number of dimensions: count commas and add one
  ndims = unique(stringr::str_count(inds, ",")) + 1
  if (ndims == 1) warning("only one dimension found. You don't need this function in this case")

  # replace the [ and ] with nothing
  inds = stringr::str_replace(inds, "\\[", "")
  inds = stringr::str_replace(inds, "\\]", "")

  # extract the index in each dimension that each element corresponds to
  dim_ids = matrix(as.numeric(unlist(stringr::str_split(inds, ","))), length(inds), ndims, byrow = T)

  # number of elements per dimension
  dims = apply(dim_ids, 2, max)

  # total number of elements
  ni = nrow(dim_ids)

  # output container
  output = array(NA, dims)

  # loop through elements and place them in the right position in out
  # different operations for different ndim
  # the function can handle objects up to 10 dimensions currently
  for (i in 1:ni) {
    if (ndims == 1) output[dim_ids[i]] = v[i]
    if (ndims == 2) output[dim_ids[i,1],dim_ids[i,2]] = v[i]
    if (ndims == 3) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3]] = v[i]
    if (ndims == 4) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4]] = v[i]
    if (ndims == 5) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4],dim_ids[i,5]] = v[i]
    if (ndims == 6) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4],dim_ids[i,5],dim_ids[i,6]] = v[i]
    if (ndims == 7) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4],dim_ids[i,5],dim_ids[i,6],dim_ids[i,7]] = v[i]
    if (ndims == 8) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4],dim_ids[i,5],dim_ids[i,6],dim_ids[i,7],dim_ids[i,8]] = v[i]
    if (ndims == 9) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4],dim_ids[i,5],dim_ids[i,6],dim_ids[i,7],dim_ids[i,8],dim_ids[i,9]] = v[i]
    if(ndims == 10) output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[i,4],dim_ids[i,5],dim_ids[i,6],dim_ids[i,7],dim_ids[i,8],dim_ids[i,9],dim_ids[i,10]] = v[i]
  }

  # return output
  return(output)
}
