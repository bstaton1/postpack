#' @title Convert a vector to the array structure used in model
#' @description Use element names to place vector elements in
#'   the appropriate location of an array.
#' @param v A vector with names indicating the index location of each element in a new array.
#'   See the details (particularly the example) for more about what this means.
#' @details Suppose you have an AxB matrix in your model, and you would like to
#'   create an object that stores the posterior means in the same AxB matrix as found in
#'   the model. For an AxB matrix, this is not too difficult to do "by-hand".
#'   However, if there are also dimensions C, D, and E, missing values, etc. it becomes more difficult.
#' @note Up to 10 dimensions are currently supported. Please submit an
#'   [issue](https://github.com/bstaton1/postpack/issues)
#'   should you find that you need more dimensions.
#' @return An array with elements of `v` placed in the appropriate location based on their
#'   index names.
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # find an array node from your model
#' match_params(cjs, "SIG")
#'
#' # extract the posterior mean of it
#' SIG_mean = post_summ(cjs, "SIG")["mean",]
#'
#' # note that it has element names
#' SIG_mean
#'
#' # create a matrix with elements in the proper place
#' array_format(SIG_mean)
#'
#'@export

array_format = function(v) {
  # stop if v doesn't have names
  if (is.null(names(v))) {
    stop("'v' must have names with element indices included")
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
  if (ndims > 10) stop ("more than 10 dimensions found. See ?array_format")

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
