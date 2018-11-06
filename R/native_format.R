#' Convert a vector to the structure used in model
#'
#' This function unvectorizes output based on the element names.
#'
#' @param v a vector with names indicating its elements' appropriate placement in a new object.
#'   For example, say you have a 2x2 matrix \code{m} in your JAGS model. \code{summ_post(post, "m")} provides
#'   a matrix made up of 5 rows (summary stats) and 4 columns (the elements of the matrix).
#'   Running \code{native_format(v = summ_post(post, "m")["mean",])} will return a 2x2
#'   matrix with the posterior means of each element stored in the appropriate place.
#' @param row_names logical. Do you want to have row names that reflect the node of interest
#'   inserted? Defaults to \code{TRUE}.
#' @param sub_name a character vector of length 1. If you wish to include a suffix (e.g., "_mean")
#'   after the column and row names in the object. Ignored if \code{row_names = FALSE}.
#' @details By "vectorized", I mean that if in your model you if you have a matrix \code{m},
#' in the summary you will get output stored like \code{m[1,1]; m[2,1], etc.}. If you have a vector
#' with these names, this function turns that vector into a matrix with the elements stored in the
#' appropriate place. This is sometimes useful for plotting or quick comparisons of values,
#' because the programmer thinks in terms of the way the model is storing the elements,
#' which is not necessarily the same as the way it is stored in the summary.
#' Currently up to five dimensions are supported (\code{x[,,,,]}.
#' If more are desired, please submit an issue on the GitHub repository and I'm happy to add them.
#'
#'@export

native_format = function(v, row_names = T, sub_name = "") {
  # stop if v doesn't have names
  if (is.null(names(v))) {
    stop("'v' must have names from an mcmc.list or matrix")
  }

  #the node names
  all_p = names(v)

  # extract the indices
  inds = stringr::str_extract(all_p, "\\[.+\\]")

  # extract the base name
  base = unique(stringr::str_replace(all_p, "\\[.+\\]", ""))

  # number of dimensions
  ndims = unique(stringr::str_count(inds, ",")) + 1

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

  # add an underscore if necessary
  subname = ifelse(sub_name != "", paste("_", sub_name, sep = ""), sub_name)

  # loop through elements and place them in the right position in out
  # different operations for different ndim
  # the function can handle objects up to 5 dimensions currently
  for (i in 1:ni) {
    if(ndims == 1) {
      output[dim_ids[i]] = v[i]
      dimnames(output) = list(paste(base, subname, "[", dim_ids, "]", sep = ""))
    }

    if (ndims == 2) {
      output[dim_ids[i,1],dim_ids[i,2]] = v[i]
      dimnames(output) = list(
        paste(base, subname, "[", unique(dim_ids[,1]), ",]", sep = ""),
        paste(base, subname, "[,", unique(dim_ids[,2]), "]", sep = "")
      )
    }

    if (ndims == 3) {
      output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3]] = v[i]
    }

    if (ndims == 4) {
      output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[,4]] = v[i]
    }

    if (ndims == 5) {
      output[dim_ids[i,1],dim_ids[i,2],dim_ids[i,3],dim_ids[,4],dim_ids[,5]] = v[i]
    }
  }

  # return output
  return(output)
}
