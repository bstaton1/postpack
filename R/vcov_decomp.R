#' Decompose Variance-Covariance Matrix Node
#'
#' For each posterior sample, extract the standard deviation and correlation components
#' of a monitored node representing a variance-covariance matrix,
#' and return the output as a \code{mcmc.list} object
#'
#' @param post an object of class \code{mcmc.list}
#' @param p a character vector with length >= 1. Passed to \code{\link{match_p}}.
#'   Must match only one node in \code{post}, and that node must be a matrix.
#' @param progress logical. Given this calculation can be slow for large matrices,
#'   do you wish to have a progress indicator printed to the console? Defaults to \code{TRUE}
#' @param sigma_base_name a character vector with length == 1. What should the base node name be
#'   for the standard deviation vector component? Defaults to \code{sigma}, which becomes \code{sigma[1]}, \code{sigma[2]},
#'   etc. in the output
#' @param rho_base_name same as \code{sigma_base_name}, but for the correlation matrix component
#' @param invert logical. Do you wish to take the inverse of the matrix node matched by \code{p}
#'   prior to performing the calculations? This would be necessary if the matrix node was expressed as
#'   a precision matrix as required in the BUGS language. Triggers a call to \code{\link[base]{solve}}.
#'   Defaults to \code{FALSE}
#' @param check logical. Do you wish to perform checks sequentially for (a) square-ness, (b) symmetry, and (c) positive definite-ness
#'   before proceeding with the calculations? Defaults to \code{TRUE}, if set to \code{FALSE} unexpected output may be returned or
#'   other errors related to items a, b, and c may be triggered - this is not advised, though may be required
#'   if wishing to set \code{invert = TRUE}.
#'
#' @return an object of class \code{mcmc.list}
#' @export

vcov_decomp = function(post, p, progress = T, sigma_base_name = "sigma", rho_base_name = "rho", invert = F, check = T) {

  # check for only one node match; the normal checks will be done by match_p
  matched_p = match_p(post, p, ubase = T)
  if (length(matched_p) > 1) {
    stop("more than one unique base name matched by p: \n\n    ", paste(paste0("'", matched_p, "'"), collapse = ", "))
  }

  # extract desired node
  Sigma_samps = post_subset(post, p, matrix = T, chains = T, iters = T)
  id_mat = Sigma_samps[,c("CHAIN","ITER")]
  Sigma_samps = Sigma_samps[,-which(colnames(Sigma_samps) %in% c("CHAIN", "ITER"))]
  test_mat = array_format(Sigma_samps[1,])

  # check for whether p is a matrix: search for 1 comma
  comma_counts = stringr::str_count(colnames(Sigma_samps), ",")
  if (length(comma_counts) < 1 | !all(comma_counts == 1)) {
    stop("the node matched by p ('", matched_p, "') is not a matrix in the model")
  }

  # "optional" checks for covariance matrix
  if (check) {
    # for whether p is a square matrix
    if(!matrixcalc::is.square.matrix(test_mat)) {
      stop("the node matched by p ('", matched_p, "') is a matrix in the model, but not a square one")
    }

    # for whether p is a symmetric matrix
    if (!matrixcalc::is.symmetric.matrix(test_mat)) {
      stop("the node matched by p ('", matched_p, "') is a square matrix in the model, but not a symmetric one")
    }

    # check for whether p is positive definite
    if (!matrixcalc::is.positive.definite(test_mat)) {
      stop("the node matched by p ('", matched_p, "') is a symmetric matrix in the model, but it is not positive definite")
    }
  }

  # passes all checks, proceed with calculations
  # the dimensions
  n = sqrt(ncol(Sigma_samps))
  ni = post_dim(post, "saved")

  # containers
  sigma_samps = matrix(NA, ni, n)
  rho_samps_array = array(NA, dim = c(n, n, ni))
  rho_samps = matrix(NA, ni, n^2)

  if (progress) cat("Decomposing variance-covariance matrix node: ", matched_p, " (", n, "x", n, ")\n\n  ", sep = "")

  # calculate the sigma vector and rho matrix for each posterior sample
  for (i in 1:ni) {
    if (progress) cat("\r", floor(i/ni * 100), "%", sep = "")
    Sigma_tmp = array_format(Sigma_samps[i,])
    if (invert) Sigma_tmp = solve(Sigma_tmp)
    sigma_samps[i,] = sqrt(diag(Sigma_tmp))
    for (row in 1:n) {
      for (col in 1:n) {
        rho_samps_array[row,col,i] = Sigma_tmp[row,col]/(sigma_samps[i,row] * sigma_samps[i,col])
      }
    }
    rho_samps[i,] = as.numeric(rho_samps_array[,,i])
  }; if(progress) cat("\n")

  # generate names for rho
  rho_names = matrix(NA, n, n)
  for (col in 1:n) {
    for (row in 1:n) {
      rho_names[row,col] = paste0(rho_base_name, "[", row, ",", col, "]")
    }
  }; rho_names = as.character(rho_names)

  # generate names for sigma
  sigma_names = paste0(sigma_base_name, "[", 1:n, "]")

  # assign names
  colnames(rho_samps) = rho_names
  colnames(sigma_samps) = sigma_names

  # combine into large matrix
  out = cbind(id_mat, sigma_samps, rho_samps)

  # convert to an mcmc.list
  matrix2mcmclist(out)
}

