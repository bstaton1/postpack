#' @title Decompose the posterior of a variance-covariance node
#' @description For each posterior sample, extract the standard deviation and correlation components
#'   of a monitored node representing a variance-covariance matrix.
#' @param post A [`mcmc.list`][coda::mcmc.list] object.
#' @param param A vector of regular expressions specifying the nodes to match for plotting.
#'   Must match only one base node name in `post`, and that node must store samples from a matrix within the model.
#'   See [match_params()] and `vignette("pattern-matching")` for more details.
#' @param sigma_base_name Base node name to assign to the standard deviation vector component?
#'   Defaults to `"sigma"`, which becomes `"sigma[1]"`, `"sigma[2]"`, etc. in the output.
#' @param rho_base_name Same as `sigma_base_name`, but for the correlation matrix component.
#' @param invert Take the inverse of the matrix node matched by `param`
#'   prior to performing the calculations? This would be necessary if the matrix node was expressed as
#'   a precision matrix as used in the BUGS language. Triggers a call to [base::solve()].
#' @param check Perform checks sequentially that the matrix node is (a) square, (b) symmetrical, and (c) positive definite
#'   before proceeding with the calculations? If set to `FALSE`, unexpected output may be returned or
#'   other errors related to items a, b, and c may be triggered - this is not advised, though may be required
#'   if wishing to set `invert = TRUE`.
#' @param auto_escape Automatically escape `"["` and `"]"` characters for pattern matching?
#'   See [match_params()] for details.
#' @return A [`mcmc.list`][coda::mcmc.list] object.
#' @examples
#' # load example mcmc.list
#' data(cjs)
#'
#' # "SIG" is a covariance matrix node
#' SIG_decomp = vcov_decomp(cjs, "SIG")
#'
#' # extract the posterior mean correlation matrix, and reformat
#' array_format(post_summ(SIG_decomp, "rho")["mean",])
#' @export

vcov_decomp = function(post, param, sigma_base_name = "sigma", rho_base_name = "rho", invert = FALSE, check = TRUE, auto_escape = TRUE) {

  # check for only one node match; the normal checks will be done by match_params
  matched_param = match_params(post, param, type = "base_only", auto_escape = auto_escape)
  if (length(matched_param) > 1) {
    stop("more than one unique base name matched by param: \n\n    ", paste(paste0("'", matched_param, "'"), collapse = ", "))
  }

  # extract desired node
  Sigma_samps = post_subset(post, param, matrix = TRUE)
  test_mat = array_format(Sigma_samps[1,])

  # check for whether node matched by param is a matrix: search for 1 comma
  comma_counts = stringr::str_count(colnames(Sigma_samps), ",")
  if (length(comma_counts) < 1 | !all(comma_counts == 1)) {
    stop("the node matched by param ('", matched_param, "') is not a matrix in the model")
  }

  # "optional" checks for covariance matrix
  if (check) {
    # for whether node matched by param is a square matrix
    if(!matrixcalc::is.square.matrix(test_mat)) {
      stop("the node matched by param ('", matched_param, "') is a matrix in the model, but not a square one")
    }

    # for whether node matched by param is a symmetric matrix
    if (!matrixcalc::is.symmetric.matrix(test_mat)) {
      stop("the node matched by param ('", matched_param, "') is a square matrix in the model, but not a symmetric one")
    }

    # check for whether p is positive definite
    if (!matrixcalc::is.positive.definite(test_mat)) {
      stop("the node matched by param ('", matched_param, "') is a symmetric matrix in the model, but it is not positive definite")
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

  message("Decomposing variance-covariance matrix node", ifelse(invert, " (after inverting)", ""),  ": ", matched_param, " (", n, "x", n, ")\n\n  ", sep = "")

  # calculate the sigma vector and rho matrix for each posterior sample
  for (i in 1:ni) {
    Sigma_tmp = array_format(Sigma_samps[i,])
    if (invert) Sigma_tmp = solve(Sigma_tmp)
    sigma_samps[i,] = sqrt(diag(Sigma_tmp))
    for (row in 1:n) {
      for (col in 1:n) {
        rho_samps_array[row,col,i] = Sigma_tmp[row,col]/(sigma_samps[i,row] * sigma_samps[i,col])
      }
    }
    rho_samps[i,] = as.numeric(rho_samps_array[,,i])
  }

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
  out = cbind(id_mat(post), sigma_samps, rho_samps)

  # convert to an mcmc.list
  post_convert(out)
}
