#' Combine two objects containing posterior samples
#'
#' Bind together two objects containing posterior samples into one \code{mcmc.list} object.
#' Intended for use when derived quantities are calculated from monitored posterior samples,
#' and you wish to combine them into the master list, as though they were calculated and monitored during MCMC sampling.
#' It is not advised to combine samples from two MCMC runs (because covariance
#' of MCMC sampling would be lost).
#'
#' @param post1 an object of class \code{mcmc.list} or \code{matrix}
#' @param post2 an object of class \code{mcmc.list} or \code{matrix}
#' @param dup_id a character vector of length == 1. If any
#'   node names are duplicated in post2, what should be appended to the
#'   end of these node names in the output? If this occurs a warning will be returned.
#'   Defaults to \code{"_p2"}
#'
#' @details Some important things to note:
#' \itemize{
#'   \item{if the object passed to \code{post1} is a matrix, \code{post2} must be an mcmc.list, and vice versa}
#'   \item{for matrix objects, nodes should be stored as columns and samples should be stored as rows. Column names should be present}
#'   \item{the objects passed to \code{post1} and \code{post2} must contain the same number of samples per node}
#'   \item{if the objects passed to \code{post1} and \code{post2} are both mcmc.lists, they must have the same number of chains, iterations, burnin, and thinning interval}
#'   \item{if the node names are empty (e.g., only one node in an mcmc.list or missing column names in a matrix), the node names will be coerced to \code{"var1"}, \code{"var2"}, etc.}
#' }
#' @examples
#' # load example mcmc.list
#' data(cjs, package = "postpack")
#'
#' # create two subsets from cjs: one as mcmc.list and one as matrix
#' # also works if both are mcmc.list objects
#' p1 = post_subset(cjs, "b0")
#' p2 = post_subset(cjs, "b1", matrix = T)
#'
#' # combine them into one mcmc.list
#' head(post_bind(p1, p2))
#' @export

post_bind = function(post1, post2, dup_id = "_p2") {

  # error handlers for class correctness
  class1 = class(post1)[1]; class2 = class(post2)[1]
  classes = c(class1, class2)

  if (!all(classes %in% c("mcmc.list", "matrix"))) {
    stop ("post1 and post2 must be objects of class 'mcmc.list' or 'matrix'")
  }

  if (all(classes == "matrix")) {
    stop ("one of post1 or post2 must be an object of class 'mcmc.list'")
  }

  # case A: post1 is a mcmc.list, post2 is a matrix
  # case B: post1 is a matrix, post2 is a mcmc.list
  # case C: post1 is a mcmc.list, post2 is a mcmc.list

  # perform checks for case A
  if (class1 == "mcmc.list" & class2 == "matrix") {
    ids = id_mat(post1)
    if (nrow(ids) != nrow(post2)) {
      stop ("the number of samples in post1 (mcmc.list object) and post2 (matrix object) are unequal")
    }
    if (is.null(colnames(post2))) {
      colnames(post2) = paste0("var", 1:ncol(post2))
      warning ("the nodes stored in post2 did not have names. They have been assigned:\n", list_out(colnames(post2), final = "and", wrap = '"', per_line = 5, indent = "    "))
    }
    post1_m = as.matrix(post1)
    post2_m = post2
  }

  # perform checks for case B
  if (class1 == "matrix" & class2 == "mcmc.list") {
    ids = id_mat(post2)
    if (nrow(ids) != nrow(post1)) {
      stop ("the number of samples in post1 (matrix object) and post2 (mcmc.list object) are unequal")
    }
    if (is.null(colnames(post1))) {
      colnames(post1) = paste0("var", 1:ncol(post1))
      warning ("the nodes stored in post1 did not have names. They have been assigned:\n", list_out(colnames(post1), final = "and", wrap = '"', per_line = 5, indent = "    "))
    }
    post1_m = post1
    post2_m = as.matrix(post2)
  }

  # perform checks for case C
  if (class1 == "mcmc.list" & class2 == "mcmc.list") {
    ids1 = id_mat(post1)
    ids2 = id_mat(post2)
    if (nrow(ids1) != nrow(ids2)) {
      stop ("the number of samples in post1 and post2 are unequal")
    } else {
      if (!all(ids1 == ids2)) {
        stop ("the chains or sample iteration numbers are inconsistent between post1 and post2")
      }
    }
    post1_m = as.matrix(post1)
    post2_m = as.matrix(post2)
    ids = ids1
  }

  # cbind them
  post_m = cbind(ids, post1_m, post2_m)

  # if any names are duplicated, append them for unique identification and return a warning
  dups = duplicated(colnames(post_m))
  if (any(dups)) {
    dup_names = colnames(post_m)[dups]
    colnames(post_m)[dups] = paste0(dup_names, dup_id)
    warning ("the following node names were duplicated between post1 and post2:\n",
             list_out(dup_names, final = "and", wrap = '"', per_line = 5, indent = "    "),
             "\n The node names of post2 that were duplicated have been altered to:\n",
             list_out(colnames(post_m)[dups], final = "and", wrap = '"', per_line = 5, indent = "    "))
  }

  # coerce back to mcmc.list
  post_convert(post_m)
}
