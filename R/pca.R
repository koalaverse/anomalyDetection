#' @title Principal Component Analysis
#'
#' @description
#' \code{principal_components} relates the data to a set of a components through
#' the eigen-decomposition of the correlation matrix, where each component explains
#' some variance of the data and returns the results as an object of class prcomp.
#'
#' @param data numeric data.
#' @param retx a logical value indicating whether the rotated variables should be returned.
#' @param center a logical value indicating whether the variables should be shifted to be
#'     zero centered. Alternately, a vector of length equal the number of columns of x can
#'     be supplied. The value is passed to scale.
#' @param scale. a logical value indicating whether the variables should be scaled to have
#'     unit variance before the analysis takes place. The default is FALSE for consistency
#'     with S, but in general scaling is advisable. Alternatively, a vector of length equal
#'     the number of columns of \code{data} can be supplied. The value is passed to scale.
#' @param tol a value indicating the magnitude below which components should be omitted.
#'     (Components are omitted if their standard deviations are less than or equal to tol
#'     times the standard deviation of the first component.) With the default null setting,
#'     no components are omitted. Other settings for tol could be \code{tol = 0} or
#'     \code{tol = sqrt(.Machine$double.eps)}, which would omit essentially constant components.
#' @param ... arguments passed to or from other methods.
#'
#' @details
#'
#' The calculation is done by a singular value decomposition of the (centered and
#'     possibly scaled) data matrix, not by using eigen on the covariance matrix.
#'     This is generally the preferred method for numerical accuracy
#'
#' @return \code{principal_components} returns a list containing the following components:
#'     \enumerate{
#'       \item \code{pca_sdev}: the standard deviations of the principal components (i.e., the square roots of the eigenvalues of the correlation matrix, though the calculation is actually done with the singular values of the data matrix).
#'       \item \code{pca_loadings}: the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#'       \item \code{pca_rotated}: if \code{retx} is \code{TRUE} the value of the rotated data (the centred (and scaled if requested) data multiplied by the rotation matrix) is returned. Hence, \code{cov(x)} is the diagonal matrix \code{diag(sdev^2)}.
#'       \item \code{pca_center}: the centering used
#'       \item \code{pca_scale}: whether scaling was used
#'     }
#'
#' @seealso
#'
#' \code{\link{prcomp}}, \code{\link{biplot.prcomp}}, \code{\link{screeplot}}, \code{\link{cor}},
#' \code{\link{cov}}, \code{\link{svd}}, \code{\link{eigen}}
#'
#'
#' @examples
#' x <- matrix(rnorm(200 * 3), ncol = 10)
#' principal_components(x)
#' principal_components(x, scale = TRUE)
#'
#' @export

principal_components <- function(data, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, ...) {

  pca <- stats::prcomp(data, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, ...)
  list(
    pca_sdev = pca[[1]],
    pca_loadings = pca[[2]],
    pca_rotated = pca[[5]],
    pca_center = pca[[3]],
    pca_scale = pca[[4]]
       )

}

#' Easy Access to Principal Component Analysis Results
#'
#' \code{principal_components_result} Provides easy access to principal
#' component analysis results
#'
#' @param data list output from \code{principal_components}
#' @param results principal component analysis results to extract. Can use either
#'   results name or number (i.e. pca_loadings or 2):
#'     \enumerate{
#'       \item \code{pca_sdev}
#'       \item \code{pca_loadings} (default)
#'       \item \code{pca_rotated}
#'       \item \code{pca_center}
#'       \item \code{pca_scale}
#'     }
#'
#'
#' @return Returns one of the selected results:
#'     \enumerate{
#'       \item \code{pca_sdev}: the standard deviations of the principal components (i.e., the square roots of the eigenvalues of the correlation matrix, though the calculation is actually done with the singular values of the data matrix).
#'       \item \code{pca_loadings}: the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#'       \item \code{pca_rotated}: if \code{retx} is \code{TRUE} the value of the rotated data (the centred (and scaled if requested) data multiplied by the rotation matrix) is returned. Hence, \code{cov(x)} is the diagonal matrix \code{diag(sdev^2)}.
#'       \item \code{pca_center}: the centering used
#'       \item \code{pca_scale}: whether scaling was used
#'     }
#'
#' @seealso
#'
#' \code{\link{principal_components}} for computing the principal components results
#'
#' @examples
#'
#' # An efficient means for getting principal component analysis results
#' x <- matrix(rnorm(200 * 3), ncol = 10)
#'
#' principal_components(x) %>%
#'   principal_components_result(pca_loadings)
#'
#' @export

principal_components_result <- function(data, results = 2) {

  result_input <- deparse(substitute(results))
  result_options <- names(data)

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing argument: data argument", call. = FALSE)
  }
  if(missing(result_input)) {
    stop("Missing argument: results argument", call. = FALSE)
  }

  if(result_input %in% paste(1:5)) {
    data[[results]]
  } else if(result_input %in% result_options) {
    data[[result_input]]
  } else {
    stop("Invalid results argument: see ?principal_components_result for options", call. = FALSE)
  }
}
