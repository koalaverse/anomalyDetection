#' @title Principal Component Analysis
#'
#' @description
#' \code{principal_components} relates the data to a set of a components through
#' the eigen-decomposition of the correlation matrix, where each component explains
#' some variance of the data. The eigenvalues can be normalized to determine how
#' much of the variance each component represents.
#'
#' @param data numeric data
#'
#' @return
#'
#' @examples
#' x <- matrix(rnorm(200 * 3), ncol = 10)
#'principal_components(x)
#'
#' @export

principal_components <- function(data) {
  data <- as.matrix(data)
  N <- nrow(data)
  M <- ncol(data)
  R <- cor(data)
  tmp <- eigen(R)
  tmp2 <- sort(tmp$values, decreasing = TRUE, index.return = TRUE)
  eigval <- tmp2$x
  eigvec <- tmp$vectors[, order(tmp2$ix)]

  # loadings matrix
  pca_loadings <- matrix(0, M, M)
  for (i in 1:M) {
    pca_loadings[,i] <- sqrt(eigval[i]) %*% eigvec[, i]
    next
  }

  #component scores
  xbar <- colMeans(data)
  Xd <- data - matrix(1, N, 1) %*% t(xbar)
  v <- suppressWarnings(diag(M) * diag(1 / sqrt(var(data))))
  Xs <- Xd %*% v
  Y <- Xs %*% eigvec
  pca_scores <- Y

  output <- list(eigval = eigval,
                 eigvec = eigvec,
                 pca_loadings = pca_loadings,
                 pca_scores = pca_scores)

  return(output)

}

#' Easy Access to Principal Component Analysis Results
#'
#' \code{principal_components_result} Provides easy access to principal
#' component analysis results
#'
#' @param data list output from \code{principal_components}
#' @param results principal component analysis results to extract. Can use either
#'   results name or number (i.e. eigvec or 2):
#'     \enumerate{
#'       \item \code{eigval} (default)
#'       \item \code{eigvec}
#'       \item \code{pca_loadings}
#'       \item \code{pca_scores}
#'     }
#'
#'
#' @return Returns the one of the selected results:
#'     \enumerate{
#'       \item \code{eigval}: numeric vector of computed eigenvalues
#'       \item \code{eigvec}: numerical matrix of computed eigenvectors
#'       \item \code{pca_loadings}: numerical matrix of computed loadings
#'       \item \code{pca_scores}: numerical matrix of computed component (aka factor) scores
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
#'   principal_components_result(pca_scores)
#'
#' @export

principal_components_result <- function(data, results = eigval) {

  result_input <- deparse(substitute(results))
  result_options <- names(data)

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing argument: data argument", call. = FALSE)
  }
  if(missing(result_input)) {
    stop("Missing argument: results argument", call. = FALSE)
  }

  if(result_input %in% paste(1:4)) {
    data[[results]]
  } else if(result_input %in% result_options) {
    data[[result_input]]
  } else {
    stop("Invalid results argument: see ?principal_components_result for options", call. = FALSE)
  }
}
