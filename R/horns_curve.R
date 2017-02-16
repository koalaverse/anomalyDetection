#' @title Horn's Parallel Analysis
#'
#' @description
#' \code{horns_curve} computes the average eigenvalues produced by a Monte Carlo
#' simulation that randomly generates a large number of matrices of size \code{N} x \code{p},
#' where each element is drawn from a standard normal probability distribution.
#' The values can then be plotted or compared to the true eigenvalues of a dataset
#' for a dimensionality assessment.
#'
#' @param N integer value
#' @param p integer value
#'
#' @return A vector of length \code{p} with the computed average eigenvalues
#'
#' @references
#'
#' J. L. Horn, "A rationale and test for the number of factors in factor analysis," Psychometrika, vol. 30, no. 2, pp. 179-185, 1965.
#'
#' @examples
#'
#' \dontrun{
#' # Perform Horn's Parallel analysis with matrix \code{x} dimensions
#' x <- matrix(rnorm(200*3), ncol = 10)
#' N <- nrow(x)
#' p <- ncol(x)
#' horns_curve(N, p)
#'
#' # Graph the scree line for a dimensionality assessment
#' curvepoints <- horns_curve(N, p)
#' plot(curvepoints)
#' }
#'
#'@export

horns_curve <- function(N, p) {

  # return error if parameters are missing
  if(missing(N)) {
    stop("Missing argument N argument", call. = FALSE)
  }
  if(missing(p)) {
    stop("Missing argument p argument", call. = FALSE)
  }

  K <- 1000
  Eigvals_master <- matrix(0, K, p)

  for (i in 1:K){
    M <- matrix(stats::rnorm(N*p),N,p)
    C <- stats::cov(M)
    Eigvals_C <- eigen(C)$values
    tmp <- sort(Eigvals_C, decreasing = TRUE)
    Eigvals_master[i,] <- tmp
    next
  }
  curvepoints <- colMeans(Eigvals_master)
  return(curvepoints)
}
