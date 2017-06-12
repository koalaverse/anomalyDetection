#' @title Horn's Parallel Analysis
#'
#' @description
#' \code{horns_curve} computes the average eigenvalues produced by a Monte Carlo
#'   simulation that randomly generates a large number of matrices of size \code{n} x \code{p},
#'   where each element is drawn from a standard normal probability distribution. If a
#'   data matrix or data frame is supplied \code{n} and \code{p} will be extracted from
#'   the data dimensions.  Otherwise, \code{n} and \code{p} must be supplied.
#'
#' @param data numeric data
#' @param n integer value representing number of rows (default = NULL)
#' @param p integer value representing number of columns (default = NULL)
#'
#' @return A vector of length \code{p} with the computed average eigenvalues. The
#'   values can then be plotted or compared to the true eigenvalues of a dataset
#'   for a dimensionality assessment.
#'
#' @references
#'
#' J. L. Horn, "A rationale and test for the number of factors in factor analysis," Psychometrika, vol. 30, no. 2, pp. 179-185, 1965.
#'
#' @examples
#' # Perform Horn's Parallel analysis with matrix n x p dimensions
#' x <- matrix(rnorm(200*3), ncol = 10)
#'
#' # using data
#' horns_curve(x)
#'
#' # using n & p inputs
#' horns_curve(data = NULL, n = 25, p = 10)
#'
#' # Graph the scree line for a dimensionality assessment
#' horns_curve(x) %>%
#'   plot()
#'
#'@export

horns_curve <- function(data, n = NULL, p = NULL) {

  # throw error if data AND n or p is supplied
  if(!is.null(data) && !is.null(n)) {
    stop("If data is supplied then n and p arguments must be NULL")
  }
  if(!is.null(data) && !is.null(p)) {
    stop("If data is supplied then n and p arguments must be NULL")
  }

  # throw error if n or p are greater than length 1
  if(length(n) > 1 || length(p) > 1) {
    stop("Length of n and p must not exceed 1")
  }

  # throw error if data is NULL AND n or p is missing
  if(is.null(data) && missing(n)) {
    stop("Missing n argument")
  }
  if(is.null(data) && missing(p)) {
    stop("Missing p argument")
  }

  # assign n and p values based on argument inputs
  if(is.null(data)) {
    # return error if parameters are missing
    if(!is.numeric(n) || !is.numeric(p)) {
      stop("n and p must be numeric", call. = FALSE)
    }
    n <- n
    p <- p
  } else {
    n <- nrow(data)
    p <- ncol(data)
  }

  K <- 1000
  Eigvals_master <- matrix(0, K, p)

  for (i in seq_len(K)){
    M <- matrix(stats::rnorm(n * p), n, p)
    C <- stats::cov(M)
    Eigvals_C <- eigen(C)$values
    tmp <- sort(Eigvals_C, decreasing = TRUE)
    Eigvals_master[i,] <- tmp
  }
  curvepoints <- colMeans(Eigvals_master)
  return(curvepoints)
}
