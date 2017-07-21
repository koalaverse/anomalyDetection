#' Horn's Parallel Analysis
#'
#' Computes the average eigenvalues produced by a Monte Carlo simulation that
#' randomly generates a large number of \code{n}x\code{p} matrices of standard
#' normal deviates.
#'
#' @param data A matrix or data frame.
#'
#' @param n Integer specifying the number of rows.
#'
#' @param p Integer specifying the number of columns.
#'
#' @param nsim Integer specifying the number of Monte Carlo simulations to run.
#' Default is \code{1000}.
#'
#' @return A vector of length \code{p} containing the averaged eigenvalues. The
#' values can then be plotted or compared to the true eigenvalues from a dataset
#' for a dimensionality reduction assessment.
#'
#' @references
#' J. L. Horn, "A rationale and test for the number of factors in factor
#' analysis," Psychometrika, vol. 30, no. 2, pp. 179-185, 1965.
#'
#' @rdname horns_curve
#'
#' @export
#'
#' @examples
#' # Perform Horn's Parallel analysis with matrix n x p dimensions
#' x <- matrix(rnorm(200 * 10), ncol = 10)
#' horns_curve(x)
#' horns_curve(n = 200, p = 10)
#' plot(horns_curve(x))  # scree plot
horns_curve <- function(data, n, p, nsim = 1000L) {
  if (!missing(data)) {
    if (!inherits(data, c("data.frame", "matrix"))) {
      stop("data must be a data frame or matrix")
    }
    compute_hc(n = nrow(data), p = ncol(data), nsim = nsim)[, , drop = TRUE]
  } else {
    if (missing(n)) {
      stop("missing n; please supply the number of rows")
    }
    if (missing(p)) {
      stop("missing p; please supply the number of columns")
    }
    if (!(n %% 2 %in% c(0, 1)) || n < 1) {
      stop("n should be a positive integer")
    }
    if (!(p %% 2 %in% c(0, 1)) || p < 1) {
      stop("p should be a positive integer")
    }
    compute_hc(n = n, p = p, nsim = nsim)[, , drop = TRUE]
  }
}

