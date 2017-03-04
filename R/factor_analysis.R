#' @title Factor Analysis with Varimax Rotation
#'
#' @description
#' \code{factor_analysis} reduces the structure of the data by relating the
#' correlation between variables to a set of factors, using the eigen-decomposition
#' of the correlation matrix.
#'
#' @param data numeric data
#' @param hc_points vector of eigenvalues [designed to use output from horns_curve() function]
#'
#' @return A list containing:
#'     \enumerate{
#'       \item \code{fa_loadings}: numerical matrix with the original factor loadings
#'       \item \code{fa_scores}: numerical matrix with the row scores for each factor
#'       \item \code{fa_loadings_rotated}: numerical matrix with the varimax rotated factor loadings
#'       \item \code{fa_scores_rotated}: numerical matrix with the row scores for each varimax rotated factor
#'       \item \code{num_factors}: numeric vector identifying the number of factors
#'     }
#'
#' @references
#'
#' H. F. Kaiser, "The Application of Electronic Computers to Factor Analysis,"
#' Educational and Psychological Measurement, 1960.
#'
#' @seealso
#'
#' \code{\link{horns_curve}} for computing the average eigenvalues used for \code{hc_points} argument
#'
#' @examples
#'
#' # Perform Factor Analysis with matrix \code{x}
#' x <- matrix(rnorm(200*3), ncol = 10)
#'
#' x %>%
#'   horns_curve() %>%
#'   factor_analysis(x, hc_points = .)
#'
#' @export

factor_analysis <- function(data, hc_points) {

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing argument: data argument", call. = FALSE)
  }
  if(missing(hc_points)) {
    stop("Missing argument: hc_points argument", call. = FALSE)
  }

  data <- as.matrix(data)
  N <- nrow(data)
  M <- ncol(data)
  R <- stats::cor(data)
  tmp <- eigen(R)
  tmp2 <- sort(tmp$values, decreasing = TRUE, index.return = TRUE)
  eigval <- tmp2$x
  eigvec <- tmp$vectors[, order(tmp2$ix)]

  # dimensionality assessment - finds the number of factors needed to account for
  # the variance in the data
  num_factors <- sum(eigval >= hc_points)

  xbar <- colMeans(data)
  Xd <- data - matrix(1, N, 1) %*% t(xbar)
  v <- suppressWarnings(diag(M) * diag(1 / sqrt(stats::var(data))))
  Xs <- Xd %*% v

  eigval2 <- eigval[1:num_factors]
  eigvec2 <- eigvec[,1:num_factors]
  lambda_mat <- matrix(0, M, num_factors)
  for (i in 1:num_factors) {
    lambda_mat[,i] <- sqrt(eigval2[i]) %*% eigvec2[,i]
    next
  }

  # generalized inverse is necessary to avoid matrix close to singularity
  fa_scores <- Xs %*% MASS::ginv(R) %*% lambda_mat

  rotation <- stats::varimax(lambda_mat)
  B <- lambda_mat %*% rotation$rotmat
  fa_scores_rotated <- Xs %*% MASS::ginv(R) %*% B

  output <- list(fa_loadings = lambda_mat,
                 fa_scores = fa_scores,
                 fa_loadings_rotated = B,
                 fa_scores_rotated = fa_scores_rotated,
                 num_factors = num_factors)

  return(output)

}


#' Easy Access to Factor Analysis Results
#'
#' \code{factor_analysis_result} Provides easy access to factor analysis results
#'
#' @param data list output from \code{factor_analysis}
#' @param results factor analysis results to extract. Can use either results
#'   name or number (i.e. fa_scores or 2)::
#'     \enumerate{
#'       \item \code{fa_loadings} (default)
#'       \item \code{fa_scores}
#'       \item \code{fa_loadings_rotated}
#'       \item \code{fa_scores_rotated}
#'       \item \code{num_factors}
#'     }
#'
#'
#' @return Returns the one of the selected results:
#'     \enumerate{
#'       \item \code{fa_loadings}: numerical matrix with the original factor loadings
#'       \item \code{fa_scores}: numerical matrix with the row scores for each factor
#'       \item \code{fa_loadings_rotated}: numerical matrix with the varimax rotated factor loadings
#'       \item \code{fa_scores_rotated}: numerical matrix with the row scores for each varimax rotated factor
#'       \item \code{num_factors}: numeric vector identifying the number of factors
#'     }
#'
#' @seealso
#'
#' \code{\link{factor_analysis}} for computing the factor analysis results
#'
#' @examples
#'
#' # An efficient means for getting factor analysis results
#' x <- matrix(rnorm(200*3), ncol = 10)
#' N <- nrow(x)
#' p <- ncol(x)
#'
#' x %>%
#'   horns_curve() %>%
#'   factor_analysis(x, hc_points = .) %>%
#'   factor_analysis_results(fa_scores_rotated)
#'
#' @export

factor_analysis_results <- function(data, results = 1) {

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
    stop("Invalid results argument: see ?factor_analysis_results for options", call. = FALSE)
  }
}




