#' Mahalanobis Distance
#'
#' \code{mahalanobis_distance} calculates the distance between the elements in data and
#' the mean vector of the data for outlier detection. Values are independent of
#' the scale between variables.
#'
#' @param data numeric data
#'
#' @return A list containing:
#'     \enumerate{
#'       \item \code{md}: vector of Mahalanobis distances, one for each matrix row
#'       \item \code{md_sort}: sorted vector of Mahalanobis distances
#'       \item \code{md_index}: ordering index vector
#'       \item \code{bd}: matrix of the absolute values of the breakdown distances; used to see which columns drive the Mahalanobis distance
#'     }
#'
#' @references
#'
#' W. Wang and R. Battiti, "Identifying Intrusions in Computer Networks with
#' Principal Component Analysis," in First International Conference on Availability,
#' Reliability and Security, 2006.
#'
#' @examples
#'
#' x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))
#' mahalanobis_distance(x)
#'
#' @export

mahalanobis_distance <- function(data) {

  data <- as.matrix(data)
  N <- nrow(data)
  M <- ncol(data)
  md <- as.vector(rep(0,N),mode = "numeric")
  bd <- matrix(rep(0,N), nrow = N, ncol = M)
  C <- cov(data)
  IC <- MASS::ginv(C)
  CM <- as.matrix(colMeans(data))


  for (i in 1:N) {
    D <- (data[i,] - t(CM))
    md[i] <- D %*% IC %*% t(D)
    bd[i,] <- abs(D / sqrt(diag(C)))
  }

  colnames(bd) <- colnames(data)

  tmp <- sort(md, decreasing = TRUE, index.return = TRUE)
  md_sort <- tmp$x
  md_index <- tmp$ix

  output <- list(md = md,
                 md_sort = md_sort,
                 md_index = md_index,
                 bd = bd)

  return(output)

}


#' Easy Access to Mahalanobis Distance Results
#'
#' \code{mahalanobis_distance_result} Provides easy access to Mahalanobis Distance results
#'
#' @param data list output from \code{mahalanobis_distance}
#' @param results Mahalanobis Distance results to extract:
#'     \enumerate{
#'       \item \code{md}
#'       \item \code{md_sort}
#'       \item \code{md_index}
#'       \item \code{bd}
#'     }
#'
#'
#' @return Returns the one of the selected results:
#'     \enumerate{
#'       \item \code{md}: vector of Mahalanobis distances, one for each matrix row
#'       \item \code{md_sort}: sorted vector of Mahalanobis distances
#'       \item \code{md_index}: ordering index vector
#'       \item \code{bd}: matrix of the absolute values of the breakdown distances; used to see which columns drive the Mahalanobis distance
#'     }
#'
#' @seealso
#'
#' \code{\link{mahalanobis_distance}} for computing the Mahalanobis Distance results
#'
#' @examples
#'
#' # An efficient means for getting Mahalanobis Distance results
#' library(magrittr)
#' x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))
#' mahalanobis_distance(x) %>%
#'   mahalonbis_distance_results(bd)
#'
#' @export

mahalanobis_distance_results <- function(data, results) {

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing argument N argument", call. = FALSE)
  }
  if(missing(results)) {
    stop("Missing argument p argument", call. = FALSE)
  }

  data[[deparse(substitute(results))]]
}


#' Normalizing Mahalanobis Breakdown Distances
#'
#' \code{bd_normalizer} Provides efficient normalization of Mahalanobis Breakdown
#' Distances
#'
#' @param data breakdown distances from \code{mahalanobis_distance}
#'
#' @return Returns matrix of same size as \code{data} input
#'
#' @seealso
#'
#' \code{\link{mahalanobis_distance}} for computing the Mahalanobis Breakdown Distance results
#' \code{\link{mahalanobis_distance_results}} for retrieving the Mahalanobis Breakdown Distance results
#'
#' @examples
#'
#' # An efficient means for getting Mahalanobis Distance results
#' library(magrittr)
#' x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))
#' mahalanobis_distance(x) %>%
#'   mahalonbis_distance_results(bd) %>%
#'   bd_normalizer()
#'
#' @export

bd_normalizer <- function(data) {
  data %*% diag(1 / colSums(data))
}

