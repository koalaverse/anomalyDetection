#' @title Mahalanobis Distance
#'
#' @description
#' \code{mahalanobis_distance} calculates the distance between the elements in data
#'   and the mean vector of the data for outlier detection. Values are independent
#'   of the scale between variables.
#'
#' @param data numeric data
#' @param output character vector stating the results to be returned. Can be "md"
#'   to return the Mahalanobis distances (default), "bd" to return the absolute breakdown
#'   distances (used to see which columns drive the Mahalanobis distance), or "both"
#'   to return both md and bd values.
#' @param normalize logical value of either \code{TRUE} or \code{FALSE}. If \code{TRUE}
#'   will normalize the breakdown distances within each variable so that breakdown distances
#'   across variables can be compared.
#'
#' @return Depending on the \code{output} parameter, the output will return either:
#'     \enumerate{
#'       \item \code{md}: vector of Mahalanobis distances, one for each matrix row
#'       \item \code{bd}: matrix of the absolute values of the breakdown distances; used to see which columns drive the Mahalanobis distance
#'       \item \code{both}: matrix containing both Mahalanobis and breakdown distances
#'     }
#'
#' @references
#'
#' W. Wang and R. Battiti, "Identifying Intrusions in Computer Networks with
#' Principal Component Analysis," in First International Conference on Availability,
#' Reliability and Security, 2006.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'
#' x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))
#'
#' # add Mahalanobis distance results to data frame
#' x %>%
#'   dplyr::mutate(MD = mahalanobis_distance(x))
#'
#' # add Mahalanobis distance and breakdown distance results to data frame
#' x %>%
#'   cbind(mahalanobis_distance(x, "both"))
#'
#' # add Mahalanobis distance and normalized breakdown distance results to data frame
#' x %>%
#'   cbind(mahalanobis_distance(x, "both", normalize = TRUE))
#' }
#'
#' @export

mahalanobis_distance <- function(data, output = "md", normalize = FALSE) {

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing data argument", call. = FALSE)
  }
  if(output != "md" && output != "bd" && output != "both") {
    stop("Invalid output argument; must be 'md', 'bd', or 'both'", call. = FALSE)
  }
  if(! normalize %in% c(TRUE, FALSE)) {
    stop("Invalid normalize argument; must be TRUE or FALSE", call. = FALSE)
  }

  data <- as.matrix(data)
  N <- nrow(data)
  M <- ncol(data)
  md <- rep(0,N)
  bd <- matrix(rep(0,N), nrow = N, ncol = M)
  C <- stats::cov(data)
  IC <- MASS::ginv(C)
  CM <- as.matrix(colMeans(data))


  for (i in seq_len(N)) {
    D <- (data[i,] - t(CM))
    md[i] <- D %*% IC %*% t(D)
    bd[i,] <- abs(D / sqrt(diag(C)))
  }

  if(normalize) {
    bd <- bd %*% diag(1 / colSums(data))
  }

  colnames(bd) <- paste(colnames(data), "BD", sep = "_")
  output_list <- list(md = md,
                 bd = bd)

  if(output == "md") {
    output <- output_list$md
  } else if(output == "bd") {
    output <- output_list$bd
  } else {
    output <- cbind(output_list$md, output_list$bd)
    colnames(output) <- c("MD", paste(colnames(data), "BD", sep = "_"))
  }

  return(output)

}

