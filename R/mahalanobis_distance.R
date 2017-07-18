#' Mahalanobis Distance
#'
#' Calculates the distance between the elements in a data set and the mean
#' vector of the data for outlier detection. Values are independent of the scale
#' between variables.
#'
#' @param data A matrix or data frame. Data frames will be converted to matrices
#' via \code{data.matrix}.
#'
#' @param output Character string specifying which distance metric(s) to
#' compute. Current options include: \code{"md"} for Mahalanobis distance
#' (default); \code{"bd"} for absolute breakdown distance (used to see which
#' columns drive the Mahalanobis distance); and \code{"both"} to return both
#' distance metrics.
#'
#' @param normalize Logical indicating whether or not to normalize the breakdown
#' distances within each column (so that breakdown distances across columns can
#' be compared).
#'
#' @return If \code{output = "md"}, then a vector containing the Mahalanobis
#' distances is returned. Otherwise, a matrix.
#'
#' @references
#' W. Wang and R. Battiti, "Identifying Intrusions in Computer Networks with
#' Principal Component Analysis," in First International Conference on
#' Availability, Reliability and Security, 2006.
#'
#' @rdname mahalanobis_distance
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate some data
#' x <- data.frame(C1 = rnorm(100), C2 = rnorm(100), C3 = rnorm(100))
#'
#' # Add Mahalanobis distances
#' x %>% dplyr::mutate(MD = mahalanobis_distance(x))
#'
#' # Add Mahalanobis and breakdown distances
#' x %>% cbind(mahalanobis_distance(x, output = "both"))
#'
#' # Add Mahalanobis and normalized breakdown distances
#' x %>% cbind(mahalanobis_distance(x, output = "both", normalize = TRUE))
#' }
mahalanobis_distance <- function(data, output = c("md", "bd", "both"),
                                 normalize = FALSE) {
  UseMethod("mahalanobis_distance")
}


#' @rdname mahalanobis_distance
#' @export
mahalanobis_distance.matrix <- function(data, output = c("md", "bd", "both"),
                                        normalize = FALSE) {

  # Check params
  output <- match.arg(output)

  # Check column names
  if (is.null(colnames(data))) {
    colnames(data) <- seq_len(ncol(data))
  }

  # Compute distances
  if (output == "md") {  # mahalanobis distance only
    compute_md(data)[, , drop = TRUE]
  } else if (output == "bd") {  # absolute breakdown distance only
    out <- compute_bd(data, normalize = normalize)
    colnames(out) <- paste0(colnames(data), "_BD")
    out
  } else {  # both
    out <- compute_md_and_bd(data, normalize = normalize)
    colnames(out) <- c("MD", paste0(colnames(data), "_BD"))
    out
  }

}


#' @rdname mahalanobis_distance
#' @export
mahalanobis_distance.data.frame <- function(data,
                                            output = c("md", "bd", "both"),
                                            normalize = FALSE) {
  mahalanobis_distance.matrix(data.matrix(data), output = output,
                              normalize = normalize)
}
