#' @title Breakdown for Mahalanobis Distance
#'
#' @description
#' \code{bd_row} indicates which variables in data are driving the Mahalanobis
#' distance for a specific row \code{r}, relative to the mean vector of the data.
#'
#' @param data numeric data
#' @param row row of interest
#' @param n number of values to return.  By default, will return all variables
#' (columns) with their respective differences.  However, you can choose to view
#' only the top \code{n} variables by setting the \code{n} value.
#'
#' @return
#'
#' Returns a vector indicating the variables in \code{data} that are driving the
#' Mahalanobis distance for the respective row.
#'
#' @seealso
#'
#' \code{\link{mahalanobis_distance}} for computing the Mahalanobis Distance values
#'
#' @examples
#' \dontrun{
#' x = matrix(rnorm(200*3), ncol = 10)
#' colnames(x) = paste0("C", 1:ncol(x))
#'
#' # compute the relative differences for row 5 and return all variables
#' x %>%
#'   mahalanobis_distance("bd", normalize = TRUE) %>%
#'   bd_row(5)
#'
#' # compute the relative differences for row 5 and return the top 3 variables
#' # that are influencing the Mahalanobis Distance the most
#' x %>%
#'   mahalanobis_distance("bd", normalize = TRUE) %>%
#'   bd_row(5, 3)
#'   }
#'
#' @export

bd_row <- function(data, row, n = NULL) {

  # return error if parameters are missing or invalid
  if(missing(data)) {
    stop("Missing data argument", call. = FALSE)
  }
  if(! row %in% seq_len(nrow(data))) {
    stop("Invalid row value", call. = FALSE)
  }
  if(length(row) != 1) {
    stop("row value must be a single integer", call. = FALSE)
  }
  if(!isTRUE(n %in% seq_len(ncol(data))) && !is.null(n)) {
    stop("Invalid n value", call. = FALSE)
  }

  C <- stats::cov(data)
  CM <- as.matrix(colMeans(data))
  D <- (data[row,] - t(CM))
  bd <- D /sqrt(diag(C))
  bd_abs <- abs(bd)
  tmp <- sort(bd_abs, decreasing = TRUE, index.return = TRUE)
  bd_sort <- tmp$x
  bd_index <- tmp$ix

  output <- bd_sort
  names(output) <- colnames(data)[bd_index]

  if(!is.null(n)) {
     output <- output[seq_len(n)]
  }

  return(output)

}

