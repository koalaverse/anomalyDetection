#' @title Block Inspection
#'
#' @description
#' \code{inspect_block} creates a list where the original data has been divided
#'   into blocks denoted in the state vector. Streamlines the process of inspecting
#'   specific blocks of interest.
#'
#' @param data data
#' @param block_length integer value to divide data
#'
#' @return A list where each item is a data frame that contains the original data
#'   for each block denoted in the state vector.
#'
#' @seealso
#'
#' \code{\link{tabulate_state_vector}} for creating the state vector matrix based
#'   on desired blocks.
#'
#' @examples
#'
#' inspect_block(security_logs, 30)
#'
#' @export
#'
inspect_block <- function(data, block_length){

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing argument: data argument", call. = FALSE)
  }
  if(missing(block_length)) {
    stop("Missing argument: block_length argument", call. = FALSE)
  }

  # return error if arguments are wrong type
  if(!is.data.frame(data) & !is.matrix(data)) {
    stop("data must be a data frame or matrix", call. = FALSE)
  }
  if(is.null(nrow(data)) | isTRUE(nrow(data) < block_length)) {
    stop("Your data input does not have sufficient number of rows", call. = FALSE)
  }
  if(!is.numeric(block_length)) {
    stop("block_length must be a numeric input", call. = FALSE)
  }

  num_rows <- nrow(data)
  num_blocks <- as.numeric(floor(num_rows / block_length))

  Blocks <- vector("list", num_blocks)

  i <- 1
  start <- 1
  for (i in 1:num_blocks) {
    stopp <-  block_length*i
    Blocks[[i]] <- data[start:stopp,]
    start <- stopp + 1
    next
  }

  return(Blocks)
}
