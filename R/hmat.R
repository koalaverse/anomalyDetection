#' @title Plot a Histogram Matrix
#'
#' @description Display a histogram matrix for visual inspection of anomalous
#' observation detection. The color of the blocks represents how anomalous each
#' block is, where a lighter blue represents a more anomalous block. The size
#' of the points indicate which values are driving the anomaly, with larger
#' blocks representing more anomalous values.
#'
#' @param data the data set (data frame or matrix)
#' @param input the type of input data being passed to the function. \code{data} for
#' a raw categorical data set, \code{SV} for a state vector input, and \code{MD} if the
#' input has already had the Mahalanobis distances calculated
#' @param top how many of the most anomalous blocks you would like to display
#' (default 20)
#' @param order whether to show the anomalous blocks in numeric order or in order of
#' most anomalous to least anomalous (default is "numeric", other choice is "anomaly")
#' @param block_length argument fed into \code{tabulate_state_vector}, necessary if
#' \code{input = data}
#' @param level_limit argument fed into \code{tabulate_state_vector}, if the
#' number of unique categories for a variable exceeds this number, only keep
#' a limited number of the most popular values (default 50)
#' @param level_keep argument fed into \code{tabulate_state_vector}, if \code{level_limit}
#' is exceeded, keep this many of the most popular values (default 10)
#' @param partial_block argument fed into \code{tabulate_state_vector}, if the number of
#' entries is not divisible by the \code{block_length}, this logical decides
#' whether to keep the smaller last block (default \code{TRUE})
#' @param na.rm whether to keep track of missing values as part of the analysis or
#' ignore them (default \code{FALSE})
#' @param min_var argument fed into \code{mc_adjust}, if a column in the state
#' vector has variance less than this value, remove it (default 0.1)
#' @param max_cor argument fed into \code{mc_adjust}, if a column in the state
#' vector has correlation greater than this value, remove it (default 0.9)
#' @param action argument fed into \code{mc_adjust}, if a column does not fall in
#' the specified range, determine what to do with it (default "exclude")
#' @param output argument fed into \code{mahalanobis_distance} that decides
#' whether to add a column for the Mahalanobis Distance ('MD'), the breakdown
#' distances ('BD') or both (default "both")
#' @param normalize argument fed into \code{mahalanobis_distance} that decides
#' whether to normalize the values by column (default = FALSE)
#'
#' @examples
#' \dontrun{
#' # Data set input
#' hmat(security_logs,block_length = 8)
#'
#' # Data Set input with top 10 blocks displayed
#' hmat(security_logs, top = 10, block_length = 5)
#'
#' # State Vector Input
#' tabulate_state_vector(security_logs, block_length = 6, level_limit = 20) %>%
#'   hmat(input = "SV")
#' }
#'
#' @export

hmat <- function(data, input = "data", top = 20, order = "numeric", block_length = NULL,
                 level_limit = 50, level_keep = 10, partial_block = TRUE, na.rm = FALSE,
                 min_var = 0.1, max_cor = 0.9, action = "exclude",
                 output = "both", normalize = FALSE) {

  # if the input is a vector or NULL, throw a warning
  if (is.vector(data)) {
    stop("data must be a matrix or data frame")
  }
  if (is.null(data)) {
    stop("data is NULL")
  }

  # if the order input is not "numeric" or "anomaly", stop
  if (order != "numeric" & order != "anomaly") {
    stop("Invalid Input: argument order must be either 'numeric' or 'anomaly'")
  }


  # if the input is a raw data set, not a state vector
  if (input == "data") {

    if(is.null(block_length)) {
      stop("Please supply a block_length argument")
    }

    suppressWarnings(
      suppressMessages(
        temp <- dplyr::mutate_(
            tibble::as.tibble(
              anomalyDetection::mahalanobis_distance(
                anomalyDetection::mc_adjust(
                  anomalyDetection::tabulate_state_vector(data,block_length,
                                                          level_limit,
                                                          level_keep,
                                                          partial_block,
                                                          na.rm)
                ,min_var, max_cor, action)
              ,output, normalize)
            )
          ,.dots = list(Block = quote(as.factor(1:n()))))
      )
    )
    temp$Ranked <- rank(dplyr::desc(temp$MD), ties.method = "random")
    temp <- dplyr::filter_(
      tidyr::gather_(temp,"Variable","BD",names(temp)[-c(1,length(names(temp))-1,length(names(temp)))])
      ,.dots = ~ Ranked <= top)
    temp$Variable <- substr(temp$Variable,1,nchar(temp$Variable)-3)
    if (order == "anomaly") {
      temp$Block <- stats::reorder(temp$Block, temp$Ranked)
    }
    return(
      ggplot2::ggplot(temp,
        ggplot2::aes_string(x = "Block", y = "Variable",
                             color = "MD", size = "BD")) +
        ggplot2::geom_point()
    )


  } else if (input == "SV") {

    suppressWarnings(
      suppressMessages(
        temp <- dplyr::mutate_(
          tibble::as.tibble(
            anomalyDetection::mahalanobis_distance(
              anomalyDetection::mc_adjust(data,min_var, max_cor, action)
              ,output, normalize)
          )
          ,.dots = list(Block = quote(as.factor(1:n()))))
      )
    )
    temp$Ranked <- rank(dplyr::desc(temp$MD), ties.method = "random")
    temp <- dplyr::filter_(
      tidyr::gather_(temp,"Variable","BD",names(temp)[-c(1,length(names(temp))-1,length(names(temp)))])
      ,.dots = ~ Ranked <= top)
    temp$Variable <- substr(temp$Variable,1,nchar(temp$Variable)-3)
    if (order == "anomaly") {
      temp$Block <- stats::reorder(temp$Block, temp$Ranked)
    }
    return(
      ggplot2::ggplot(temp,
                      ggplot2::aes_string(x = "Block", y = "Variable",
                                          color = "MD", size = "BD")) +
        ggplot2::geom_point()
    )

  } else if (input == "MD") {

    suppressWarnings(
      suppressMessages(
        temp <- dplyr::mutate_(
          tibble::as.tibble(data)
          ,.dots = list(Block = quote(as.factor(1:n()))))
      )
    )
    temp$Ranked <- rank(dplyr::desc(temp$MD), ties.method = "random")
    temp <- dplyr::filter_(
      tidyr::gather_(temp,"Variable","BD",names(temp)[-c(1,length(names(temp))-1,length(names(temp)))])
      ,.dots = ~ Ranked <= top)
    temp$Variable <- substr(temp$Variable,1,nchar(temp$Variable)-3)
    if (order == "anomaly") {
      temp$Block <- stats::reorder(temp$Block, temp$Ranked)
    }
    return(
      ggplot2::ggplot(temp,
                      ggplot2::aes_string(x = "Block", y = "Variable",
                                          color = "MD", size = "BD")) +
        ggplot2::geom_point()
    )

  } else {

    stop("Invalid Input: input should be set as 'data','SV', or 'MD'.")

  }
}
