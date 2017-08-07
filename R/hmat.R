#' @title Plot a Histogram Matrix
#'
#' @description Display a histogram matrix for visual inspection of anomalous
#' observation detection. The color of the blocks represents how anomalous each
#' block is, where a lighter blue represents a more anomalous block. The size
#' of the points indicate which values are driving the anomaly, with larger
#' blocks representing more anomalous values.
#'
#' @param data the data set (data frame or matrix)
#' @param input the type of input data being passed to the function. `data` for
#' a raw categorical data set, `SV` for a state vector input, and `MD` if the
#' input has already had the Mahalanobis distances calculated
#' @param top how many of the most anomalous blocks you would like to display,
#' with default 20
#' @param block_length argument fed into `tabulate_state_vector`, necessary if
#' `input = data`
#' @param level_limit argument fed into `tabulate_state_vector`, if the
#' number of unique categories for a variable exceeds this number, only keep
#' a limited number of the most popular values (default 50)
#' @param level_keep argument fed into `tabulate_state_vector`, if `level_limit`
#' is exceeded, keep this many of the most popular values (default 10)
#' @param keep argument fed into `tabulate_state_vector`, if the number of
#' entries is not divisible by the `block_length`, this logical decides
#' whether to keep the smaller last block (default TRUE)
#' @param min_var argument fed into `mc_adjust`, if a column in the state
#' vector has variance less than this value, remove it (default 0.1)
#' @param max_cor argument fed into `mc_adjust`, if a column in the state
#' vector has correlation greater than this value, remove it (default 0.9)
#' @param action argument fed into `mc_adjust`, if a column does not fall in
#' the specified range, determine what to do with it (default "exclude")
#' @param output argument fed into `mahalanobis_distance` that decides
#' whether to add a column for the Mahalanobis Distance ('MD'), the breakdown
#' distances ('BD') or both (default "both")
#' @param normalize argument fed into `mahalanobis_distance` that decides
#' whether to normalize the values by column (default = FALSE)
#'
#' @examples
#' # Data set input
#' security_logs %>%
#'   hmat(block_length = 8)
#'
#' # Data Set input with top 10 blocks displayed
#' security_logs %>%
#'   hmat(top = 10, block_length = 5)
#'
#' # State Vector Input
#' security_logs %>%
#'   tabulate_state_vector(block_length = 6, level_limit = 20) %>%
#'   hmat(input = "SV")
#'
#' # Mahalanobis Distance input
#' security_logs %>%
#'   tabulate_state_vector(block_length = 4) %>%
#'   mc_adjust() %>%
#'   mahalanobis_distance("both", normalize = TRUE) %>%
#'   hmat(input = "MD")
#'
#' # ggplot2 additional commands included
#' security_logs %>%
#'   hmat(block_length = 8) +
#'   ggplot2::ggtitle("Histogram Matrix of security_logs") +
#'   ggplot2::xlab("Top 20 Blocks") +
#'   ggplot2::ylab(NULL)
#'
#' @export

hmat <- function(data, input = "data", top = 20, block_length = NULL,
                 level_limit = 50, level_keep = 10, keep = TRUE, min_var = 0.1,
                 max_cor = 0.9, action = "exclude", output = "both",
                 normalize = FALSE) {

  # if the input is a vector or NULL, throw a warning
  if (is.vector(data)) {
    stop("data must be a matrix or data frame")
  }
  if (is.null(data)) {
    stop("data is NULL")
  }

  # if the input is a raw data set, not a state vector
  if (input == "data") {

    if(is.null(block_length)) {
      stop("Please supply a block_length argument")
    }

    suppressWarnings(
      data %>%
        anomalyDetection::tabulate_state_vector(block_length,
                                                level_limit,
                                                level_keep,
                                                keep) %>%
        anomalyDetection::mc_adjust(min_var, max_cor, action) %>%
        anomalyDetection::mahalanobis_distance(output, normalize) %>%
        tibble::as_tibble() %>%
        dplyr::mutate_(.dots = list(Block = quote(as.factor(1:n())))) %>%
        dplyr::mutate_(Ranked = ~ rank(-MD, ties = "random")) %>%
        tidyr::gather_("Variable","BD",
                       names(.)[-c(1,length(names(.))-1,length(names(.)))]) %>%
        dplyr::filter_(.dots = ~ Ranked <= top) %>%
        dplyr::mutate_(Variable = ~ substr(Variable,1,nchar(Variable)-3)) %>%
        ggplot2::ggplot(ggplot2::aes_string(x = "Block", y = "Variable",
                                            color = "MD", size = "BD")) +
        ggplot2::geom_point() %>%
        return()
    )

  } else if (input == "SV") {

    data %>%
      anomalyDetection::mc_adjust(min_var, max_cor, action) %>%
      anomalyDetection::mahalanobis_distance(output, normalize) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_(.dots = list(Block = quote(as.factor(1:n())))) %>%
      dplyr::mutate_(Ranked = ~ rank(-MD, ties = "random")) %>%
      tidyr::gather_("Variable","BD",
                     names(.)[-c(1,length(names(.))-1,length(names(.)))]) %>%
      dplyr::filter_(.dots = ~ Ranked <= top) %>%
      dplyr::mutate_(Variable = ~ substr(Variable,1,nchar(Variable)-3)) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "Block", y = "Variable",
                                          color = "MD", size = "BD")) +
      ggplot2::geom_point() %>%
      return()

  } else if (input == "MD") {

    data %>%
      tibble::as_tibble() %>%
      dplyr::mutate_(.dots = list(Block = quote(as.factor(1:n())))) %>%
      dplyr::mutate_(Ranked = ~ rank(-MD, ties = "random")) %>%
      tidyr::gather_("Variable","BD",
                     names(.)[-c(1,length(names(.))-1,length(names(.)))]) %>%
      dplyr::filter_(.dots = ~ Ranked <= top) %>%
      dplyr::mutate_(Variable = ~ substr(Variable,1,nchar(Variable)-3)) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = "Block", y = "Variable",
                                          color = "MD", size = "BD")) +
      ggplot2::geom_point() %>%
      return()

  } else {

    stop("Invalid Input: input should be set as 'data','SV', or 'MD'.")

  }
}
