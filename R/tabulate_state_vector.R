#' @title Tabulate State Vector
#'
#' @description
#' \code{tabulate_state_vector} employs a tabulated vector approach to transform
#' security log data into unique counts of data attributes based on time blocks.
#' Taking a contingency table approach, this function separates variables of type
#' character or factor into their unique levels and counts the number of occurrences
#' for those levels within each block. Due to the large number of unique IP addresses,
#' this function allows for the user to determine how many IP addresses they would
#' like to investigate. The function tabulates the most popular IP addresses.
#'
#' @param data data
#' @param block_length integer value to divide data by
#' @param level_limit integer value to determine the cutoff for the number of
#'   factors in a column to display before being reduced to show the number of
#'   levels to keep (default is 50)
#' @param level_keep integer value indicating the top number of factor levels to
#'   retain if a column has more than the level limit (default is 10)
#' @param keep a logical which determines whether incomplete blocks are kept in
#'   the analysis in the case where the number of log entries isn't evenly
#'   divisible by the block_length
#' @param na.rm whether to keep missing values as part of the analysis or
#'   ignore them
#'
#' @return A data frame where each row represents one block and the columns count
#'   the number of occurrences that character/factor level occurred in that block
#'
#' @examples
#' tabulate_state_vector(security_logs, 30)
#'
#' @export

tabulate_state_vector <- function(data, block_length, level_limit = 50,
                                  level_keep = 10L, keep = FALSE,
                                  na.rm = FALSE) {

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
    stop("Your data input does not have sufficient number of rows",
         call. = FALSE)
  }
  if(!is.numeric(block_length)) {
    stop("block_length must be a numeric input", call. = FALSE)
  }
  if(!is.numeric(level_limit)) {
    stop("level_keep must be a numeric input", call. = FALSE)
  }
  if(!is.numeric(level_keep)) {
    stop("level_keep must be a numeric input", call. = FALSE)
  }

  # Calculate the number of leftover log entries
  left <- nrow(data) %% block_length

  # Calculate number of blocks necessary and throw a warning if
  # observations are removed
  if (keep == TRUE) {
    nblocks <- ceiling(nrow(data)/block_length)
    if (left != 0) {
      warning(paste("The last block contains only",as.character(left),
                    "observations"))
    }
  } else {
    nblocks <- floor(nrow(data)/block_length)
    data <- data[1:(nrow(data)-left),]
    if (left != 0) {
      warning(paste("You have removed the last",as.character(left),
                    "observations from the data set"))
    }
  }

  # Determine which variables have more categories than level_limit
  purrr::map(data,unique) %>%
    purrr::map_dbl(length) %>%
    subset(. > level_limit) %>%
    names() -> overflowvars

  # Only evaluate if there is an issue with overflow variables
  if (length(overflowvars) > 0) {

    # Throw warning indicating there is overflow
    message("Some variables contain more than ",as.character(level_limit),
            " levels. Only the ",as.character(level_keep)," most popular",
            " levels of these variables will be tabulated.")

    # Find and store 10 most popular levels of each overflow variable
    suppressMessages(
      data %>%
        dplyr::select_(~overflowvars) %>%
        tidyr::gather_("Variables", "Values",overflowvars) %>%
        stats::na.omit() %>%
        dplyr::filter_(.dots = ~ substr(Values,1,1) != "0") %>%
        dplyr::group_by_("Variables") %>%
        dplyr::count_("Values") %>%
        dplyr::top_n(level_keep) %>%
        dplyr::group_by_("Variables") %>%
        dplyr::mutate_(.dots = list(n = quote(1:n()))) %>%
        tidyr::spread_("Variables","Values") %>%
        dplyr::select_(~overflowvars) -> popvars
    )

    # replace unpopular categories with "0"
    for (i in overflowvars) {
      data[[i]][!is.na(data[[i]]) & !(data[[i]] %in% popvars[[i]])] <- "0"
    }
  }


  # construct state vector
  if (na.rm == TRUE) {

    # Remove NAs
    data %>%
      dplyr::mutate_(.dots = list(BLK = quote(as.numeric(floor((1:n()-1)/block_length)+1)))) %>%
      tidyr::gather_("Variables", "Values",names(.)[-length(names(.))]) %>%
      stats::na.omit() %>%
      dplyr::filter_(.dots = ~ Values != "0") %>%
      dplyr::mutate_(Values = ~ dplyr::if_else(grepl("[A-Za-z]", Values),
                                               Values,
                                               paste0(Variables,"_",Values))) %>%
      dplyr::select_(~ c(BLK,Values)) %>%
      dplyr::group_by_("BLK") %>%
      table() %>%
      tibble::as.tibble() %>%
      tidyr::spread_("Values","n") %>%
      dplyr::select_(~ names(.)[-1]) %>%
      purrr::map_df(as.numeric) %>%
      tibble::as.tibble() %>%
      return()

  } else {

    # Do not remove NAs
    data %>%
      dplyr::mutate_(.dots = list(BLK = quote(as.numeric(floor((1:n()-1)/block_length)+1)))) %>%
      tidyr::gather_("Variables", "Values",names(.)[-length(names(.))]) %>%
      dplyr::mutate_(Values = ~ dplyr::if_else(is.na(Values),
                                               paste0(Variables,"_NA"),
                                               Values)) %>%
      dplyr::filter_(.dots = ~ Values != "0") %>%
      dplyr::mutate_(Values = ~ dplyr::if_else(grepl("[A-Za-z]", Values),
                                               Values,
                                               paste0(Variables,"_",Values))) %>%
      dplyr::select_(~ c(BLK,Values)) %>%
      dplyr::group_by_("BLK") %>%
      table() %>%
      tibble::as.tibble() %>%
      tidyr::spread_("Values","n") %>%
      dplyr::select_(~ names(.)[-1]) %>%
      purrr::map_df(as.numeric) %>%
      tibble::as.tibble() %>%
      return()

  }

}
