#' @title Tabulate State Vector
#'
#' @description
#' \code{tabulate_state_vector} employs a tabulated vector approach to transform
#' security log data into unique counts of data attributes based on time blocks.
#' Taking a contingency table approach, this function separates variables of type
#' character or factor into their unique levels and counts the number of occurrences
#' for those levels within each block. Due to the large number of unique IP address,
#' this function allows for the user to determine how many IP addresses they would
#' like to investigate (takes the top occurrences for IP variables).
#'
#' @param data data
#' @param block_length integer value to divide data by
#' @param level_limit integer value to determine the cutoff for the number of
#'   factors in a column to display before being reduced to show the number of
#'   levels to keep (default is 50)
#' @param level_keep integer value indicating the top number of factor levels to
#'   retain if a column has more than the level limit (default is 10)
#'
#' @return A data frame where each row represents one block and the columns count
#'   the number of occurrences that character/factor level occurred in that block
#'
#' @examples
#' tabulate_state_vector(security_logs, 30)
#'
#' @export

tabulate_state_vector <- function(data, block_length, level_limit = 50, level_keep = 10) {

  # return error if parameters are missing
  if(missing(data)) {
    stop("Missing argument: data argument", call. = FALSE)
  }
  if(missing(block_length)) {
    stop("Missing argument: block_length argument", call. = FALSE)
  }

  # return error if arguments are wrong type
  if(!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data frame or matrix", call. = FALSE)
  }
  if(is.null(nrow(data)) || isTRUE(nrow(data) < block_length)) {
    stop("Your data input does not have sufficient number of rows", call. = FALSE)
  }
  if(!is.numeric(block_length)) {
    stop("block_length must be a numeric input", call. = FALSE)
  }

  #this is how many levels you want to keep
  level_keep <- as.integer(level_keep)

  #find number of rows
  num_rows <- nrow(data)
  num_blocks <- as.numeric(floor(num_rows / block_length))

  # change character variables to factors
  change_fct <- function(x) {
    if(is.character(x)) {
      x <- factor(x, exclude = NULL)
    } else {
      x <- x
    }

    if(is.factor(x)){
      x <- addNA(x)
    }

    x
  }

  data <- purrr::map_df(data, change_fct)

  # the next block of code is to determine the block width to dimensionalize the State Vector
  ## find how many numeric variables there are
  numeric_vars <- as.numeric(sum(vapply(data, is.numeric, logical(1))))
  ## find which columns are less than or equal to the level limit
  list1 <- which(vapply(data, nlevels, integer(1)) <= level_limit, arr.ind = TRUE)
  ## find which columns are greater than the level limit
  list2 <- which(vapply(data, nlevels, integer(1)) > level_limit, arr.ind = TRUE)
  ## if no columns above level limit, set level_keep to 0
  if (length(list2) == 0) level_keep <- 0
  ## count the number of levels in all the columns found in list 1
  num_levels <- data[, list1] %>% vapply(nlevels, numeric(1)) %>% sum()
  ## determines the width of the state vector
  block_width <- num_levels + numeric_vars + length(list2) * level_keep
  State_Vector <- matrix(nrow = num_blocks, ncol = block_width)

  i <- 1
  start <- 1
  for (i in seq_len(num_blocks)){
    stopp <- block_length*i
    # create a temp variable to represent iterative block
    assign("temp",data[start:stopp,])
    # subset temp with only the columns that are of type factor
    temp_fac <- temp %>% dplyr::select_if(is.factor)
    # find which columns in temp_fac are less than or equal to the level limit
    list1 <- which(vapply(temp_fac, nlevels, integer(1)) <= level_limit, arr.ind = TRUE)
    # find which columns in temp_fac are greater than the level limit
    list2 <- which(vapply(temp_fac, nlevels, integer(1)) > level_limit, arr.ind = TRUE)
    # for all columns in list 1, create a summary list
    vec1 <- sapply(temp_fac[list1], summary)
    if (length(list2) != 0) {
      j <- 1
      vec3 <- vector("list", length = length(list2)*level_keep)
      for (j in seq_along(list2)){
        # finds the top most occurring factor levels for those that exceed the level limit
        temp1 <- utils::head(sapply(temp_fac[list2[j]],summary), n = level_keep)
        temp2 <- as.vector(temp1)
        vec3[[j]] <- temp2
      }
    }
    # subset the temp with only the columns that are of type numeric
    temp_num <- temp %>% dplyr::select_if(is.numeric)
    # sum the entire column
    vec2 <- vapply(temp_num, sum, numeric(1))
    vec0 <- c(unlist(vec1), unlist(vec2))
    if (exists("vec3")) vec0 <- c(vec0, unlist(vec3))
    State_Vector[i,] <- vec0
    start <- stopp + 1
  }

  # the following set of code is to build the name list for the level_keep variables
  list2names <- NULL
  if (length(list2) != 0) {
    k <- 1
    for (k in seq_along(list2)){
      # takes the original name of the column and adds a number to it
      list2names <- c(list2names, paste(names(temp_fac[list2[k]]), seq_len(level_keep), sep="_"))
    }
  }

  # create names
  namelist <- c(unlist(sapply(temp_fac[list1], levels)), names(temp_num), list2names)

  # turn NAs into NA1, NA2, etc.
  missing_names <- is.na(namelist)
  missing_n <- sum(missing_names)
  replace_missing <- paste0("NA", seq_len(missing_n))
  namelist[missing_names] <- replace_missing

  # add column names
  colnames(State_Vector) <- namelist

  return(tibble::as_tibble(State_Vector))

}
