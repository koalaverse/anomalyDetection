#' Tabulate State Vector
#'
#' \code{tabulate_state_vector} divides data into blocks, where each block is
#' defined by descriptive statistics. Taking a contingency table approach, this
#' function separates variables of type factor into their unique factor levels
#' and counts the number of occurrences for those levels within each block. Due
#' to the large number of unique IP address, this function allows for the user
#' to determine how many IP addresses they would like to investigate (takes the
#' top occurrences for IP variables).
#'
#' @param data data
#' @param blocks integer value to divide data
#' @param num_IPs integer value for the number of IP addresses to investigate
#' @param IP_names list of names for IP columns
#'
#' @return
#'
#'
#'
#' @examples
#'
#'
#' @export

tabulate_state_vector <- function(data, blocks, num_IPs, IP_names = NULL) {

  num_rows <- nrow(data)
  num_blocks <- as.numeric(floor(num_rows / blocks))
  numeric_vars <- as.numeric(sum(sapply(data, is.numeric) == TRUE))
  mylist <- which(sapply(data, nlevels) < 100, arr.ind = TRUE)
  num_levels <- as.numeric(sum(sapply(data[mylist], nlevels)))
  block_width <- num_levels + numeric_vars + num_IPs*2
  State_Vector <- matrix(nrow = num_blocks, ncol = block_width)

  start <- 1

  for (i in 1:num_blocks){

    stopp <- blocks*i
    assign("temp", data[start:stopp,])
    temp_fac <- dplyr::select_if(temp, is.factor)
    mylist <- which(sapply(temp_fac, nlevels) < 100, arr.ind = TRUE)
    vec1 <- sapply(temp_fac[mylist], summary)

    if (IP_names[1] %in% names(temp) == TRUE) {
      vec3 = head(summary(temp[, IP_names[1]]), n = num_IPs)
    }

    if (IP_names[2] %in% names(temp) == TRUE) {
      vec3 = c(vec3, head(summary(temp[, IP_names[2]]), n = num_IPs))
    }

    temp_num <- dplyr::select_if(temp, is.numeric)
    vec2 <- sapply(temp_num,sum)
    vec0 <- c(unlist(vec1), unlist(vec2))

    if (exists("vec3")) {
      vec0 <- c(vec0, vec3)
    }

    State_Vector[i,] <- vec0
    start <- stopp + 1
    next
  }

  namelist <- c(unlist(sapply(temp_fac[mylist], levels)),
                names(temp_num),
                paste("S", 1:num_IPs, sep=""),
                paste("D", 1:num_IPs, sep=""))

  colnames(State_Vector) <- namelist
  return(State_Vector)

}
