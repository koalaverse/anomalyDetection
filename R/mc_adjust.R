#' @title Multi-Collinearity Adjustment
#'
#' @description
#' \code{mc_adjust} handles issues with multi-collinearity.
#'
#' @param data named numeric data object (either data frame or matrix)
#' @param min_var numeric value between 0-1 for the minimum acceptable variance (default = 0.1)
#' @param max_cor numeric value between 0-1 for the maximum acceptable correlation (default = 0.9)
#' @param action select action for handling columns causing multi-collinearity issues
#'    \enumerate{
#'       \item \code{exclude}: exclude all columns causing multi-collinearity issues (default)
#'       \item \code{select}: identify the columns causing multi-collinearity issues
#'         and allow the user to interactively select those columns to remove
#'     }
#'
#' @details
#'
#' \code{mc_adjust} handles issues with multi-collinearity by first removing
#'   any columns whose variance is close to or less than \code{min_var}. Then, it
#'   removes linearly dependent columns. Finally, it removes any columns that have
#'   a high absolute correlation value equal to or greater than \code{max_cor}.
#'
#' @return
#' \code{mc_adjust} returns the numeric data object supplied minus variables
#'   violating the minimum acceptable variance (\code{min_var}) and the
#'   maximum acceptable correlation (\code{max_cor}) levels.
#'
#'
#' @examples
#' \dontrun{
#' x <- matrix(runif(100), ncol = 10)
#' x %>%
#'   mc_adjust()
#'
#' x %>%
#'   mc_adjust(min_var = .15, max_cor = .75, action = "select")
#'}
#'
#' @export

mc_adjust <- function(data, min_var = 0.1, max_cor = 0.9, action = "exclude") {

  # add argument validation
  if(is.null(colnames(data))) {
    colnames(data) <- paste0("var", seq_len(ncol(data)))
  }

  if(!is.numeric(min_var) || !is.numeric(max_cor)) {
    stop("min_var and max_cor must be numeric inputs")
  }

  if(action != "exclude" && action != "select") {
    stop("The action argument must be either 'exclude' or 'select'")
  }

  # convert data to matrix
  data <- as.matrix(data)

  # remove any columns with minimal variance
  col2rmv <- matrixStats::colVars(data) < min_var
  if(any(col2rmv)) {

    # throw error if all columns are removed
    if(sum(col2rmv) == ncol(data)) {
       stop("All variables have been removed based on the min_var\n",
            "level. Consider adjusting minimum acceptable variance\n",
            "levels to allow for some variables to be retained.")
      }

    # deliver message if over 50% of the variables are being removed
    if(sum(col2rmv) > ncol(data)*.5) {
      message("Over 50% of the variables have been removed based\n",
              "on the min_var level.")
     }

    # subset data to remove minimal variance columns
    newdata <- data[ , ! col2rmv]
  } else {

    # no subsetting required if no columns meet the minimal variance threshold
    newdata <- data
  }

  # remove linearly dependent columns
  col2rmv <- caret::findLinearCombos(newdata)$remove
  if(!is.null(col2rmv)){
    newdata <- newdata[ , -col2rmv]
  }

  # identify columns with strong correlation
  C <- stats::cor(newdata)
  samp <- data.frame(which(apply(abs(C), MARGIN = 2, function(x) dplyr::between(x, max_cor, 1.0)), arr.ind = TRUE))
  mylist <- data.frame(num = seq_len(ncol(C)), name = colnames(newdata))
  temp <- qdapTools::lookup(samp, mylist)
  col2rmv <- colnames(newdata) %in% temp[duplicated(temp)]

  # remove strong correlation columns
  if(action == "exclude" && sum(col2rmv) > 0) {

    # throw error if all columns are removed
    if(sum(col2rmv) == ncol(newdata)) {
      stop("All variables have been removed based on the max_cor\n",
           "level. Consider adjusting maximum acceptable correlation\n",
           "levels to allow for some variables to be retained.")
    }

    # deliver message if over 50% of the variables are being removed
    if(sum(col2rmv) > ncol(newdata)*.5) {
      message("Over 50% of the variables have been removed based\n",
              "on the max_cor level.")
    }

    # subset data to remove minimal variance columns
    newdata <- newdata[ , ! col2rmv]
    tibble::as_tibble(newdata)
  } else if(action == "select" && sum(col2rmv) > 0) {
    # create data frame to report high correlated variables
    options_to_rm <- data.frame(v1 = rownames(C)[row(C)[upper.tri(C)]],
                                v2 = colnames(C)[col(C)[upper.tri(C)]],
                                v3 = round(C[upper.tri(C)], 3),
                                stringsAsFactors = FALSE)

    options_to_rm <- options_to_rm[abs(options_to_rm$v3) >= abs(max_cor), ]

      message("The following variable pairs exceed the max_cor input:\n")

      for(i in seq_len(nrow(options_to_rm))) {
        cat(options_to_rm[i, 1], " & ", options_to_rm[i, 2], " (r = ", options_to_rm[i, 3],")", sep = "")
        cat("\n")
      }

      cat("\n")

      # interactive response
      fun <- function() {
        answer <- readline("Which variables do you want to remove? ")

        answer <- strsplit(answer, " ")[[1]]
        while(!all(answer %in% colnames(newdata))){
          answer <- readline("The names you entered do not all match existing variable names. Please try again. ")
          answer <- strsplit(answer, " ")[[1]]
        }
        answer
      }

      col2rmv <- if(interactive()) fun()

      newdata <- newdata[, ! colnames(newdata) %in% col2rmv]
      tibble::as_tibble(newdata)

  } else {
    tibble::as_tibble(newdata)
  }

}
