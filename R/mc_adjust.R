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

  if(!is.numeric(min_var) || !is.numeric(max_cor)) {
    stop("min_var and max_cor must be numeric inputs")
  }

  if(action != "exclude" && action != "select") {
    stop("The action argument must be either 'exclude' or 'select'")
  }

  # add argument validation
  if(is.null(colnames(data))) {
    colnames(data) <- paste0("var", seq_len(ncol(data)))
  }

  # treat data as a data frame
  data <- as.data.frame(data)

  # Remove columns with minimal variance
  low_var <- names(data)[diag(stats::var(data)) > min_var]
  # throw error if all columns are removed
  if(length(low_var) == 0) {
    stop("All variables have been removed based on the min_var\n",
         "level. Consider adjusting minimum acceptable variance\n",
         "levels to allow for some variables to be retained.")
  } else {
    if (action == "exclude") {data <- data[,low_var, drop = FALSE]}
  }

  # Remove linearly dependent columns (but only if there are at least 2 columns)
  if (ncol(data) > 1) {
    cor_mat <- stats::cor(data)
    cor_mat[lower.tri(cor_mat, diag = TRUE)] <- 0
    high_cor <- names(data[,sapply(as.data.frame(cor_mat),function(x) max(abs(x)) < 0.9)])
    if (action == "exclude") {data <- data[,high_cor, drop = FALSE]; return(tibble::as.tibble(data))}

    if (action == "select" & ncol(data) != length(intersect(low_var, high_cor))) {
      col2rmv <- setdiff(names(data),intersect(low_var, high_cor))
      message("The following variables are set to be removed:")
      message(paste(col2rmv,"\n"))
      keep <- unlist(strsplit(readline("Which of these variables would you like to keep? "), split = " "))
      if (all(keep %in% col2rmv)) {
        data <- data[,union(intersect(low_var, high_cor),keep), drop = FALSE]
        return(tibble::as.tibble(data))
      } else if (keep == ""){
        data <- data[,intersect(low_var, high_cor), drop = FALSE]
      } else {
        stop("One or more of the variables entered is not an option.")
      }
    }
  } else {
    return(tibble::as.tibble(data))
  }

}
