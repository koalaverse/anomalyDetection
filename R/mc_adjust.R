#' @title Multi-Collinearity Adjustment
#'
#' @description
#' \code{mc_adjust} handles issues with multi-collinearity.
#'
#' @param data numeric data
#' @param delta1 numeric value between 0-1 for the column variance (default = 0.1)
#' @param delta2 numeric value between 0-1 for the correlation values (default = 0.9)
#'
#' @details
#'
#' \code{mc_adjust} handles issues with multi-collinearity by first removing
#' any columns whose variance is close to \eqn{0 + Δ1}. Then, it removes linearly
#' dependent columns. Finally, it removes any columns that have a high absolute
#' correlation value between \eqn{1 - Δ2} and \eqn{1}.
#'
#' @return
#'
#'
#'
#' @examples
#'
#'
#' @export

mc_adjust <- function(data, delta1 = 0.1, delta2 = 0.9){

  # remove any columns with minimal variance
  col2rmv <- which(matrixStats::colVars(data) < delta1)
  if(length(col2rmv) > 0) {
    newdata <- subset(data, select = -col2rmv)
  } else {
    newdata <- data
  }

  # remove linearly dependent columns
  col2rmv <- caret::findLinearCombos(newdata)$remove
  if(!is.null(col2rmv)){
    newdata <- subset(newdata, select = -col2rmv)
  }

  # identify columns with strong correlation
  C <- cor(newdata)
  samp <- data.frame(which(apply(abs(C), MARGIN = 2, function(x) dplyr::between(x, delta2, 1.0)), arr.ind = TRUE))
  mylist <- data.frame(matrix(nrow = nrow(C), ncol = 2))
  colnames(mylist) <- c("num", "name")
  mylist[,1] <- 1:ncol(C)
  mylist[,2] <- colnames(newdata)
  temp <- qdapTools::lookup(samp, mylist)
  col2rmv <- which(colnames(newdata) %in% temp[duplicated(temp)])

  # remove strong correlation columns
  if(length(col2rmv) > 0) newdata <- subset(newdata, select = -col2rmv)

  return(newdata)
}
