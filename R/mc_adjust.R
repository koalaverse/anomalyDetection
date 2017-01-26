#' Multi-Collinearity Adjustment
#'
#' \code{mc_adjust} handles issues with multi-collinearity.
#'
#' @param data data
#' @param blocks integer value to divide data
#' @param num_IPs integer value for the number of IP addresses to investigate
#' @param IP_names list of names for IP columns
#'
#' @details
#'
#' \code{mc_adjust} handles issues with multi-collinearity by first removing
#' any columns whose variance is close to \eqn{0 + Δ1}. Then, it removes linearly
#' dependent columns. Finally, it removes any columns that have a high absolute
#' correlation value between \eqn{1-Δ2} and \eqn{1}.
#'
#' @return
#'
#'
#'
#' @examples
#'
#'
#' @export
