#' @title Find All Factors
#'
#' @description
#' \code{get_all_factors} finds all factor pairs for a given integer (i.e. a number
#' that divides evenly into another number).
#'
#' @param n number to be factored
#'
#' @return
#'
#' A list containing the integer vector(s) containing all factors for the given
#' \code{n} inputs.
#'
#' @source
#'
#' \url{http://stackoverflow.com/a/6425597/3851274}
#'
#' @examples
#'
#' # Find all the factors of 39304
#' get_all_factors(39304)
#'
#' @export

get_all_factors <- function(n) {

  # check input type
  if(!is.numeric(n)) {
    stop("Invalid n argument: see ?get_all_factors for details", call. = FALSE)
  }

  prime_factor_tables <- lapply(
    stats::setNames(n, n),
    function(i)
    {
      if(i == 1) return(data.frame(x = 1L, freq = 1L))
      plyr::count(as.integer(gmp::factorize(i)))
    }
  )
  lapply(
    prime_factor_tables,
    function(pft) {
      powers <- plyr::alply(pft, 1, function(row) row$x ^ seq.int(0L, row$freq))
      power_grid <- do.call(expand.grid, powers)
      sort(unique(apply(power_grid, 1, prod)))
    }
  )
}
