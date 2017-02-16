#' Security Log Data
#'
#' A mock dataset containing common information that appears in security logs.
#'
#' @format A data frame with 300 rows and 10 variables:
#' \describe{
#'   \item{Device_Vendor}{Company who made the device}
#'   \item{Device_Product}{Name of the security device}
#'   \item{Device_Action}{Outcome result of access}
#'   \item{Src_IP}{IP address of the source}
#'   \item{Dst_IP}{IP address of the destination}
#'   \item{Src_Port}{Port identifier of the source}
#'   \item{Dst_Port}{Port identifier of the destination}
#'   \item{Protocol}{Transport protocol used}
#'   \item{Country_Src}{Country of the source}
#'   \item{Bytes_TRF}{Number of bytes transferred}
#' }
"security_logs"
