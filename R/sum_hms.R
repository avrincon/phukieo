#' Sum times in format "hh:mm:ss"
#'
#' @description
#' sums a character vector of times in format "hh:mm:ss"
#'
#' @param times a character vector of times
#'
#' @return returns a character vector of summed times
#' @export

sum_hms <- function(times) {
  times.sec <- sum(lubridate::period_to_seconds(lubridate::hms(times)))
  sec_to_hms(times.sec)
}
