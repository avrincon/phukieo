#' Subtract two times in format "hh:mm:ss"
#'
#' @param time1 time, will be subtracted by time2
#' @param time2 time, will subtract from time 1
#'
#' @return Returns a character vector in format "hh:mm:ss"
#' @export
#'

subtract_hms <- function(time1, time2) {
  #
  t1.sec <- lubridate::period_to_seconds(lubridate::hms(time1))
  t2.sec <- lubridate::period_to_seconds(lubridate::hms(time2))

  res <- t1.sec - t2.sec
  sec_to_hms(res)
}
