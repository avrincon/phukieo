#' Get obs time in secs
#'
#' @description Function to extract obstime in seconds of particular focal
#' @param obs_time data frame of obs time created by obs_time_calculator()
#' @param id character vector of animal IDs
#'
#' @return will return vector of single numeric value in seconds
#' @export
#' @import dplyr

get_obs_time_s <- function(obs_time, id) {
   o.t <- obs_time %>%
    filter(focal_animal == id) %>%
    pull("total_obs_time")

  o.t <- lubridate::period_to_seconds(lubridate::hms(o.t))
}
