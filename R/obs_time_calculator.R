#' Calculate observation time
#'
#' @description
#' function calculates total focal time and total out of sight time from focaltime
#' then subtracts oos time from focal time
#'
#' @param focaltime data frame with duration of focal observation per focal animal
#' @param oos data frame with duration of out of sight time per focal animal
#'
#' @return a data frame
#' @export
#' @import dplyr
#'

obs_time_calculator <- function(focaltime, oos){

  focal_totals <- sum_duration(focaltime, focal_animal)
  oos_totals <- sum_duration(oos, focal_animal)

  # change duration column name to be more accurate
  names(focal_totals)[2] <- "focal_time"
  names(oos_totals)[2] <- "out_of_sight_time"

  # merge focal_totals and oos_totals by column "focal_animal"
  obs_time <- left_join(focal_totals, oos_totals, by = "focal_animal")
  obs_time <- obs_time %>%
    mutate(total_obs_time = subtract_hms(focal_time, out_of_sight_time))
}
