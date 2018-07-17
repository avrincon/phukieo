#' function to add obs time of focal and participant
#'
#' @param obs_time A data frame of observation time, 
#' created by obs_time_calculator()
#' @param fcl Observation time of focal animal in "hh:mm:ss"
#' @param prt Observation time of partner in "hh:mm:ss"
#'
#' @return The sum of fcl and prt in seconds
#' @export


dyad_obs_total <- function(obs_time, fcl, prt) {
  # function to add obs time of focal and participant
  
  # extract focal and participant total obs time from df
  fsub <- get_obs_time_s(obs_time, fcl)
  psub <- get_obs_time_s(obs_time, prt)
  # add to get total obstime for these two dyads
  total <- sum(fsub, psub)
}
