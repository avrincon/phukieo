#' Calculate the duration of a behaviour without a partner.
#'
#' @description
#' Written specifically for focal protocol duration and out of sight time.
#'
#' @param d_focal frame of focal observations.
#' Currently needs all these columns with exact same names:
#' "date", "protocol_start_time", "focal_animal", "observer", "action_time",
#' "actor", "action","at_sec_since_mdn"
#' @inheritParams ap_sb_gr_duration_calculator
#'
#' @return adds two columns to d_focal. One with the total duration in seconds
#' (duration_s), and another with the total duration in hh:mm:ss (duration)
#'
#' @export
#' @import dplyr
#'
#' @examples
#'

prot_oos_duration_calculator <- function(d_focal,
                                         start_codes, end_codes,
                                         focals){
  # create empty list to add dyad duration dfs
  list.duration <- vector(mode = "list", length = length(focals)*2)
  # list index
  li <- 1L

  # fill in dataframe with start_action1-de durations
  for (d.f in focals) {
    dyad.info <-
      d_focal %>%
      filter(focal_animal == d.f &
               (actor == d.f) &
               (action %in% c(start_codes, end_codes))) %>%
      select("date", "protocol_start_time", "focal_animal", "observer",
             "action_time", "actor", "action","at_sec_since_mdn")

    if(nrow(dyad.info) > 0 & nrow(dyad.info) %% 2 != 0){
      # error message when dyad does not have equal number of start and end codes
      stop(
        paste("Number of 'start' codes is NOT equal to the number of 'end' codes.",
              "Focal =", d.f, ":" ,"Partner =", d.p))

    } else if (nrow(dyad.info) != 0) {
      # calculate action duration, format to hh:mm:ss
      dyad.info <-
        dyad.info %>%
        mutate(
          duration_s =
            case_when(
              action %in% start_codes ~ lead(at_sec_since_mdn) - at_sec_since_mdn,
              action %in% end_codes ~ at_sec_since_mdn - lag(at_sec_since_mdn)),
          duration = sec_to_hms(duration_s))

      # add dyad duration to list
      list.duration[[li]] <- dyad.info

      li <- li + 1L
    } # end if
  } # end for dyad_focal
  list.duration <- bind_rows(list.duration)
} # end function
