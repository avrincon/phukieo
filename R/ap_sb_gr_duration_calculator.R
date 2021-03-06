#' Calculate the duration of a behaviour with a partner.
#'
#' @description
#' Written specifically for approach, body contact and grooming.
#'
#' @param d_focal data frame of focal observations.
#' Currently needs all these columns with exact same names:
#' "date", "protocol_start_time", "focal_animal", "observer", "group", "action_time", "actor", "action", "receiver", "modifier_1", "modifier_2", "action_partner", "at_sec_since_mdn"
#' @param start_codes a character vector of action codes
#' that indicate when a behaviour starts.
#' @param end_codes a character vector of action codes
#' that indicate when a behaviour ends.
#' @param focals a character vector of focal animals
#' @param partners a character vector of partners which took part in the behaviour.
#'
#' @return adds two columns to d_focal. One with the total duration in seconds
#' (duration_s), and another with the total duration in hh:mm:ss (duration)
#'
#' @export
#' @import dplyr


ap_sb_gr_duration_calculator <- function(d_focal,
                                         start_codes, end_codes,
                                         focals, partners){
  # create empty list to add dyad duration dfs
  list.duration <- vector(mode = "list",
                          length = length(focals)*length(partners)*2)
  # list index
  li <- 1L

  # fill in dataframe with start_action1-de durations
  for (d.f in focals) {
    for(d.p in partners) {
      dyad.info <-
        d_focal %>%
        filter(focal_animal == d.f &
                 (actor %in% c(d.f, d.p)) &
                 (action %in% c(start_codes, end_codes)) &
                 (receiver %in% c(d.f, d.p))) %>%
        select("date", "protocol_start_time", "focal_animal", "observer",
              "group", "action_time", "actor", "action", "receiver",
               "modifier_1", "modifier_2", "action_partner", "at_sec_since_mdn")

      if(nrow(dyad.info) > 0 & nrow(dyad.info) %% 2 != 0){
        # error message when dyad does not have equal number of start and end codes
        stop("Number of 'start' codes is NOT equal to the number of 'end' codes.",
             "Focal =", d.f, ":" ,"Partner =", d.p)

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
    } # end for dyad_participant
  } # end for dyad_focal
  list.duration <- bind_rows(list.duration)
} # end function
