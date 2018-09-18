#' Add tracker columns
#'
#'
#'
#' @param x A data frame of focal observations
#'
#' @return
#' Adds three columns: si_ei_tracker, bc_tracker, da_pa_tracker
#'
#' @export
#' @import dplyr

add_trackers <- function(x) {
  x <-
    x %>%
    mutate(
      si_ei_tracker =
        case_when(
          action == "si" ~ str_c("si", focal_animal, action_partner, sep = "_"),
          action == "ei" ~ str_c("ei", focal_animal, action_partner, sep = "_"),
          TRUE ~ ""),
      bc_tracker =
        case_when(
          action %in% c("sb", "bs") ~ str_c("sb", focal_animal, action_partner,
                                            sep = "_"),
          action %in% c("eb", "be") ~ str_c("eb", focal_animal, action_partner,
                                            sep = "_"),
          action == "tb" ~ str_c("tb", focal_animal, action_partner, sep = "_"),
          action == "sp" ~ str_c("sp", focal_animal, action_partner, sep = "_"),
          action == "ep" ~ str_c("ep", focal_animal, action_partner, sep = "_"),
          TRUE ~ ""),
      da_pa_tracker =
        case_when(
          action == "da" ~ str_c("da", focal_animal, action_partner, sep = "_"),
          action == "pa" ~ str_c("pa", focal_animal, action_partner, sep = "_"),
          TRUE ~ ""))
}
