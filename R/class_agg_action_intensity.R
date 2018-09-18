#' Classify aggression intensity
#'
#' This is a helper function for classify_da_id(). Will classify individual aggressions as high or low intensity.
#'
#' @param x A data frame of focal observations
#' @param agg.low A character vector of low intensity aggression codes
#' @param agg.high A character vector of high intensity aggression codes
#' @param ids A character vector of aggression partner IDs
#'
#' @return A data frame with two new columns: for agg given and rec
#' @export
#'
#' @import dplyr

class_agg_action_intensity <- function(x, agg.low, agg.high, ids) {

  x <-
    x %>%
    mutate(
      da_giv_temp =
        case_when(actor == focal_animal & receiver %in% ids &
                    action %in% agg.low ~ "low",
                  actor == focal_animal & receiver %in% ids &
                    action %in% agg.high ~ "high",
                  TRUE ~ "none"),
      da_rec_temp =
        case_when(receiver == focal_animal & actor %in% ids &
                    action %in% agg.low ~ "low",
                  receiver == focal_animal & actor %in% ids &
                    action %in% agg.high ~ "high",
                  TRUE ~ "none")
    )
}
