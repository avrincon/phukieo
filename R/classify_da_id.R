#' Classify da_pa_ids
#'
#'Checks if an aggression inside a da_pa_id was given or received at least once. Prioritises high intensity over low.
#'
#' @param x A data frame of focal observations
#' @param agg.low A character vector of low intensity aggression codes
#' @param agg.high A character vector of high intensity aggression codes
#' @param id A character vector of aggression partner IDs
#' @param tag A character string that denotes the class of aggression partner. will be added to the new column name. e.g. "mm" or "mf".
#'
#' @return A data frame with two new columns.
#' @export
#'
#' @import dplyr

classify_da_id <- function(x, agg.low, agg.high, id, tag = NULL) {

  # get MM agg but only within group
  x.da <-
    x %>%
    drop_na(da_pa_id) %>%
    rowid_to_column()

  x.da3 <- class_agg_action_intensity(x.da, agg.low, agg.high, id)
  x.da4 <- class_da_id(x.da3, tag)

  # delete temporary helper columns
  x.da5 <- x.da4 %>% select(-da_giv_temp, -da_rec_temp)

  # merge with main focal
  x2 <- x %>% left_join(x.da5)

  # delete helper rowid
  out <- x2 %>% select(-rowid)

  return(out)
}
