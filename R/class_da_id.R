#' Classify da pa id
#'
#' This is a helper function for classify_da_id(). Requires the output of class_agg_action_intensity().
#' Checks if an aggression inside a da_pa_id was given or received at least once. Prioritises high intensity over low.
#'
#' @param x A data frame of focal observations
#' @param tag A character string that denotes the class of aggression partner. will be added to the new column name. e.g. "mm" or "mf".
#'
#' @return A data frame with two new columns.
#' @export
#'
#' @import dplyr

class_da_id <- function(x, tag = NULL) {
  # highlight for each da_pa_id whether focal had at least one high/low intesity agg

  if(is.null(tag)) {
    giv_name <- "da_giv"
    rec_name <- "da_rec"
  } else {
    giv_name <- paste0("da_giv_", tag)
    rec_name <- paste0("da_rec_", tag)
  }

  x <-
    x %>%
    group_by(da_pa_id) %>%
    mutate(
      !! giv_name :=
        case_when(any(da_giv_temp == "high") ~ "high",
                  any(da_giv_temp == "low") ~ "low",
                  TRUE ~ "none"),
      !! rec_name :=
        case_when(any(da_rec_temp == "high") ~ "high",
                  any(da_rec_temp == "low") ~ "low",
                  TRUE ~ "none")
    ) %>%
    ungroup()

}
