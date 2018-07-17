#' Sum duration of grouped variables
#'
#' @description
#' returns the sum duration after grouping by variables
#'
#' @param df a data.frame
#' @param ... unquoted varialbel names to group_by
#'
#' @export
#' @import dplyr


sum_duration <- function(df, ...) {
  # Groups df by ... (unquoted variable names)
  # and returns summary of total duration
  group_var <- quos(...)

  s.df <- df %>%
    group_by(!!!group_var) %>%
    summarise(total_duration = sum_hms(duration))
}
