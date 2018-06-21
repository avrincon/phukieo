#' Check for missing start/end codes
#'
#' @param x A data frame of focal observations
#' @param start_codes A character vector of start codes that indicate a behaviour has started (e.g. c("ap", "at")).
#' @param end_codes A character vector of end codes that indicate a behaviour has finished (e.g. c("de", do)).
#'
#' @return Returns the focal data with an added "check" column. Look for "check next"
#' which indicates where there two consecutive start or end codes.
#' @export
#'
#' @examples
#' @import dplyr

check_missing_start_end <- function(x, start_codes = "sg", end_codes = "eg") {

  if(! any(x$action[[1]] %in% c(start_codes, end_codes))) {
    stop("Some actions are not a start or end code.
         Please check that the start and end codes provided are correct or
         that the focal data has been subseted to contain only start and
         end codes (and no other behaviours).")

  } else {

    out <-
      x %>%
      mutate(
        check =
          case_when(
            action %in% start_codes &
              lead(action) %in% start_codes ~ "check next",
            action %in% end_codes &
              lead(action) %in% end_codes ~ "check next",
            TRUE ~ "ok"))
  }
  return(out)
}
