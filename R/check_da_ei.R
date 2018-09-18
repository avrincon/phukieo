#' Check for da's without an ei
#'
#' Currently needs these exact columns: "da_check", at_sec_since_mdn", "focal_animal", "action_partner", "date"
#'
#' da_check needs to only be an empty character column.
#'
#' @param x A data of focal observations
#' @param offset Number of seconds of difference to check for si/ei code.
#' Default = 1
#' @export
#'
#' @import dplyr
check_da_ei <- function(x, offset = 1) {
  x$da_check <- ""

  for(i in seq_len(nrow(x))){
    if(x$action[i] %in% c("da", "pa")) {

      at <- x$at_sec_since_mdn[[i]]
      focal <- x$focal_animal[[i]]
      partner <- x$action_partner[[i]]
      adate <- x$date[[i]]

      sub_da <-
        x %>%
        filter(at_sec_since_mdn %in% c(at, at + offset) &
                 focal_animal == focal &
                 date == adate)

      if(! "ei" %in% sub_da$action){
        x$da_check[i] <- "check"
      }
    }
  }
  return(x)
}
