#' Highlight si/ei of affiliative behaviours
#'
#' Currently needs these exact columns: "si_ei_tracker", "bc_tracker",
#' "at_sec_since_mdn", "focal_animal", "action_partner", "date"
#'
#' Tracker columns can be added with add_trackers()
#'
#' Checks if si/ei codes belongs to sb/sp/tb, and highlights if it does.
#'
#' @param x A data frame of focal observations
#' @param offset Number of seconds of difference to check for si/ei code.
#' Default = 1
#'
#' @export
#' @import dplyr
#'

# create subset of actions at 0 and +1 sec of si
# check if sb with same partner is present in this window

highlight_sibc <- function(x, offset = 1) {
  for(i in seq_len(nrow(x))){
    # if si tracker cell is NOT blank, get info for subsetting
    if(x$si_ei_tracker[[i]] != ""){
      at <- x$at_sec_since_mdn[[i]]
      focal <- x$focal_animal[[i]]
      partner <- x$action_partner[[i]]
      adate <- x$date[[i]]
      # if cell contains si, filter for actions at +1 sec
      if(str_detect(x$si_ei_tracker[[i]], "si_")){

        sub_si <- x %>% filter(at_sec_since_mdn %in% c(at, at + offset) &
                                  focal_animal == focal &
                                  date == adate)
        # if a sb is present in this subset, then the si is for a sb
        if(str_c("sb", focal, partner, sep = "_") %in% sub_si$bc_tracker){
          x$si_ei_tracker[[i]] <- str_c("si_sb", focal, partner, sep = "_")
        } else if(str_c("tb", focal, partner, sep = "_") %in% sub_si$bc_tracker){
          x$si_ei_tracker[[i]] <- str_c("si_tb", focal, partner, sep = "_")
        } else if(str_c("sp", focal, partner, sep = "_") %in% sub_si$bc_tracker){
          x$si_ei_tracker[[i]] <- str_c("si_sp", focal, partner, sep = "_")
        }

        # else if cell contains ei, filter for cells at  -1 sec
      } else if(str_detect(x$si_ei_tracker[[i]], "ei_")){

        sub_ei <- x %>% filter(at_sec_since_mdn %in% c(at, at - offset) &
                                  focal_animal == focal &
                                  date == adate)
        # if eb is present in this subset, then ei is for an eb
        if(str_c("eb", focal, partner, sep = "_") %in% sub_ei$bc_tracker){
          x$si_ei_tracker[[i]] <- str_c("ei_eb", focal, partner, sep = "_")
        } else if(str_c("tb", focal, partner, sep = "_") %in% sub_ei$bc_tracker){
          x$si_ei_tracker[[i]] <- str_c("ei_tb", focal, partner, sep = "_")
        } else if(str_c("ep", focal, partner, sep = "_") %in% sub_ei$bc_tracker){
          x$si_ei_tracker[[i]] <- str_c("ei_ep", focal, partner, sep = "_")
        }
      }
    }
  }
  return(x)
}
