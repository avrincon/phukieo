#' Highlight si/ei
#'
#' @param x A data frame of focal observations
#'
#' @export
#' @import stringr

highlight_si <- function(x) {
  # function to highlight beggining to end interaction of da and pa

  for(i in seq_len(nrow(x)-1)){
    # if row is blank, do nothing
    if(x$si_ei_tracker[[i]]  == ""){

      # if row contains "si_..." AND next row is blank,
      # then paste cell contents onto next row
    } else if(stringr::str_detect(x$si_ei_tracker[[i]], "si_") &
              x$si_ei_tracker[[i+1]] == ""){
      x$si_ei_tracker[[i+1]] <- x$si_ei_tracker[[i]]

      # if row contains "si_..." AND next row contains "ei_...",
      # then do nothing
    } else if(stringr::str_detect(x$si_ei_tracker[[i]], "si_") &
              stringr::str_detect(x$si_ei_tracker[[i+1]],  "ei_")){

      # if row contains "ei_...", then do nothing
    } else if(stringr::str_detect(x$si_ei_tracker[[i]], "ei_")){
    }
  }
  return(x)
}
