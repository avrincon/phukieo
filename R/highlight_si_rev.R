#' Highlight da-pa in reverse
#'
#' function to highlight beggining to end interaction of da and pa
#' action_codes: if previous action is present in action_codes then it will keep
#' highlighting. Highlighter stops if code is not present.
#'
#' @param x A data frame of focal observations
#' @param action_codes A character vector of action codes
#'
#' @export
#'
highlight_si_rev <- function(x, action_codes) {
  # start at the end and go backwards, (delete las entry in vector)
  # because we only know where the end of the da is and not the start
  for(i in rev(seq_len(nrow(x)))[-nrow(x)]){

    # if action is a da/pa out side an si/ei tracker, then paste tracker id
    if(x$da_check[i] == "out" & x$action[i] %in% c("da", "pa")){
      x$si_ei_tracker[i] <-
        stringr::str_c("si", x$focal_animal[i], x$action_partner[i], sep = "_")
      x$da_check[i-1] <- "out"

      # if da check says out (pasted with if() above), then paste tracker id
    }else if(x$da_check[i] == "out" & x$action[i-1] %in% action_codes){
      x$si_ei_tracker[i] <- x$si_ei_tracker[i+1]
      x$da_check[i-1] <- "out"

    } else if(x$da_check[i] == "out" & (!x$action[i-1] %in% action_codes)){
      x$si_ei_tracker[i] <- x$si_ei_tracker[i+1]
    }
  }
  return(x)
}
