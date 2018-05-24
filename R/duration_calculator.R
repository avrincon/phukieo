#' Calculate the duration of a behaviour
#'
#' @description
#' Calculates the duration of behaviours in a focal protocol.
#'
#' @inheritParams ap_sb_gr_duration_calculator
#' @param partners OPTIONAL character vector of action partners
#'
#' @return adds two columns to d_focal. One with the total duration in seconds
#' (duration_s), and another with the total duration in hh:mm:ss (duration)
#'
#' @export
#' @import dplyr

duration_calculator <- function(d_focal, start_codes, end_codes,
                                focals, partners = NULL){
  # if dyad participant argument is missing
  if(is.null(partners)){
    # then want to calculate protocol duration or out of sight duration
    prot_oos_duration_calculator(d_focal, start_codes, end_codes, focals)

  } else {
    # else want to calculate ap/sb/gr duration
    ap_sb_gr_duration_calculator(d_focal, start_codes, end_codes,
                                 focals, partners)
  } # end else
} # end function

