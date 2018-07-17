#' Create a duration matrix
#'
#' @description Convert a data frame, usually created by duration_calculator(),
#' with durations for a behaviour to a matrix.
#'
#' @param action_totals A data frame with durations
#' @param m_rows A character vector of IDs to go in matrix rows
#' @param m_cols A character vector of IDs to go in matrix columns
#'
#' @return A matrix with total duratioin per dyads (in seconds)
#' @export
#'

dur_matrix <- function(action_totals, matrix_rows, matrix_cols) {
  # create empty matrix and fill in total proximity time
  d.matrix <- matrix(data = integer(),
                     nrow = length(matrix_rows),
                     ncol = length(matrix_cols),
                     byrow = TRUE,
                     dimnames = list(matrix_rows,matrix_cols))

  action_totals$total_duration <-
    lubridate::period_to_seconds(lubridate::hms(action_totals$total_duration))

  for(focal in matrix_rows) {
    for (participant in matrix_cols) {
      # look up dyad action duration total in action_totals df
      dyad_action_total <- action_totals %>%
        filter(dyad == paste(focal, participant, sep = "-")) %>%
        pull("total_duration")


      if (! identical(dyad_action_total, numeric(0))){
        #  if dyad did interact, then paste total action time in correct matrix cell
        d.matrix[focal, participant] <- dyad_action_total
      }
    }
  }
  return(d.matrix)
}
