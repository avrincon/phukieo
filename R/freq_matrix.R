#' Create a frequency matrix
#'
#' @description Create a matrix with the frequency that a certain behaviour occurs
#' for a particular dyad (e.g. proximity, body contact or grooming bouts)
#'
#'
#' @param action_df A data frame with all occurences of a single behaviour.
#' Usually created by duration_calculator().
#' @param m_rows A character vector of IDs to go in matrix rows
#' @param m_cols A character vector of IDs to go in matrix columns
#'
#' @return A matrix with frequencies per dyad
#' @export
#'

freq_matrix <- function(action_df, matrix_rows, matrix_cols) {
  # create empty matrix and fill in count/frequency of proximity bouts
  count_matrix <- matrix(data = integer(),
                         nrow = length(matrix_rows),
                         ncol = length(matrix_cols),
                         byrow = TRUE,
                         dimnames = list(matrix_rows, matrix_cols))

  for(focal in matrix_rows) {
    for (participant in matrix_cols) {
      # sum sumber of times that focal and participant are in the same row
      dyad_count_total <- sum(action_df$focal_animal == focal &
                                action_df$action_partner == participant)
      # fill matrix with total number of times that a focal and participant are in proximity
      if (! identical(focal, participant)){
        count_matrix[focal, participant] <- dyad_count_total
      }
    }
  }
  return(count_matrix)
}
