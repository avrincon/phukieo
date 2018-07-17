#' function that creates an obstime matrix
#'
#' @param obs_time A data frame of total observation time
#' created by obs_time_calculator()
#' @param m_rows A character vector of IDs to go on row of matrix
#' @param m_cols A character vector of IDs to go on column of matrix
#'
#' @return A matrix with combined total observation time (in seconds)
#' for each dyad.
#'
#' @export
#'

obs_time_matrix <- function(obs_time, matrix_rows, matrix_cols){

  # create empty matrix and fill in observation time
  dur_matrix <- matrix(data = integer(),
                       nrow = length(matrix_rows),
                       ncol = length(matrix_cols),
                       byrow = TRUE,
                       dimnames = list(matrix_rows, matrix_cols))

  for(focal in matrix_rows) {
    for (participant in matrix_cols) {
      # add obs time for each dyad
      total <- dyad_obs_total(obs_time, focal, participant)
      # fill matrix with total obs time but only if focal and participant are not the same
      if (! identical(focal, participant)){
        dur_matrix[focal, participant] <- total
      }
    }
  }
  return(dur_matrix)
}
