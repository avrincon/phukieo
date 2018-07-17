#' Transpose matrix and sum content
#'
#' @description
#' Transposes matrix and adds cell contents of transposed and original
#'
#' @param m A numerical matrix. Must be symetrical.
#'
#' @return A matrix
#' @export
#'


transp_sum_matrix <- function(m) {
  # transposes matrix and adds cell contents of transposed and original

  # get transposed matrix
  t.m <- t(m)

  # sum frequency of both matrices and create matrix
  # This matrix contains the combined frequencies for each dyad
  m.sum <- matrix(mapply(sum, m, t.m,
                         MoreArgs = list(na.rm = T)),
                  ncol = ncol(m))

  # label row and column names
  rownames(m.sum) <- rownames(m)
  colnames(m.sum) <- colnames(m)
  # # alternative?
  # dimnames(m.sum) <- dimnames(m)
  # Set NA values across diagonal
  diag(m.sum) <- NA

  return(m.sum)
}
