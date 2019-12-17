#' transform matrix into sequence
#'
#' @param mat a square matrix
#'
#' @return a data.frame
#' @export
#' @examples
#' data(archie)
#' mat2seq(archie)


mat2seq <- function(mat) {
  winner <- c()
  loser <- c()
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (mat[i, j] > 0) {
        winner <- c(winner, rep(rownames(mat)[i], mat[i, j]))
        loser <- c(loser, rep(colnames(mat)[j], mat[i, j]))
      }
    }
  }
  x <- cbind(winner, loser)
  x <- x[sample(1:nrow(x)), ]
  Date <- seq(from = as.Date("2000-01-01"),
              to = as.Date("2000-01-01") + length(winner) - 1,
              by = "day")
  res <- data.frame(Date, winner = x[,1], loser = x[, 2])
  return(res)
}
