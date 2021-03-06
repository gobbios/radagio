#' difference matrix
#'
#' difference matrix
#'
#' @param mat square interaction matrix with winner in rows and losers in columns
#'
#' @return data frame with inconsistencies and their strength
#'
#' @author Christof Neumann
#'
#' @details helper function for \code{\link{ISI}}
#'
#' @examples
#' data(bonobos, package = "EloRating")
#' radagio:::.diffmat(bonobos)
#'


.diffmat <- function(mat) {
  x <- ncol(mat)
  matrix(rep(0:x, x), ncol = x, byrow = TRUE)[1:x, ]
}

