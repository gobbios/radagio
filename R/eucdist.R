#' mean Euclidean distance of two vectors
#'
#' @param x vector 1
#' @param y vector 2
#'
#' @return numeric
#' @export
#'
#' @examples
#' X <- c(1, 2, 2)
#' Y <- c(1, 2, 3)
#' eucdist(X, Y)

eucdist <- function(x, y) {
  return(sqrt(sum((x - y)^2))/length(x))
}
