#' ranking
#'
#' return ranks that treat tied values either as average or that use the minimum rank and shrink ranks so that the gap between adjacent ranks is always 1
#' @param x numeric, the dominance index
#' @param tiestreat either \code{"average"} or \code{"minshrink"}
#'
#' @return a vector with ranks (the same length as \code{x})
#' @export
#'
#' @examples
#' x <- c(1, 2, 2, 7, 8)
#' # maximum rank possible is 5
#' ranks(x, tiestreat = "avg")
#' ranks(x, tiestreat = "minshrink")
#' x <- c(7, 2, 2, 3, 10, 10)
#' # maximum rank possible is 6
#' ranks(x, tiestreat = "avg")
#' ranks(x, tiestreat = "minshrink")


ranks <- function(x, tiestreat = c("avg", "minshrink")) {

  if (tiestreat == "avg") return(rank(x, ties.method = "average"))

  if (tiestreat == "minshrink") {
    r <- rank(x, ties.method = "min")
    for (i in (min(r) + 1):length(x)) {
      if (i %in% r) r[r == i] <- max(r[r < i]) + 1
    }
    return(r)
  }
}
