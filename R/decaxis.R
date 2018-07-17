
#' draw axes with different decimals in the labels
#'
#' @param xside numeric, side on which to draw the axis
#' @param xat numeric, locations to draw ticks and labels
#' @param ... other arguments to be passed on from the \code{axis} function
#' @details the function draws an axis such that integer numbers along a sequence are printed as such integer labels, e.g. "0" instead of "0.0"
#' @return draws an axis on an existing plot
#' @export
#'
#' @examples
#' plot(runif(10), runif(10), axes = FALSE, xlim = c(0, 1), ylim = c(0, 1))
#' decaxis(xside = 1, las = 1, cex.axis = 0.7)
#' decaxis(xside = 2, las = 1, col = "red", xat = c(0, 0.5, 1))
#' decaxis(xside = 3, xat = c(0.1, 0.75), col.axis = "gold")
#' box()
#'
#' plot(-2:2, -2:2, axes = FALSE)
#' # normal axis
#' axis(1, at = seq(-2, 2, 0.5))
#' # axis with "0 decimals" removed
#' decaxis(2, xat = seq(-2, 2, 0.5), las = 1)
#' box()

decaxis <- function(xside, xat = c(0, 0.2, 0.4, 0.6, 0.8, 1), ...) {
  # create labels
  labs <- character(length(xat))
  for(i in 1:length(labs)) {
    if(xat[i] - round(xat[i]) == 0) {
      labs[i] <- as.character(round(xat[i]))
    } else {
      labs[i] <- as.character(xat[i])
    }
  }
  # draw axis with stripped custom labels
  axis(side = xside, at = xat, labels = labs, ...)
}
