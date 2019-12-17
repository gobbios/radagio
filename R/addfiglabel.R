#' add an identifier to subpanels in a plot
#'
#' @param lab character, the text to appear (e.g. "(a)")
#' @param xloc numeric, the location of the text along the x-axis (default: -1)
#' @param yloc numeric, the location of the text along the y-axis (default: 1)
#' @param ... additional parameters passed on the \code{text()} function, e.g. \code{cex}, \code{col} etc
#' @importFrom graphics text
#' @return a plot
#' @export
#' @details the function produces a new plot on top of the current one, albeit without axes and annotations (think of a new 'layer'). The text is then written according to the coordinates passed on, which by default places the text label in the top left corner.
#' @examples
#' par(mfcol = c(2, 2))
#' plot(1:10)
#' addfiglabel("(a)")
#' plot(1:10)
#' addfiglabel("(b)", cex = 0.5, col = "red")
#' par(mar = c(2,2,2,2))
#' plot(1:10)
#' addfiglabel("(c)")
#' plot(1:10)
#' addfiglabel("(d)", cex = 3)

addfiglabel <- function(lab, xloc = -1, yloc = 1, ...) {
  # store old margins
  oldmars <- par("mar")
  # new plot with 0-margins
  par(new = TRUE, mar = c(0, 0, 0, 0))
  # new plot
  plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n",
       xlim = c(-1, 1), ylim = c(-1, 1))
  # add text
  text(x = xloc, y = yloc, labels = lab, xpd = TRUE, adj = c(0, 1), ...)
  # reset graphical parameters
  par(new = FALSE, mar = oldmars)
}
