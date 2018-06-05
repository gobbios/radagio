#' Plotting simulation results
#'
#' @param reslist the output of the simulation steps (see vignettes and appendices of manuscript)
#' @param ov the output of the simulation steps (see vignettes and appendices of manuscript)
#' @param metric character, \code{"euc"} for Euclidean distance or \code{"cor"} for correlation
#' @param mode character, which calculations to take for Elo and DS
#' @param ylimits numeric of length 2, overrides internal setting for y-axis limits
#' @param linesstyle logic, should different methods differ also with regards to line style, by default \code{FALSE}
#' 
#' @importFrom stats aggregate cor quantile
#' @importFrom graphics axis layout legend points segments box
#' @details 
#' This is a idiosyncratic function that is only usable in the context of the simulations we run here. See the vignettes of the package.
#' 
#' The \code{"mode"} argument determines which software to use for Elo-ratings and DS. The ADAGIO rankings themselves are always taken from the ADAGIO software. The ISI ranks are always taken from a function included in this package.
#' \itemize{
#'   \item \code{"ADAGIO"}: Take ranks for Elo and DS from the ADAGIO software.
#'   \item \code{"eloRA"}: Calculate ranks for Elo and DS from original scores calculated with the \code{EloRating} (for DS) and \code{EloChoice} (for Elo-rating) packages. The ranking is done via the \code{"average"} option in the \code{rank()} function.
#'   \item \code{"eloRM"}: Same as above, but the ranks are derived in the following way: in case of ties the minimum rank is taken and subsequently ranks are shrunk so that there will be no gaps. See examples in \code{\link{ranks}}. 
#'   \item \code{"eloO"}: Here the original scores for Elo and DS are used, and if \code{metric="cor"} the Pearson correlation is calculated.
#' }
#' @return a plot
#' @export
#'

plotfunc <- function(reslist, ov, metric = c("euc", "cor"), mode = c("ADAGIO", "eloRA", "eloRM", "eloO"), ylimits = NULL, linesstyle = FALSE) {
  # needs ov and reslist in workspace!!! 
  
  # get columns according to settings
  acol <- "adagio"
  icol <- "isiO"
  
  if(mode == "ADAGIO") {
    ecol <- "elo"
    dcol <- "ds"
    cormeth <- "s"
    ya <- ifelse(metric == "cor", "Spearman correlation", "Euclidean distance") # y-axis label
  }
  if(mode == "eloRA") {
    ecol <- "eloRA"
    dcol <- "dsRA"
    cormeth <- "s"
    ya <- ifelse(metric == "cor", "Spearman correlation", "Euclidean distance") # y-axis label
  }
  if(mode == "eloRM") {
    ecol <- "eloRA"
    dcol <- "dsRA"
    cormeth <- "s"
    ya <- ifelse(metric == "cor", "Spearman correlation", "Euclidean distance") # y-axis label
  }
  if(mode == "eloO") {
    ecol <- "eloO"
    dcol <- "dsO"
    cormeth <- "p" # pearson only makes sense if we use the original scores (not ranks) for Elo and DS
    ya <- ifelse(metric == "cor", "Pearson correlation", "Euclidean distance") # y-axis label
  }
  
  # calculate metrics
  if(metric == "euc") {
    ov$A <- unlist(lapply(reslist, function(X)eucdist(x = X[, acol], y = X[, "truerank"])))
    ov$E <- unlist(lapply(reslist, function(X)eucdist(x = X[, ecol], y = X[, "truerank"])))
    ov$D <- unlist(lapply(reslist, function(X)eucdist(x = X[, dcol], y = X[, "truerank"])))
    ov$I <- unlist(lapply(reslist, function(X)eucdist(x = X[, icol], y = X[, "truerank"])))
  }
  
  if(metric == "cor") {
    ov$A <- unlist(lapply(reslist, function(X)cor(x = X[, acol], y = X[, "truerank"], method = cormeth)))
    ov$E <- unlist(lapply(reslist, function(X)cor(x = X[, ecol], y = X[, "truerank"], method = cormeth)))
    ov$D <- unlist(lapply(reslist, function(X)cor(x = X[, dcol], y = X[, "truerank"], method = cormeth)))
    ov$I <- unlist(lapply(reslist, function(X)cor(x = X[, icol], y = X[, "truerank"], method = cormeth)))
    if(mode == "eloO") {
      ov$E <- ov$E * (-1)
      ov$D <- ov$D * (-1)
    }
  }
  
  # get values for x-axis (to show interactions instead of multiplier)
  xax <- 1:length(unique(ov$mul))
  xaxlabsbase <- 2^xax
  xlims <- c(0.5, max(xax) + 0.5)
  # horizontal offset to avoid overplotting 
  offs <- c(-0.15, -0.05, 0.05, 0.15)
  
  varnames <- c("A", "E", "D", "I") # column names of the metrics
  xcols <- c("goldenrod3",  "blue", "darkgreen", "darkred")
  xpch <- c(4, 1, 3, 2)
  if(linesstyle) xlty <- c(1, 2, 3, 4) else xlty <- c(1, 1, 1, 1)
  
  if(metric == "cor") ylims <- c(0, 1) else ylims <- c(0, 3) # set y-axis limits
  if(!is.null(ylimits)) ylims <- ylimits
  # calculate data for plotting (mean of the metric and 10% and 90% percentiles)
  xdata <- aggregate(ov[, varnames], by = list(gs = ov$gs, btmod = ov$btmo, ints = ov$mul), function(X)c(mean(X), quantile(X, probs = c(0.1, 0.9))))
  
  # set up plot
  layout(matrix(c(1, 1, 2:7), nrow = 4, byrow = TRUE), heights = c(1.2, 4, 4, 4))
  par(mar = c(0, 0, 0, 0))
  plot(0, 0, "n", axes = FALSE)
  legend("center", legend = c("ADAGIO+p+b", "Elo-rating", "David's score", "I&SI"), pch = xpch, col = xcols, bty = "n", ncol = 4, xjust = 0.5, lty = xlty, x.intersp = 0.4, text.width = 0.4)
  
  # do the actual plotting
  # tick marks/labels for y-axis
  ifelse(metric == "euc", yat <- c(0, 0.5, 1, 1.5, 2, 2.5, 3), yat <- c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  par(mar = c(5, 4, 4, 2))
  G=50; M="2"
  for(G in c(10, 20, 50)) {
    for(M in c("lin", "2")) {
      x <- xdata[xdata$gs == G & xdata$btmod == M, ]
      mtitle <- paste("Group size = ", G, ", mode = ", ifelse(M == "lin", "linear", "exponential"), sep = "")
      plot(0, 0, xlim = xlims, ylim = ylims, "n", main = mtitle, axes = F, xlab = "Number of interactions", ylab = ya, cex.main = 0.7); 
      xaxlabs <- xaxlabsbase * G
      axis(1, at = xax, labels = xaxlabs, tcl = 0.2)

      axis(2, las = 1, tcl = 0.2, at = yat, labels = as.character(yat))
      box()
      
      i=1
      for(i in 1:length(varnames)) {
        XX <- x[[varnames[i]]]
        segments(xax + offs[i], XX[, "10%"], xax + offs[i], XX[, "90%"], col = xcols[i], lty = xlty[i], lwd = 1.2)
        points(xax + offs[i], XX[, 1], pch = xpch[i], cex = 0.8, col = xcols[i])
      }
    }
  }
}
