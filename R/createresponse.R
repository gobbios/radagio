

#' generate a response variable, given ranks and effect size
#'
#' @param ranks a named vector with the ranks (and individual IDs as name); typically the output from \code{\link{datagenfunc}}
#' @param Nmulti integer, the number of observations to create per individual
#' @param effectsize numeric, the effect size
#' @param error numeric, the error
#' @details the function calculates a response variable with the following formula:
#' 
#' resp = \code{effectsize} * \code{ranks} + \code{rnorm(..., mean = 0, sd = error)}
#' 
#' in addition, a random intercept is generated:
#' 
#' random intercept = \code{rnorm(length(ranks))}
#' @return a \code{data.frame} with \code{length(ranks)} * \code{Nmulti} rows
#' @export
#' @importFrom stats rnorm
#'
#' @examples
#' ranks <- 1:4
#' names(ranks) <- LETTERS[1:4]
#' xdata <- createresponse(ranks, 2, effectsize = 2, error = 0.5)
#' plot(xdata$rank, xdata$response)

createresponse <- function(ranks, Nmulti, effectsize, error) {
  res <- expand.grid(rank = ranks, obsnum = 1:Nmulti)
  res$ID <- names(ranks)
  # random intercepts
  ri <- rnorm(length(ranks))
  names(ri) <- names(ranks)
  # generate response
  res$response <- 0 + effectsize * as.numeric(scale(res$rank)) + rnorm(nrow(res), mean = 0, sd = error) + ri[res$ID]
  
  return(res)
}
