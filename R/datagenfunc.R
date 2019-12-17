#' Data generation
#'
#' Generate dominance matrices following the description of Douglas et al 2017.
#'
#' @param gs integer, group size
#' @param Nintm integer, multiplier for interactions
#' @param mode character, either \code{"lin"} for linear mode or \code{"2"}, \code{"3"} or \code{"4"} for exponential modes
#' @param shrinkranks logical, should generated ranks be 'shrunk' so that no gaps occur (default is \code{TRUE}), see details
#' @param allowties logical, should tied/shared ranks be allowed in the data generation (default is \code{TRUE})
#'
#' @return a list, the first is the dominance matrix, the second are the true ranks (upon which simulation of outcomes of interactions is based)
#' @export
#' @details
#' if \code{shrinkranks = FALSE} rank orders are returned that may have gaps, e.g. an allowed generated order might be 1, 1, 3, 3, 5, whereas 1, 1, 2, 2, 3 would not be allowed in this scenario (but it would be if \code{shrinkranks = TRUE})
#'
#' @examples
#' set.seed(123)
#' datagenfunc(10, 2, shrinkranks = TRUE)[[2]]
#' # two IDs with rank=3 and one with rank=4
#' set.seed(123)
#' datagenfunc(10, 2, shrinkranks = FALSE)[[2]]
#' # two IDs with rank=3 and none with rank=4, but the next rank would be rank=5


# gs <- 35; Nintm=5; mode="lin"; shrinkranks=T; allowties=T
datagenfunc <- function(gs, Nintm, mode="lin", shrinkranks=TRUE, allowties=TRUE) {
  # make ranks
  (r <- sample(1:gs, gs, replace = allowties))
  if(shrinkranks) {
    for(i in (min(r)+1):gs) {
      if(i %in% r) r[r==i] <- max(r[r<i]) + 1
    }
    r <- r - min(r) + 1 # make the highest rank 1
  }

  if(!shrinkranks) {
    r <- rank(r, ties.method = "min")
  }

  (rs <- r)

  mat <- matrix(ncol=gs, nrow=gs, 0)
  nint <- gs * Nintm
  if(mode == "lin") {
    for(i in 1:nint) {
      is <- sample(1:gs, 2)

      # winning chance for first:
      onechance <- rs[is[2]]/sum(rs[is])

      oneres <- sample(c(1,0), 1, prob=c(onechance, 1-onechance))
      if(oneres == 1) mat[is[1], is[2]] <- mat[is[1], is[2]] + 1
      if(oneres == 0) mat[is[2], is[1]] <- mat[is[2], is[1]] + 1

    }
  }

  if(mode %in% c("2", "3", "4")) {
    EX <- as.numeric(mode)


    for(i in 1:nint) {
      is <- sample(1:gs, 2)

      # winning chance for first:
      onechance <- EX^rs[is[2]] / ( EX^rs[is[1]] + EX^rs[is[2]]   )

      oneres <- sample(c(1,0), 1, prob=c(onechance, 1-onechance))
      if(oneres == 1) mat[is[1], is[2]] <- mat[is[1], is[2]] + 1
      if(oneres == 0) mat[is[2], is[1]] <- mat[is[2], is[1]] + 1

    }
  }

  onames <- sort(apply(as.matrix(expand.grid(letters, letters)), 1, paste, collapse=""))[1:length(rs)]
  names(rs) <- onames
  colnames(mat) <- rownames(mat) <- onames

  return(list(mat, rs))
}
