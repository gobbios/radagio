
#' add dominance indices to a data set given a dominance matrix
#'
#' @param mat a dominance matrix, most likely from \code{\link{datagenfunc}}
#' @param respdata a \code{data.frame} that contains a \code{ID} column, which in turn contains IDs that appear in \code{mat}
#' @param radagio logical, by default \code{TRUE}, see details
#' @details currently, four metrics are calculated:
#' \itemize{
#'   \item David's score (via the EloRating package)
#'   \item randomized Elo-rating (via the EloChoice package)
#'   \item I&SI (this package)
#'   \item ADAGIO (this package)
#' }
#' 
#' All metrics are returned twice, on their original scale and z-transformed. In addition, the 'true rank' (referred to in the \code{rank} column) is also z-transformed.
#' 
#' This function adds new columns to \code{xdata}. For this to work, \code{xdata} has to have at least two columns with the name \code{ID} and \code{rank}.
#' 
#' If \code{radagio = FALSE}, the function returns ranks as individuals ranked by their proportion of wins. This is meant as a testing strategy because running the ADAGIO algorithm in turn requires the Java ADAGIO software to be run, which first of all is very slow and also interupts workflow because this produces a pop-up window to appear for each instance. The intended use of this argument is to try out simulations that require the \code{calcrank()} function. If the simulation works, then this argument can be set to \code{radagio = TRUE} (its default).
#' 
#' @return a \code{data.frame} with metrics added
#' @export
#' @importFrom EloRating DS
#' @importFrom EloChoice elochoice
#'
#' @examples
#' \dontrun{
#' domdata <- datagenfunc(gs = 5, Nintm = 3)
#' xdata <- createresponse(domdata[[2]], 2, effectsize = 2, error = 0.5)
#' calcranks(domdata[[1]], xdata)
#' }

calcranks <- function(mat, respdata, radagio = TRUE) {
  # DS
  dsdat <- DS(mat)
  ds <- dsdat[, 3]
  names(ds) <- dsdat[,1]
  respdata$ds <- ds[as.character(respdata$ID)]
  respdata$z.ds <- as.numeric(scale(respdata$ds))
  # randomized Elo
  domseq <- mat2seq(mat)
  elo <- colMeans(elochoice(domseq$winner, domseq$loser, runs = 1000)$ratmat)
  # elo <- elochoice(domseq$winner, domseq$loser)$ratmat[1, , drop = T]
  respdata$elo <- elo[as.character(respdata$ID)]
  respdata$z.elo <- as.numeric(scale(respdata$elo))
  # scale original rank
  respdata$z.rank <- as.numeric(scale(respdata$rank))
  # ADAGIO
  if(radagio == TRUE) {
    ada <- adagio(mat)
    ad <- ada$res$adagio; names(ad) <- as.character(ada$res$ID)
  } else {
    ada <- colSums(mat)/(rowSums(mat) + colSums(mat))
    if(NaN %in% ada) ada[is.nan(ada)] <- 0
    ada <- ada[sort(names(ada))]
    ad <- rank(ada)
  }
  
  respdata$adagio <- ad[as.character(respdata$ID)]
  respdata$z.adagio <- as.numeric(scale(respdata$adagio))
  # ISI (average if there are multiple solutions)
  isi <- ISI(mat = mat, runs = 20000, printmessages = FALSE)
  r <- 1:ncol(mat)
  if(length(isi) == 1) {
    nms <- colnames(isi[[1]])
    isi <- r; names(isi) <- nms
  } else {
    isi <- lapply(isi, colnames)
    tempisi <- matrix(ncol = 2, nrow = 0)
    for(i in 1:length(isi)) {
      nms <- isi[[i]]
      tempisi <- rbind(tempisi, cbind(nms, r))
    }
    tempisi <- data.frame(ID = tempisi[, 1], isi = as.numeric(tempisi[, 2]))
    isi <- tapply(tempisi$isi, tempisi$ID, mean)
  }
  respdata$isi <- isi[as.character(respdata$ID)]
  respdata$z.isi <- as.numeric(scale(respdata$isi))
  
  return(respdata)
}

