#' linearity index
#'
#' @param interactionmatrix the dominance matrix
#' @param loops numeric, the number of loops
#'
#' @return a data.frame with the results
#' @export
#' @references 
#' de Vries, H. (1995). An improved test of linearity in dominance hierarchies containing unknown or tied relationships. Animal Behaviour, 50(5), 1375-1389. \url{https://doi.org/10.1016/0003-3472(95)80053-0}
#' 
#' Landau, H. (1951). On dominance relations and the structure of animal societies: I. Effect of inherent characteristics. The bulletin of mathematical biophysics, 13(1), 1-19. \url{https://doi.org/10.1007/BF02478336}
#' @examples
#' data(archie)
#' hindex(archie)

hindex <- function(interactionmatrix, loops=1000){
  # expected h can also be calculated as 3/(N+1), see MatMan helpfile...
  mat <- interactionmatrix
  # triangle indices
  mu <- upper.tri(mat); #diag(mu) <- NA
  
  # transposed matrix
  tmat <- t(mat)
  
  # upper and lower values
  uv <- mat[mu]; lv <- tmat[mu]
  
  # position of unknowns and ties
  up <- which(uv==lv & uv ==0)
  tp <- which(uv==lv & uv !=0)
  
  # number of unknown and tied relationships
  unknown <- length(up)
  tied <- length(tp)
  
  # onezero matrix
  ozmat <- (mat - tmat > 0) + 1 - 1
  # replace tied with 0.5
  ozmat[mat - tmat == 0 & mat != 0] <- 0.5
  
  N <- ncol(mat)
  
  # and some generic matrices, and values
  zeromat <- mat - mat
  onemat <- zeromat + 1
  s <- c(0,1); d <- sum(mu)
  h_0 <- h_r <- numeric(loops)
  
  
  # diverge depending on existence of unknown relationships
  if(unknown > 0) {
    for(i in 1:loops) {
      rmat <- ozmat #ozmat2
      rmat[mu][up] <- sample(s, unknown, replace=T)
      rmat <- t(rmat)
      rmat[mu] <- (onemat - t(rmat))[mu]
      h_0[i] <- (12/(N^3-N))*sum((rowSums(rmat) - (0.5*(N-1)))^2)
      
      rmat <- zeromat
      rmat[mu] <- sample(s, d, replace=T)
      rmat <- t(rmat)
      rmat[mu] <- (onemat - t(rmat))[mu]
      h_r[i] <- (12/(N^3-N))*sum((rowSums(rmat) - (0.5*(N-1)))^2)
      
    }
    
    pval <- sum(h_r >= h_0)/loops; #mean(h_0)
    
  }
  
  if(unknown == 0) {
    h_0[] <- (12/(N^3-N))*sum((rowSums(ozmat) - (0.5*(N-1)))^2)
    for (i in 1:loops) {
      rmat <- zeromat
      rmat[mu] <- sample(s, d, replace=T)
      rmat <- t(rmat)
      rmat[mu] <- (onemat - t(rmat))[mu]
      h_r[i] <- (12/(N^3-N))*sum((rowSums(rmat) - (0.5*(N-1)))^2)
      
    }
    pval <- sum(h_r >= h_0)/loops; mean(h_0); mean(h_r)
    
  }
  
  ozmat[mat - tmat == 0] <- 0.5; diag(ozmat) <- 0
  temp <- (rowSums(ozmat) - (0.5*(N-1)))^2
  
  (h_Index<-(12/(N^3-N))*sum(temp))
  
  ifelse(unknown<1, h_index_devries <- h_Index, h_index_devries <- h_Index + ((6*unknown)/(N^3-N)))
  
  expected_h <- mean(h_r)
  
  results <- data.frame(c("N", "h index", "h' index","expected h", "p right", "randomizations", "tied", "unknown"), c(N,round(h_Index,4),round(h_index_devries,4),round(expected_h,4),round(pval,4),loops, tied, unknown)); colnames(results)<-c("variable", "value")
  
  
  if(results$value[5]==0) results$value[5] <- 1/loops
  
  return(results)
}

