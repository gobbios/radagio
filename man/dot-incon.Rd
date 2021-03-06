% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISI-incon.R
\name{.incon}
\alias{.incon}
\title{number of inconsistencies}
\usage{
.incon(mat)
}
\arguments{
\item{mat}{square interaction matrix with winner in rows and losers in columns}
}
\value{
integer, the number of inconsistencies in the matrix
}
\description{
calculate number of inconsistencies
}
\examples{
data(bonobos, package = "EloRating")
radagio:::.incon(bonobos)

}
\references{
de Vries, H. 1998. Finding a dominance order most consistent with a linear hierarchy: a new procedure and review. Animal Behaviour, 55, 827-843. (\href{https://dx.doi.org/10.1006/anbe.1997.0708}{DOI: 10.1006/anbe.1997.0708})
}
\author{
Christof Neumann
}
