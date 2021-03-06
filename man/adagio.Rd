% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adagio.R
\name{adagio}
\alias{adagio}
\alias{radagio}
\title{ADAGIO from R}
\usage{
adagio(mat, bottomup = TRUE, preproc = TRUE, plotfig = FALSE,
  cleanup = TRUE, tempfolderidentifier = NULL, pause = 1)
}
\arguments{
\item{mat}{a square matrix, should have column and row names}

\item{bottomup}{logical, bottom-up (default) or top-down ranking}

\item{preproc}{logical, should pre-processing be performed (default) or not}

\item{plotfig}{logical, should the output graph be displayed (by default \code{FALSE})}

\item{cleanup}{logical, should the temporary files be deleted after they have been read and processed (defaults to TRUE)}

\item{tempfolderidentifier}{character, is used to create subfolders for each iteration if you use the function in a loop (see details)}

\item{pause}{numerical, by default 1, make a pause of this length (in seconds) between writing the adagio output to disk and reading it back (see details)}
}
\value{
a list with three items. The first contains a data frame with the ordinal rankings returned by the ADAGIO software for its algorithm and for Elo-rating and David's score. The second data frame contains some additional information about the matrix (from ADAGIO output: 'adagio.output'). The third item is a character, which reflects the path to the temporary folder (which might be empty if \code{cleanup = TRUE}).
}
\description{
ADAGIO from R
}
\details{
The option to display the results graph is only very rudimentary and the quality of the output is rather poor. It is merely meant as a cross-check whether the R wrapper for ADAGIO is working properly. If you want higher quality output currently you would need to run the java program directly, or inspect the folder with the results directly (see below).

The plotting option behaves somewhat erratically at the moment, and sometimes no plot is produced at all or one that is essentially blank. Currently, I do not know the reason for this.

Sometimes, the general output of ADAGIO (file 'adagio.output') is empty. The reason for this is unclear to me. Currently, if this happens, a general output is still generated but all values in \code{$gen} are \code{NA}. As far as I can see, this only happens when the function is run repeatedly in a loop.

To solve this issue, each time the \code{adagio()} is run, it creates a subfolder in the temporary folder of the current R session. If you run the function in a loop, please set the \code{tempfolderidentifier=} argument, so that for each run of the function a new subfolder is created that you can identify afterwards. If you leave this argument at its default, a random subfolder is created, which should be okay if you use the function one time at a time (i.e. not in large loops). To see where the files are stored, set the \code{returnpath=} argument. And also note that, if you quit or restart your R session the folder with the ADAGIO output will be deleted by R! If you want to inspect the folder, you need to do so before you quit or restart your current R session.

If you get an error message like "could not read ... tempmat.csv.david.ranks ... there is no such file" or something similar, you can try to increase the \code{pause=} argument. I have not thoroughly investigated why this error occurs sometimes, but it appears to not occur if we set a pause between running the Java program and reading its output back. Very hackish, but it seems to work.

I realize that ADAGIO itself has the option to process multiple files (matrices) at the same time and return all the results in single files. However, the way I use ADAGIO I would like to have the results per matrix and I have programmed the function accordingly. This might change in the future.
}
\note{
This package (and the \code{adagio()} function in particular) is simply meant as a wrapper for the ADAGIO software. In essence, it runs the actual Java program via a call to \code{system2()}, reads the output of the ADAGIO software and produces a summary of the results ready to be handled within R.
}
\examples{
\dontrun{
 data(archie)
 adagio(archie)
}
}
\references{
Douglas, P. H., Ngomo, A. C. N., & Hohmann, G. (2017). A novel approach for dominance assessment in gregarious species: ADAGIO. Animal Behaviour, 123, 21-32. \url{https://doi.org/10.1016/j.anbehav.2016.10.014}

ADAGIO software: \url{https://github.com/ngonga/adagio/releases} (version 1.1)
}
