#' ADAGIO from R
#' @aliases radagio
#' @aliases radagio-package
#' @param mat a square matrix, should have column and row names
#' @param bottomup logical, bottom-up (default) or top-down ranking
#' @param preproc logical, should pre-processing be performed (default) or not
#' @param plotfig logical, should the output graph be displayed (by default \code{FALSE})
#' @param cleanup logical, should the temporary files be deleted after they have been read and processed (defaults to TRUE)
#' @param tempfolderidentifier character, is used to create subfolders for each iteration if you use the function in a loop (see details)
#' @param pause numerical, by default 1, make a pause of this length (in seconds) between writing the adagio output to disk and reading it back (see details)
#' @note
#' This package (and the \code{adagio()} function in particular) is simply meant as a wrapper for the ADAGIO software. In essence, it runs the actual Java program via a call to \code{system2()}, reads the output of the ADAGIO software and produces a summary of the results ready to be handled within R.
#' @details
#' The option to display the results graph is only very rudimentary and the quality of the output is rather poor. It is merely meant as a cross-check whether the R wrapper for ADAGIO is working properly. If you want higher quality output currently you would need to run the java program directly, or inspect the folder with the results directly (see below).
#'
#' The plotting option behaves somewhat erratically at the moment, and sometimes no plot is produced at all or one that is essentially blank. Currently, I do not know the reason for this.
#'
#' Sometimes, the general output of ADAGIO (file 'adagio.output') is empty. The reason for this is unclear to me. Currently, if this happens, a general output is still generated but all values in \code{$gen} are \code{NA}. As far as I can see, this only happens when the function is run repeatedly in a loop.
#'
#' To solve this issue, each time the \code{adagio()} is run, it creates a subfolder in the temporary folder of the current R session. If you run the function in a loop, please set the \code{tempfolderidentifier=} argument, so that for each run of the function a new subfolder is created that you can identify afterwards. If you leave this argument at its default, a random subfolder is created, which should be okay if you use the function one time at a time (i.e. not in large loops). To see where the files are stored, set the \code{returnpath=} argument. And also note that, if you quit or restart your R session the folder with the ADAGIO output will be deleted by R! If you want to inspect the folder, you need to do so before you quit or restart your current R session.
#'
#' If you get an error message like "could not read ... tempmat.csv.david.ranks ... there is no such file" or something similar, you can try to increase the \code{pause=} argument. I have not thoroughly investigated why this error occurs sometimes, but it appears to not occur if we set a pause between running the Java program and reading its output back. Very hackish, but it seems to work.
#'
#' I realize that ADAGIO itself has the option to process multiple files (matrices) at the same time and return all the results in single files. However, the way I use ADAGIO I would like to have the results per matrix and I have programmed the function accordingly. This might change in the future.
#'
#' @return a list with three items. The first contains a data frame with the ordinal rankings returned by the ADAGIO software for its algorithm and for Elo-rating and David's score. The second data frame contains some additional information about the matrix (from ADAGIO output: 'adagio.output'). The third item is a character, which reflects the path to the temporary folder (which might be empty if \code{cleanup = TRUE}).
#' @importFrom graphics par plot rasterImage
#' @importFrom utils read.table write.csv write.table
#' @import jpeg
#' @export
#'
#' @references
#' Douglas, P. H., Ngomo, A. C. N., & Hohmann, G. (2017). A novel approach for dominance assessment in gregarious species: ADAGIO. Animal Behaviour, 123, 21-32. \url{https://doi.org/10.1016/j.anbehav.2016.10.014}
#'
#' ADAGIO software: \url{https://github.com/ngonga/adagio/releases} (version 1.1)
#'
#' @examples
#' \dontrun{
#'  data(archie)
#'  adagio(archie)
#' }

adagio <- function(mat,
                   bottomup = TRUE,
                   preproc = TRUE,
                   plotfig = FALSE,
                   cleanup = TRUE,
                   tempfolderidentifier = NULL,
                   pause = 1) {
  # run some checks
  if (is.null(Sys.which("java"))) stop("java not found", call. = FALSE)
  if (Sys.which("java") == "") stop("java not found", call. = FALSE)
  if (ncol(mat) != nrow(mat)) stop("matrix needs to be square", call. = FALSE)
  # ADAGIO seems to work if there are NAs...
  testmat <- mat
  testmat[diag(testmat)] <- 0
  if (NA %in% testmat) {
    stop("no NAs allowed in matrix (except for the diagonal)", call. = FALSE)
  }
  if (is.null(rownames(mat)) | is.null(colnames(mat))) {
    warning("please provide a matrix with column and row names", call. = FALSE)
  }

  # set temp location
  temploc <- tempdir()
  if (!is.null(tempfolderidentifier)) {
    # but modify (and create) if subfolder was specified
    temploc <- paste0(temploc, "/adag_", tempfolderidentifier)
    if (!dir.exists(temploc)) dir.create(temploc)
  } else {
    temploc <- paste0(temploc,
                      paste0("/adag_",
                             paste(sample(letters, 10, replace = TRUE),
                                   collapse = "")))
    if (!dir.exists(temploc)) dir.create(temploc)
  }

  # write matrix to file in temp location
  write.csv(mat, file = paste0(temploc, "/tempmat.csv"), quote = FALSE)
  # get adagio location
  adagioloc <- paste0(system.file(package = "radagio"), "/adagio/adagio.jar")

  # prepare call to adagio program
  adagiocall <- paste0(" -jar ", adagioloc, " -folder ", temploc)
  if (preproc & bottomup)   adagiocall <- paste0(adagiocall, " -ranking bottom -preprocessing true")
  if (preproc & !bottomup)  adagiocall <- paste0(adagiocall, " -ranking top    -preprocessing true")
  if (!preproc & bottomup)  adagiocall <- paste0(adagiocall, " -ranking bottom -preprocessing false")
  if (!preproc & !bottomup) adagiocall <- paste0(adagiocall, " -ranking top    -preprocessing false")

  # run adagio
  r <- system2(Sys.which("java"),
               args = adagiocall,
               stdout = TRUE,
               stderr = TRUE)

  Sys.sleep(pause)

  # read back the output
  ds <- scan(paste0(temploc, "/tempmat.csv.david.ranks"),
             what = "character", quiet = TRUE)
  if (length(ds) > 0) {
    ds <- data.frame(matrix(ds, ncol = 2, byrow = TRUE))
    ds[, 1] <- as.character(ds[, 1])
    ds[, 2] <- as.numeric(as.character(ds[, 2]))
  } else {
    ds <- data.frame(X1 = colnames(mat), X2 = NA)
  }

  elos <- scan(paste0(temploc, "/tempmat.csv.elo.ranks"),
               what = "character", quiet = TRUE)
  if (length(elos) > 0) {
    elos <- data.frame(matrix(elos, ncol = 2, byrow = TRUE))
    elos[, 1] <- as.character(elos[, 1])
    elos[, 2] <- as.numeric(as.character(elos[, 2]))
  } else {
    elos <- data.frame(X1 = colnames(mat), X2 = NA)
  }

  ada <- scan(paste0(temploc, "/tempmat.csv.adagio.ranks"),
              what = "character", quiet = TRUE)
  if (length(ada) > 0) {
    ada <- data.frame(matrix(ada, ncol = 2, byrow = TRUE))
    ada[, 1] <- as.character(ada[, 1])
    ada[, 2] <- as.numeric(as.character(ada[, 2]))
  } else {
    ada <- data.frame(X1 = colnames(mat), X2 = NA)
  }

  gen <- scan(paste0(temploc, "/adagio.output"),
              what = "character", quiet = TRUE)
  if (length(gen) == 37) {
    gen <- as.numeric(gen[28:37])
    names(gen) <- c("Nind", "h", "hprime", "hpr", "hpl", "error",
                    "removedweight", "removededges", "dci", "proctime")
    gen <- data.frame(as.list(gen))
  } else {
    gen <- rep(NA, 10)
    names(gen) <- c("Nind", "h", "hprime", "hpr", "hpl", "error",
                    "removedweight", "removededges", "dci", "proctime")
    gen <- data.frame(as.list(gen))
  }

  # clean output
  ds <- ds[order(ds[, 1]), ]
  elos <- elos[order(elos[, 1]), ]
  ada <- ada[order(ada[, 1]), ]
  res <- data.frame(ada, elos[, 2], ds[, 2])
  colnames(res) <- c("ID", "adagio", "elo", "ds")

  # check whether IDs match for the three outputs
  # (i.e. did the sorting work properly)
  if (sum(ada[, 1] != elos[, 1]) > 0) stop("sorting error 1")
  if (sum(ada[, 1] != ds[, 1]) > 0) stop("sorting error 2")

  # collate results
  gen$bottumup[1] <- bottomup
  gen$preproc[1] <- preproc
  res <- list(res = res, gen = gen, filepath = temploc)

  # do plot
  if (plotfig) {
    par(mar = c(0, 0, 0, 0))
    ri <- readJPEG(paste0(temploc, "/tempmat.csv.adagio.result.jpg"))
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         asp = 1, ann = FALSE, axes = FALSE)
    rasterImage(ri, 0, 0, 1, 1)
  }

  # cleanup: delete ADAGIO output files
  if (cleanup) {
    file.remove(paste0(temploc, "/tempmat.csv.elo.ranks"))
    file.remove(paste0(temploc, "/tempmat.csv.david.ranks"))
    file.remove(paste0(temploc, "/tempmat.csv.adagio.ranks"))
    file.remove(paste0(temploc, "/adagio.output"))
    file.remove(paste0(temploc, "/tempmat.csv"))
    file.remove(paste0(temploc, "/tempmat.csv.adagio.matrix"))
    file.remove(paste0(temploc, "/tempmat.csv.adagio.result.jpg"))
  }

  return(res)
}
