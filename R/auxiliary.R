

generateTempdir <- function(basedir) {
  tryRes <- FALSE
  fullpath <- NULL
  for (tryNum in 1:100) {
    dirnameProposal <- paste0(format(Sys.time(),"%y-%m-%d_%H-%M-%S_"),tryNum)
    fullpath <- file.path(basedir, dirnameProposal)
    tryRes <- try(tryCatch(dir.create(fullpath, showWarnings=TRUE)),silent=FALSE)
    if (isTRUE(tryRes)) break
  }
  if (!isTRUE(tryRes)) stop("Could not create temporary directory")
  fullpath
}
