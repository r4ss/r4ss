#' Check input argument \code{model}
#' 
#' Check that the executable name provided in \code{model},
#' an input argument to numerous \code{r4ss} functions,
#' does not contain the extension and is available.
#' 
#' @template model
#' @param mydir The directory where \code{model} is located.
#' @author Kelli Faye Johnson
#' @return A cleaned \code{model} name based on the input argument.

check_model <- function(model, mydir = getwd()) {
  modelnoexe <- gsub("\\.exe$", "", model)

  # Check that the file exists
  os <- ifelse(grepl("windows", .Platform$OS.type, ignore.case = TRUE),
    "windows", "linux")
  exename <- paste0(modelnoexe,
    switch(os, windows = ".exe", linux = ""))
  if (!file.exists(file.path(mydir, exename))) {
    stop(exename, " does not exist in ", mydir)
  }

  return(modelnoexe)
}
