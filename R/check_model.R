#' Check input argument `model`
#'
#' Check that the executable name provided in `model`,
#' an input argument to numerous `r4ss` functions,
#' does not contain the extension and is available.
#'
#' @template model
#' @param mydir The directory where `model` is located.
#' @template exe_in_path
#' @author Kelli F. Johnson, Ian G. Taylor
#' @return A cleaned `model` name based on the input argument.

check_model <- function(model, mydir = getwd(), exe_in_path = FALSE) {
  modelnoexe <- gsub("\\.exe$", "", model)

  # Check that the file exists
  os <- ifelse(grepl("windows", .Platform[["OS.type"]], ignore.case = TRUE),
    "windows", "linux"
  )
  exename <- paste0(
    modelnoexe,
    switch(os,
      windows = ".exe",
      linux = ""
    )
  )
  if (exe_in_path) {
    if (Sys.which(exename)[[1]] == "") {
      stop(exename, " does not exist in the path")
    }
  } else {
    if (!file.exists(file.path(mydir, exename))) {
      stop(exename, " does not exist in ", mydir)
    }
  }

  return(modelnoexe)
}