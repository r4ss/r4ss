#' Find location of executable within path or specified directory
#'
#' Check that the executable name provided in `model`,
#' an input argument to numerous `r4ss` functions,
#' does not contain the extension and is available.
#'
#' @template exe
#' @param dir The directory where `exe` is located (if not in path). Can
#' also be a vector of directories.
#' @template verbose
#' @author Kelli F. Johnson, Ian G. Taylor
#' @return A list containing the cleaned `exe` name based on the input
#' argument and the path (or paths if `dir` is a vector) to where it is found.

check_exe <- function(exe = "ss", dir = getwd(), verbose = FALSE) {
  # check to make sure the first input is in the correct format
  if (!is.character(dir)) {
    stop("Input 'dir' should be a character vector")
  }
  # remove extension from exe (if present)
  exe_no_extension <- gsub("\\.exe$", "", exe)

  # Check that the file exists
  os <- ifelse(grepl("windows", .Platform[["OS.type"]], ignore.case = TRUE),
    "windows", "linux"
  )
  exename <- paste0(
    exe_no_extension,
    switch(os,
      windows = ".exe",
      linux = ""
    )
  )
  # look for exe in path
  path_to_exe <- Sys.which(exename)[[1]]
  if (path_to_exe != "") {
    # if exe not found in path
    # normalize path and remove exe name from the end
    # e.g. convert "C:\SS\SSB672~1.01_\ss.exe" to "C:/SS/SSv3.30.19.01_Apr15"
    path_to_exe <- dirname(normalizePath(path_to_exe))
    if (verbose) {
      message("Executable found in path at ", path_to_exe)
    }
  } else {
    # if not found in path then
    # check for exe in specified directory (or directories)
    for (idir in seq_along(dir)) {
      if (file.exists(file.path(dir[idir], exename))) {
        path_to_exe[idir] <- dir[idir]
        if (verbose) {
          message("Executable found in directory ", path_to_exe)
        }
      } else {
        # if not in path or specified directory, create error
        stop(exename, " not found in ", dir[idir], " nor in the path.")
      }
    }
  }

  # return list of exe name and path to exe
  list(
    exe = exename,
    path = path_to_exe
  )
}
