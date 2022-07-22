#' Find location of executable within path or specified directory
#'
#' Check that the Stock Synthesis executable name provided in `exe`,
#' an input argument to numerous `r4ss` functions is available in the
#' location specified by `dir` or in the path.
#'
#' @template exe
#' @param dir The directory where `exe` is located (if not in path). Can
#' also be a vector of directories.
#' @template verbose
#' @author Kelli F. Johnson, Ian G. Taylor
#' @return A list containing the cleaned `exe` name based on the input
#' argument and the path (or paths if `dir` is a vector) to where it is found.
#' 
#' @description The `check_exe()` function first checks the PATH for the
#' specified SS3 executable name and returns the file's location if
#' found. If the SS3 executable is not found in the PATH, then it is
#' assumed the that executable exists in the specified directory. Linux
#' systems have an existing executable utility `/usr/sbin/ss` in the
#' path. If `exe = "ss"` and this file is found by `check_exe()``, it
#' will be ignored based on the smaller file size relative to the SS3
#' executable. Linux users who want to use the workflow of having SS3 in
#' their PATH should name the SS3 file something besides `ss`, such as
#' `ss3` or `ss_linux`.

check_exe <- function(exe = "ss", dir = getwd(), verbose = FALSE) {
  # check to make sure the first input is in the correct format
  if (!is.character(dir)) {
    stop("Input 'dir' should be a character vector")
  }
  # remove extension from exe (if present)
  exe_no_extension <- gsub("\\.exe$", "", exe)

  # Check that the file exists
  exename <- paste0(
    exe_no_extension,
    switch(.Platform[["OS.type"]],
      windows = ".exe",
      unix = ""
    )
  )
  # look for exe in path
  path_to_exe <- Sys.which(exename)[[1]]
  # set flag for exe in path
  exe_in_path <- path_to_exe != ""
    
  if (exe_in_path) {
    # if exe is found in path
    # make sure it has a size that makes sense for Stock Synthesis
    # (linux systems have a command line tool called "ss" in a location
    # like /usr/sbin/ but it's size is much smaller (about 100k vs 7MB)
    if (file.info(path_to_exe)[["size"]] < 1e6) {
      if (verbose) {
        message("Executable found in path that isn't Stock Synthesis:",
          path_to_exe
        )
      }
      exe_in_path <- FALSE
    } else {
      # normalize path and remove exe name from the end
      # e.g. convert "C:\SS\SSB672~1.01_\ss.exe" to "C:/SS/SSv3.30.19.01_Apr15"
      path_to_exe <- dirname(normalizePath(path_to_exe))
      if (verbose) {
        message("Executable found in path at ", path_to_exe)
      }
    }
  }

  if (!exe_in_path) {
    # if exe not found in path then check for exe in specified directory
    # (or directories)
    for (idir in seq_along(dir)) {
      if (file.exists(file.path(dir[idir], exename))) {
        path_to_exe[idir] <- dir[idir]
        if (verbose) {
          message("Executable found in directory ", path_to_exe)
        }
        # add ./ to exename so it knows to run in the current directory
        if (.Platform[["OS.type"]] == "unix") {
          exename <- paste0("./", exename)
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
