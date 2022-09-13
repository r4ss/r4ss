#' Find location of executable within path or specified directory
#'
#' Check that the Stock Synthesis executable name provided in `exe`,
#' an input argument to numerous `r4ss` functions is available in the
#' location specified by `dir` or in the path.
#'
#' @template exe
#' @param dir The directory where `exe` is located (if not in path).
#' Defaults to `getwd()` but can be an absolute path, a path relative to
#' the working directory or a path relative to a directory that's in the
#' PATH. Can also be a vector of directories.
#' @template verbose
#' @author Kelli F. Johnson, Ian G. Taylor
#' @return A list containing `$exe` and `$path`.
#' `$exe` is the cleaned version of the `exe` file name input. Windows
#' systems will include
#' ".exe" in the returned value. On Linux and Mac systems, the returned
#' `$exe` will include "./" if the executable was found in the specified
#' directory `dir`. This will be a single character string, unlike `$path` which
#' will be a vector if the input `dir` is a vector.
#' The `$path` element of the list includes the normalized path (or
#' paths if `dir` is a vector) where the executable was found. If `dir`
#' is a vector and the executable is missing from a subset of those
#' directories, NA is returned for those elements of `$path`.
#' If the specified `exe` input is not found in any of the `dir` input
#' values nor in the PATH, then the function stops with an error.
#'
#' @export
#' @seealso [run()]
#' @examples
#' \dontrun{
#' # check for executable called "ss" or "ss.exe" in the PATH
#' check_exe()
#' # check for executable with a different name in the PATH
#' check_exe(exe = "ss_win")
#' # check for executable in a specific directory
#' check_exe(exe = "ss_linux", dir = "~/ss/ss_v3.30.19.01")
#' }
#' @description The `check_exe()` function first checks the specified
#' directory `dir` for the specified SS3 executable name and returns the
#' file's location if found. If it is not found in the specified
#' directory, then it checks the PATH. Linux systems may have an
#' existing executable utility `/usr/sbin/ss` in the path. If `exe =
#' "ss"` and this file is found by `check_exe()``, it will be ignored
#' based on the smaller file size relative to the SS3 executable. Linux
#' users who want to use the workflow of having SS3 in their PATH should
#' name the SS3 file something besides `ss`, such as `ss3` or
#' `ss_linux`.

check_exe <- function(exe = "ss", dir = getwd(), verbose = FALSE) {
  # check to make sure the first input is in the correct format
  if (!is.character(exe)) {
    stop("Input 'exe' should be a character vector")
  }
  if (!is.character(dir)) {
    stop("Input 'dir' should be a character vector")
  }
  # remove extension from exe (if present)
  exe_no_extension <- gsub("\\.exe$", "", exe)

  # exe name with extension added back on Windows
  exename <- paste0(
    exe_no_extension,
    switch(.Platform[["OS.type"]],
      windows = ".exe",
      unix = ""
    )
  )
  # path.expand will resolve any use of "~" in input exe
  # if exename doesn't include any path info (e.g. "ss")
  # it will remain unchanged
  exename <- path.expand(exename)

  # placeholder to store path(s)
  path_to_exe <- NULL
  # check for exe in specified directory (or directories)
  for (idir in seq_along(dir)) {
    if (file.exists(file.path(dir[idir], exename))) {
      path_to_exe[idir] <- path.expand(dir[idir])
      if (verbose) {
        message("Executable found in directory ", path_to_exe)
      }
      # add ./ to exename so it knows to run in the current directory
      # but only if the exename doesn't include additional directory
      # information
      if (.Platform[["OS.type"]] == "unix" && basename(exename) == exename) {
        exename <- paste0("./", exename)
      }
    }
  }

  # if exe wasn't found in specified directory, look in PATH
  if (is.null(path_to_exe)) {
    path_to_exe <- Sys.which(exename)[[1]]

    # if exe is found in PATH
    if (path_to_exe != "") {
      # make sure it has a size that makes sense for Stock Synthesis
      # (linux systems have a command line tool called "ss" in a location
      # like /usr/sbin/ but it's size is much smaller (about 100k vs 7MB)
      if (file.info(path_to_exe)[["size"]] < 1e6) {
        if (verbose) {
          message(
            "Executable found in PATH that isn't Stock Synthesis: ",
            path_to_exe
          )
        }
        path_to_exe <- ""
      } else {
        if (verbose) {
          message("Executable found in PATH at ", path_to_exe)
        }
        # if found in PATH and file size is large enough,
        # then normalize path and remove exe name from the end
        # e.g. convert "C:\\SS\\SSB672~1.01_\\ss.exe" to "C:/SS/SSv3.30.19.01_Apr15"
        path_to_exe <- dirname(normalizePath(path_to_exe))
      }
    }
    if (path_to_exe == "") {
      # if not in path or specified directory, create error
      stop(
        exename, " not found in ",
        ifelse(test = length(dir) == 1,
          yes = dir, # spell out directory in error if `dir` isn't a vector
          no = "any of the input 'dir' values"
        ), # generic if it's a vector
        " nor in the path."
      )
    }
  } # end check for is.null(path_to_exe)

  # return list of exe name and path to exe
  list(
    exe = exename,
    path = path_to_exe
  )
}
