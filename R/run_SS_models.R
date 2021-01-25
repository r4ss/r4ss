#' Run multiple Stock Synthesis models
#'
#' Loops over a vector of directories and iteratively runs SS in each one
#'
#' @param dir Directory, or vector of directories, containing the
#' model input files. NULL will run the model in the working directory.
#' @param exe Executable name or path to executable (absolute path, or relative
#'  to the working directory). First, if \code{exe_in_path} is
#'  FALSE, The function will look an executable with the same name in each
#'  element of dir. Then, if it is not found in each, the function will
#'  assume that exe is the path to the executable and there is only 1 copy of
#'  the executable. Note that if there is an exe in your PATH with the same
#'  name, this will be used even if \code{exe_in_path} is FALSE.
#' @template extras
#' @param exe_in_path logical. If TRUE, will look for exe in the PATH. If FALSE,
#' will look for exe in the model folders. Default = FALSE.
#' @param skipfinished Skip any folders that already contain a Report.sso file.
#' This can be helpful if the function is interrupted.
#' @template show_in_console
#' @param intern Deprecated. Use `show_in_console` instead.
#' @template verbose
#' @param exe_in_path logical. If TRUE, will look for exe in the PATH. If FALSE,
#' will look for exe in the model folders. Default = FALSE.
#' @return Returns table showing which directories had model run and which
#' had errors like missing executable or Report.sso already present
#' @author Ian Taylor, Kathryn Doering
#' @export
#' @seealso [copy_SS_inputs()],
#' [populate_multiple_folders()]
#' @examples
#' \dontrun{
#' extdata_mods <- system.file("extdata", package = "r4ss")
#' dir <- c(
#'   file.path(extdata_mods, "simple_3.30.12"),
#'   file.path(extdata_mods, "simple_3.30.13")
#' )
#' # if ss or ss.exe is available in both directories:
#' run_SS_models(dir = dir)
#' }
#'
run_SS_models <- function(dir = NULL,
                          exe = "ss",
                          extras = "-nox",
                          exe_in_path = FALSE,
                          skipfinished = TRUE,
                          show_in_console = TRUE,
                          intern = lifecycle::deprecated(),
                          verbose = TRUE,
                          exe_in_path = FALSE) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(intern)) {
    lifecycle::deprecate_warn(
      when = "1.45.1",
      what = "run_SS_models(intern)",
      details = "Please use show_in_console instead"
    )
    show_in_console <- !intern
  }

  # check to make sure the first input is in the correct format
  if (is.null(dir)) {
    dir <- getwd()
  }
  if (!is.character(dir)) {
    stop("Input 'dir' should be NULL a character vector")
  }
  if (!is.logical(show_in_console)) {
    stop("Input 'show_in_console' should be TRUE or FALSE")
  }
  if (!show_in_console & !is.character(console_output_file) ) {
    stop("Input 'console_output_file' should be a character string")
  }
  
  wd_orig <- getwd()
  on.exit(setwd(wd_orig), add = TRUE)

  # vector of NA values to store results
  results <- rep(NA, length(dir))

  # this should always be "windows" or "unix" (includes Mac and Linux)
  OS <- .Platform[["OS.type"]]

  # figure out name of executable based on 'exe' input which may contain .exe
  if (length(grep(".exe", tolower(exe))) == 1) {
    # if input 'exe' includes .exe then assume it's Windows and just use the name
    exe <- exe
  } else {
    # if 'exe' doesn't include .exe then append it (for Windows computers only)
    exe <- paste(exe, ifelse(OS == "windows", ".exe", ""), sep = "")
  }
  if (exe_in_path == TRUE) {
    browser()
    tmp_exe <- Sys.which(exe)[[1]] # get 1st ss exe with name exe that is in your path
    if (tmp_exe == "") {
      stop("Exe named ", exe, " was not found in your PATH.")
    } else {
      exe <- tmp_exe
    }
  } else {
    # check if exe is in PATH, to warn user this will be used by default
    tmp_exe <- Sys.which(basename(exe))[[1]] # get 1st ss exe with name exe that is in your path
    if (tmp_exe != "") {
      warning(
        "A binary named ", basename(exe),
        " was found in your PATH and will be",
        " used by default even though exe_in_path is FALSE. Please remove",
        " the exe in your PATH to avoid this behavior. This binary is",
        " located at ", normalizePath(tmp_exe), "."
      )
    }
    # check if model is in the same dir as each folder of dir vec. If not
    # found in all folders, see if can find using the relative or absolute path.
    # If can't find, revert back to assuming the exe is in each folder of the
    # dir.
    all_exes_in_folder <- lapply(dir, function(dir, mod, OS) {
      exe_exists <- file.exists(file.path(dir, mod))
      exe_exists
    }, mod = exe, OS = OS)
    if (!all(unlist(all_exes_in_folder) == TRUE)) {
      exe_exists_abs_path <- file.exists(normalizePath(exe))
      if (exe_exists_abs_path) {
        message("Assuming path to the exe is provided in the input 'exe'")
        exe <- (normalizePath(exe))
      } else {
        message(
          "Assuming executable is in each dir folder, but missing from ",
          "some folders"
        )
      }
    }
  }

  # loop over directories
  for (idir in seq_along(dir)) {
    # directory where stuff will happen
    dir <- dir[idir]

    # confirm that dir exists
    if (!dir.exists(dir)) {
      warning("not a directory:", dir)
      results[idir] <- "not a directory"
    } else {
      if (skipfinished & "Report.sso" %in% dir(dir)) {
        # skip directories that have results in them
        message("Skipping ", dir, " since it contains",
                " a Report.sso file and skipfinished = TRUE")
        results[idir] <- "contained Report.sso"
      } else {
        # run model
        message("changing working directory to ", dir)
        setwd(dir) # change working directory

        command <- exe
        if (OS != "windows" & !exe_in_path & (basename(exe) == exe)) {
          command <- paste0("./", exe)
        }
        message("Running model in directory: ", getwd())
        message("Using the command: ", command, " ", extras)
        if (!show_in_console) {
          message("Input 'show_in_console' = FALSE, ",
                  "so writing console output to ",
                  console_output_file)
        }
        console_output <- system2(command,
                                  args = extras,
                                  stdout = ifelse(show_in_console,
                                                  "",
                                                  TRUE),
                                  stderr = ifelse(show_in_console,
                                                  "",
                                                  TRUE))
        if (!show_in_console) {
          writeLines(c(
            "###",
            "console output",
            as.character(Sys.time()),
            "###",
            " ",
            console_output
          ),
          con = console_output_file
          )
          message("console output written to ", console_output_file)
        }
        if (isTRUE(console_output > 0)) {
          results[idir] <- "model run failed"
        } else if (isTRUE(console_output == 0)) {
          results[idir] <- "ran model"
        } else {
          results[idir] <- "unknown run status"
        }
        setwd(wd_orig) # needed when using relative paths
      } # end model run
    } # end code for exe present
  } # end loop over directories
  # return table of results
  return(data.frame(dir = dir, results = results))
}
