#' Run multiple Stock Synthesis models
#'
#' Loops over a vector of directories and iteratively runs SS in each one
#'
#' @param dirvec List of directories containing the model files
#' @param model Executable name or path to executable (absolute path, or relative
#'  to the working directory). First, if exe_in_path is
#'  FALSE, The function will look an executable with the same name in each
#'  element of dirvec. Then, if it is not found in each, the function will
#'  assume that model is the path to the executable and there is only 1 copy of
#'  the executable. Note that if there is an exe in your PATH with the same
#'  name, this will be used even if exe_in_path is FALSE.
#' @param extras Additional commands to use when running SS. Default = "-nox"
#' will reduce the amount of command-line output.
#' @param systemcmd Should R call SS using "system" function instead of "shell".
#' This may be required when running R in Emacs. Default = FALSE.
#' @param skipfinished Skip any folders that already contain a Report.sso file.
#' This can be helpful if the function is interrupted.
#' @template show_in_console
#' @param intern Deprecated. Use `show_in_console` instead.
#' @template verbose
#' @param exe_in_path logical. If TRUE, will look for exe in the PATH. If FALSE,
#' will look for exe in the model folders. Default = FALSE.
#' @return Returns table showing which directories had model run and which
#' had errors like missing executable or Report.sso already present
#' @author Ian Taylor
#' @export
#' @seealso [copy_SS_inputs()],
#' [populate_multiple_folders()]
#' @examples
#' \dontrun{
#' extdata_mods <- system.file("extdata", package = "r4ss")
#' dirvec <- c(
#'   file.path(extdata_mods, "simple_3.30.12"),
#'   file.path(extdata_mods, "simple_3.30.13")
#' )
#' # if ss or ss.exe is available in both directories:
#' run_SS_models(dirvec = dirvec)
#' }
#'
run_SS_models <- function(dirvec = NULL,
                          model = "ss",
                          extras = "-nox",
                          systemcmd = FALSE,
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
  if (!is.character(dirvec)) {
    stop("Input 'dirvec' should be a character vector")
  }
  wd_orig <- getwd()
  on.exit(setwd(wd_orig), add = TRUE)

  # vector of NA values to store results
  results <- rep(NA, length(dirvec))

  # this should always be "windows" or "unix" (includes Mac and Linux)
  OS <- .Platform[["OS.type"]]

  # figure out name of executable based on 'model' input which may contain .exe
  if (length(grep(".exe", tolower(model))) == 1) {
    # if input 'model' includes .exe then assume it's Windows and just use the name
    exe <- model
  } else {
    # if 'model' doesn't include .exe then append it (for Windows computers only)
    exe <- paste(model, ifelse(OS == "windows", ".exe", ""), sep = "")
  }
  if (exe_in_path == TRUE) {
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
    # If can't find, revert back to assuming the model is in each folder of the
    # dirvec.
    all_exes_in_folder <- lapply(dirvec, function(dir, mod, OS) {
      exe_exists <- file.exists(file.path(dir, mod))
      exe_exists
    }, mod = exe, OS = OS)
    if (!all(unlist(all_exes_in_folder) == TRUE)) {
      exe_exists_abs_path <- file.exists(normalizePath(exe))
      if (exe_exists_abs_path) {
        message("Assuming path to the exe provided in model")
        exe <- (normalizePath(exe))
      } else {
        message(
          "Assuming model is in each dirvec folder, but missing from ",
          "some folders"
        )
      }
    } else {
      message("Assuming model is in each dirvec folder.")
    }
  }

  # loop over directories
  for (idir in seq_along(dirvec)) {
    # directory where stuff will happen
    dir <- dirvec[idir]

    # confirm that dir exists
    if (!dir.exists(dir)) {
      warning("not a directory:", dir)
      results[idir] <- "not a directory"
    } else {
      if (skipfinished & "Report.sso" %in% dir(dir)) {
        # skip directories that have results in them
        message("Skipping ", dir, " since it contains a Report.sso file and skipfinished=TRUE")
        results[idir] <- "contained Report.sso"
      } else {
        # run model
        message("changing working directory to ", dir)
        setwd(dir) # change working directory

        command <- paste(exe, extras)
        if (OS != "windows" & (basename(exe) == exe)) {
          command <- paste0("./", command)
        }
        message("Running model in directory: ", getwd())
        message("Using the command: ", command)
        if (OS == "windows" & !systemcmd) {
          console.output <- shell(cmd = command, intern = !show_in_console)
        } else {
          console.output <- system(command, intern = !show_in_console, show.output.on.console = show_in_console)
        }
        if (!show_in_console) {
          writeLines(c(
            "###",
            "console output",
            as.character(Sys.time()),
            "###",
            " ",
            console.output
          ),
          con = "console.output.txt"
          )
          message("console output written to console.output.txt")
        }
        if (isTRUE(console.output > 0)) {
          results[idir] <- "model run failed"
        } else if (isTRUE(console.output == 0)) {
          results[idir] <- "ran model"
        } else {
          results[idir] <- "unknown run status"
        }
        setwd(wd_orig) # needed when using relative paths
      } # end model run
    } # end code for exe present
  } # end loop over directories
  # return table of results
  return(data.frame(dir = dirvec, results = results))
}
