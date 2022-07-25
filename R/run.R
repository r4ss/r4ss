#' Run one or more Stock Synthesis models
#'
#' Checks for presence of a Stock Synthesis executable and then runs the
#' model with any additional arguments specified by `extras`. If input
#' `dir` is a vector, it iteratively runs the SS3 model in each one.
#' Parallelization is not available at this time.
#'
#' @param dir Directory, or vector of directories, containing the
#' model input files.
#' @template exe
#' @template extras
#' @param skipfinished Skip any folders that already contain a Report.sso file.
#' This can be helpful if the function is interrupted.
#' @template show_in_console
#' @param console_output_file File to store console output (if
#' show_in_console = FALSE)
#' @template verbose
#'
#' @return Returns a table showing which directories had models run and which
#' had errors like missing executable or Report.sso already present.
#' The three possible messages are "model run failed", "ran model", and
#' "unknown run status".
#'
#' @description The `run()` function checks for the executable via
#' [r4ss::check_exe()]. This involves first checking first checking the
#' specified 
#' directory `dir` for the specified SS3 executable name. If it is not
#' found in the specified
#' directory, then it checks the PATH. Linux systems may have an
#' existing executable utility `/usr/sbin/ss` in the path. If `exe =
#' "ss"` and this file is found by [check_exe()], it will be ignored
#' based on the smaller file size relative to the SS3 executable. Linux
#' users who want to use the workflow of having SS3 in their PATH should
#' name the SS3 file something besides `ss`, such as `ss3` or
#' `ss_linux`.
#' 
#' @author Ian G. Taylor, Kathryn I. Doering
#' @export
#' @seealso [copy_SS_inputs()],
#' [populate_multiple_folders()]
#' @examples
#' \dontrun{
#' dir <- file.path(system.file("extdata", package = "r4ss"), "simple_small")
#' r4ss::run(dir = dir)
#' }
#'
run <- function(dir = getwd(),
                exe = "ss",
                extras = "",
                skipfinished = TRUE,
                show_in_console = TRUE,
                console_output_file = "console.output.txt",
                verbose = TRUE) {
  # check to make sure the first input is in the correct format
  if (!is.character(dir)) {
    stop("Input 'dir' should be a character vector")
  }
  if (!is.logical(show_in_console)) {
    stop("Input 'show_in_console' should be TRUE or FALSE")
  }
  if (!show_in_console & !is.character(console_output_file)) {
    stop("Input 'console_output_file' should be a character string")
  }

  wd_orig <- getwd()
  on.exit(setwd(wd_orig), add = TRUE)

  # vector of NA values to store results
  results <- rep(NA, length(dir))

  # this should always be "windows" or "unix" (includes Mac and Linux)
  OS <- .Platform[["OS.type"]]

  # sort out path to executable
  check_exe_results <- check_exe(dir = dir, exe = exe, verbose = verbose)
  command <- check_exe_results[["exe"]]

  # loop over directories
  for (idir in seq_along(dir)) {
    # directory where stuff will happen
    dir <- dir[idir]

    # confirm that dir exists
    if (!dir.exists(dir[idir])) {
      warning("not a directory:", dir[idir])
      results[idir] <- "not a directory"
    } else {
      if (skipfinished & "Report.sso" %in% dir(dir[idir])) {
        # skip directories that have results in them
        message(
          "Skipping ", dir[idir], " since it contains",
          " a Report.sso file and skipfinished = TRUE"
        )
        results[idir] <- "contained Report.sso"
      } else {
        # run model
        if (verbose) {
          message("changing working directory to ", dir[idir])
        }
        setwd(dir[idir]) # change working directory

        if (verbose) {
          message(
            "Running model in directory: ", getwd(), "\n",
            "Using the command: ", command, " ", extras
          )
        }
        if (!show_in_console) {
          if (verbose) {
            message(
              "Input 'show_in_console' = FALSE, ",
              "so writing console output to ",
              console_output_file
            )
          }
        }
        console_output <- system2(command,
          args = extras,
          stdout = ifelse(show_in_console,
            "",
            TRUE
          ),
          stderr = ifelse(show_in_console,
            "",
            TRUE
          )
        )
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
          if (verbose) {
            message("console output written to ", console_output_file)
          }
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
