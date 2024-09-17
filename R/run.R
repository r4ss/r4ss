#' Run a Stock Synthesis model
#'
#' Checks for presence of a Stock Synthesis executable and then runs the
#' model with any additional arguments specified by `extras`.
#'
#' @param dir Directory containing the model input files.
#' @template exe
#' @template extras
#' @param skipfinished Skip any folders that already contain a Report.sso file.
#' This can be helpful if the function is interrupted while running iteratively.
#' @param show_in_console Show output in the R console? If FALSE,
#' then the console output is saved to a file (specified by
#' `console_output_file`) at the end of the model run.
#' @param console_output_file File to store console output (if
#' show_in_console = FALSE).
#' @template verbose
#'
#' @return Returns one of five messages:
#' "ran model", "model run failed", "unknown run status", "not a
#' directory", or "contained Report.sso".
#'
#' @description The `run()` function checks for the executable via
#' [r4ss::check_exe()]. This involves first checking the
#' specified
#' directory `dir` for the specified SS3 executable name. If it is not
#' found in the specified
#' directory, then it checks the PATH. Linux systems may have an
#' existing executable utility `/usr/sbin/ss` in the path. If `exe =
#' "ss3"` and this file is found by [check_exe()], it will be ignored
#' based on the smaller file size relative to the SS3 executable. Linux
#' users who want to use the workflow of having SS3 in their PATH should
#' name the SS3 file something besides `ss`, such as `ss3` or
#' `ss_linux`.
#'
#' @author Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson
#' @export
#' @family run functions
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", "simple_small", package = "r4ss")
#' r4ss::run(dir = dir)
#' }
#'
run <- function(dir = getwd(),
                exe = "ss3",
                extras = "",
                skipfinished = TRUE,
                show_in_console = FALSE,
                console_output_file = "console.output.txt",
                verbose = TRUE) {
  # check to make sure the first input is in the correct format
  if (!is.character(dir)) {
    stop("Input 'dir' should be a character string")
  }
  if (!is.logical(show_in_console)) {
    stop("Input 'show_in_console' should be TRUE or FALSE")
  }
  if (!show_in_console & !is.character(console_output_file)) {
    stop("Input 'console_output_file' should be a character string")
  }
  if (length(dir) > 1) {
    lifecycle::deprecate_stop(
      when = "1.46.2",
      what = "run(dir = 'must be a single directory')",
      details = "Please use the {purrr} package or other tools to apply run() to multiple directories"
    )
  }

  wd_orig <- getwd()
  on.exit(setwd(wd_orig), add = TRUE)

  # sort out path to executable
  check_exe_results <- check_exe(dir = dir, exe = exe, verbose = verbose)
  command <- check_exe_results[["exe"]]

  # confirm that dir exists
  if (!dir.exists(dir)) {
    warning("not a directory:", dir)
    results <- "not a directory"
  } else {
    if (file.exists(file.path(dir, "Report.sso")) && skipfinished) {
      # skip directories that have results in them
      message(
        "Skipping ", dir, " because it contains",
        " a Report.sso file and skipfinished = TRUE"
      )
      results <- "contained Report.sso"
    } else {
      # run model
      setwd(dir) # change working directory
      # provide some messages
      if (verbose) {
        message(
          "Changing working directory to ", dir,
          " and running model using the command: ", command, " ", extras
        )
      }
      if (!show_in_console && verbose) {
        message(
          "Input 'show_in_console' = FALSE, ",
          "so writing console output to ",
          console_output_file
        )
      }
      # call system2() to actually run the model
      console_output <- tryCatch(
        system2(
          command = command,
          args = extras,
          stdout = ifelse(show_in_console,
            "",
            TRUE
          ),
          stderr = ""
        ),
        error = function(err) {
          if (grepl("'CreateProcess' failed to run", err)) {
            stop(
              "There is a problem with the SS3 executable, perhaps due to mismatch ",
              "with the operating system. Please make sure that you have the correct",
              "executable and it is named appropriately for your operating system"
            )
          } else {
            err
          }
        }
      )

      # write console output to file if not shown in console
      if (!show_in_console) {
        writeLines(
          c(
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
      # determine if run finished
      # console_output will either be a character vector of all the
      # lines of output, or a code returned by system2() which will be
      # 0 if the run completed with no issues
      # various other possible codes if the run fails
      results <- dplyr::case_when(
        grepl("Run has completed", tail(console_output, 1)) ~ "ran model", # 3.30.19 and earlier
        grepl("Finished running model", tail(console_output, 1)) ~ "ran model", # 3.30.20 format
        grepl("Fatal Error", tail(console_output, 5)) ~ "model run failed",
        console_output[1] == 0 ~ "ran model",
        console_output[1] > 0 ~ "model run failed",
        TRUE ~ "unknown run status"
      )
    } # end model run
  } # end code for exe present

  return(results)
}
