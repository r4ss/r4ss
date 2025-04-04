#' Directory check
#'
#' @details
#' Check that
#' 1. The user knows that the data will not be saved if `dir = NULL`.
#' 2. The directory exists if it can be created.
#' 3. The function fails if the directory cannot be created.
#'
#' Note: this function was copied from the {nwfscSurvey} package:
#' https://github.com/pfmc-assessments/nwfscSurvey/blob/main/R/check_dir.R
#' rather than adding an additional dependency on that package.
#'
#' @template dir
#' @template verbose
#'
#' @author Chantel R. Wetzel
#' @export
#'
#' @examples
#' check_dir(getwd(), verbose = FALSE)
#' # See more output
#' check_dir(getwd())
#'
check_dir <- function(dir, verbose = TRUE) {
  if (is.null(dir) || length(dir) == 0) {
    if (verbose) {
      cli::cli_alert_info("Output will not be saved in dir because dir = NULL.")
    }
  } else {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    if (!file.exists(dir)) {
      cli::cli_abort(
        "[ENOENT] Failed to make the following directory:
          '{dir}'"
      )
    }
  }
}
