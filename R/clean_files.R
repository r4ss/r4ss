#' Clean unnecessary Stock Synthesis output files
#'
#' Lists or deletes temporary and output files produced by ADMB and Stock
#' Synthesis. Only files directly within `dir` are considered; subdirectories
#' are not searched or removed.
#'
#' @param dir Directory containing the Stock Synthesis output files.
#' @param action One or both of `"list"` and `"delete"`. If `"list"` is
#'   included, matching files are printed and returned. If `"delete"` is
#'   included, matching files are deleted.
#'
#' @return A character vector containing the paths of matching files. The
#'   result is returned invisibly when `action` does not include `"list"`.
#' @export
#' @family run functions
#'
#' @examples
#' model_dir <- tempdir()
#' clean_files(model_dir, action = "list")
clean_files <- function(dir, action = c("list", "delete")) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    cli::cli_abort(
      "Input {.arg dir} should be a character string for a directory."
    )
  }
  if (!dir.exists(dir)) {
    cli::cli_abort("Directory does not exist: {.path {dir}}")
  }
  if (
    !is.character(action) ||
      length(action) == 0L ||
      anyNA(action) ||
      any(!action %in% c("list", "delete"))
  ) {
    cli::cli_abort(
      "Input {.arg action} should contain only {.val list} and/or {.val delete}."
    )
  }

  patterns <- c(
    "*.p0*",
    "*.b0*",
    "*.r0*",
    "*.std",
    "*.htp",
    "*.eva",
    "*.tds",
    "*.bar",
    "*.cov",
    "*.dep",
    "*.hes",
    "*.rpt",
    "*.rep",
    "*.cpp",
    "*.log",
    "*.obj",
    "*.tmp",
    "*.ecm",
    "*.mc2",
    "*.mcm",
    "*.hst",
    "*.psv",
    "gradient.*",
    "tmp_admb",
    "variance",
    "SIS_table.sso",
    "rebuild.sso",
    "posterior_obj_func.sso",
    "posterior_vectors.sso",
    "posteriors.sso",
    "ParmTrace.sso",
    "runnumber.ss"
  )

  # check posteriors.sso and if there are more than 100 lines,
  # don't delete any of the MCMC-related files
  posteriors_file <- file.path(dir, "posteriors.sso")
  if (file.exists(posteriors_file)) {
    num_lines <- length(readLines(posteriors_file, warn = FALSE))
    if (num_lines > 100) {
      patterns <- patterns[!grepl("posterior.*", patterns)]
      patterns <- patterns[!grepl("*.psv", patterns)]
      cli::cli_alert_info(
        "Found more than 100 lines in posteriors.sso, skipping deletion of MCMC-related files"
      )
    }
  }

  # Convert glob patterns to regular expressions for grepl
  pattern <- paste(
    vapply(patterns, utils::glob2rx, character(1L)),
    collapse = "|"
  )
  # get all files in the directory
  files <- list.files(
    path = dir, # Directory to list files from
    all.files = TRUE, # Include hidden files
    full.names = TRUE, # Return the full paths of the files
    no.. = TRUE # Exclude the special entries "." and ".."
  )
  # filter out directories and keep only files matching the pattern
  files <- files[
    !file.info(files)[["isdir"]] &
      grepl(pattern, basename(files), ignore.case = TRUE)
  ]

  # if length = 0, provide a message indicating there are no files to delete
  if (length(files) == 0L) {
    cli::cli_alert_info("No matching files found to delete.")
  }

  # Perform the requested action: delete
  if ("delete" %in% action && length(files) > 0L) {
    deleted <- file.remove(files)
    if (any(!deleted)) {
      cli::cli_abort("Failed to delete {sum(!deleted)} matching file{?s}.")
    }
  }
  cli::cli_alert_success(
    "Successfully deleted {length(files)} matching file{?s}."
  )
  # Return the list of files (invisibly if not requested via "list")
  if ("list" %in% action & length(files) > 0L) {
    basename(files)
  } else {
    invisible(basename(files))
  }
}
