#' Rename key Stock Synthesis output files by adding integer value
#'
#' Rename files found with `pattern` by adding `i` to their
#' name before the extension.
#'
#' @details
#' The `.par` file, which is the only file extension searched for
#' with the default entry that does not end in `.sso`, is
#' modified differently.`_i.sso` is added to the file name.
#' @param path Directory where model files are located.
#' @param i An integer value to append to the file name before the
#' `.sso` extension.
#' @template verbose
#' @param pattern A character value specifying the file names to search
#' for in `getwd()`.
#' @author Kelli F. Johnson
#' @returns Invisibly returns a vector of logical values specifying
#' whether or not the file was successfully renamed.
#' @seealso [jitter()]
file_increment <- function(path, i, verbose = FALSE,
                           pattern = "^[CcPRw][a-zA-Z]+\\.sso|summary\\.sso|\\.par$") {
  if (verbose) {
    message("Renaming output files to have names like Report", i, ".sso")
  }

  ignore <- file.copy(
    from = dir(path = path, pattern = pattern, full.names = TRUE),
    to = gsub("par", "par_", gsub(
      "\\.sso|(\\.par)",
      paste0("\\1", 0, ".sso"), dir(path = path, pattern = pattern, full.names = TRUE)
    ))
  )
  return(invisible(ignore))
}
