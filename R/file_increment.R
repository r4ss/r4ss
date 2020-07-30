#' Rename Stock Synthesis files by adding integer value
#' 
#' Rename files found with \code{pattern} by adding \code{i} to their
#' name before the extension.
#' 
#' @details
#' The \code{.par} file, which is the only file extension searched for
#' with the default entry that does not end in \code{.sso}, is
#' modified differently.\code{_i.sso} is added to the file name.
#' @param i An integer value to append to the file name before the
#' \code{.sso} extension.
#' @template verbose
#' @param pattern A character value specifying the file names to search
#' for in \code{getwd()}.
#' @author Kelli Faye Johnson
#' @returns Invisibly returns a vector of logical values specifying
#' whether or not the file was successfully renamed.
#'
file_increment <- function(i, verbose = FALSE,
  pattern = "^[CcPRw][a-zA-Z]+\\.sso|summary\\.sso|\\.par$"){
  if (verbose) {
    message("Renaming output files to have names like Report", i, ".sso")
  }
  ignore <- file.rename(from=dir(pattern=pattern),
    to=gsub("par", "par_", gsub("\\.sso|(\\.par)",
    paste0("\\1", i, ".sso"), dir(pattern=pattern))))
  return(invisible(ignore))
}
