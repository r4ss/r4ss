#' Calculate the number of columns in `Report.sso`
#'
#' The number of columns is important for the optimization of
#' [SS_output()]. Using a `while` loop, this function
#' finds the optimum width for reading in data as a table and
#' decreases the need for users to pre-specify a width when reading
#' in files.
#'
#' @template file
#' @param nrows Deprecated.
#' @param skip integer: the number of lines of the data file to skip
#' before beginning to read data.
#' @noRd
#' @return
#' An integer value specifying the number of columns in `file`.
#' @author Yukio Takeuchi, Kelli F. Johnson, Ian G. Taylor
#' @seealso [SS_output()]
#'
get_ncol <- function(file, skip = 0, nrows = lifecycle::deprecated()) {
  if (lifecycle::is_present(nrows)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "get_ncol(nrows)",
      details = "Input 'nrows' no longer available."
    )
  }
  nummax <- max(count.fields(file,
    skip = skip, quote = "",
    comment.char = ""
  )) + 1
  return(nummax)
}
