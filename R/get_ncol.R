#' Calculate the number of columns in \code{Report.sso}
#'
#' The number of columns is important for the optimization of 
#' \code{\link{SS_output}}. Using a \code{while} loop, this function
#' finds the optimum width for reading in data as a table and
#' decreases the need for users to pre-specify a width when reading
#' in files.
#'
#' @param file A file path that will be read using 
#' \code{\link[utils]{read.table}}.
#' @noRd
#' @return
#' An integer value specifying the number of columns in \code{file}.
#' @author Kelli Faye Johnson
#' @seealso \code{\link{SS_output}}
#'
get_ncol <- function(file) {
  numcol <- list("yes")
  initial <- 100
  while (!all(numcol[length(numcol)] == "")) {
    numcol <- utils::read.table(file,
      col.names = 1:initial, fill = TRUE, quote = "",
      colClasses = "character", nrows = -1, comment.char = "")
    initial <- initial + 100
  }
  nummax <- max(which(
    apply(numcol, 2, function(x) all(x == "")) == FALSE)) + 1
  return(nummax)
}
