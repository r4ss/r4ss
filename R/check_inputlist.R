#' Check input argument `inputlist`
#'
#' Check the elements of the `inputlist` list used as an argument in
#' `SS_write()` function.
#'
#' @param inputlist List created by the `SS_read()` function with
#' elements "dat", "ctl", "start", "fore", and (optionally) "wtatage".
#' @author Kelli F. Johnson, Ian G. Taylor
#' @return Either TRUE if the input list is valid, or FALSE if not, with
#' a warning about which elements are missing.
#' @seealso [SS_write()]

check_inputlist <- function(inputlist) {
  # list of elements of inputlist
  elements <- c("dat", "ctl", "start", "fore")

  # check for non-empty list
  if (length(inputlist) == 0) {
    stop("inputlist is empty")
  }

  if (!any(elements %in% names(inputlist))) {
    stop("input does not look like a list created by SS_read()")
  }
  # check for whether wtatage is required and if so, add it to the
  # vector of elements
  if ((!is.null(inputlist[["wtatage"]])) & inputlist[["ctl"]][["EmpiricalWAA"]]) {
    elements <- c(elements, "wtatage")
  }

  missingnames <- elements[!elements %in% names(inputlist)]
  if (length(missingnames) > 0) {
    warning(
      "The following elements are missing from the input list: ",
      paste(missingnames, collapse = ", ")
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}
