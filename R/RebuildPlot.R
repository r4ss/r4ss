#' Deprecated function to make plots from Andre Punt's Rebuilder program.
#'
#' The function has been moved to
#' https://github.com/pfmc-assessments/rebuilder.
#' This function was rarely used because it was specific to U.S. west
#' coast groundfish stocks that were overfished and in a rebuilding
#' plan. Therefore there's no need to have it available to all r4ss
#' users.
#' @template deprecated_dots
#'
#' @author Ian G. Taylor
#' @export

DoProjectPlots <- function(...) {
  lifecycle::deprecate_stop(
    when = "1.49.0",
    what = "DoProjectPlots()",
    details = "The function has been moved to https://github.com/pfmc-assessments/rebuilder."
  )
}
