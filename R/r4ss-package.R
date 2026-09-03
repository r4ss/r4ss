#' r4ss: Current Releases on GitHub
#'
#' The actively maintained r4ss package is distributed from GitHub. See
#' [r4ss_repository()] for the canonical repository URL.
#'
#' @keywords internal
"_PACKAGE"

#' Locate the Current r4ss Package
#'
#' Returns the canonical URL for the actively maintained r4ss package.
#'
#' @return A character string containing the GitHub repository URL.
#' @export
#' @examples
#' r4ss_repository()
r4ss_repository <- function() {
  "https://github.com/r4ss/r4ss"
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "The CRAN release of r4ss is a compatibility package and does not ",
    "contain the Stock Synthesis analysis functions.\n",
    "Install the current release from https://github.com/r4ss/r4ss"
  )
}