#' Old function to updates r4ss files, deprecated in favor of devtools method
#'
#' Left in place only as a pointer to the new installation approach.
#'
#' @author Ian Taylor
#' @export
#'
update_r4ss_files <- function (local = NULL, save = FALSE, revision = "newest",
                               GitHub = TRUE, override = FALSE){
  stop('\nr4ss is now hosted on GitHub\n',
       'If you install the "devtools" package, you can get updated code as\n',
       'a complete package by running the following command:\n',
       '   devtools::install_github("r4ss/r4ss")\n',
       'Note: warnings on windows computers about missting Rtools can be ignored.\n')
}
