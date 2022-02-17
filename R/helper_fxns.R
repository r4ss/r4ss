# contains small helper functions that can be used across multiple functions in
# the r4ss pkg


#' Get the name of the data .ss_new file in a directory
#'
#' In previous versions of Stock Synthesis,
#' the file new data file was named `data.ss_new`.
#' `_echo` was added to the name when the file was parsed into three separate
#' files.
#' @param dir Relative or absolute path to a directory
#' @return A string with the name of the data .ss_new file. If not found, will
#'  be NA. Both of strings are searched for using `dir(pattern = )` and
#'  if both exist, then `data_echo.ss_new` is returned.
#'
get_dat_new_name <- function(dir) {
  datname <- tail(
    dir(path = dir, pattern = "data_?e?c?h?o?\\.ss_new"),
    1
  )
  ifelse(length(datname) == 0, NA, datname)
}


#' Open png device and return info on the file being created
#'
#' this was previously contained within each of the SSplotXXX() functions.
#' It (1) translates the not-quite-matching specifications for the image to the
#' values needed by png(), then (2) returns the plotinfo data.frame
#' (which exists within each function which calls this) after adding a row
#' with the filename and caption for each new plot
#' Note: this just opens the png device which needs to be closed via dev.off()
#' outside this function.
#'
#' @param plotinfo table of information about all plots
#' @param file filename to write to (including .png extension)
#' @param plotdir directory where plots will be written
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @param caption caption for the image
#' @param alt_text alternative text for screen readers
#' (if left as NA then will be set based on the caption)
#' @author Ian G. Taylor

save_png <- function(plotinfo,
                     file,
                     plotdir,
                     pwidth,
                     pheight,
                     punits,
                     res,
                     ptsize,
                     caption = NA,
                     alt_text = NA) {

  # replace any slashes (as in 'Eggs/kg_inter_Fem')
  file <- gsub(pattern = "/", replacement = "_per_", x = file, fixed = TRUE)

  # open png device
  png(
    filename = file.path(plotdir, file),
    width = pwidth,
    height = pheight,
    units = punits,
    res = res,
    pointsize = ptsize
  )

  # change graphics parameters to input value
  if (!is.null(par)) {
    par(par)
  }

  # assemble and return info
  invisible(rbind(plotinfo, data.frame(
    file = file,
    caption = caption,
    alt_text = alt_text
  )))
}
