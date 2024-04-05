# contains small helper functions that can be used across multiple functions in
# the r4ss pkg


#' Get the name of the data .ss_new file in a directory
#'
#' In previous versions of Stock Synthesis,
#' the file new data file was named `data.ss_new`.
#' `_echo` was added to the name when the file was parsed into three separate
#' files.
#' @template dir
#' @return A string with the name of the data .ss_new file. If not found, will
#'  be NA. Both of strings are searched for using `dir(pattern = )` and
#'  if both exist, then `data_echo.ss_new` is returned.
#' @seealso [get_par_name]
#'
get_dat_new_name <- function(dir) {
  datname <- tail(
    dir(path = dir, pattern = "data_?e?c?h?o?\\.ss_new"),
    1
  )
  ifelse(length(datname) == 0, NA, datname)
}

#' Get the name of the .par file in a directory
#'
#' In previous versions of Stock Synthesis,
#' @template dir
#' @template verbose
#' @return A string with the name of the .par file. If not found, will
#'  be NA. If multiple files exist, preference is given to ss3.par
#' (default as of 3.30.22.1), followed by ss.par, followed by the most recently
#' modified file with a *.par extension (choosing the first if two were modified
#' at the same time).
#' @seealso [get_dat_new_name]

get_par_name <- function(dir, verbose = TRUE) {
  parfile <- dir(dir, pattern = ".par$")

  # if no .par file found, return NA
  if (length(parfile) == 0) {
    return(NA)
  }

  # if more than 1 .par file
  if (length(parfile) > 1) {
    # get info on all files that match the pattern
    parinfo <- file.info(file.path(dir, parfile))

    parfile <- dplyr::case_when(
      any(parfile == "ss3.par") ~ "ss3.par", # first choice
      any(parfile == "ss.par") ~ "ss.par", # second choice
      # last choice is most recently changed file (excluding directories)
      TRUE ~ parfile[!parinfo[["isdir"]] &
        parinfo[["mtime"]] == max(parinfo[["mtime"]][!parinfo[["isdir"]]])][1]
    )

    if (verbose) {
      message(
        "Multiple files in directory match pattern *.par, choosing based on the",
        " preferences described in the help for get_par_name(): ", parfile
      )
    }
  }

  # return file name as a string
  return(parfile)
}


#' Allow Multi-Plots
#' Set the par() to options suitable for ss3diags multi plots.
#'
#' See \link[graphics]{par} for more details on each parameter.
#'
#' @param mfrow determines plot frame set up
#' @param plot.cex cex graphic option
#' @param mai graphical par for plot margins
#' @param labs if TRUE margins are narrow
#' @param omi Outer margins in lines of text.
#'
#' @keywords ssplot utils
#'
#' @export
#'
sspar <- function(mfrow = c(1, 1),
                  plot.cex = 1,
                  mai = c(0.55, 0.6, 0.1, .1),
                  omi = c(0., 0., 0., 0) + 0.1,
                  labs = TRUE) {
  if (labs == F) {
    mai <- c(0.25, 0.25, 0.15, .15)
    omi <- c(0.3, 0.35, 0.2, 0.2)
  }
  par(list(mfrow = mfrow, mai = mai, mgp = c(2., 0.5, 0), omi = omi, tck = -0.02, cex = plot.cex))
}

#' Add legend to plots
#'
#' ss3diags function to add legend to plots
#'
#' @param legendlabels Optional vector of labels to include in legend.
#' @template legendloc
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendncol Number of columns for the legend.
#' @param legendcex Allows to adjust legend cex. Defaults to 1.
#' @param legendsp Space between legend labels
#' @param col Optional vector of colors to be used for lines. Input NULL
#' @param pch Optional vector of plot character values
#' @param pt.cex Adjust the cex of points.
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#'
#' @export
#'
#'
add_legend <- function(legendlabels,
                       legendloc = "topleft",
                       legendorder = NULL,
                       legendncol = 1,
                       legendcex = 1,
                       legendsp = 0.9,
                       col = NULL,
                       pch = NULL,
                       pt.cex = 0.7,
                       lty = 1,
                       lwd = 2,
                       type = "l") {
  if (is.null(legendorder)) {
    legendorder <- seq_along(legendlabels)
  }
  if (is.numeric(legendloc)) {
    Usr <- par()$usr
    legendloc <- list(
      x = Usr[1] + legendloc[1] * (Usr[2] - Usr[1]),
      y = Usr[3] + legendloc[2] * (Usr[4] - Usr[3])
    )
  }

  # if type input is "l" then turn off points on top of lines in legend
  legend.pch <- -1
  if (type == "l") {
    legend.pch <- rep(NA, length(pch))
  }
  legend(legendloc,
    legend = legendlabels[legendorder],
    col = col[legendorder],
    lty = lty[legendorder],
    seg.len = 2,
    lwd = lwd[legendorder],
    pch = legend.pch[legendorder],
    bty = "n",
    ncol = legendncol,
    pt.cex = pt.cex[legendorder],
    cex = legendcex,
    y.intersp = legendsp
  )
}

#' Make a vector of colors.
#'
#' A subset of rich.colors by Arni Magnusson from the gplots package, with the
#' addition of alpha transparency (which is now available in the gplots version
#' as well)
#'
#'
#' @param n Number of colors to generate.
#' @param alpha Alpha transparency value for all colors in vector. Value is
#' passed to rgb function.
#' @author Arni Magnusson, Ian Taylor
#' @export
rich.colors.short <- function(n, alpha = 1) {
  x <- seq(0, 1, length = n)
  r <- 1 / (1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15) / max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha = alpha))
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
#' @template plotdir
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @param caption caption for the image
#' @param alt_text alternative text for screen readers
#' (if left as NA then will be set by SS_html() based on the caption)
#' @param filenameprefix Additional text to append to PNG or PDF file names.
#' It will be separated from default name by an underscore.
#' @author Ian G. Taylor
#'
#' @export

save_png <- function(plotinfo,
                     file,
                     plotdir,
                     pwidth,
                     pheight,
                     punits,
                     res,
                     ptsize,
                     caption = NA,
                     alt_text = NA,
                     filenameprefix = NA) {
  # replace any slashes (as in 'Eggs/kg_inter_Fem')
  file <- gsub(pattern = "/", replacement = "_per_", x = file, fixed = TRUE)
  if (!is.na(filenameprefix)) {
    file <- paste0(filenameprefix, file)
  }

  # open png device
  png(
    filename = file.path(plotdir, file),
    width = pwidth,
    height = pheight,
    units = punits,
    res = res,
    pointsize = ptsize
  )

  # assemble and return info
  invisible(rbind(plotinfo, data.frame(
    file = file,
    caption = caption,
    alt_text = alt_text
  )))
}

#' Get default vector of colors for each area
#'
#' this was previously contained within SS_plots() and 4 of the SSplotXXX()
#' functions.
#'
#' @template areacols
#' @param nareas number of areas
#' @author Ian G. Taylor

get_areacols <- function(areacols, nareas) {
  # set default colors if not specified
  if (is.null(areacols)) {
    areacols <- rich.colors.short(nareas)
    if (nareas == 3) {
      areacols <- c("blue", "red", "green3")
    }
    if (nareas > 3) {
      areacols <- rich.colors.short(nareas + 1)[-1]
    }
  }
  return(areacols)
}

#' Calculate variance adjustments for discard or mean body weight data
#'
#' Function developed for U.S. west coast Sablefish assessment in 2019 to tune
#' discard data or mean body weight data which are common inputs for
#' U.S. west coast groundfish assessments but as of 2023 have not often
#' had any data weighting method applied to them.
#'
#' The calculation is based on sd_out = sqrt(mean(Obs - Exp)^2)).
#' Added sd is calculated as sd_out - sd_in where sd_in is the mean of the
#' input standard deviations (possibly including existing variance adjustments).
#' When a CV adjustment is required, the sd_out is converted to CV_out by
#' dividing by the mean of the expected values and with the added CV calculated
#' as CV_out - CV_in.
#'
#' @param data Either the "discard" or "mnwgt" elements of the list
#' returned by [SS_output()]. Other data types might work here but haven't
#' been tested.
#' @param type Either "CV" or "sd" specifying the type of control file variance
#' adjustment, where the SS3 options are `2=add_to_discard_stddev`` and
#' `3=add_to_bodywt_CV`, so if `data` is discard data, type should be "CV" and
#' if `data` is mean body weight, type should be "sd".
#'
#' @return A table of input and estimated uncertainty values in units of both
#' CV and sd including the following:
#' * `fleet` is the fleet number
#' * `mean_out` is the mean of the expected values
#' * `mean_in` is the mean of the observed values
#' * `CV_in` is the mean input CV
#' * `sd_in` is the mean input SD values (which may include variance
#' adjustments already)
#' * `sd_out` is the SD of the observed relative to the expected values,
#' calculated as described above
#' * `CV_out` is the CV of the observed relative to the expected, calculated
#' as described above
#' * `added` is the value that could be added to any existing value in the
#' "Input variance adjustments factors" section of the control file.
#' * `type` is the data type code used in "Input variance adjustments factors"
#'
#' @author Kelli F. Johnson
#'
#' @export

calc_var_adjust <- function(data, type = c("CV", "sd")) {
  # check arguments (will default to first value in vector)
  type <- match.arg(type)
  # calculate SD if not provided
  if (!"Std_in" %in% colnames(data)) data[["Std_use"]] <- data[["CV"]] * data[["Exp"]]
  # calculate CV if not provided
  if (!"CV" %in% colnames(data)) data[["CV"]] <- NA

  # make a table of values by fleet, where the *_in values are the
  # mean adjusted input variability values and the *_out values are based on
  # the variability of the observed around the expected values
  calc <- aggregate(
    list(
      "mean_out" = data[["Exp"]],
      "mean_in" = data[["Obs"]],
      "CV_in" = data[["CV"]],
      "sd_in" = data[["Std_use"]],
      "sd_out" = (data[["Obs"]] - data[["Exp"]])^2
    ),
    by = list("fleet" = data[["Fleet"]]), mean
  )
  calc[, "sd_out"] <- sqrt(calc[, "sd_out"])
  calc[, "CV_out"] <- calc[, "sd_out"] / calc[, "mean_out"]
  # calculated the CV or sd that needs to be added to get a match
  calc[["added"]] <- switch(type,
    CV = calc[, "CV_out"] - calc[, "CV_in"],
    sd = calc[, "sd_out"] - calc[, "sd_in"]
  )
  # report the "type" used in the control file table of variance adjustments
  calc[["type"]] <- switch(type,
    CV = 3,
    sd = 2
  )
  # return the table
  return(calc)
}
