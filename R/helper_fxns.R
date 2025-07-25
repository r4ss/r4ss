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
#'  if both exist, then `data_echo.ss_new` is returned. If the `dir` input
#'  points to github, then `dir()` doesn't work and `data_echo.ss_new` is
#'  always returned.
#' @seealso [get_par_name]
#'
get_dat_new_name <- function(dir) {
  datname <- tail(
    dir(path = dir, pattern = "data_?e?c?h?o?\\.ss_new"),
    1
  )
  # dir() doesn't work for github, assume newer filename
  if (grepl("raw.githubusercontent", dir)) {
    datname <- "data_echo.ss_new"
  }
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
      TRUE ~
        parfile[
          !parinfo[["isdir"]] &
            parinfo[["mtime"]] == max(parinfo[["mtime"]][!parinfo[["isdir"]]])
        ][1]
    )

    if (verbose) {
      message(
        "Multiple files in directory match pattern *.par, choosing based on the",
        " preferences described in the help for get_par_name(): ",
        parfile
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
sspar <- function(
  mfrow = c(1, 1),
  plot.cex = 1,
  mai = c(0.55, 0.6, 0.1, .1),
  omi = c(0., 0., 0., 0) + 0.1,
  labs = TRUE
) {
  if (labs == F) {
    mai <- c(0.25, 0.25, 0.15, .15)
    omi <- c(0.3, 0.35, 0.2, 0.2)
  }
  par(list(
    mfrow = mfrow,
    mai = mai,
    mgp = c(2., 0.5, 0),
    omi = omi,
    tck = -0.02,
    cex = plot.cex
  ))
}

#' Convert Time-Steps
#'
#' Function to convert non-annual into annual time-steps for retros and cpue residuals
#'
#' @param ss3out outputs from [r4ss::SS_output()] or [r4ss::SSsummarize()]
#' @param time.steps  time steps behind yrs e.g. 0.25 for quarterly
#' @param end.time last time step e.g. 2018.75 with a cpue observation
#'
#' @return Reformatted Rep file outputs
#'
#' @keywords utils rep retro retrocomps
#'
#' @export
#'
SSdiagsTime2Year <- function(ss3out, time.steps = 0.25, end.time) {
  if (is.null(ss3out[["len"]]) == F | is.null(ss3out[["len"]]) == F) {
    type <- "retrocomps"
  } else {
    type <- ifelse(is.null(ss3out[["modelnames"]]), "rep", "retro")
  }
  # Conversion match function
  convTY <- function(indices, end.time, time.steps) {
    steps <- (unique(indices[["Time"]]))
    nsteps <- length(steps)
    Time <- rev(rev(seq(0, end.time, time.steps))[1:nsteps])
    Yr <- floor(Time)
    Seas <- (Time - Yr) * 4 + 1
    conv <- match(indices[["Time"]], steps)
    indices[["Yr"]] <- Yr[conv]
    indices[["Time"]] <- Time[conv]
    indices[["Seas"]] <- Seas[conv]
    return(indices)
  }

  if (type == "rep") {
    ss3out[["cpue"]] <- convTY(ss3out[["cpue"]], end.time, time.steps)
    # if(!is.null(ss3out[["lendbase"]])) ss3out[["lendbase"]] =  convTY(ss3out[["lendbase"]],end.time,time.steps)
    # length comps not working
  }
  if (type == "retro") {
    ss3out[["indices"]] <- convTY(ss3out[["indices"]], end.time, time.steps)
    # SSB
    ssb <- ss3out[["SpawnBio"]]
    steps <- unique(ssb[["Yr"]])
    nsteps <- length(steps)
    Time <- (rev(rev(seq(0, end.time, time.steps))[1:nsteps]))
    ssb[["Time"]] <- Time
    subset <- ssb[["Time"]] %in% floor(ssb[["Time"]])
    ssb <- ssb[subset, ]
    ss3out[["SpawnBio"]] <- ssb
    ss3out[["SpawnBioLower"]] <- ss3out[["SpawnBioLower"]][subset, ]
    ss3out[["SpawnBioUpper"]] <- ss3out[["SpawnBioUpper"]][subset, ]

    ss3out[["SpawnBio"]][["Yr"]] <- ssb[["Time"]]
    ss3out[["SpawnBioLower"]][["Yr"]] <- ssb[["Time"]]
    ss3out[["SpawnBioUpper"]][["Yr"]] <- ssb[["Time"]]
    ss3out[["startyrs"]] <- rep(min(ssb[["Time"]]), ss3out[["n"]])
    ss3out[["endyrs"]] <- rep(max(ssb[["Time"]]), ss3out[["n"]])
    # Can add F and Rec if needed
  }
  if (type == "retrocomps") {
    if (!is.null(ss3out[["len"]])) {
      ss3out[["len"]] <- convTY(ss3out[["len"]], end.time, time.steps)
    }
    if (!is.null(ss3out[["age"]])) {
      ss3out[["len"]] <- convTY(ss3out[["age"]], end.time, time.steps)
    }
    ss3out[["startyrs"]] <- rep(
      min(ss3out[["len"]][["Time"]], ss3out[["age"]][["Time"]]),
      ss3out[["n"]]
    )
    ss3out[["endyrs"]] <- rep(
      max(ss3out[["len"]][["Time"]], ss3out[["age"]][["Time"]]),
      ss3out[["n"]]
    )
  }
  return(ss3out)
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
add_legend <- function(
  legendlabels,
  legendloc = "topleft",
  legendorder = NULL,
  legendncol = 1,
  legendcex = 1,
  legendsp = 0.9,
  col = NULL,
  pch = NULL,
  pt.cex = 1,
  lty = 1,
  lwd = 2,
  type = "o"
) {
  if (is.null(legendorder)) {
    legendorder <- seq_along(legendlabels)
  }
  if (is.numeric(legendloc)) {
    Usr <- par()[["usr"]]
    legendloc <- list(
      x = Usr[1] + legendloc[1] * (Usr[2] - Usr[1]),
      y = Usr[3] + legendloc[2] * (Usr[4] - Usr[3])
    )
  }

  # sort out NULL values and lengths of some inputs
  if (is.null(pch)) {
    pch <- rep(NA, length(legendlabels))
  }
  if (length(pch) < length(legendlabels)) {
    pch <- rep(pch, length.out = length(legendlabels))
  }
  if (length(pt.cex) < length(legendlabels)) {
    pt.cex <- rep(pt.cex, length.out = length(legendlabels))
  }
  # if type input is "l" then turn off points on top of lines in legend
  if (type == "l") {
    legend.pch <- rep(NA, length(pch))
  } else {
    legend.pch <- pch
  }
  legend(
    legendloc,
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
  rich.vector <- apply(rgb.m, 1, function(v) {
    rgb(v[1], v[2], v[3], alpha = alpha)
  })
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

save_png <- function(
  plotinfo,
  file,
  plotdir,
  pwidth,
  pheight,
  punits,
  res,
  ptsize,
  caption = NA,
  alt_text = NA,
  filenameprefix = NA
) {
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
  invisible(rbind(
    plotinfo,
    data.frame(
      file = file,
      caption = caption,
      alt_text = alt_text
    )
  ))
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
  if (!"Std_in" %in% colnames(data)) {
    data[["Std_use"]] <- data[["CV"]] * data[["Exp"]]
  }
  # calculate CV if not provided
  if (!"CV" %in% colnames(data)) {
    data[["CV"]] <- NA
  }

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
    by = list("fleet" = data[["Fleet"]]),
    mean
  )
  calc[, "sd_out"] <- sqrt(calc[, "sd_out"])
  calc[, "CV_out"] <- calc[, "sd_out"] / calc[, "mean_out"]
  # calculated the CV or sd that needs to be added to get a match
  calc[["added"]] <- switch(
    type,
    CV = calc[, "CV_out"] - calc[, "CV_in"],
    sd = calc[, "sd_out"] - calc[, "sd_in"]
  )
  # report the "type" used in the control file table of variance adjustments
  calc[["type"]] <- switch(type, CV = 3, sd = 2)
  # return the table
  return(calc)
}

#' Add a comment line to the input files
#'
#' Used by the SS_write* functions.
#' @param text Comment to write
#' @param con File to write to (passed to `con` input to `writeLines()`)
#' @param ... Additional arguments passed to `writeLines()`

writeComment <- function(text, con, ...) {
  if (length(grep(x = text, pattern = "^#")) != length(text)) {
    text <- paste("#_", text, sep = "")
  }
  writeLines(text = text, con = con, ...)
}

#' Add header comments to the input files
#'
#' Lines starting with #C at the top of the file are carried over to the
#' *.ss_new files by Stock Synthesis
#' This function modifies any existing header to add or replace lines
#' written by r4ss that state the write time of the file.
#'
#' @param filelist An object created by one of the r4ss::SS_read* functions.
#' @param con File to write to (passed to `con` input to `writeLines()`)
#' @author Yukio Takeuchi, Ian G. Taylor
#'
add_file_header <- function(filelist, con) {
  # #C means this header will be maintained in control.ss_new file
  # created from a SS3 model run using this control file.

  # the writeComment() function is defined within each of the SS_write*
  # functions in order

  if (is.null(filelist[["Comments"]])) {
    # empty placeholder for comments
    Comments <- NULL
  } else {
    Comments <-
      sapply(filelist[["Comments"]], function(x) {
        if (!grepl(x, pattern = "^#C")) {
          x <- paste0("#C_", x)
        }
        x
      })
    # remove comments added by earlier runs of this function
    Comments <- Comments[
      !grepl("file created using", Comments) &
        !grepl("file write time", Comments)
    ]
  }
  # add new comments
  Comments <- c(
    Comments,
    paste0(
      "#C file created using an r4ss function"
    )
  )
  Comments <- c(
    Comments,
    paste("#C file write time:", format(Sys.time(), "%Y-%m-%d  %H:%M:%S"))
  )
  # write all comments
  for (ln in Comments) {
    writeComment(text = ln, con = con)
  }
  writeComment("#", con = con)
}


#' Catch *and* save both errors and warnings, and in the case of
#' a warning, also keep the computed result.
#'
#' Copied from
#' https://svn.r-project.org/R/trunk/src/library/base/demo/error.catching.R
#'
#' @title tryCatch both warnings (with value) and errors
#' @param expr an \R expression to evaluate
#' @return a list with 'value' and 'warning', where
#'   'value' may be an error caught.
#' @author Martin Maechler;
#' Copyright (C) 2010-2023  The R Core Team
tryCatch.W.E <- function(expr) {
  W <- NULL
  w.handler <- function(w) {
    # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(
    value = withCallingHandlers(
      tryCatch(expr, error = function(e) e),
      warning = w.handler
    ),
    warning = W
  )
}

#' Check if the replist input is something that was created by SS_output()
#'
#' @param replist An object to test
check_replist <- function(replist) {
  if (is.null(replist) || !is.list(replist) || !"nfleets" %in% names(replist)) {
    cli::cli_abort(
      "The input 'replist' should refer to an R object created by the function
      'SS_output'."
    )
  }
}
