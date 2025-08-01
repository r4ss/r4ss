#' Plot matrix of either length or observed age at true age
#'
#' Distribution of length at age or observed age at true age is represented
#' as a histogram. Values are from the AGE_LENGTH_KEY and AGE_AGE_KEY sections
#' of Report.sso (ALK and AAK in the list created by SS_output)
#'
#' @template replist
#' @param option Switch set to either 1 for length at true age or
#' 2 for obs. age at true age
#' @param slices Optional input to choose which matrix (slice of the 3D-array)
#' within AAK or ALK to plot. By default all slices will be shown.
#' For ageing imprecision this should correspond to the ageing error matrix
#' number. Distribution of length at age (ALK) is ordered by season,
#' sub-season, and then morph. A future version could allow subsetting plots
#' by these dimensions.
#' @param scale Multiplier for bars showing distribution. Species with many ages
#' benefit from expanded bars. NULL value causes function to attempt automatic
#' scaling.
#' @param add Add to existing plot
#' @param col.grid A character value specifying the color of the grid lines
#' @param col.bars The color of the filled polygons.
#' @param shift_hi A numeric value specifying the amount to shift the top of
#' the polygon up.
#' @param shift_lo A numeric value specifying the amount to shift the bottom
#' of the polygon up.
#' @template plot
#' @template print
#' @template labels
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
#' @template mainTitle
#' @template plotdir
#' @author Ian G. Taylor
#' @export
#' @seealso [SSplotNumbers()]

SSplotAgeMatrix <- function(
  replist,
  option = 1,
  slices = NULL,
  scale = NULL,
  add = FALSE,
  col.grid = "grey90",
  col.bars = grey(0, alpha = .5),
  shift_hi = 0,
  shift_lo = 0,
  plot = TRUE,
  print = FALSE,
  labels = c(
    "Age", # 1
    "Length", # 2
    "True age", # 3
    "Observed age", # 4
    "for ageing error type", # 5
    "Distribution of", # 6
    "at"
  ), # 7
  pwidth = 6.5,
  pheight = 5.0,
  punits = "in",
  res = 300,
  ptsize = 10,
  cex.main = 1,
  mainTitle = TRUE,
  plotdir = "default"
) {
  # in-development function to plot matrix of length at age

  # table to store information on each plot
  plotinfo <- NULL
  if (plotdir == "default") {
    plotdir <- replist[["inputs"]][["dir"]]
  }

  # get stuff form replist created by SS_output
  # matrix of length at age (not really an age-length-key as the name implies, as
  # that would be a matrix used to convert length to age rather than age to length)

  # maximum age
  accuage <- replist[["accuage"]]
  # vector of ages
  agevec <- 0:accuage
  # length of vector (always accuage+1, but convenient to name)
  nages <- length(agevec)

  if (option == 1) {
    # option 1 is plotting distribution of length at age
    array <- replist[["ALK"]]
    if (is.null(array)) {
      warning(
        "No distribution of length at age plots produced because ",
        'replist[["ALK"]] is NULL, likely because "detailed age-structured reports"',
        "are not requested in the starter file."
      )
      return()
    }
    if (length(replist[["lbinspop"]]) == 1 && is.na(replist[["lbinspop"]])) {
      message(
        "No distribution of length at age plots produced because ",
        'replist[["ALK"]] is NULL, likely because "detailed age-structured reports"',
        "are not requested in the starter file."
      )
      return()
    }
    # vertical dimension is plotting length bins
    ybins <- replist[["lbinspop"]]
    # number of slices should be the number of sex/growth-pattern/morph combos
    nslices <- dim(array)[3]
    if (is.null(slices)) {
      slices <- 1:nslices
    }
    # axis labels
    xlab <- labels[1]
    ylab <- labels[2]
    # first part of title (can have addition info appended)
    titleStart <- paste(
      labels[6],
      tolower(paste(labels[2], labels[7], labels[1]))
    )
    # first part of PNG file name (only used if print=TRUE)
    filenameStart <- "bio1B_len_at_age_matrix_"
  }
  if (option == 2) {
    # option 2 is plotting distribution of observed age at true age
    array <- replist[["AAK"]]
    if (is.null(array)) {
      warning(
        "No distribution of observed age at true age plots produced because ",
        'replist[["AAK"]] is NULL.'
      )
      return()
    }
    # age bins
    ybins <- agebins.tmp <- sort(unique(as.numeric(dimnames(array)[[
      "ObsAgeBin"
    ]])))
    if (is.na(ybins[1])) {
      return(NULL)
    }
    # number of slices is the number of ageing error matrices
    nslices <- dim(array)[1]
    if (is.null(slices)) {
      slices <- 1:nslices
    }
    # axis labels
    xlab <- labels[3]
    ylab <- labels[4]
    # first part of title (can have addition info appended)
    titleStart <- paste(
      labels[6],
      tolower(paste(labels[4], labels[7], labels[3]))
    )
    # first part of PNG file name (only used if print=TRUE)
    filenameStart <- "numbers10_ageerror_matrix_"
  }
  if (is.null(scale)) {
    # default to 1 if scale is not provided
    scale <- 1
    # rescale if max value for middle group of ages is not in the 0.5 to 2 range
    # this approach is used because values for youngest and oldest fish are
    # often piled into a single bin

    if (accuage > 10) {
      middle.ages <- 5:8
    } else {
      if (accuage > 3) {
        middle.ages <- 2:3
      } else {
        middle.ages <- 1
      }
    }
    if (option == 1) {
      max.y <- max(array[, middle.ages + 1, ], na.rm = TRUE)
    }
    if (option == 2) {
      max.y <- max(array[,, middle.ages + 1], na.rm = TRUE)
    }
    if (max.y < 0.5 | max.y > 2.0) {
      scale <- 0.9 / max.y
    }
  }
  nybins <- length(ybins)
  if (nybins > 1) {
    ymax <- 1.1 * (ybins[nybins] + ybins[nybins] - ybins[nybins - 1])
  } else {
    # not sure best choice if only one bin
    ymax <- 2 * ybins
  }

  AgeMatrix.fn <- function(slice = 1) {
    if (option == 1) {
      # choose which morph/sex/etc of the array
      mat <- array[,, slice]
      # need to figure out which slice corresponds to which
      # morph/sex/etc. for labeling purposes
      title <- titleStart
      info <- tolower(dimnames(array)[[3]][slice])
      if (!is.null(info)) {
        title <- paste0(title, "\nfor ", info)
      }
    }
    if (option == 2) {
      # choose which ageing error matrix
      mat <- array[slice, , ]
      # concatenate title
      title <- paste(titleStart, "\n", labels[5], slice)
    }
    # option to not have plot title (to avoid redundancy with caption)
    if (!mainTitle) {
      title <- ""
    }

    if (!add) {
      plot(
        0,
        type = "n",
        las = 1,
        xlim = c(0, 1.1 * accuage),
        xaxs = "i",
        ylim = c(0, ymax),
        yaxs = "i",
        xlab = xlab,
        ylab = ylab,
        main = title
      )
    }
    # grid lines
    abline(h = ybins, v = 0:accuage, col = col.grid, lwd = 0.5)
    for (iage in nages:1) {
      a <- agevec[iage] # actual age
      yvec <- rev(mat[, iage]) # vector of values
      for (iybin in 1:nybins) {
        # lower limit of bin is value in vector of bins
        ybin_lo <- ybins[iybin]
        # upper limit is following bin value...
        if (nybins == 1) {
          # if only 1 bin, assume width=1
          ybin_hi <- ybin_lo + 1
        } else {
          # if multiple bins, set upper bound equal to next bin's lower bound
          if (iybin < nybins) {
            ybin_hi <- ybins[iybin + 1]
          } else {
            # unless it's the final bin in which case
            # it's depicted as equal in width to the previous bin even
            # though it's actually a plus group
            ybin_hi <- ybins[iybin] +
              (ybins[iybin] - ybins[iybin - 1])
          }
        }
        # add a filled rectangle for this combination of age and ybin
        rect(
          xleft = a,
          ybottom = ybin_lo + shift_lo,
          xright = a + scale * yvec[iybin],
          ytop = ybin_hi + shift_hi,
          col = col.bars,
          border = NA
        )
      } # end loop over bins (length or observed age)
      # lines(a+yvec*scale, ybins)
    } # end loop over true ages
    box()
  } # end of internal AgeMatrix.fn

  # loop over slices (ageing matrices or growth patterns/sexes)
  for (islice in slices) {
    if (plot) {
      # plot to GUI or PDF
      AgeMatrix.fn(slice = islice)
    }
    if (print) {
      # print to PNG file
      file <- paste0(filenameStart, islice, ".png")
      # caption creation is redundant with title creation inside AgeMatrix.fn
      # but is required in advance to pass to save_png
      # not sure how this issue was dealt with in other functions
      if (option == 1) {
        caption <- paste(titleStart)
        info <- tolower(dimnames(array)[[3]][islice])
        if (!is.null(info)) {
          caption <- paste(titleStart, "for", info)
        }
      }
      if (option == 2) {
        caption <- paste(titleStart, "\n", labels[5], islice)
      }
      plotinfo <- save_png(
        plotinfo = plotinfo,
        file = file,
        plotdir = plotdir,
        pwidth = pwidth,
        pheight = pheight,
        punits = punits,
        res = res,
        ptsize = ptsize,
        caption = caption
      )
      AgeMatrix.fn(slice = islice)
      dev.off()
    }
  } # end loop over slices (ageing matrices or growth patterns/sexes)

  # add category and return plotinfo
  if (!is.null(plotinfo)) {
    if (option == 1) {
      plotinfo[["category"]] <- "Bio"
    }
    if (option == 1) {
      plotinfo[["category"]] <- "Numbers"
    }
  }
  return(invisible(plotinfo))
} # end of function
