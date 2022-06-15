#' Plot of recruitment distribution among areas and seasons
#'
#' Image plot shows fraction of recruitment in each combination of area and
#' season. This is based on the RECRUITMENT_DIST section of the Report.sso
#' file.
#'
#'
#' @template replist
#' @param plot plot to active plot device?
#' @param print print to PNG files?
#' @param areanames optional vector to replace c("Area1","Area2",...)
#' @param seasnames optional vector to replace c("Season1","Season2",...)
#' @param xlab optional x-axis label (if the area names aren\'t informative
#' enough)
#' @param ylab optional y-axis label (if the season names aren\'t informative
#' enough)
#' @param main title for plot
#' @template plotdir
#' @template pwidth
#' @template pheight
#' @param punits units for PNG file
#' @template res
#' @param ptsize point size for PNG file
#' @param cex.main character expansion for plot titles
#' @template verbose
#' @author Ian Taylor
#' @export
#' @seealso [SS_plots()], [SSplotRecdevs()]
SSplotRecdist <-
  function(replist, plot = TRUE, print = FALSE,
           areanames = NULL,
           seasnames = NULL,
           xlab = "",
           ylab = "",
           main = "Distribution of recruitment by area and season",
           plotdir = "default",
           pwidth = 6.5, pheight = 5.0, punits = "in", res = 300, ptsize = 10, cex.main = 1,
           verbose = TRUE) {
    # plot of recruitment distribution between seasons and areas

    # table to store information on each plot
    plotinfo <- NULL

    if (plotdir == "default") plotdir <- replist[["inputs"]][["dir"]]

    nareas <- replist[["nareas"]]
    nseasons <- replist[["nseasons"]]
    recdist <- replist[["recruitment_dist"]]
    # if version 3.24Q or beyond, recdist is a list, so taking just the first element for now
    if ("recruit_dist_endyr" %in% names(recdist)) recdist <- recdist[["recruit_dist_endyr"]]

    areavec <- 1:nareas
    seasvec <- 1:nseasons
    if (is.null(areanames)) areanames <- paste("Area", 1:nareas, sep = "")
    if (is.null(seasnames)) seasnames <- paste("Season", 1:nseasons, sep = "")

    recmat <- matrix(0, nrow = nareas, ncol = nseasons)

    for (iarea in areavec) {
      for (iseas in seasvec) {
        if (replist[["SS_versionNumeric"]] == 3.3) { # At least 3.30.16 has this format
          recmat[iarea, iseas] <- sum(recdist[["Frac/sex"]][recdist[["Area"]] == iarea & recdist[["Seas"]] == iseas])
        } else {
          recmat[iarea, iseas] <- sum(recdist[["Value"]][recdist[["Area"]] == iarea & recdist[["Seas"]] == iseas & recdist[["Used"]] == 1])
        }
      }
    }

    recdistfun <- function() {
      image(areavec, seasvec, recmat,
        axes = F, xlab = xlab, ylab = ylab,
        main = main, cex.main = cex.main
      )
      axis(1, at = areavec, labels = areanames)
      axis(2, at = seasvec, labels = seasnames)
      box()

      for (iarea in areavec) {
        for (iseas in seasvec) {
          text(iarea, iseas, paste(round(100 * recmat[iarea, iseas], 1), "%", sep = ""))
        }
      }
    }

    rownames(recmat) <- areanames
    colnames(recmat) <- seasnames
    cat("recruitment distribution by area and season:\n")
    print(recmat)

    if (plot) recdistfun()
    if (print) {
      file <- "recruitment_distribution.png"
      caption <- "Recruitment distribution by area and season"
      plotinfo <- save_png(
        plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
        pheight = pheight, punits = punits, res = res, ptsize = ptsize,
        caption = caption
      )
      recdistfun()
      dev.off()
    }

    if (!is.null(plotinfo)) plotinfo[["category"]] <- "Recruitment"
    return(invisible(plotinfo))
  }
