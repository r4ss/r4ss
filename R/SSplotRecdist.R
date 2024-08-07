#' Plot of recruitment distribution among areas and seasons
#'
#' Image plot shows fraction of recruitment in each combination of area and
#' season. This is based on the RECRUITMENT_DIST section of the Report.sso
#' file.
#'
#'
#' @template replist
#' @template plot
#' @template print
#' @param areanames optional vector to replace c("Area1","Area2",...)
#' @param seasnames optional vector to replace c("Season1","Season2",...)
#' @param xlab optional x-axis label (if the area names aren\'t informative
#' enough)
#' @param ylab optional y-axis label (if the season names aren\'t informative
#' enough)
#' @param main title for plot
#' @param period period of recruitment distribution to show among the options
#' "Initial", "Benchmark", and "End year"
#' @param sexes either 1 to only plot female distribution, 2 for males, or 1:2
#' to make both plots
#' @template plotdir
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
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
           main = "distribution of recruitment by area and season",
           period = c("Initial", "Benchmark", "End year"),
           sexes = 1:2,
           plotdir = "default",
           pwidth = 6.5, pheight = 5.0, punits = "in", res = 300, ptsize = 10, cex.main = 1,
           verbose = TRUE) {
    # plot of recruitment distribution between seasons and areas

    # confirm that period is just one of the available options
    period <- match.arg(period)

    # table to store information on each plot
    plotinfo <- NULL

    if (plotdir == "default") plotdir <- replist[["inputs"]][["dir"]]

    nareas <- replist[["nareas"]]
    nseasons <- replist[["nseasons"]]
    nsexes <- 1
    recdist <- replist[["recruitment_dist"]]

    # if version 3.24Q or beyond, recdist is a list, so choose the
    # period requested by the function (defaults to initial)
    if ("recruit_dist_endyr" %in% names(recdist)) {
      recdist <- dplyr::case_when(
        period == "Initial" ~ recdist[["recruit_dist"]], # first choice
        period == "Benchmark" ~ recdist[["recruit_dist_Bmark"]], # second choice
        period == "End year" ~ recdist[["recruit_dist_endyr"]] # third choice
      )
    }
    # prior to 3.30.23, 2-sex models only reported female recdist so treating
    # as 1-sex model with values representing the distribution of all recruits
    # assuming no time-varying sex ratio
    if ("recr_dist_M" %in% names(recdist)) {
      nsexes <- 2
      sexes <- sexes[sexes %in% 1:2]
    } else {
      sexes <- 1
    }


    areavec <- 1:nareas
    seasvec <- 1:nseasons
    if (is.null(areanames)) areanames <- paste("Area", 1:nareas, sep = "")
    if (is.null(seasnames)) seasnames <- paste("Season", 1:nseasons, sep = "")

    # use table of recruit distribution to make 3D array
    recmat <- array(0, c(nareas, nseasons, nsexes))
    for (iarea in areavec) {
      for (iseas in seasvec) {
        if (replist[["SS_versionNumeric"]] == 3.3) { # At least 3.30.16 has this format, not sure when added to 3.30 versions
          recmat[iarea, iseas, 1] <- sum(recdist[["recr_dist_F"]][recdist[["Area"]] == iarea & recdist[["Seas"]] == iseas])
          if (nsexes == 2) { # new column for males added in 3.30.23
            recmat[iarea, iseas, 2] <- sum(recdist[["recr_dist_M"]][recdist[["Area"]] == iarea & recdist[["Seas"]] == iseas])
          }
        } else {
          recmat[iarea, iseas, 1] <- sum(recdist[["recr_dist_F"]][recdist[["Area"]] == iarea & recdist[["Seas"]] == iseas & recdist[["Used"]] == 1])
        }
      }
    }
    # rescale to sum to 1.0 to work better if sex ratio is skewed
    # see https://github.com/nmfs-ost/ss3-source-code/issues/611 for explanation
    # only required for 2-sex models before male selectivity was reported
    # for which the female-only distributions will not sum to 1.0
    if (nsexes == 1 & sum(recmat) < 1) {
      recmat[, , 1] <- recmat[, , 1] / sum(recmat[, , 1])
    }

    # some models had issues with formatting in github action tests
    # that Ian could not replicate locally, so adding this warning to
    # avoid crash
    for (sex in sexes) {
      if (!is.matrix(recmat[, , 1])) {
        warning("Problem with format of recruitment distribution info")
        return()
      }
    }

    recdistfun <- function(sex) {
      image(areavec, seasvec, recmat[, , sex],
        axes = F, xlab = xlab, ylab = ylab,
        main = paste(period, main), cex.main = cex.main
      )
      axis(1, at = areavec, labels = areanames)
      axis(2, at = seasvec, labels = seasnames)
      box()

      for (iarea in areavec) {
        for (iseas in seasvec) {
          text(iarea, iseas, paste(round(100 * recmat[iarea, iseas, sex], 1), "%", sep = ""))
        }
      }
    }

    rownames(recmat) <- areanames
    colnames(recmat) <- seasnames
    # make plots
    for (sex in sexes)
    {
      sexlabel <- "recruits"
      if (nsexes == 2 & "recr_dist_M" %in% names(recdist)) {
        sexlabel <- c("females", "males")[sex]
      }
      message1 <- paste("recruitment distribution of", sexlabel, "by area and season:\n")
      if (nsexes == 1) {
        message1 <- "recruitment distribution by area and season:\n"
      }
      message(
        message1,
        paste0(utils::capture.output(recmat[, , sex]), collapse = "\n")
      )
      if (plot) recdistfun(sex)
      if (print) {
        file <- paste0("recruitment_distribution_sex ", sex, ".png")
        caption <- paste0("Recruitment distribution of ", sexlabel, " by area and season")
        plotinfo <- save_png(
          plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
          pheight = pheight, punits = punits, res = res, ptsize = ptsize,
          caption = caption
        )
        recdistfun()
        dev.off()
      }
    }
    if (!is.null(plotinfo)) plotinfo[["category"]] <- "S-R"
    return(invisible(plotinfo))
  }
