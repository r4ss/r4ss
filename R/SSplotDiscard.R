#' Plot fit to discard fraction.
#'
#' Plot fit to discard fraction from Stock Synthesis output file.
#'
#'
#' @template replist
#' @param subplots Vector of which plots to make 
#' \itemize{
#'   \item 1 data only
#'   \item 2 data with fit
#'   \item 3 data only (log scale)
#'   \item 4 data with fit (log scale)
#' }
#' If `plotdat = FALSE` then subplots 1 and 3 are not created, regardless of
#' choice of `subplots`.
#' @template plot
#' @template print
#' @template plotdir
#' @template fleets
#' @template fleetnames
#' @param datplot Make data-only plot of discards? This can override the choice
#' of `subplots`.
#' @template labels
#' @param yhi Maximum y-value which will always be included in the plot
#' (all data included regardless). Default = 1 so that discard fractions are always
#' plotted on a 0-1 range, but total discard amounts which are greater than this value
#' will exceed it.
#' @param ymax Optional maximum y-value to include (useful if upper tails on
#' discard amounts are very high)
#' @param col1 First color to use in plot (for expected values)
#' @param col2 Second color to use in plot (for observations and intervals)
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
#' @template verbose
#' @author Ian G. Taylor, Ian J. Stewart, Robbie L. Emmet
#' @export
#' @seealso [SS_plots()]
SSplotDiscard <-
  function(replist, subplots = 1:4,
           plot = TRUE, print = FALSE,
           plotdir = "default",
           fleets = "all",
           fleetnames = "default",
           datplot = FALSE,
           labels = c(
             "Year",
             "Discard fraction",
             "Total discards",
             "for"
           ),
           yhi = 1,
           ymax = NULL,
           col1 = "blue", col2 = "black",
           pwidth = 6.5, pheight = 5.0, punits = "in",
           res = 300, ptsize = 10, cex.main = 1,
           verbose = TRUE) {
    # table to store information on each plot
    plotinfo <- NULL

    # get stuff from replist
    nfishfleets <- replist[["nfishfleets"]]
    discard <- replist[["discard"]]
    FleetNames <- replist[["FleetNames"]]
    DF_discard <- replist[["DF_discard"]] # used in SSv3.11
    discard_type <- replist[["discard_type"]] # used in SSv3.11
    discard_spec <- replist[["discard_spec"]] # used in SSv3.20
    if (fleetnames[1] == "default") {
      fleetnames <- FleetNames
    }
    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }

    # if discards exist
    if (!is.null(discard) &&
      !is.na(discard[[1]][1]) &&
      nrow(discard) > 0) {
      if (fleets[1] == "all") fleets <- 1:nfishfleets
      for (ifleet in intersect(fleets, unique(discard[["Fleet"]]))) {
        # table available beginning with SSv3.20 has fleet-specific discard specs
        if (!is.null(discard_spec)) {
          # check to make sure fleet is represented in the table
          if (!ifleet %in% discard_spec[["Fleet"]]) {
            stop("Fleet ", ifleet, " not found in table of discard specifications.")
          }
          # get degrees of freedom
          DF_discard <- discard_spec[["errtype"]][discard_spec[["Fleet"]] == ifleet]
        }
        usedisc <- discard[discard[["Fleet"]] == ifleet, ]
        FleetName <- fleetnames[ifleet]

        yr <- as.numeric(usedisc[["Yr"]])
        # only use fractional year value if there are multiple seasons
        if (any(usedisc[["Seas"]] > 1)) {
          yr <- as.numeric(usedisc[["Time"]])
        }
        ob <- as.numeric(usedisc[["Obs"]])
        std <- as.numeric(usedisc[["Std_use"]])
        if (DF_discard == -3) { # truncated normal thanks to Robbie Emmet
          ## liw <- ob - truncnorm::qtruncnorm(0.025, 0, 1, ob, std * ob)
          ## uiw <- truncnorm::qtruncnorm(0.975, 0, 1, ob, std * ob) - ob
          # correction from Robbie on 7/30/15
          liw <- ob - truncnorm::qtruncnorm(0.025, 0, 1, ob, std)
          uiw <- truncnorm::qtruncnorm(0.975, 0, 1, ob, std) - ob
        }
        if (DF_discard == -2) { # lognormal with std as interpreted as
          # the standard error (in log space) of the observation
          liw <- ob - qlnorm(0.025, log(ob), std)
          uiw <- qlnorm(0.975, log(ob), std) - ob
        }
        if (DF_discard == -1) { # normal with std as std
          liw <- ob - qnorm(0.025, ob, std)
          uiw <- qnorm(0.975, ob, std) - ob
        }
        if (DF_discard == 0) { # normal with std interpreted as CV
          liw <- ob - qnorm(0.025, ob, std)
          uiw <- qnorm(0.975, ob, std) - ob
        }
        if (DF_discard > 0) { # t-distribution with DF_discard = degrees of freedom
          liw <- -std * qt(0.025, DF_discard) # quantile of t-distribution
          uiw <- std * qt(0.975, DF_discard) # quantile of t-distribution
        }
        liw[(ob - liw) < 0] <- ob[(ob - liw) < 0] - 0.0001 # no negative limits
        xlim <- c((min(yr) - 3), (max(yr) + 3))
        ## three options for discard_units:
        ## 1:  discard_in_biomass(mt)_or_numbers(1000s)_to_match_catchunits_of_fleet
        ## 2:  discard_as_fraction_of_total_catch(based_on_bio_or_num_depending_on_fleet_catchunits)
        ## 3:  discard_as_numbers(1000s)_regardless_of_fleet_catchunits
        discard_units <- discard_spec[["units"]][discard_spec[["Fleet"]] == ifleet]
        if (discard_units == 1) {
          # type 1: biomass or numbers
          title <- paste("Total discard for", FleetName)
          caption <- title
          ylab <- labels[3]
          if (replist[["catch_units"]][ifleet] == 1) {
            ylab <- paste(ylab, "(t)")
          }
          if (replist[["catch_units"]][ifleet] == 2) {
            ylab <- paste(ylab, "(1000's)")
          }
        }
        if (discard_units == 2) {
          # type 2: discards as fractions
          title <- paste("Discard fraction for", FleetName)
          caption <- paste0(title, ". The numerator and denominator are in")
          if (replist[["catch_units"]][ifleet] == 1) {
            caption <- paste(caption, "biomass.")
          }
          if (replist[["catch_units"]][ifleet] == 2) {
            caption <- paste(caption, "numbers.")
          }

          ylab <- labels[2]
        }
        if (discard_units == 3) {
          # type 3: discards as numbers
          title <- paste("Total discard for", FleetName)
          caption <- title
          ylab <- "Total discards (1000's)"
        }

        # wrap up plot command in function
        dfracfunc <- function(addfit, log = FALSE) {
          plotCI(
            x = yr, y = ob, uiw = uiw, liw = liw, log = ifelse(log, "y", ""),
            ylab = ylab, xlab = labels[1], main = title2,
            ylo = 0, yhi = yhi, ymax = ymax,
            col = col2, sfrac = 0.005, lty = 1,
            xlim = xlim, pch = 21, bg = "white"
          )
          abline(h = 0, col = "grey")
          if (addfit) points(yr, usedisc[["Exp"]], col = col1, pch = "-", cex = 2)
        }

        # make plots
        if (!datplot) {
          subplots <- setdiff(subplots, c(1, 3)) # don't do subplot 1 if datplot=FALSE
        }
        for (isubplot in subplots) { # loop over subplots (data only or with fit)
          if (isubplot %in% c(1,3)) {
            addfit <- FALSE
          } else {
            addfit <- TRUE
          }
          if (plot) {
            dfracfunc(addfit = addfit, log = ifelse(isubplot %in% 3:4, TRUE, FALSE))
          }
          if (print) {
            if (!addfit) {
              file <- paste0("discard_data", FleetName, ".png")
            } else {
              file <- paste0("discard_fit", FleetName, ".png")
            }
            if (isubplot %in% 3:4) {
              file <- gsub("discard_", "discard_log_", file)
              caption2 <- gsub("fraction for", "fraction on a log scale for", caption)
              title2 <- gsub("fraction for", "fraction on a log scale for", title)
            } else {
              caption2 <- caption
              title2 <- title
            }
            plotinfo <- save_png(
              plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
              pheight = pheight, punits = punits, res = res, ptsize = ptsize,
              caption = caption2
            )
            dfracfunc(addfit = addfit, log = ifelse(isubplot %in% 3:4, TRUE, FALSE))
            dev.off()
          }
        } # end loop over subplots
      } # discard series
    }
    if (!is.null(plotinfo)) plotinfo[["category"]] <- "Discard"
    return(invisible(plotinfo))
  } # end of function
