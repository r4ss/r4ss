#' Apply Francis composition weighting method TA1.8 for conditional age-at-length fits
#'
#' Uses an extension of method TA1.8 (described in Appendix A of Francis, 2011)
#' to do stage-2 weighting of conditional age at length composition data from a
#' Stock Synthesis model.
#'
#' The function outputs a multiplier, *w*,
#' (with bootstrap 95% confidence intervals) so that
#' *N2i* = *w* x *N1i*,
#' where *N1i* and *N2i* are the stage-1 and stage-2 multinomial
#' sample sizes for the *i*th composition. Optionally makes a plot
#' of observed and expected mean ages, with two alternative
#' sets of confidence limits - based on *N1i* (thin lines) and *N2i*
#' (thick lines) - for the observed values.
#'
#' This function formerly reported two versions of w differ according to whether
#' the calculated mean ages are
#' indexed by year (version A) or by year and length bin (version B).
#' However, research by Punt (2017) found Version A to perform better and
#' version B is no longer recommended and is only reported if requested by the user.
#'
#' CAUTIONARY/EXPLANATORY NOTE. The large number of options available in SS makes it
#' very difficult to be sure that what this function does is appropriate for all
#' combinations of options. The following notes (for version A) might help anyone
#' wanting to check or correct the code.
#' \enumerate{
#'   \item The code first removes unneeded rows
#' from database condbase.
#'   \item The remaining rows of the database are grouped
#' (indexed by vector indx) and relevant statistics (e.g., observed and expected
#' mean age), and ancillary data, are calculated for each group (these are stored
#' in pldat - one row per group).
#'   \item If the data are to be plotted they are further
#' grouped by fleet, with one panel of the plot per fleet.
#'   \item A single multiplier, *w*, is calculated to apply to all the
#' selected data.
#' }
#'
#' @param fit Stock Synthesis output as read by r4SS function SS_output
#' @param fleet vector of one or more fleet numbers whose data are to
#' be analysed simultaneously (the output N multiplier applies
#' to all fleets combined)
#' @template fleetnames
#' @param part vector of one or more partition values; analysis is restricted
#' to composition data with one of these partition values.
#' Default is to include all partition values (0, 1, 2).
#' @param seas string indicating how to treat data from multiple seasons
#' 'comb' - combine seasonal data for each year and plot against Yr
#' 'sep' - treat seasons separately, plotting against Yr.S
#' If is.null(seas) it is assumed that there is only one season in
#' the selected data (a warning is output if this is not true) and
#' option 'comb' is used.
#' @param plotit if TRUE, make an illustrative plot like one or more
#' panels of Fig. 4 in Francis (2011).
#' @param printit if TRUE, print results to R console.
#' @param datonly if TRUE, don't show the model expectations
#' @param plotadj if TRUE, plot the confidence intervals associated with
#' the adjusted sample sizes (TRUE by default unless datonly = TRUE)
#' @param maxpanel maximum number of panels within a plot
#' @param FullDiagOut Print full diagnostics?
#' @param ShowVersionB Report the Version B value in addition to the default?
#' @param add add to existing plot
#' @author R.I.C Chris Francis, Andre E. Punt, Ian G. Taylor
#' @export
#' @family tuning functions
#' @references Francis, R.I.C.C. (2011). Data weighting in statistical
#' fisheries stock assessment models. Can. J. Fish. Aquat. Sci. 68: 1124-1138.
#' https://doi.org/10.1139/f2011-025.
#'
#' Punt, A.E. (2017). Some insights into data weighting in integrated stock assessments.
#' Fish. Res. 192:52-65. https://doi.org/10.1016/j.fishres.2015.12.006.
#'
SSMethod.Cond.TA1.8 <-
  function(
    fit,
    fleet,
    part = 0:2,
    seas = NULL,
    plotit = TRUE,
    printit = FALSE,
    datonly = FALSE,
    plotadj = !datonly,
    maxpanel = 1000,
    FullDiagOut = FALSE,
    ShowVersionB = FALSE,
    fleetnames = NULL,
    add = FALSE
  ) {
    # Check the type is correct and the pick.sex is correct
    is.in <- function(x, y) !is.na(match(x, y))

    # replace default fleetnames with user input if requested
    if (is.null(fleetnames)) {
      # use fleetnames in the model
      fleetnames <- fit[["FleetNames"]]
    } else {
      # if custom names input, check length
      if (length(fleetnames) != fit[["nfleets"]]) {
        stop(
          "fleetnames needs to be NULL or have length = nfleets = ",
          fit[["nfleets"]]
        )
      }
    }

    # Select the type of datbase
    dbase <- fit[["condbase"]]
    if (length(unique(dbase[["Bin"]])) == 1) {
      warning("Francis weighting method doesn't work with only 1 age bin")
      return()
    }
    sel <- is.in(dbase[["Fleet"]], fleet) & is.in(dbase[["Part"]], part)
    if (sum(sel) == 0) {
      return()
    }

    dbase <- dbase[sel, ]
    if (is.null(seas)) {
      seas <- "comb"
      if (length(unique(dbase[["Seas"]])) > 1) {
        message("Combining data from multiple seasons")
      }
    }

    indx <- paste(
      dbase[["Fleet"]],
      dbase[["Yr"]],
      if (seas == "sep") dbase[["Seas"]] else ""
    )
    uindx <- unique(indx)
    if (length(uindx) == 1) {
      # presumably the method is meaningless of there's only 1 point,
      # but it's good to be able to have the function play through
      warning("Only one point to plot")
      return()
    }

    pldat <- matrix(
      0,
      length(uindx),
      12,
      dimnames = list(
        uindx,
        c(
          "Obsmn",
          "Obslo",
          "Obshi",
          "semn",
          "Expmn",
          "Std.res",
          "ObsloAdj",
          "ObshiAdj",
          "Total",
          "Fleet",
          "Yr",
          "EffN"
        )
      )
    )
    pldat <- cbind(pldat, Lbin = 0)

    # Find the wieghting factor for this combination of factors
    AllRes <- NULL
    for (i in seq_along(uindx)) {
      # each row of pldat is an individual comp
      subdbase <- dbase[indx == uindx[i], ]

      Lbins <- unique(subdbase[["Lbin_lo"]])
      Intermediate <- matrix(
        0,
        length(Lbins),
        5,
        dimnames = list(Lbins, c("Obsmn", "Varn", "Expmn", "N", "Resid"))
      )
      for (j in seq_along(Lbins)) {
        ILbin <- Lbins[j]
        subsubdbase <- subdbase[subdbase[["Lbin_lo"]] == ILbin, ]
        if (length(subsubdbase[["Yr"]]) > 0) {
          xvar <- subsubdbase[["Bin"]]
          AbarNObs <- sum(subsubdbase[["Obs"]] * xvar) /
            sum(subsubdbase[["Obs"]])
          AbarNPre <- sum(subsubdbase[["Exp"]] * xvar) /
            sum(subsubdbase[["Exp"]])
          AbarVarn <- (sum(subsubdbase[["Exp"]] * xvar^2) /
            sum(subsubdbase[["Exp"]]) -
            AbarNPre^2)
          Intermediate[j, "Obsmn"] <- AbarNObs
          Intermediate[j, "Expmn"] <- AbarNPre
          Intermediate[j, "Varn"] <- AbarVarn
          Intermediate[j, "N"] <- mean(subsubdbase[["Nsamp_adj"]])
          Intermediate[j, "Resid"] <- (AbarNObs - AbarNPre) /
            sqrt(AbarVarn / mean(subsubdbase[["Nsamp_adj"]]))
        }
      }
      Total <- sum(Intermediate[, "N"])
      Weights <- Intermediate[, "N"] / Total

      AbarNObs <- 0
      AbarNPre <- 0
      AbarVarn <- 0
      for (j in seq_along(Lbins)) {
        AbarNObs <- AbarNObs + as.double(Intermediate[j, "Obsmn"] * Weights[j])
        AbarNPre <- AbarNPre + as.double(Intermediate[j, "Expmn"] * Weights[j])
        AbarVarn <- AbarVarn +
          as.double(Weights[j]^2 * Intermediate[j, "Varn"]) /
            as.double(Intermediate[j, "N"])
      }
      AbarVarn <- sqrt(AbarVarn)

      pldat[i, "Obsmn"] <- AbarNObs
      pldat[i, "Expmn"] <- AbarNPre
      pldat[i, "semn"] <- AbarVarn
      pldat[i, "Obslo"] <- pldat[i, "Obsmn"] - 2 * pldat[i, "semn"]
      pldat[i, "Obshi"] <- pldat[i, "Obsmn"] + 2 * pldat[i, "semn"]
      pldat[i, "Std.res"] <- (pldat[i, "Obsmn"] - pldat[i, "Expmn"]) /
        pldat[i, "semn"]
      pldat[i, "Fleet"] <- mean(subdbase[["Fleet"]])
      pldat[i, "Total"] <- Total
      pldat[i, "Yr"] <- mean(
        if (seas == "comb") subdbase[["Yr"]] else subdbase[["Yr.S"]]
      )
      pldat[i, "EffN"] <- 1 / var(Intermediate[, "Resid"])
      AllRes <- c(AllRes, Intermediate[, "Resid"])
    }
    Nmult <- 1 / var(pldat[, "Std.res"], na.rm = TRUE)

    # Find the adjusted confidence intervals
    for (i in seq_along(uindx)) {
      pldat[i, "ObsloAdj"] <- pldat[i, "Obsmn"] -
        2 * pldat[i, "semn"] / sqrt(Nmult)
      pldat[i, "ObshiAdj"] <- pldat[i, "Obsmn"] +
        2 * pldat[i, "semn"] / sqrt(Nmult)
    }
    if (FullDiagOut == TRUE) {
      print(pldat)
    }

    Nfleet <- length(unique(pldat[, "Fleet"]))
    if (plotit) {
      # make plot
      plindx <- paste(pldat[, "Fleet"])
      uplindx <- unique(plindx)

      # Select number of panels
      Npanel <- length(uplindx)
      ## Ian T. 9/25/14: changing from having at least 4 panels to no minimum
      # NpanelSet <- max(4,min(length(uplindx),maxpanel))
      NpanelSet <- min(length(uplindx), maxpanel)
      Nr <- ceiling(sqrt(NpanelSet))
      Nc <- ceiling(NpanelSet / Nr)
      par(
        mfrow = c(Nr, Nc),
        mar = c(2, 2, 1, 1) + 0.1,
        mgp = c(0, 0.5, 0),
        oma = c(1.2, 1.2, 0, 0),
        las = 1
      )
      par(cex = 1)
      # loop over panels
      for (i in 1:Npanel) {
        subpldat <- pldat[plindx == uplindx[i], , drop = FALSE]
        x <- subpldat[, "Yr"]
        ylim <- range(
          subpldat[, c("ObsloAdj", "ObshiAdj", "Expmn")],
          na.rm = TRUE
        )
        if (any(is.infinite(ylim))) {
          # 0 sample sizes caused problems with ylim, override with wide range
          # plot may not make sense but will help users note that a problem exists
          # (as opposed to skipping the plot)
          warning("NaN values in Francis calculations, plot may not make sense")
          ylim <- c(0, fit[["accuage"]])
        }
        # make empty plot (unless adding to existing plot)
        if (!add) {
          plot(
            x,
            subpldat[, "Obsmn"],
            pch = "-",
            xlim = if (length(x) > 1) range(x) else c(x - 0.5, x + 0.5),
            ylim = ylim,
            xlab = "",
            ylab = ""
          )
        }
        # add intervals for status-quo sample sizes adjustment
        segments(
          x,
          subpldat[, "Obslo"],
          x,
          subpldat[, "Obshi"],
          lwd = 3,
          lend = 3
        )
        if (plotadj) {
          # add adjusted intervals
          arrows(
            x,
            subpldat[, "ObsloAdj"],
            x,
            subpldat[, "ObshiAdj"],
            lwd = 1,
            length = 0.03,
            angle = 90,
            code = 3
          )
        }
        points(x, subpldat[, "Obsmn"], pch = 21, bg = "grey")
        ord <- order(x)
        # add model expectation
        if (!datonly) {
          if (length(x) > 1) {
            lines(x[ord], subpldat[ord, "Expmn"], lwd = 3, col = 4)
          } else {
            lines(
              c(x - 0.5, x + 0.5),
              rep(subpldat[, "Expmn"], 2),
              lwd = 3,
              col = 4
            )
          }
        }
        # Lines
        fl <- fleetnames[subpldat[1, "Fleet"]]
        yr <- paste(subpldat[1, "Yr"])
        lab <- paste(fl)
        if (!add) {
          mtext(lab, side = 3, at = mean(x))
        }
      }
      if (!add) {
        mtext("Mean age", side = 2, outer = TRUE, las = 0)
        mtext("Year", side = 1, outer = TRUE)
      }
    }
    if (!datonly) {
      # calculate intervals and return adjustments only if datonly=FALSE
      tmp <- matrix(
        sample(pldat[, "Std.res"], 1000 * nrow(pldat), replace = TRUE),
        nrow(pldat)
      )
      confint <- as.vector(quantile(
        apply(tmp, 2, function(x) 1 / var(x, na.rm = TRUE)),
        c(0.025, 0.975),
        na.rm = TRUE
      ))
      Output <- c(w = Nmult, lo = confint[1], hi = confint[2])

      Outs <- paste0(
        "Francis CAA Weights: ",
        fleetnames[fleet],
        ": ",
        round(Nmult, 5),
        " (",
        round(confint[1], 5),
        "-",
        round(confint[2], 5),
        ")"
      )
      if (printit) {
        print(Outs)
      }
      if (ShowVersionB) {
        # Original Francis method (a.k.a. Francis-B)
        Nmult2 <- 1 / var(AllRes, na.rm = TRUE)
        tmp <- matrix(
          sample(AllRes, 1000 * length(AllRes), replace = TRUE),
          length(AllRes)
        )
        confint2 <- as.vector(quantile(
          apply(tmp, 2, function(x) 1 / var(x, na.rm = TRUE)),
          c(0.025, 0.975)
        ))

        Outs <- paste0(
          "Francis CAA Weights-Version B (not recommended): ",
          fleetnames[fleet],
          ": ",
          round(Nmult2, 5),
          " (",
          round(confint2[1], 5),
          "-",
          round(confint2[2], 5),
          ")"
        )
        if (printit) {
          print(Outs)
        }
      }
      return(Output)
    }
  }
