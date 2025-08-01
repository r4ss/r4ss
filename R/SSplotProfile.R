#' Plot likelihood profile results
#'
#' Makes a plot of change in negative-log-likelihood for each likelihood
#' component that contributes more than some minimum fraction of change in
#' total.
#'
#'
#' @param summaryoutput List created by the function [SSsummarize()].
#' @template plot
#' @template print
#' @param models Optional subset of the models described in
#' `summaryoutput`. Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param profile.string Character string used to find parameter over which the
#' profile was conducted. If `exact=FALSE`, this can be a substring of
#' one of the SS3 parameter labels found in the Report.sso file.
#' For instance, the default input 'steep'
#' matches the parameter 'SR_BH_steep'. If `exact=TRUE`, then
#' profile.string needs to be an exact match to the parameter label.
#' @param profile.label Label for x-axis describing the parameter over which
#' the profile was conducted. NULL value will be replaced by an informative
#' label if the parameter label contains one of the follow strings:
#' "steep", "R0", "NatM", "L_at_Amax", "sigmaR", or "LnQ".
#' @param exact Should the `profile.string` have to match the parameter
#' label exactly, or is a substring OK.
#' @param ylab Label for y-axis. Default is "Change in -log-likelihood".
#' @param components Vector of likelihood components that may be included in
#' plot. List is further refined by any components that are not present in
#' model or have little change over range of profile (based on limit
#' `minfraction`). Hopefully this doesn't need to be changed.
#' @param component.labels Vector of labels for use in the legend that matches
#' the vector in `components`.
#' @param minfraction Minimum change in likelihood (over range considered) as a
#' fraction of change in total likelihood for a component to be included in the
#' figure.
#' @param sort.by.max.change Switch giving option to sort components in legend
#' in order of maximum amount of change in likelihood (over range considered).
#' Default=TRUE.
#' @param col Optional vector of colors for each line.
#' @param pch Optional vector of plot characters for the points.
#' @param lty Line type for the likelihood components.
#' @param lty.total Line type for the total likelihood.
#' @param lwd Line width for the likelihood components.
#' @param lwd.total Line width for the total likelihood.
#' @param cex Character expansion for the points representing the likelihood
#' components.
#' @param cex.total Character expansion for the points representing the total
#' likelihood.
#' @param xlim Range for x-axis. Change in likelihood is calculated relative to
#' values within this range.
#' @param ymax Maximum y-value. Default is 10% greater than largest value
#' plotted.
#' @param xaxs The style of axis interval calculation to be used for the x-axis
#' (see ?par for more info)
#' @param yaxs The style of axis interval calculation to be used for the y-axis
#' (see ?par for more info).
#' @param type Line type (see ?plot for more info).
#' @template legend
#' @template legendloc
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
#' @template plotdir
#' @param add_cutoff Add dashed line at ~1.92 to indicate 95% confidence interval
#' based on common cutoff of half of chi-squared of p=.95 with 1 degree of
#' freedom: `0.5*qchisq(p=cutoff_prob, df=1)`. The probability value
#' can be adjusted using the `cutoff_prob` below.
#' @param cutoff_prob Probability associated with `add_cutoff` above.
#' @param add_no_prior_line Add line showing total likelihood without
#' the prior (only appears when profiled parameter that includes a prior)
#' @template verbose
#' @param \dots Additional arguments passed to the `plot` command.
#' @note Someday the function [profile()] will be improved and
#' made to work directly with this plotting function, but they don't yet work
#' well together. Thus, even if [profile()] is used, the output
#' should be read using [SSgetoutput()] or by multiple calls to
#' [SS_output()].
#' @author Ian G. Taylor, Ian J. Stewart
#' @export
#' @seealso [SSsummarize()], [SSgetoutput()]
#' @family profile functions

SSplotProfile <-
  function(
    summaryoutput,
    plot = TRUE,
    print = FALSE,
    models = "all",
    profile.string = "steep",
    profile.label = NULL,
    exact = FALSE,
    ylab = "Change in -log-likelihood",
    components = c(
      "TOTAL",
      "Catch",
      "Equil_catch",
      "Survey",
      "Discard",
      "Mean_body_wt",
      "Length_comp",
      "Age_comp",
      "Size_at_age",
      "SizeFreq",
      "Morphcomp",
      "Tag_comp",
      "Tag_negbin",
      "Recruitment",
      "InitEQ_Regime",
      "Forecast_Recruitment",
      "Parm_priors",
      "Parm_softbounds",
      "Parm_devs",
      "F_Ballpark",
      "Crash_Pen"
    ),
    component.labels = c(
      "Total",
      "Catch",
      "Equilibrium catch",
      "Index data",
      "Discard",
      "Mean body weight",
      "Length data",
      "Age data",
      "Size-at-age data",
      "Generalized size data",
      "Morph composition data",
      "Tag recapture distribution",
      "Tag recapture total",
      "Recruitment",
      "Initital equilibrium recruitment",
      "Forecast recruitment",
      "Priors",
      "Soft bounds",
      "Parameter deviations",
      "F Ballpark",
      "Crash penalty"
    ),
    minfraction = 0.01,
    sort.by.max.change = TRUE,
    col = "default",
    pch = "default",
    lty = 1,
    lty.total = 1,
    lwd = 2,
    lwd.total = 3,
    cex = 1,
    cex.total = 1.5,
    xlim = "default",
    ymax = "default",
    xaxs = "r",
    yaxs = "r",
    type = "o",
    legend = TRUE,
    legendloc = "topright",
    pwidth = 6.5,
    pheight = 5.0,
    punits = "in",
    res = 300,
    ptsize = 10,
    cex.main = 1,
    plotdir = NULL,
    add_cutoff = FALSE,
    cutoff_prob = 0.95,
    add_no_prior_line = TRUE,
    verbose = TRUE,
    ...
  ) {
    if (print) {
      if (is.null(plotdir)) {
        stop("to print PNG files, you must supply a directory as 'plotdir'")
      }
      # create directory if it's missing
      if (!file.exists(plotdir)) {
        if (verbose) {
          message("creating directory:", plotdir)
        }
        dir.create(plotdir, recursive = TRUE)
      }
    }

    if (length(components) != length(component.labels)) {
      stop(
        "Inputs 'components' and 'component.labels' should have equal length"
      )
    }

    # get stuff from summary output
    n <- summaryoutput[["n"]]
    likelihoods <- summaryoutput[["likelihoods"]]
    if (is.null(likelihoods)) {
      stop(
        "Input 'summaryoutput' needs to be a list output from SSsummarize\n",
        "and have an element named 'likelihoods'."
      )
    }
    pars <- summaryoutput[["pars"]]
    par_prior_likes <- summaryoutput[["par_prior_likes"]]

    # check number of models to be plotted
    if (models[1] == "all") {
      models <- 1:n
    } else {
      if (!all(models %in% 1:n)) {
        stop(
          "Input 'models' should be a vector of values from 1 to n=",
          n,
          " (for your inputs).\n"
        )
      }
    }

    # find the parameter that the profile was over
    if (exact) {
      parnumber <- match(profile.string, pars[["Label"]])
    } else {
      parnumber <- grep(profile.string, pars[["Label"]])
    }
    if (length(parnumber) <= 0) {
      stop(
        "No parameters matching profile.string='",
        profile.string,
        "'",
        sep = ""
      )
    }
    parlabel <- pars[["Label"]][parnumber]
    if (length(parlabel) > 1) {
      stop(
        "Multiple parameters matching profile.string='",
        profile.string,
        "':\n",
        paste(parlabel, collapse = ", "),
        "\nYou may need to use 'exact=TRUE'.",
        sep = ""
      )
    }

    # get vector of parameter values
    parvec <- as.numeric(pars[pars[["Label"]] == parlabel, models])
    if (verbose) {
      message(
        "Parameter matching profile.string=",
        profile.string,
        ": ",
        parlabel
      )
      message(
        "Parameter values (after subsetting based on input 'models'): ",
        paste0(parvec, collapse = ", ")
      )
    }

    # get vector of prior likelihoods for this parameter
    par_prior_like_vec <- as.numeric(par_prior_likes[
      par_prior_likes[["Label"]] == parlabel,
      models
    ])
    # turn off addition of "Total without prior" line if there is no prior
    # on the parameter being profiled over
    if (all(is.na(par_prior_like_vec))) {
      add_no_prior_line <- FALSE
    }
    par_prior_like_vec[is.na(par_prior_like_vec)] <- 0
    if (all(par_prior_like_vec == 0)) {
      add_no_prior_line <- FALSE
    }

    if (verbose & add_no_prior_line) {
      message(
        "Parameter prior likelihoods: ",
        paste0(par_prior_like_vec, collapse = ", ")
      )
    }

    # set x-axis limits
    if (xlim[1] == "default") {
      xlim <- range(parvec)
    }

    # rearange likelihoods to be in columns by type
    # Fixed bug that crashes plot when only a subset of components are listed (Steve Teo)
    prof.table <- data.frame(t(likelihoods[
      likelihoods[["Label"]] %in% components,
      models
    ]))
    names(prof.table) <- likelihoods[
      likelihoods[["Label"]] %in% components,
      ncol(likelihoods)
    ]
    component.labels.good <- rep("", ncol(prof.table))
    for (icol in 1:ncol(prof.table)) {
      ilabel <- which(components == names(prof.table)[icol])
      # print(names(prof.table)[icol])
      # print(ilabel)
      # print(component.labels[ilabel])
      component.labels.good[icol] <- component.labels[ilabel]
    }

    # calculate total likelihood without any prior on the profiled parameter
    TOTAL_no_prior <- prof.table[["TOTAL"]] - par_prior_like_vec

    # subtract minimum value from each likelihood component (over requested parameter range)
    subset <- parvec >= xlim[1] & parvec <= xlim[2]
    for (icol in 1:ncol(prof.table)) {
      prof.table[, icol] <- prof.table[, icol] - min(prof.table[subset, icol])
    }
    TOTAL_no_prior <- TOTAL_no_prior - min(TOTAL_no_prior)
    if (ymax == "default") {
      ymax <- 1.1 * max(prof.table[subset, ])
    }
    ylim <- c(0, ymax)

    # remove columns that have change less than minfraction change relative to total
    column.max <- apply(prof.table[subset, ], 2, max)
    change.fraction <- column.max / column.max[1]
    include <- change.fraction >= minfraction

    nlines <- sum(include)
    if (verbose) {
      message(
        "Likelihood components showing max change as fraction of total change.\n",
        "To change which components are included, change input 'minfraction'.\n"
      )
      print(data.frame(
        frac_change = round(change.fraction, 4),
        include = include,
        label = component.labels.good
      ))
    }
    # stop function if nothing left
    if (nlines == 0) {
      stop("No components included, 'minfraction' should be smaller.")
    }
    component.labels.used <- component.labels.good[include]

    # reorder values
    prof.table <- prof.table[order(parvec), include]
    TOTAL_no_prior <- TOTAL_no_prior[order(parvec)]
    parvec <- parvec[order(parvec)]

    # reorder columns by largest change (if requested, and more than 1 line)
    change.fraction <- change.fraction[include]
    if (nlines > 1) {
      if (sort.by.max.change) {
        neworder <- c(1, 1 + order(change.fraction[-1], decreasing = TRUE))
        prof.table <- prof.table[, neworder]
        component.labels.used <- component.labels.used[neworder]
      }
    }

    # add TOTAL_no_prior to table
    # Note: this is done at this stage rather than when first calculated
    # to avoid dealing with this column while filtering and reordering
    # the other columns
    prof.table <- data.frame(prof.table, TOTAL_no_prior)
    if (add_no_prior_line) {
      component.labels.used <- c(component.labels.used, "Total without prior")
    }

    # define colors and line types
    if (col[1] == "default") {
      col <- rich.colors.short(nlines)
    }
    if (pch[1] == "default") {
      pch <- 1:nlines
    }
    # total without prior matches total (first value)
    if (add_no_prior_line) {
      col <- c(col, col[1])
      pch <- c(pch, NA)
    }

    # make total line wider with bigger points (or whatever user chooses)
    # uses switch() instead of ifelse() because ifelse() doesn't return NULL
    lwd <- c(
      lwd.total,
      rep(lwd, nlines - 1),
      switch(add_no_prior_line + 1, NULL, lwd)
    )
    cex <- c(
      cex.total,
      rep(cex, nlines - 1),
      switch(add_no_prior_line + 1, NULL, cex.total)
    )
    lty <- c(
      lty.total,
      rep(lty, nlines - 1),
      switch(add_no_prior_line + 1, NULL, 2)
    )

    # intuitive profile.label using the parameter label
    if (is.null(profile.label)) {
      if (grepl("steep", parlabel)) {
        profile.label <- "Spawner-recruit steepness (h)"
      }
      if (grepl("R0", parlabel)) {
        profile.label <- paste0(
          "Log of unfished equilibrium recruitment, ",
          expression(log(R[0]))
        )
      }
      if (grepl("NatM", parlabel) && grepl("Fem", parlabel)) {
        profile.label <- "Female natural mortality (M)"
      }
      if (grepl("NatM", parlabel) && grepl("Mal", parlabel)) {
        profile.label <- "Male natural mortality (M)"
      }
      if (grepl("LnQ", parlabel)) {
        profile.label <- paste0("Log of catchability, ", expression(log(q)))
      }
      if (grepl("sigmaR", parlabel)) {
        profile.label <- "SigmaR"
      }
      if (grepl("L_at_Amax", parlabel) && grepl("Fem", parlabel)) {
        profile.label <- "Female length at Amax"
      }
      if (grepl("L_at_Amax", parlabel) && grepl("Mal", parlabel)) {
        profile.label <- "Male length at Amax"
      }
      if (is.null(profile.label)) {
        # use parameter label for x-axis label
        profile.label <- parlabel
        message(
          "The input profile.label = NULL and the parameter label doesn't ",
          "correspond to an automatically generated label. ",
          "Setting profile.label equal to the parameter label."
        )
      }
    }

    # make plot
    plotprofile <- function() {
      plot(
        0,
        type = "n",
        xlim = xlim,
        ylim = ylim,
        xlab = profile.label,
        ylab = ylab,
        yaxs = yaxs,
        xaxs = xaxs,
        ...
      )
      abline(h = 0, col = "grey")
      # optionally add horizontal line at ~1.92 (or other value depending
      # on chosen probability)
      if (add_cutoff) {
        abline(h = 0.5 * qchisq(p = cutoff_prob, df = 1), lty = 2)
      }
      matplot(
        x = parvec,
        y = prof.table,
        type = type,
        pch = pch,
        col = col,
        cex = cex,
        lty = lty,
        lwd = lwd,
        add = T
      )

      if (legend) {
        legend(
          legendloc,
          bty = "n",
          legend = component.labels.used,
          lwd = lwd,
          pt.cex = cex,
          lty = lty,
          pch = pch,
          col = col
        )
      }
      box()
    }

    if (plot) {
      plotprofile()
    }
    if (print) {
      save_png(
        plotinfo = NULL,
        file = "profile_plot_likelihood.png",
        plotdir = plotdir,
        pwidth = pwidth,
        pheight = pheight,
        punits = punits,
        res = res,
        ptsize = ptsize
      )
      plotprofile()
      dev.off()
    }
    out <- data.frame(parvec = parvec, prof.table)
    names(out)[1] <- parlabel
    return(invisible(out))
  }
