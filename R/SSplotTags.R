#' Plot tagging data and fits
#'
#' Plot observed and expected tag recaptures in aggregate and by tag group.
#'
#'
#' @template replist
#' @param subplots vector controlling which subplots to create
#' @param latency period of tag mixing to exclude from plots (in future could
#' be included in SS output)
#' @param taggroups which tag groups to include in the plots. Default=NULL
#' causes all groups to be included.
#' @param rows number or rows of panels for regular plots
#' @param cols number or columns of panels for regular plots
#' @param tagrows number or rows of panels for multi-panel plots
#' @param tagcols number or columns of panels for multi-panel plots
#' @template plot
#' @template print
#' @param pntscalar maximum bubble size for balloon plots; each plot scaled
#' independently based on this maximum size and the values plotted. Often some
#' plots look better with one value and others with a larger or smaller value.
#' Default=2.6
#' @param minnbubble minimum number of years below which blank years will be
#' added to bubble plots to avoid cropping
#' @template pwidth
#' @template pheight
#' @template punits
#' @template ptsize
#' @template res
#' @template cex.main
#' @param col1 color for bubbles
#' @param col2 color for lines with expected values
#' @param col3 shading color for observations within latency period
#' @param col4 shading color for observations after latency period
#' @template labels
#' @template plotdir
#' @template verbose
#' @author Andre E. Punt, Ian G. Taylor, Ashleigh J. Novak
#' @import ggplot2
#' @export
#' @seealso [SS_plots()], [SS_output()]
SSplotTags <-
  function(replist = replist, subplots = 1:10, latency = NULL, taggroups = NULL,
           rows = 1, cols = 1,
           tagrows = 3, tagcols = 3,
           plot = TRUE, print = FALSE,
           pntscalar = 2.6, minnbubble = 8,
           pwidth = 6.5, pheight = 5.0, punits = "in", ptsize = 10, res = 300, cex.main = 1,
           col1 = rgb(0, 0, 1, .7), col2 = "red", col3 = "grey95", col4 = "grey70",
           labels = c(
             "Year", # 1
             "Frequency", # 2
             "Tag Group", # 3
             "Fit to tag recaptures by tag group", # 4
             "Post-latency tag recaptures aggregated across tag groups", # 5
             "Observed tag recaptures by year and tag group", # 6
             "Residuals for post-latency tag recaptures: (obs-exp)/sqrt(exp)", # 7
             "Observed and expected post-latency tag recaptures by year and tag group", # 8
             "Summarized observed and expected numbers of recaptures by fleet", # 9
             "Pearson residuals by tag group"
           ), # 10
           plotdir = replist[["inputs"]][["dir"]],
           verbose = TRUE) {
    # table to store information on each plot
    plotinfo <- NULL

    

    tagdbase2 <- replist[["tagdbase2"]]
    if (is.null(tagdbase2) || nrow(tagdbase2) == 0) {
      if (verbose) {
        message("skipping tag plots because there's no tagging data")
      }
    } else {
      # filter tag groups if requested
      if (!is.null(taggroups)) {
        tagdbase2 <- tagdbase2[tagdbase2[["Repl."]] %in% taggroups, ]
        message(
          "Filtered tag groups for plotting based on input vector taggroups\n",
          "Plots will show", length(unique(tagdbase2[["Repl."]])),
          "out of", length(unique(replist[["tagdbase2"]][["Repl."]])),
          "total included in the model."
        )
      }

      # calculations needed for printing to multiple PNG files
      grouprange <- unique(tagdbase2[["Repl."]])
      ngroups <- length(unique(tagdbase2[["Repl."]]))
      npages <- ceiling(ngroups / (tagrows * tagcols))
      nseasons <- replist[["nseasons"]]
      width <- 0.5 / nseasons
      tagreportrates <- replist[["tagreportrates"]]
      tagrelease <- replist[["tagrelease"]]
      tagsalive <- replist[["tagsalive"]]
      tagtotrecap <- replist[["tagtotrecap"]]
      if (is.null(latency)) {
        latency <- replist[["tagfirstperiod"]]
      }

      tagfun1 <- function() {
        # obs & exp recaps by tag group
        par(
          mfcol = c(tagrows, tagcols), mar = c(2.5, 2.5, 2, 1),
          cex.main = cex.main, oma = c(2, 2, 2, 0)
        )

        mu <- tagdbase2[["Exp"]] # mean expected number of recaptures

        # Get overdispersion parameter from model output
        parameters <- replist[["parameters"]]
        overdispersion <- parameters %>%
          dplyr::filter(stringr::str_detect(.data[["Label"]], "TG_overdispersion_")) %>%
          dplyr::select(.data[["Value"]]) # grabs the overdispersion parms for each Tag Group
        tau <- overdispersion[["Value"]][1]

        k <- c(1:length(tagdbase2[["Exp"]]))
        for (i in 1:length(tagdbase2[["Exp"]])) {
          k[i] <- mu[i] / (tau - 1) # variance
        }
        i <- c(1:length(tagdbase2[["Exp"]]))
        CI_down <- stats::qnbinom(c(0.975), size = k[i], mu = mu[i])
        CI_up <- stats::qnbinom(c(0.025), size = k[i], mu = mu[i])
        new_tagdbase2 <- cbind(tagdbase2, CI_up, CI_down)
        new_tagdbase2 <- new_tagdbase2 %>%
          dplyr::mutate(
            CI_down = ifelse(is.nan(CI_down), NA, CI_down),
            CI_up = ifelse(is.nan(CI_up), NA, CI_up)
          )
        new_tagdbase2[["title"]] <- paste("TG_", as.character(new_tagdbase2[["Repl."]]), sep = "")

        newfigure1 <- ggplot(new_tagdbase2, aes(x = .data[["Yr"]], y = .data[["Obs"]])) +
          geom_bar(aes(fill = as.factor(.data[["Seas"]])), position = "dodge", stat = "identity", alpha = 0.6) +
          geom_point(aes(x = .data[["Yr"]], y = .data[["Exp"]], fill = as.factor(.data[["Seas"]])), position = position_dodge(0.9)) +
          facet_wrap(forcats::fct_inorder(as.factor(new_tagdbase2[["title"]])), scales = "free") +
          geom_errorbar(aes(ymin = CI_down, ymax = CI_up, color = as.factor(.data[["Seas"]])), position = position_dodge(0.9), width = 0.25) +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(strip.background = element_blank()) +
          labs(x = "Year", y = "Frequency", fill = "Season", color = "Season")
        print(newfigure1)

        # restore default single panel settings
        par(mfcol = c(rows, cols), mar = c(5, 5, 4, 2) + .1, oma = rep(0, 4))
      }

      # new system which takes latency value as input
      tgroups <- sort(unique(tagdbase2[["Repl."]]))
      x <- NULL
      for (igroup in tgroups) {
        # subset results for only 1 tag group
        temp <- tagdbase2[tagdbase2[["Repl."]] == igroup, ]
        # remove the first rows corresponding to the latency period
        temp <- temp[-(1:latency), ]
        x <- rbind(x, temp)
      }

      # obs vs exp tag recaptures by year aggregated across group
      tagobs <- aggregate(x[["Obs"]], by = list(x[["Yr.S"]], x[["Repl."]]), FUN = sum, na.rm = TRUE)
      tagexp <- aggregate(x[["Exp"]], by = list(x[["Yr.S"]], x[["Repl."]]), FUN = sum, na.rm = TRUE)
      Recaps <- data.frame(
        Yr.S = tagobs[, 1], Group = tagobs[, 2],
        Obs = tagobs[, 3], Exp = tagexp[, 3]
      )
      xlim <- range(Recaps[["Yr.S"]])
      xx2 <- aggregate(Recaps[["Obs"]], by = list(Recaps[["Yr.S"]]), FUN = sum, na.rm = TRUE)
      xx3 <- aggregate(Recaps[["Exp"]], by = list(Recaps[["Yr.S"]]), FUN = sum, na.rm = TRUE)
      RecAg <- data.frame(Yr.S = xx2[, 1], Obs = xx2[, 2], Exp = xx3[, 2])

      tagfun2 <- function() {
        # obs vs exp tag recaptures by year aggregated across group
        plot(0,
          xlim = xlim + c(-0.5, 0.5),
          ylim = c(0, max(RecAg[["Obs"]], RecAg[["Exp"]]) * 1.05),
          type = "n", xaxs = "i", yaxs = "i",
          xlab = labels[1], ylab = labels[2], main = labels[5],
          cex.main = cex.main
        )
        for (iy in 1:nrow(RecAg)) {
          xx <- c(
            RecAg[["Yr.S"]][iy] - width, RecAg[["Yr.S"]][iy] - width,
            RecAg[["Yr.S"]][iy] + width, RecAg[["Yr.S"]][iy] + width
          )
          yy <- c(0, RecAg[["Obs"]][iy], RecAg[["Obs"]][iy], 0)
          polygon(xx, yy, col = col4)
        }
        lines(RecAg[["Yr.S"]], RecAg[["Exp"]], type = "o", pch = 16, lty = 1, lwd = 2)
      }

      Recaps[["Pearson"]] <- (Recaps[["Obs"]] - Recaps[["Exp"]]) / sqrt(Recaps[["Exp"]])
      Recaps[["Pearson"]][Recaps[["Exp"]] == 0] <- NA

      tagfun3 <- function() {
        # bubble plot of observed recapture data
        plottitle <- labels[6]
        bubble3(
          x = Recaps[["Yr.S"]], y = Recaps[["Group"]], z = Recaps[["Obs"]],
          xlab = labels[1], ylab = labels[3], col = col1,
          las = 1, main = plottitle, cex.main = cex.main, maxsize = pntscalar,
          allopen = FALSE, minnbubble = minnbubble
        )
      }
      tagfun4 <- function() {
        # bubble plot of residuals
        plottitle <- labels[7]
        bubble3(
          x = Recaps[["Yr.S"]], y = Recaps[["Group"]], z = Recaps[["Pearson"]],
          xlab = labels[1], ylab = labels[3], col = col1,
          las = 1, main = plottitle, cex.main = cex.main, maxsize = pntscalar,
          allopen = FALSE, minnbubble = minnbubble
        )
      }
      tagfun5 <- function() {
        # line plot by year and group
        plottitle <- labels[8]
        plot(0,
          type = "n", xlim = range(Recaps[["Yr.S"]]),
          ylim = range(Recaps[["Group"]]) + c(0, 1), xlab = labels[1], ylab = labels[3],
          main = plottitle, cex.main = cex.main
        )
        rescale <- .9 * min(ngroups - 1, 5) / max(Recaps[["Obs"]], Recaps[["Exp"]])
        for (igroup in sort(unique(Recaps[["Group"]]))) {
          lines(Recaps[["Yr.S"]][Recaps[["Group"]] == igroup],
            igroup + 0 * Recaps[["Obs"]][Recaps[["Group"]] == igroup],
            col = "grey", lty = 3
          )
          points(Recaps[["Yr.S"]][Recaps[["Group"]] == igroup],
            igroup + rescale * Recaps[["Obs"]][Recaps[["Group"]] == igroup],
            type = "o", pch = 16, cex = .5
          )
          lines(Recaps[["Yr.S"]][Recaps[["Group"]] == igroup],
            igroup + rescale * Recaps[["Exp"]][Recaps[["Group"]] == igroup],
            col = col2, lty = "42", lwd = 2
          )
        }
        legend("topleft",
          bty = "n", lty = c("91", "42"),
          pch = c(16, NA), pt.cex = c(.5, NA),
          col = c(1, 2), lwd = c(1, 2), legend = c("Observed", "Expected")
        )
      }
      tagfun6 <- function() {
        # a function to plot tag parameters after transformation
        # into reporting rate and tag loss quantities

        par(mfrow = c(2, 2))
        # first plot is reporting rate parameters
        barplot(
          height = tagreportrates[["Init_Reporting"]],
          names.arg = tagreportrates[["Fleet"]], ylim = c(0, 1), yaxs = "i",
          ylab = "Reporting rate", xlab = "Fleet number",
          main = "Initial reporting rate"
        )
        box()

        # second plot shows any decay in reporting rate over time
        matplot(0:5, exp((0:5) %*% t(tagreportrates[["Report_Decay"]])),
          type = "l", lwd = 3, lty = 1, col = rich.colors.short(nrow(tagreportrates)),
          ylim = c(0, 1.05), yaxs = "i",
          ylab = "Reporting rate", xlab = "Time at liberty (years)",
          main = "Reporting rate decay"
        )

        # third plot shows initial tag loss
        barplot(
          height = tagrelease[["Init_Loss"]],
          names.arg = tagrelease[["Fleet"]], ylim = c(0, 1), yaxs = "i",
          ylab = "Initial tag loss", xlab = "Tag group",
          main = "Initial tag loss\n(fraction of tags lost at time of tagging)"
        )
        box()

        # fourth plot shows chronic tag loss
        barplot(
          height = tagrelease[["Chron_Loss"]],
          names.arg = tagrelease[["Fleet"]], ylim = c(0, 1), yaxs = "i",
          ylab = "Chronic tag loss", xlab = "Tag group",
          main = "Chronic tag loss\n(fraction of tags lost per year)"
        )
        box()

        # restore default single panel settings
        par(mfcol = c(rows, cols), mar = c(5, 5, 4, 2) + .1, oma = rep(0, 4))
      }

      tagfun7 <- function() {
        # a function to plot the "tags alive" matrix
        xvals <- as.numeric(substring(names(tagsalive)[-1], 7))
        matplot(xvals, t(tagsalive[, -1]),
          type = "l", lwd = 3,
          col = rich.colors.short(nrow(tagsalive)),
          xlab = "Period at liberty",
          ylab = "Estimated number of alive tagged fish",
          main = "'Tags alive' by tag group"
        )
        abline(h = 0, col = "grey")
      }

      tagfun8 <- function() {
        # a function to plot the "total recaptures" matrix
        xvals <- as.numeric(substring(names(tagtotrecap)[-1], 7))
        matplot(xvals, t(tagtotrecap[, -1]),
          type = "l", lwd = 3,
          col = rich.colors.short(nrow(tagtotrecap)),
          xlab = "Period at liberty",
          ylab = "Estimated number of recaptures",
          main = "'Total recaptures' by tag group"
        )
        abline(h = 0, col = "grey")
      }

      tagdata <- replist[["tagdbase1"]]

      # need to get the max number of fleets you have so you can rep over that.
      max_num_fleets <- max(tagdata[["Fleet"]])
      # make a new column to bind to your other frame that contains
      # the breakdown of obs + exp recaps by fleet
      expected_by_fleets <- as.data.frame(rep(tagdbase2[["Exp"]], each = max_num_fleets))
      names(expected_by_fleets)[1] <- "Expected" # rename column
      new_tagdata <- cbind(tagdata, expected_by_fleets) # bind new column to tagdata dataframe
      fleet_numbers <- new_tagdata %>%
        dplyr::mutate(
          Numbers_Obs = round(.data[["Obs"]] * .data[["Nsamp_adj"]]),
          Numbers_Exp = round(.data[["Exp"]] * .data[["Expected"]])
        ) # generate exp. and obs. recaptures by fleet
      fleet_numbers2 <- fleet_numbers %>%
        dplyr::group_by(.data[["Fleet"]], .data[["Yr"]]) %>%
        dplyr::summarize(sum_exp = sum(.data[["Numbers_Exp"]]), sum_obs = sum(.data[["Numbers_Obs"]]))

      fleet_numbers2[["fleet_title"]] <- paste("Fleet_", as.character(fleet_numbers2[["Fleet"]]), sep = "")

      mycols <- rep("black", max_num_fleets) # set colors for plotting expected values
      myfill <- rep("gray35", max_num_fleets)

      tagfun9 <- function() {
        # summarized observed and expected numbers of recaptures by fleet
        fleet_plot2 <- ggplot(fleet_numbers2, aes(x = .data[["Yr"]], y = .data[["sum_obs"]])) +
          geom_bar(aes(fill = as.factor(.data[["Fleet"]])), stat = "identity", alpha = 0.5) +
          geom_line(aes(y = .data[["sum_exp"]], color = as.factor(.data[["Fleet"]])), linetype = 1, size = 1) +
          facet_wrap(forcats::fct_inorder(as.factor(fleet_numbers2[["fleet_title"]])), scales = "free") +
          scale_fill_manual(values = myfill) +
          scale_color_manual(values = mycols) +
          labs(x = "Year", y = "Number of recaptures", subtitle = "Observed (bar) and expected (line)") +
          theme(strip.background = element_blank(), legend.position = "none")
        print(fleet_plot2)
      }

      # Calculate Pearson Residuals by fleet
      fleetnumbers_PRs <- fleet_numbers %>%
        dplyr::mutate(Pearson = (.data[["Numbers_Obs"]] - .data[["Numbers_Exp"]]) / sqrt(.data[["Numbers_Exp"]]))

      fleetnumbers_PRs[["Pearson"]][is.nan(fleetnumbers_PRs[["Pearson"]])] <- NA
      fleetnumbers_PRs[["Pearson"]][!is.finite(fleetnumbers_PRs[["Pearson"]])] <- NA

      # Adjust names
      fleetnumbers_PRs[["TGTitle"]] <- paste("TG_", as.character(fleetnumbers_PRs[["Repl."]]), sep = "")

      # Set limits and breaks of residuals for plotting
      limits <- as.numeric(round(range(fleetnumbers_PRs[["Pearson"]], na.rm = TRUE)))
      breaks <- seq(limits[1], limits[2], by = 3)
      legend_size <- c(abs(breaks))

      tagfun10 <- function() {
        # pearson residuals by tag group and fleets
        pearson_plot1 <- ggplot(fleetnumbers_PRs, aes(x = .data[["Yr"]], y = forcats::fct_inorder(as.factor(.data[["Fleet"]])))) +
          geom_point(aes(size = .data[["Pearson"]], color = .data[["Pearson"]]), alpha = 0.75, na.rm = TRUE) +
          scale_color_continuous(name = "Pearson\nResiduals", limits = limits, breaks = breaks, type = "viridis") +
          scale_size_continuous(name = "Pearson\nResiduals", limits = limits, breaks = breaks) +
          facet_wrap(forcats::fct_inorder(as.factor(fleetnumbers_PRs[["TGTitle"]])), scales = "free_x") +
          xlab("Year") +
          ylab("Fleets") +
          guides(color = guide_legend(), size = guide_legend(override.aes = list(size = legend_size)))
        print(pearson_plot1)
      }

      # make plots
      if (plot) {
        if (1 %in% subplots) tagfun1()
        if (2 %in% subplots) tagfun2()
        if (3 %in% subplots) tagfun3()
        if (4 %in% subplots) tagfun4()
        if (5 %in% subplots) tagfun5()
        if (6 %in% subplots) tagfun6()
        if (7 %in% subplots) tagfun7()
        if (8 %in% subplots) tagfun8()
        if (9 %in% subplots) tagfun9()
        if (10 %in% subplots) tagfun10()
      }
      # send to files if requested
      if (print) {
        if (1 %in% subplots) {
          file <- "tags_by_group.png"
          caption <- "Expected and observed recaptures by tag group"
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun1()
          dev.off()
        }
        if (2 %in% subplots) {
          file <- "tags_aggregated.png"
          caption <- labels[5]
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun2()
          dev.off()
        }
        if (3 %in% subplots) {
          file <- "tags_data_bubbleplot.png"
          caption <- labels[6]
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun3()
          dev.off()
        }
        if (4 %in% subplots) {
          file <- "tags_residuals.png"
          caption <- labels[7]
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun4()
          dev.off()
        }
        if (5 %in% subplots) {
          file <- "tags_lines.png"
          caption <- labels[8]
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun5()
          dev.off()
        }
        if (6 %in% subplots) {
          file <- "tags_parameters.png"
          caption <- "Tag-related parameters"
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun6()
          dev.off()
        }
        if (7 %in% subplots) {
          file <- "tags_alive.png"
          caption <- "'Tags alive' by tag group"
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun7()
          dev.off()
        }
        if (8 %in% subplots) {
          file <- "tags_total_recaptures.png"
          caption <- "Total tag recaptures"
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun8()
          dev.off()
        }
        if (9 %in% subplots) {
          file <- "summarized_recaptures_fleet.png"
          caption <- "summarized observed and expected numbers of recaptures by fleet"
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun9()
          dev.off()
        }
        if (10 %in% subplots) {
          file <- "pearson_residuals_taggroup.png"
          caption <- "Pearson residuals by tag group"
          plotinfo <- save_png(
            plotinfo = plotinfo, file = file, plotdir = plotdir, pwidth = pwidth,
            pheight = pheight, punits = punits, res = res, ptsize = ptsize,
            caption = caption
          )
          tagfun10()
          dev.off()
        }
      }
      flush.console()
    } # end if data
    if (!is.null(plotinfo)) plotinfo[["category"]] <- "Tag"
    return(invisible(plotinfo))
  } # end SSplotTags
