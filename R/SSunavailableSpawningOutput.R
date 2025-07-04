#' Plot unavailable spawning output
#'
#' Calculate and plot the unavailable spawning output- separating out ones that
#' are unavailable because they're too small to be selected from ones that are
#' too big to be selected
#'
#' @template replist
#' @template plot
#' @template print
#' @template plotdir
#' @template pwidth
#' @template pheight
#' @template punits
#' @template res
#' @template ptsize
#' @template cex.main
#' @author Megan Stachura, Andrew Cooper, Andi Stephens, Neil Klaer, Ian G. Taylor
#' @export

SSunavailableSpawningOutput <-
  function(
    replist,
    plot = TRUE,
    print = FALSE,
    plotdir = "default",
    pwidth = 6.5,
    pheight = 5.0,
    punits = "in",
    res = 300,
    ptsize = 10,
    cex.main = 1
  ) {
    # table to store information on each plot
    plotinfo <- NULL
    ageselex <- replist[["ageselex"]]
    accuage <- replist[["accuage"]]
    catch <- replist[["catch"]]
    fleet_type <- replist[["fleet_type"]]
    if (!"kill_bio" %in% names(catch)) {
      # model is 3.24 with less info
      catch[["kill_bio"]] <- catch[["Obs"]]
      # Check to make sure all the catch units are the same
      catch.units.same <- all(
        replist[["catch_units"]][1] ==
          replist[["catch_units"]][1:replist[["nfishfleets"]]]
      )
      if (!catch.units.same) {
        warning(
          "Catch units for all fleets are not equal. Calculated weighted
             mean selectivity for calculating unavailable spawning
             output may not be accurate."
        )
      }
      # define fleet_type which is missing from 3.24
      fleet_type <- c(
        rep(1, replist[["nfishfleets"]]),
        rep(3, replist[["nfleets"]] - replist[["nfishfleets"]])
      )
    }

    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }

    # Run the code for each area
    for (area in 1:replist[["nareas"]]) {
      ##########################################################################
      # step 1: calculate catch by fleet by year

      timeseries <- replist[["timeseries"]]

      # get the fishing fleets that fish in this area and aren't surveys (type=3)
      fleets.this.area <- replist[["fleet_ID"]][
        replist[["fleet_area"]] == area &
          fleet_type != 3
      ]
      years.with.catch <- sort(unique(catch[["Yr"]][
        catch[["Fleet"]] %in% fleets.this.area & catch[["kill_bio"]] > 0
      ]))

      ##########################################################################
      # Step 2: Female numbers at age matrix by year
      num.at.age <- replist[["natage"]]

      # old line which used "subset"
      ## num.at.age.female <- subset(num.at.age, Sex==1 & Era=="TIME" & BegMid=="B" & Area==area & Yr %in% years.with.catch)
      # replacement line without "subset"
      num.at.age.female <- num.at.age[
        num.at.age[["Sex"]] == 1 &
          num.at.age[["Era"]] == "TIME" &
          num.at.age$"Beg/Mid" == "B" &
          num.at.age[["Area"]] == area &
          num.at.age[["Yr"]] %in% years.with.catch,
      ]
      years <- num.at.age.female[["Yr"]]
      seas <- num.at.age.female[["Seas"]]
      first.col <- which(names(num.at.age.female) == "0")
      num.at.age.female <- num.at.age.female[,
        first.col:ncol(num.at.age.female)
      ]
      if (max(seas) > 1) {
        row.names(num.at.age.female) <- paste(years, seas, sep = "")
      } else {
        row.names(num.at.age.female) <- years
      }

      ##########################################################################
      # step 3: create an average derived age-based selectivity across fleets
      #         and years using a weighted average catch by fleet by year
      mean.selectivity <- matrix(
        ncol = ncol(num.at.age.female),
        nrow = nrow(num.at.age.female),
        NA
      )

      for (y in seq_along(years.with.catch)) {
        # Get the female selectivity at age in this year for all fleets
        year.get <- years.with.catch[y]
        age.selectivity.female.year <-
          ageselex[
            ageselex[["Sex"]] == 1 &
              ageselex[["Factor"]] == "Asel2" &
              ageselex[["Yr"]] == year.get &
              ageselex[["Fleet"]] %in% fleets.this.area,
          ]
        catch.by.fleet.year <- rep(0, length(fleets.this.area))

        for (ifleet in seq_along(fleets.this.area)) {
          f <- fleets.this.area[ifleet]
          catch.by.fleet.year[ifleet] <- catch[["kill_bio"]][
            catch[["Fleet"]] == f &
              catch[["Yr"]] == year.get
          ]
        }
        # Weight the selectivity at length by fleet for this year based on
        # the propotion of catch that came from each fleet in this year
        for (a in 8:ncol(age.selectivity.female.year)) {
          mean.selectivity[y, a - 7] <- sum(
            catch.by.fleet.year *
              age.selectivity.female.year[, a] /
              sum(catch.by.fleet.year)
          )
        }
      }

      ##########################################################################
      # step 4: multiply the numbers at age matrix by the average selectivity
      #         curves to derive exploitable numbers at age

      exploitable.females.at.age <- num.at.age.female * mean.selectivity

      ##########################################################################
      # step 5: generate spawning output estimates from exploitable numbers at age

      # old line which used "subset"
      ## biology.at.age.female <- subset(replist[["endgrowth"]], Sex==1)
      # replacement line without "subset"
      biology.at.age.female <- replist[["endgrowth"]][
        replist[["endgrowth"]][["Sex"]] == 1,
      ]

      exploitable.spawning.output.by.age <- matrix(
        nrow = nrow(exploitable.females.at.age),
        ncol = ncol(exploitable.females.at.age),
        NA
      )
      for (y in 1:nrow(exploitable.females.at.age)) {
        for (a in 1:ncol(exploitable.females.at.age)) {
          exploitable.spawning.output.by.age[
            y,
            a
          ] <- biology.at.age.female$"Mat*Fecund"[a] *
            exploitable.females.at.age[y, a]
        }
      }

      exploitable.spawning.output <- rowSums(exploitable.spawning.output.by.age)

      ##########################################################################
      # step 6: subtract the results of step 5 from the spawning output from
      #         the assessment to determine unavailable spawning output

      total.spawning.output.by.age <- matrix(
        nrow = nrow(num.at.age.female),
        ncol = ncol(num.at.age.female),
        NA
      )
      for (y in 1:nrow(num.at.age.female)) {
        for (a in 1:ncol(num.at.age.female)) {
          total.spawning.output.by.age[
            y,
            a
          ] <- biology.at.age.female$"Mat*Fecund"[a] *
            num.at.age.female[y, a]
        }
      }

      total.spawning.output <- rowSums(total.spawning.output.by.age)

      # Calculate the cryptic spwning output, by age and total across ages
      cryptic.spawning.output <- total.spawning.output -
        exploitable.spawning.output
      cryptic.spawning.output.by.age <- total.spawning.output.by.age -
        exploitable.spawning.output.by.age

      # If due to rounding error any of the cryptic spawning output values
      # are less than zero, change them to zero
      cryptic.spawning.output[cryptic.spawning.output < 0] <- 0
      cryptic.spawning.output.by.age[cryptic.spawning.output.by.age < 0] <- 0

      ##########################################################################
      # Step 7: Calculate unavailable mature fish with small and big fish separated

      # Get the age that corresponds to the peak mean selectivity for each year
      # If there are multiple ages with the same maximum selectivity, this will
      # give you the first (youngest) maximum
      max.selectivity.cols <- apply(mean.selectivity, 1, function(x) {
        which.max(x)
      })
      small.cells <- matrix(
        0,
        nrow = nrow(cryptic.spawning.output.by.age),
        ncol = ncol(cryptic.spawning.output.by.age)
      )
      large.cells <- matrix(
        0,
        nrow = nrow(cryptic.spawning.output.by.age),
        ncol = ncol(cryptic.spawning.output.by.age)
      )
      for (i in 1:nrow(small.cells)) {
        small.cells[i, 1:(max.selectivity.cols[i] - 1)] <- 1
        large.cells[i, max.selectivity.cols[i]:ncol(small.cells)] <- 1
      }
      small.unavailable.mature.by.age <- cryptic.spawning.output.by.age *
        small.cells
      large.unavailable.mature.by.age <- cryptic.spawning.output.by.age *
        large.cells

      # If due to rounding error any of the cryptic spawning output values
      # are less than zero, change them to zero
      small.unavailable.mature.by.age[small.unavailable.mature.by.age < 0] <- 0
      large.unavailable.mature.by.age[large.unavailable.mature.by.age < 0] <- 0

      small.unavailable.mature <- rowSums(small.unavailable.mature.by.age)
      large.unavailable.mature <- rowSums(large.unavailable.mature.by.age)

      ##########################################################################
      # Step 8:  Plot the results

      # Wrap up the plotting commands in a function
      CrypticPlots <- function() {
        ### Plot total and cryptic spawning output
        layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE))
        par(mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0))
        data <- rbind(
          small.unavailable.mature,
          large.unavailable.mature,
          exploitable.spawning.output
        )
        colnames(data) <- years
        stackpoly(
          years,
          as.matrix(t(data)),
          main = "",
          xlab = "Year",
          ylab = "",
          ylim = c(0, max(total.spawning.output) * 1.25),
          col = c("red", "green4", "blue"),
          las = 1
        )
        mtext(replist[["SpawnOutputLabel"]], 3, line = 0.25)

        legend(
          "topright",
          c("Unavailable Small", "Unavailable Large", "Available"),
          bty = "n",
          fill = c("red", "green4", "blue")
        )

        ### Plot the portion of the spawning output that is cryptic by year
        portion.unavailable <- (cryptic.spawning.output / total.spawning.output)
        portion.unavailable.small <- (small.unavailable.mature /
          total.spawning.output)
        portion.unavailable.large <- (large.unavailable.mature /
          total.spawning.output)
        plot(
          years,
          portion.unavailable,
          xlab = "Year",
          ylab = "",
          ylim = c(0, 1.1),
          type = "l",
          lwd = 2,
          las = 1,
          yaxs = "i"
        )
        mtext("Proportion of Spawning Output Unavailable", 3, line = 0.25)
        lines(years, portion.unavailable.small, col = "red", lwd = 2)
        lines(years, portion.unavailable.large, col = "green4", lwd = 2)
        legend(
          "topright",
          c(
            "Unavailable Small",
            "Unavailable Large",
            "Unavailable Total"
          ),
          bty = "n",
          col = c("red", "green4", "black"),
          lty = c(1, 1, 1)
        )

        ### Plot cryptic spawning output by age by year as a bubble plot
        multiplier <- 2 / max(cryptic.spawning.output.by.age, na.rm = TRUE)
        # allow bubbles to extend beyond boundaries of plot region
        par(xpd = NA)
        plot(
          1,
          1,
          type = "n",
          ylim = c(0, ceiling(accuage * 1.1)),
          xlim = c(min(years.with.catch), max(years.with.catch)),
          las = 1,
          ylab = "Age",
          xlab = "Year",
          bty = "n",
          yaxs = "i"
        )
        mtext(
          "Unavailable Spawning Output by Age and Year",
          3,
          outer = FALSE,
          line = 0.75
        )
        for (y in seq_along(years.with.catch)) {
          for (a in 0:accuage) {
            # plot circles for small unavailable spawning output
            radius.small <- small.unavailable.mature.by.age[y, a + 1] *
              multiplier
            symbols(
              x = years.with.catch[y],
              y = a,
              circles = radius.small,
              fg = "black",
              bg = "red",
              lty = 1,
              lwd = 0.001,
              inches = FALSE,
              add = TRUE
            )

            # plot circles for large unavailable spawning output
            radius.large <- large.unavailable.mature.by.age[y, a + 1] *
              multiplier
            symbols(
              x = years.with.catch[y],
              y = a,
              circles = radius.large,
              fg = "black",
              bg = "green4",
              lty = 1,
              lwd = 0.001,
              inches = FALSE,
              add = TRUE
            )
          }
        }

        # Plot the bubble plot legend
        y.legend <- ceiling(accuage * 1.05)
        larger.circle <- signif(max(cryptic.spawning.output.by.age), digits = 2)
        symbols(
          x = min(years.with.catch) +
            0.3 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + larger.circle * multiplier,
          circles = larger.circle * multiplier,
          fg = "black",
          bg = "white",
          lty = 1,
          lwd = 0.001,
          inches = FALSE,
          add = TRUE
        )
        text(
          x = min(years.with.catch) +
            0.3 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + larger.circle * multiplier,
          labels = larger.circle,
          pos = 3
        )

        smaller.circle <- larger.circle / 2
        symbols(
          x = min(years.with.catch) +
            0.2 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + smaller.circle * multiplier,
          circles = smaller.circle * multiplier,
          fg = "black",
          bg = "white",
          lty = 1,
          lwd = 0.001,
          inches = FALSE,
          add = TRUE
        )
        text(
          x = min(years.with.catch) +
            0.2 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + smaller.circle * multiplier,
          labels = smaller.circle,
          pos = 3
        )

        symbols(
          x = min(years.with.catch) +
            0.65 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + smaller.circle * multiplier,
          circles = smaller.circle * multiplier,
          fg = "black",
          bg = "red",
          lty = 1,
          lwd = 0.001,
          inches = FALSE,
          add = TRUE
        )
        text(
          x = min(years.with.catch) +
            0.65 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + smaller.circle * multiplier,
          labels = "Small",
          pos = 3
        )

        symbols(
          x = min(years.with.catch) +
            0.85 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + smaller.circle * multiplier,
          circles = smaller.circle * multiplier,
          fg = "black",
          bg = "green4",
          lty = 1,
          lwd = 0.001,
          inches = FALSE,
          add = TRUE
        )
        text(
          x = min(years.with.catch) +
            0.85 * (max(years.with.catch) - min(years.with.catch)),
          y = y.legend + smaller.circle * multiplier,
          labels = "Large",
          pos = 3
        )

        # Add a line to show where the cut off between small and large unavailable
        lines(years.with.catch, max.selectivity.cols - 1.5)
        # restore clipping to plot region
        par(xpd = FALSE)

        ##### Plot mean selecitivities by length by year
        cols <- colorRampPalette(
          c("blue", "orange"),
          bias = 1,
          space = c("rgb", "Lab"),
          interpolate = c("linear", "spline"),
          alpha = FALSE
        )(nrow(mean.selectivity))
        cols.legend <- c(cols[1], cols[length(cols)])
        cols <- adjustcolor(cols, alpha.f = 0.3)

        plot(
          0,
          0,
          type = "n",
          ylim = c(0, 1.1),
          xlim = c(0, accuage),
          xlab = "Age",
          ylab = "Selectivity",
          las = 1,
          yaxs = "i"
        )
        mtext(
          "Weighted Mean Selectivity by Year",
          3,
          outer = FALSE,
          line = 0.25
        )
        for (i in 1:nrow(mean.selectivity)) {
          lines(0:accuage, mean.selectivity[i, ], col = cols[i])
        }
        legend(
          "bottomright",
          legend = c(min(years.with.catch), max(years.with.catch)),
          bty = "n",
          col = c(cols.legend[1], cols.legend[2]),
          lty = c(1, 1)
        )

        # Plot title
        title.text <- "Unavailable Spawning Output"
        if (replist[["nareas"]] > 1) {
          title.text <- paste0(title.text, ", Area ", area)
        }
        mtext(title.text, side = 3, outer = TRUE, line = 0.5)
      }

      if (plot) {
        CrypticPlots()
      }

      if (print) {
        if (replist[["nareas"]] > 1) {
          file <- paste0("UnavailableSpawningOutput_Area", area, ".png")
          caption <- paste("Unavailable Spawning Output, Area", area)
        } else {
          file <- paste0("UnavailableSpawningOutput.png")
          caption <- paste("Unavailable Spawning Output")
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
        CrypticPlots()
        dev.off()
      }
    }

    # Return the plot info
    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "Sel"
    }
    return(invisible(plotinfo))
  }
