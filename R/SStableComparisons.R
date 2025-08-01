#' make table comparing quantities across models
#'
#' Creates a table comparing key quantities from multiple models, which is a
#' reduction of the full information in various parts of the list created using
#' the `SSsummarize` function.
#'
#'
#' @param summaryoutput list created by `SSsummarize`
#' @param models optional subset of the models described in
#' `summaryoutput`.  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' @param likenames Labels for likelihood values to include, should match
#' substring of labels in `summaryoutput[["likelihoods"]]`.
#' @param names Labels for parameters or derived quantities to include, should
#' match substring of labels in `summaryoutput[["pars"]]` or
#' `summaryoutput[["quants"]]`.
#' @param digits Optional vector of the number of decimal digits to use in
#' reporting each quantity.
#' @param modelnames optional vector of labels to use as column names. Default
#' is 'model1','model2',etc.
#' @param csv write resulting table to CSV file?
#' @param csvdir directory for optional CSV file
#' @param csvfile filename for CSV file
#' @template verbose
#' @param mcmc summarize MCMC output in table?
#' @author Ian Taylor
#' @export
#' @seealso [SSsummarize()], [SSplotComparisons()],
#' [SS_output()]
SStableComparisons <- function(
  summaryoutput,
  models = "all",
  likenames = c(
    "TOTAL",
    "Survey",
    "Length_comp",
    "Age_comp",
    "priors",
    "Size_at_age"
  ),
  names = c(
    "Recr_Virgin",
    "R0",
    "steep",
    "NatM",
    "L_at_Amax",
    "VonBert_K",
    "SSB_Virg",
    "Bratio_2025",
    "SPRratio_2024"
  ),
  digits = NULL,
  modelnames = "default",
  csv = FALSE,
  csvdir = "workingdirectory",
  csvfile = "parameter_comparison_table.csv",
  verbose = TRUE,
  mcmc = FALSE
) {
  if (verbose) {
    message("running SStableComparisons")
  }

  # get stuff from summary output
  n <- summaryoutput[["n"]]
  nsexes <- summaryoutput[["nsexes"]]
  pars <- summaryoutput[["pars"]]
  quants <- summaryoutput[["quants"]]
  likelihoods <- summaryoutput[["likelihoods"]]
  npars <- summaryoutput[["npars"]]
  indices <- summaryoutput[["indices"]]

  if (models[1] == "all") {
    models <- 1:n
  }
  ncols <- length(models)
  nsexes <- nsexes[models]

  if (modelnames[1] == "default") {
    modelnames <- paste("model", 1:ncols, sep = "")
  }
  tab <- as.data.frame(matrix(NA, nrow = 0, ncol = ncols + 1))

  # get MLE values for table
  if (!mcmc) {
    if (!is.null(likenames)) {
      likenames <- paste(likenames, "_like", sep = "")
      likelihoods[["Label"]] <- paste(likelihoods[["Label"]], "_like", sep = "")
      names <- c(likenames, names)
    }
    nnames <- length(names)

    bigtable <- rbind(
      likelihoods[, c(n + 1, models)],
      pars[, c(n + 1, models)],
      quants[, c(n + 1, models)]
    )
    # loop over big list of names to get values
    for (iname in 1:nnames) {
      name <- names[iname]
      if (!is.null(digits)) {
        digit <- digits[iname]
      }
      if (verbose) {
        message("name=", name, ";")
      }
      if (name == "BLANK") {
        if (verbose) {
          message("added a blank row to the table")
        }
        # add to table
        tab <- rbind(tab, " ")
      } else {
        # get values
        vals <- bigtable[grep(name, bigtable[["Label"]], fixed = TRUE), ]
        # scale recruits into billions, or millions, or thousands
        if (substring(name, 1, 4) == "Recr" & length(grep("like", name)) == 0) {
          median.value <- median(as.numeric(vals[1, -1]), na.rm = TRUE)
          if (median.value > 1e6) {
            vals[1, -1] <- round(vals[1, -1] / 1e6, 6)
            vals[1, 1] <- paste0(vals[1, 1], "_billions")
          } else if (median.value > 1e3) {
            vals[1, -1] <- round(vals[1, -1] / 1e3, 6)
            vals[1, 1] <- paste0(vals[1, 1], "_millions")
          } else {
            vals[1, 1] <- paste0(vals[1, 1], "_thousands")
          }
        }
        if (substring(name, 1, 8) == "TotYield") {
          vals[1, -1] <- round(vals[1, -1] / 1e3, 3)
          vals[1, 1] <- paste0(vals[1, 1], "_thousand_mt")
        }
        if (
          substring(name, 1, 3) %in%
            c("SPB", "SSB") &
            all(!is.na(summaryoutput[["SpawnOutputUnits"]])) &&
            all(summaryoutput[["SpawnOutputUnits"]] == "biomass")
        ) {
          vals[1, -1] <- round(vals[1, -1] / 1e3, 3)
          vals[1, 1] <- paste0(vals[1, 1], "_thousand_mt")
        }

        if (name %in% c("Q", "Q_calc")) {
          Calc_Q <- aggregate(Calc_Q ~ name + Fleet, data = indices, FUN = mean)
          fleetvec <- sort(as.numeric(unique(Calc_Q[["Fleet"]])))
          vals <- data.frame(matrix(
            NA,
            nrow = length(fleetvec),
            ncol = ncol(bigtable)
          ))
          names(vals) <- names(bigtable)
          for (ifleet in seq_along(fleetvec)) {
            f <- fleetvec[ifleet]
            vals[ifleet, 1] <- paste("Q_calc_mean_fleet_", f, sep = "")
            vals[ifleet, -1] <- Calc_Q[["Calc_Q"]][Calc_Q[["Fleet"]] == f]
          }
        }
        if (verbose) {
          message(
            "added ",
            nrow(vals),
            " row",
            ifelse(nrow(vals) != 1, "s", "")
          )
        }
        if (!is.null(digits)) {
          if (verbose) {
            message("rounded to", digit, "digits")
          }
          vals[, -1] <- round(vals[, -1], digit)
        }
        # add to table
        tab <- rbind(tab, vals)
      } # end if not blank
    } # end loop over names
  } # end if not mcmc

  # get medians if using MCMC
  if (mcmc) {
    nnames <- length(names)
    for (iname in 1:nnames) {
      name <- names[iname]
      if (!is.null(digits)) {
        digit <- digits[iname]
      }
      if (verbose) {
        message("name=", name, "; ", sep = "")
      }
      if (name == "BLANK") {
        if (verbose) {
          message("added a blank row to the table")
        }
        # add to table
        tab <- rbind(tab, " ")
      } else {
        vals <- as.data.frame(matrix(NA, ncol = ncols + 1, nrow = 1))
        vals[1] <- name
        for (imodel in models) {
          ### loop over models and create a vector of medians to put into tab
          mcmcTable <- summaryoutput[["mcmc"]][[imodel]]
          # get values
          # for future functionality grabbing more than one column
          tmp <- mcmcTable[, grep(name, names(mcmcTable), fixed = TRUE)]
          if (!is.null(dim(tmp))) {
            if (ncol(tmp) > 0) {
              stop(
                "This only works with a single column from the mcmc. Use a specific name"
              )
            }
          }
          if (!is.null(dim(tmp)) && ncol(tmp) == 0) {
            vals[1, imodel + 1] <- NA
          } else {
            vals[1, imodel + 1] <- median(tmp) # First element is label
          }
        }
        # scale recruits into billions, or millions, or thousands
        if (substring(name, 1, 4) == "Recr") {
          median.value <- median(as.numeric(vals[1, -1]), na.rm = TRUE)
          if (median.value > 1e6) {
            vals[1, -1] <- round(vals[1, -1] / 1e6, 6)
            vals[1, 1] <- paste0(vals[1, 1], "_billions")
          } else if (median.value > 1e3) {
            vals[1, -1] <- round(vals[1, -1] / 1e3, 6)
            vals[1, 1] <- paste0(vals[1, 1], "_millions")
          } else {
            vals[1, 1] <- paste0(vals[1, 1], "_thousands")
          }
        }
        if (substring(name, 1, 8) == "TotYield") {
          vals[1, -1] <- round(vals[1, -1] / 1e3, 3)
          vals[1, 1] <- paste0(vals[1, 1], "_thousand_mt")
        }
        if (
          substring(name, 1, 3) %in%
            c("SPB", "SSB") &
            all(!is.na(summaryoutput[["SpawnOutputUnits"]])) &&
            all(summaryoutput[["SpawnOutputUnits"]] == "biomass")
        ) {
          vals[1, -1] <- round(vals[1, -1] / 1e3, 3)
          vals[1, 1] <- paste0(vals[1, 1], "_thousand_mt")
        }
        if (!is.null(digits)) {
          if (verbose) {
            message("rounded to", digit, "digits")
          }
          vals[, -1] <- round(vals[, -1], digit)
        }

        if (verbose) {
          message("added an mcmc row")
        }
        # add to table
        tab <- rbind(tab, vals)
      } # end if not blank
    } # end loop over names
  } # end if mcmc

  names(tab) <- c("Label", modelnames)
  # check for presence of any content of table and reset rownames
  if (nrow(tab) > 0) {
    rownames(tab) <- 1:nrow(tab)
  } else {
    warning(
      "'names' and 'likenames' didn't match any variables so output is empty\n"
    )
  }

  # write CSV if requested
  if (csv) {
    if (csvdir == "workingdirectory") {
      csvdir <- getwd()
    }
    fullpath <- paste(csvdir, csvfile, sep = "/")
    message("writing table to: ", fullpath)
    write.csv(tab, fullpath, row.names = FALSE)
  }
  # return table
  return(tab)
}
