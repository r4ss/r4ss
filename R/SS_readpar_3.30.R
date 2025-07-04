#' read .par file from SS version 3.30
#'
#' Read Stock Synthesis (version 3.30) parameter file into list object in R.
#'
#'
#' @param parfile Filename either with full path or relative to working directory.
#' @param datsource list or character. If list, should be a list produced
#' from [SS_readdat()]. If character, should be the full file location of an
#' SS data file.
#' @param ctlsource list or character. If list, should be a list produced
#' from [SS_writectl()]. If character, should be the full file location of an
#' SS control file.
#' @template verbose
#' @author Nathan R. Vaughan
#' @export
#' @seealso [SS_writepar_3.30()],
#' [SS_readctl()],
#' [SS_readdat()],
#' [SS_readstarter()],
#' [SS_readforecast()],
#' [SS_writectl()],
#' [SS_writedat()],
#' [SS_writestarter()],
#' [SS_writeforecast()],
SS_readpar_3.30 <- function(parfile, datsource, ctlsource, verbose = TRUE) {
  if (is.character(datsource)) {
    datlist <- SS_readdat(file = datsource, version = "3.30", verbose = FALSE)
  } else if (is.list(datsource)) {
    datlist <- datsource
  } else {
    stop(
      "Reading parameter file contents requires a data file location or list object be specified"
    )
  }

  if (is.character(ctlsource)) {
    ctllist <- SS_readctl(
      file = ctlsource,
      use_datlist = TRUE,
      datlist = datlist,
      version = "3.30",
      verbose = FALSE
    )
  } else if (is.list(ctlsource)) {
    ctllist <- ctlsource
  } else {
    stop(
      "Reading parameter file contents requires a control file location or list object be specified"
    )
  }

  # function to read Stock Synthesis parameter files
  if (verbose) {
    message("running SS_readpar_3.30")
  }
  parvals <- readLines(parfile, warn = FALSE)

  parlist <- list()
  parlist[["headerlines"]] <- parvals[1:3]
  dev_parm_start <- NULL
  dev_parm_end <- NULL
  dev_parm_labels <- NULL
  # Build mortality and growth parameter list
  if (length(grep("MGparm", parvals)) > 0) {
    # Read in the values for mortality and growth parameters
    MG_seq <- as.numeric(parvals[(grep("MGparm", parvals) + 1)])
    # Create list object from the base control file parameter matrix
    if (!is.null(ctllist[["MG_parms"]])) {
      parlist[["MG_parms"]] <- ctllist[["MG_parms"]][, 3:4]
    } else {
      stop("Missing ctllist[['MG_parms']]")
    }
    # Add time varying mortality and growth parameters if they exist
    if (
      any(ctllist[["MG_parms"]][, c("env_var&link", "dev_link", "Block")] != 0)
    ) {
      if (!is.null(ctllist[["MG_parms_tv"]])) {
        parlist[["MG_parms"]] <- rbind(
          parlist[["MG_parms"]],
          ctllist[["MG_parms_tv"]][, 3:4]
        )
      } else {
        tmp_parlabs <- get_tv_parlabs(
          full_parms = ctllist[["MG_parms"]],
          block_design = ctllist[["Block_Design"]]
        )
        tmp_tv <- data.frame(
          INIT = rep(NA, times = length(tmp_parlabs)),
          PRIOR = rep(NA, times = length(tmp_parlabs)),
          row.names = tmp_parlabs,
          stringsAsFactors = FALSE
        )
        parlist[["MG_parms"]] <- rbind(parlist[["MG_parms"]], tmp_tv)
      }
      dev_temp <- ctllist[["MG_parms"]][
        ctllist[["MG_parms"]][, 9] > 0,
        ,
        drop = FALSE
      ]
      if (length(dev_temp[, 9]) > 0) {
        dev_parm_start <- c(dev_parm_start, dev_temp[, 10])
        dev_parm_end <- c(dev_parm_end, dev_temp[, 11])
        dev_parm_labels <- c(
          dev_parm_labels,
          paste0(rownames(dev_temp), "_dev_seq")
        )
      }
    }
    # Add seasonal mortality and growth parameters if they exist
    if (!is.null(ctllist[["MG_parms_seas"]])) {
      parlist[["MG_parms"]] <- rbind(
        parlist[["MG_parms"]],
        ctllist[["MG_parms_seas"]][, 3:4]
      )
    }
    # Rename columns and add final parameter estimate data from par file
    colnames(parlist[["MG_parms"]]) <- c("INIT", "ESTIM")
    parlist[["MG_parms"]][, 2] <- MG_seq
  }

  # Build stock recruitment parameter list
  if (length(grep("SR_parm", parvals)) > 0) {
    # Read in the values for stock recruitment parameters
    SR_seq <- as.numeric(parvals[(grep("SR_parm", parvals) + 1)])
    # Create list object from the base control file parameter matrix
    if (!is.null(ctllist[["SR_parms"]])) {
      parlist[["SR_parms"]] <- ctllist[["SR_parms"]][, 3:4]
    } else {
      stop("Missing ctllist[['SR_parms']]")
    }
    # Add time varying stock recruitment parameters if they exist
    if (
      any(ctllist[["SR_parms"]][, c("env_var&link", "dev_link", "Block")] != 0)
    ) {
      if (!is.null(ctllist[["SR_parms_tv"]])) {
        parlist[["SR_parms"]] <- rbind(
          parlist[["SR_parms"]],
          ctllist[["SR_parms_tv"]][, 3:4]
        )
      } else {
        tmp_parlabs <- get_tv_parlabs(
          full_parms = ctllist[["SR_parms"]],
          block_design = ctllist[["Block_Design"]]
        )
        tmp_tv <- data.frame(
          INIT = rep(NA, times = length(tmp_parlabs)),
          PRIOR = rep(NA, times = length(tmp_parlabs)),
          row.names = tmp_parlabs,
          stringsAsFactors = FALSE
        )
        parlist[["SR_parms"]] <- rbind(parlist[["SR_parms"]], tmp_tv)
      }
      dev_temp <- ctllist[["SR_parms"]][
        ctllist[["SR_parms"]][, 9] > 0,
        ,
        drop = FALSE
      ]
      if (length(dev_temp[, 9]) > 0) {
        dev_parm_start <- c(dev_parm_start, dev_temp[, 10])
        dev_parm_end <- c(dev_parm_end, dev_temp[, 11])
        dev_parm_labels <- c(
          dev_parm_labels,
          paste0(rownames(dev_temp), "_dev_seq")
        )
      }
    }
    # Rename columns and add final parameter estimate data from par file
    colnames(parlist[["SR_parms"]]) <- c("INIT", "ESTIM")
    parlist[["SR_parms"]][, 2] <- SR_seq
  }

  # Read in recruitment deviation cycle parameters if they exist
  if (length(grep("recdev_cycle_parm", parvals)) > 0) {
    parlist[["recdev_cycle_parm"]] <- ctllist[["recr_cycle_pars"]][, 3:4]
    rec_temp <- as.numeric(strsplit(
      parvals[(grep("recdev_cycle_parm", parvals) + 1)],
      " "
    )[[1]])
    rec_temp <- rec_temp[!is.na(rec_temp)]
    parlist[["recdev_cycle_parm"]][, 2] <- rec_temp
    colnames(parlist[["recdev_cycle_parm"]]) <- c("INIT", "ESTIM")
  }

  # Build and read in early phase recruitment deviations if they exist
  if (length(grep("recdev_early", parvals)) > 0) {
    if (ctllist[["recdev_early_start"]] < 0) {
      # change to absolute
      ctllist[["recdev_early_start"]] <- ctllist[["MainRdevYrFirst"]] +
        ctllist[["recdev_early_start"]]
    }
    parlist[["recdev_early"]] <- matrix(
      NA,
      nrow = (ctllist[["MainRdevYrFirst"]] - ctllist[["recdev_early_start"]]),
      ncol = 2
    )
    parlist[["recdev_early"]][, 1] <- ctllist[["recdev_early_start"]]:(ctllist[[
      "MainRdevYrFirst"
    ]] -
      1)
    rec_temp <- as.numeric(strsplit(
      parvals[(grep("recdev_early", parvals) + 1)],
      " "
    )[[1]])
    rec_temp <- rec_temp[!is.na(rec_temp)]
    parlist[["recdev_early"]][, 2] <- rec_temp
    colnames(parlist[["recdev_early"]]) <- c("year", "recdev")
  }

  # Build and read in main phase recruitment deviations if do recruitment deviations = 1
  if (length(grep("recdev1", parvals)) > 0) {
    parlist[["recdev1"]] <- matrix(
      NA,
      nrow = (ctllist[["MainRdevYrLast"]] - ctllist[["MainRdevYrFirst"]] + 1),
      ncol = 2
    )
    parlist[["recdev1"]][, 1] <- ctllist[["MainRdevYrFirst"]]:ctllist[[
      "MainRdevYrLast"
    ]]
    rec_temp <- as.numeric(strsplit(
      parvals[(grep("recdev1", parvals) + 1)],
      " "
    )[[1]])
    rec_temp <- rec_temp[!is.na(rec_temp)]
    parlist[["recdev1"]][seq_along(rec_temp), 2] <- rec_temp
    parlist[["recdev1"]] <- parlist[["recdev1"]][seq_along(rec_temp), ]
    colnames(parlist[["recdev1"]]) <- c("year", "recdev")
  }

  # Build and read in main phase recruitment deviations if do recruitment deviations = 2
  if (length(grep("recdev2", parvals)) > 0) {
    parlist[["recdev2"]] <- matrix(
      NA,
      nrow = (ctllist[["MainRdevYrLast"]] - ctllist[["MainRdevYrFirst"]] + 1),
      ncol = 2
    )
    parlist[["recdev2"]][, 1] <- ctllist[["MainRdevYrFirst"]]:ctllist[[
      "MainRdevYrLast"
    ]]
    rec_temp <- as.numeric(strsplit(
      parvals[(grep("recdev2", parvals) + 1)],
      " "
    )[[1]])
    rec_temp <- rec_temp[!is.na(rec_temp)]
    parlist[["recdev2"]][seq_along(rec_temp), 2] <- rec_temp
    parlist[["recdev2"]] <- parlist[["recdev2"]][seq_along(rec_temp), ]
    colnames(parlist[["recdev2"]]) <- c("year", "recdev")
  }

  # Build and read in forecast phase recruitment deviations if they exist
  if (length(grep("Fcast_recruitments", parvals)) > 0) {
    rec_temp <- as.numeric(strsplit(
      parvals[(grep("Fcast_recruitments", parvals) + 1)],
      " "
    )[[1]])
    rec_temp <- rec_temp[!is.na(rec_temp)]
    parlist[["recdev_forecast"]] <- matrix(
      NA,
      nrow = length(rec_temp),
      ncol = 2
    )
    parlist[["recdev_forecast"]][, 1] <- (ctllist[["MainRdevYrLast"]] +
      1):(ctllist[["MainRdevYrLast"]] + length(rec_temp))
    parlist[["recdev_forecast"]][, 2] <- rec_temp
    colnames(parlist[["recdev_forecast"]]) <- c("year", "recdev")
  }

  # Build and read in forecast phase implementation error values if they exist
  if (length(grep("Fcast_impl_error", parvals)) > 0) {
    imp_temp <- as.numeric(strsplit(
      parvals[(grep("Fcast_impl_error", parvals) + 1)],
      " "
    )[[1]])
    imp_temp <- imp_temp[!is.na(imp_temp)]
    parlist[["Fcast_impl_error"]] <- matrix(
      NA,
      nrow = length(imp_temp),
      ncol = 2
    )
    parlist[["Fcast_impl_error"]][, 1] <- (ctllist[["MainRdevYrLast"]] +
      1):(ctllist[["MainRdevYrLast"]] + length(imp_temp))
    parlist[["Fcast_impl_error"]][, 2] <- imp_temp
    colnames(parlist[["Fcast_impl_error"]]) <- c("year", "impl_error")
  }

  # Read in initial Fishing mortality rates if they exist
  if (length(grep("init_F", parvals)) > 0) {
    parlist[["init_F"]] <- as.numeric(parvals[(grep("init_F", parvals) + 1)])
  }

  # Build and read in annual fleet specific fishing mortality rates if they exist
  if (length(grep("F_rate", parvals)) > 0) {
    temp_Frate_1 <- datlist[["catch"]]
    temp_Frate_1 <- temp_Frate_1[
      order(
        temp_Frate_1[["fleet"]],
        temp_Frate_1[["year"]],
        temp_Frate_1[["seas"]]
      ),
    ]
    # This option filters the catch data frame to only include values for fleets that
    # have estimated parameters when using F_method=4 which allows some fleets to
    # have F parameters and some to use the hybrid method.
    if (!is.null(ctllist[["F_4_Fleet_Parms"]])) {
      temp_Frate_1 <- temp_Frate_1[
        is.element(
          temp_Frate_1[["fleet"]],
          ctllist[["F_4_Fleet_Parms"]][["fleet"]][
            ctllist[["F_4_Fleet_Parms"]][["first_parm_phase"]] != 99
          ]
        ),
        ,
        drop = FALSE
      ]
    }
    temp_Frate_1 <- temp_Frate_1[temp_Frate_1[["year"]] > 0, 1:4, drop = FALSE]
    temp_Frate_2 <- temp_Frate_1[temp_Frate_1[["catch"]] > 0, , drop = FALSE]
    if (length(temp_Frate_1[, 1]) == length(grep("F_rate", parvals))) {
      temp_Frate_1[, 4] <- as.numeric(parvals[(grep("F_rate", parvals) + 1)])
      colnames(temp_Frate_1) <- c("year", "seas", "fleet", "F")
      parlist[["F_rate"]] <- temp_Frate_1
    } else if (length(temp_Frate_2[, 1]) == length(grep("F_rate", parvals))) {
      temp_Frate_2[, 4] <- as.numeric(parvals[(grep("F_rate", parvals) + 1)])
      colnames(temp_Frate_2) <- c("year", "seas", "fleet", "F")
      parlist[["F_rate"]] <- temp_Frate_2
    } else {
      stop(
        "The length of the catch matrix (",
        length(temp_Frate_1[, 1]),
        ", or ",
        length(temp_Frate_2[, 1]),
        "
           with zero catches removed) does not match with the length of the F_rate parameter vector (",
        length(grep("F_rate", parvals)),
        ")"
      )
    }
  }

  # Build catchability Q parameter list
  if (length(grep("Q_parm", parvals)) > 0) {
    # Read in the values for catchability Q parameters
    Q_seq <- as.numeric(parvals[(grep("Q_parm", parvals) + 1)])
    # Create list object from the base control file parameter matrix
    if (!is.null(ctllist[["Q_parms"]])) {
      parlist[["Q_parms"]] <- ctllist[["Q_parms"]][, 3:4]
    } else {
      stop("Missing ctllist[['Q_parms']]")
    }
    # Add time varying catchability Q parameters if they exist
    if (
      any(ctllist[["Q_parms"]][, c("env_var&link", "dev_link", "Block")] != 0)
    ) {
      if (!is.null(ctllist[["Q_parms_tv"]])) {
        parlist[["Q_parms"]] <- rbind(
          parlist[["Q_parms"]],
          ctllist[["Q_parms_tv"]][, 3:4]
        )
      } else {
        tmp_parlabs <- get_tv_parlabs(
          full_parms = ctllist[["Q_parms"]],
          block_design = ctllist[["Block_Design"]]
        )
        tmp_tv <- data.frame(
          INIT = rep(NA, times = length(tmp_parlabs)),
          PRIOR = rep(NA, times = length(tmp_parlabs)),
          row.names = tmp_parlabs,
          stringsAsFactors = FALSE
        )
        parlist[["Q_parms"]] <- rbind(parlist[["Q_parms"]], tmp_tv)
      }
      dev_temp <- ctllist[["Q_parms"]][
        ctllist[["Q_parms"]][, 9] > 0,
        ,
        drop = FALSE
      ]
      if (length(dev_temp[, 9]) > 0) {
        dev_parm_start <- c(dev_parm_start, dev_temp[, 10])
        dev_parm_end <- c(dev_parm_end, dev_temp[, 11])
        dev_parm_labels <- c(
          dev_parm_labels,
          paste0(rownames(dev_temp), "_dev_seq")
        )
      }
    }
    # Rename columns and add final parameter estimate data from par file
    colnames(parlist[["Q_parms"]]) <- c("INIT", "ESTIM")
    parlist[["Q_parms"]][, 2] <- Q_seq
  }

  # Read in selectivity parameters
  # Build list object for base selectivity parameters
  if (length(grep("selparm", parvals)) > 0) {
    # Add base size selectivity parameters if they exist
    if (!is.null(ctllist[["size_selex_parms"]])) {
      if (!is.null(parlist[["S_parms"]])) {
        parlist[["S_parms"]] <- rbind(
          parlist[["S_parms"]],
          ctllist[["size_selex_parms"]][, 3:4]
        )
      } else {
        parlist[["S_parms"]] <- ctllist[["size_selex_parms"]][, 3:4]
      }
    }
    # Add base age selectivity parameters if they exist
    if (!is.null(ctllist[["age_selex_parms"]])) {
      if (!is.null(parlist[["S_parms"]])) {
        parlist[["S_parms"]] <- rbind(
          parlist[["S_parms"]],
          ctllist[["age_selex_parms"]][, 3:4]
        )
      } else {
        parlist[["S_parms"]] <- ctllist[["age_selex_parms"]][, 3:4]
      }
    }
    # Add dirichlet selectivity parameters if they exist
    if (!is.null(ctllist[["dirichlet_parms"]])) {
      if (!is.null(parlist[["S_parms"]])) {
        parlist[["S_parms"]] <- rbind(
          parlist[["S_parms"]],
          ctllist[["dirichlet_parms"]][, 3:4]
        )
      } else {
        parlist[["S_parms"]] <- ctllist[["dirichlet_parms"]][, 3:4]
      }
    }
    # Add time varying size selectivity parameters if they exist
    if (
      any(
        ctllist[["size_selex_parms"]][, c(
          "env_var&link",
          "dev_link",
          "Block"
        )] !=
          0
      )
    ) {
      if (!is.null(ctllist[["size_selex_parms_tv"]])) {
        parlist[["S_parms"]] <- rbind(
          parlist[["S_parms"]],
          ctllist[["size_selex_parms_tv"]][, 3:4]
        )
      } else {
        tmp_parlabs <- get_tv_parlabs(
          full_parms = ctllist[["size_selex_parms"]],
          block_design = ctllist[["Block_Design"]]
        )
        tmp_tv <- data.frame(
          INIT = rep(NA, times = length(tmp_parlabs)),
          PRIOR = rep(NA, times = length(tmp_parlabs)),
          row.names = tmp_parlabs,
          stringsAsFactors = FALSE
        )
        parlist[["S_parms"]] <- rbind(parlist[["S_parms"]], tmp_tv)
      }
      dev_temp <- ctllist[["size_selex_parms"]][
        ctllist[["size_selex_parms"]][, 9] > 0,
        ,
        drop = FALSE
      ]
      if (length(dev_temp[, 9]) > 0) {
        dev_parm_start <- c(dev_parm_start, dev_temp[, 10])
        dev_parm_end <- c(dev_parm_end, dev_temp[, 11])
        dev_parm_labels <- c(
          dev_parm_labels,
          paste0(rownames(dev_temp), "_dev_seq")
        )
      }
    }
    # Add time varying age selectivity parameters if they exist
    if (
      any(
        ctllist[["age_selex_parms"]][, c(
          "env_var&link",
          "dev_link",
          "Block"
        )] !=
          0
      )
    ) {
      if (!is.null(ctllist[["age_selex_parms_tv"]])) {
        parlist[["S_parms"]] <- rbind(
          parlist[["S_parms"]],
          ctllist[["age_selex_parms_tv"]][, 3:4]
        )
      } else {
        tmp_parlabs <- get_tv_parlabs(
          full_parms = ctllist[["age_selex_parms"]],
          block_design = ctllist[["Block_Design"]]
        )
        tmp_tv <- data.frame(
          INIT = rep(NA, times = length(tmp_parlabs)),
          PRIOR = rep(NA, times = length(tmp_parlabs)),
          row.names = tmp_parlabs,
          stringsAsFactors = FALSE
        )
        parlist[["S_parms"]] <- rbind(parlist[["S_parms"]], tmp_tv)
      }
      dev_temp <- ctllist[["age_selex_parms"]][
        ctllist[["age_selex_parms"]][, 9] > 0,
        ,
        drop = FALSE
      ]
      if (length(dev_temp[, 9]) > 0) {
        dev_parm_start <- c(dev_parm_start, dev_temp[, 10])
        dev_parm_end <- c(dev_parm_end, dev_temp[, 11])
        dev_parm_labels <- c(
          dev_parm_labels,
          paste0(rownames(dev_temp), "_dev_seq")
        )
      }
    }

    # Add 2DAR selectivity parameters if they exist
    if (!is.null(ctllist[["pars_2D_AR"]])) {
      if (!is.null(parlist[["S_parms"]])) {
        parlist[["S_parms"]] <- rbind(
          parlist[["S_parms"]],
          ctllist[["pars_2D_AR"]][, 3:4]
        )
      } else {
        parlist[["S_parms"]] <- ctllist[["pars_2D_AR"]][, 3:4]
      }
    }

    # Add 2DAR devs if they exist
    if (!is.null(ctllist[["specs_2D_AR"]])) {
      dev_temp <- ctllist[["specs_2D_AR"]]
      if (length(dev_temp[, 1]) > 0) {
        dev_parm_start <- c(dev_parm_start, rep(1, length(dev_temp[, 1])))
        dev_parm_end <- c(
          dev_parm_end,
          (dev_temp[, 3] - dev_temp[, 2] + 1) *
            (dev_temp[, 5] - dev_temp[, 4] + 1)
        )
        dev_parm_labels <- c(
          dev_parm_labels,
          paste0(rownames(dev_temp), "_dev_seq")
        )
      }
    }

    colnames(parlist[["S_parms"]]) <- c("INIT", "ESTIM")
    # Read in values for selectivity parameters and add to list
    S_seq <- as.numeric(parvals[(grep("selparm", parvals) + 1)])
    parlist[["S_parms"]][, 2] <- S_seq
  }

  # Build tag recapture parameter list
  if (length(grep("TG_parm", parvals)) > 0) {
    # Read in the values for tag recapture parameters
    TG_seq <- as.numeric(parvals[(grep("TG_parm", parvals) + 1)])
    # Create tag recapture list object and fill from control file list objects
    if (!is.null(ctllist[["TG_Loss_init"]])) {
      if (!is.null(parlist[["TG_parms"]])) {
        parlist[["TG_parms"]] <- rbind(
          parlist[["TG_parms"]],
          ctllist[["TG_Loss_init"]][, 3:4]
        )
      } else {
        parlist[["TG_parms"]] <- ctllist[["TG_Loss_init"]][, 3:4]
      }
    }
    # Create tag recapture list object and fill from control file list objects
    if (!is.null(ctllist[["TG_Loss_chronic"]])) {
      if (!is.null(parlist[["TG_parms"]])) {
        parlist[["TG_parms"]] <- rbind(
          parlist[["TG_parms"]],
          ctllist[["TG_Loss_chronic"]][, 3:4]
        )
      } else {
        parlist[["TG_parms"]] <- ctllist[["TG_Loss_chronic"]][, 3:4]
      }
    }
    # Create tag recapture list object and fill from control file list objects
    if (!is.null(ctllist[["TG_overdispersion"]])) {
      if (!is.null(parlist[["TG_parms"]])) {
        parlist[["TG_parms"]] <- rbind(
          parlist[["TG_parms"]],
          ctllist[["TG_overdispersion"]][, 3:4]
        )
      } else {
        parlist[["TG_parms"]] <- ctllist[["TG_overdispersion"]][, 3:4]
      }
    }
    # Create tag recapture list object and fill from control file list objects
    if (!is.null(ctllist[["TG_Report_fleet"]])) {
      if (!is.null(parlist[["TG_parms"]])) {
        parlist[["TG_parms"]] <- rbind(
          parlist[["TG_parms"]],
          ctllist[["TG_Report_fleet"]][, 3:4]
        )
      } else {
        parlist[["TG_parms"]] <- ctllist[["TG_Report_fleet"]][, 3:4]
      }
    }
    # Create tag recapture list object and fill from control file list objects
    if (!is.null(ctllist[["TG_Report_fleet_decay"]])) {
      if (!is.null(parlist[["TG_parms"]])) {
        parlist[["TG_parms"]] <- rbind(
          parlist[["TG_parms"]],
          ctllist[["TG_Report_fleet_decay"]][, 3:4]
        )
      } else {
        parlist[["TG_parms"]] <- ctllist[["TG_Report_fleet_decay"]][, 3:4]
      }
    }
    # Rename columns and add final parameter estimate data from par file
    colnames(parlist[["TG_parms"]]) <- c("INIT", "ESTIM")
    parlist[["TG_parms"]][["ESTIM"]] <- TG_seq
  }

  # Read in parameter deviations if they exist
  if (length(grep("parm_dev", parvals)) > 0) {
    # Build parameter deviations list
    parlist[["parm_devs"]] <- list()
    # Read in the values for parameter deviations for each vector
    for (i in seq_along(dev_parm_labels)) {
      years_temp <- dev_parm_start[i]:dev_parm_end[i]
      dev_temp <- as.numeric(strsplit(
        parvals[(grep("parm_dev", parvals)[i] + 1)],
        " "
      )[[1]])
      dev_temp <- dev_temp[!is.na(dev_temp)]
      parlist[["parm_devs"]][[i]] <- matrix(
        c(years_temp, dev_temp),
        nrow = length(years_temp),
        ncol = 2,
        byrow = FALSE
      )
      colnames(parlist[["parm_devs"]][[i]]) <- c("year", "dev")
      names(parlist[["parm_devs"]])[i] <- dev_parm_labels[i]
    }
  }

  return(parlist)
}
