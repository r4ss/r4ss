#' Deprecate: read data file from SS version 3.00
#'
#' Read Stock Synthesis (version 3.00) data file into list object in R.
#' This function was formerly called SS_readdat. That name is now used
#' for a wrapper function that calls either SS_readdat_3.24 or SS_readdat_3.30
#' (and potentially additional functions in the future).
#'
#' Support for 3.24 models within the r4ss `SS_read*` and `SS_write*()`
#' functions is ending, so please update models to 3.30. 
#'
#'
#' @template file
#' @template verbose
#' @param echoall Deprecated.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar
#' @export
#' @seealso [SS_readdat()], [SS_readdat_3.30()]
#' [SS_readstarter()], [SS_readforecast()],
#' [SS_writestarter()],
#' [SS_writeforecast()], [SS_writedat()]
SS_readdat_3.00 <- function(file, verbose = TRUE,
                            echoall = lifecycle::deprecated(), section = NULL) {
                                # deprecate. Remove code upon next release.
  lifecycle::deprecate_warn(
    when = "1.45.3",
    what = "SS_readdat_3.00()",
    details = "Please update model to version 3.30."
  )
  # function to read Stock Synthesis data files
  if (lifecycle::is_present(echoall)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SS_readdat_3.00(echoall)",
      details = "Please use verbose = TRUE instead."
    )
  }

  if (verbose) message("running SS_readdat_3.00")
  dat <- readLines(file, warn = FALSE)

  # split apart any bootstrap or expected value sections in data.ss_new
  if (!is.null(section)) {
    Nsections <- as.numeric(substring(dat[grep("Number_of_datafiles", dat)], 24))
    if (!section %in% 1:Nsections) stop("The 'section' input should be within the 'Number_of_datafiles' in a data.ss_new file.\n")
    if (section == 1) {
      end <- grep("#_expected values with no error added", dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[grep("#_observed data:", dat):end]
    }
    if (section == 2) {
      end <- grep("#_bootstrap file: 1", dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[grep("#_expected values with no error added", dat):end]
    }
    if (section >= 3) {
      start <- grep(paste("#_bootstrap file:", section - 2), dat)
      end <- grep(paste("#_bootstrap file:", section - 1), dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[start:end]
    }
  }

  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2], " ")[[1]][1]
  if (!is.na(temp) && temp == "Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for (i in 1:length(dat)) {
    # split along blank spaces
    mysplit <- strsplit(dat[i], split = "[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit != ""]
    # if final value is a number is followed immediately by a pound ("1#"),
    # this needs to be split
    nvals <- length(mysplit)
    if (nvals > 0) mysplit[nvals] <- strsplit(mysplit[nvals], "#")[[1]][1]
    # convert to numeric
    nums <- suppressWarnings(as.numeric(mysplit))
    if (sum(is.na(nums)) > 0) {
      maxcol <- min((1:length(nums))[is.na(nums)]) - 1
    } else {
      maxcol <- length(nums)
    }
    if (maxcol > 0) {
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  datlist <- list()

  datlist[["eof"]] <- FALSE

  # specifications
  datlist[["sourcefile"]] <- file
  datlist[["type"]] <- "Stock_Synthesis_data_file"
  datlist[["ReadVersion"]] <- "3.00"
  if (verbose) {
    message("SS_readdat_3.00 - SS version = ", datlist[["ReadVersion"]])
  }


  # model dimensions
  datlist[["styr"]] <- allnums[i]
  i <- i + 1
  datlist[["endyr"]] <- allnums[i]
  i <- i + 1
  datlist[["nseas"]] <- nseas <- allnums[i]
  i <- i + 1
  datlist[["months_per_seas"]] <- allnums[i:(i + nseas - 1)]
  i <- i + nseas
  datlist[["spawn_seas"]] <- allnums[i]
  i <- i + 1
  datlist[["Nfleet"]] <- Nfleet <- allnums[i]
  i <- i + 1
  datlist[["Nsurveys"]] <- Nsurveys <- allnums[i]
  i <- i + 1
  Ntypes <- Nfleet + Nsurveys
  datlist[["N_areas"]] <- allnums[i]
  i <- i + 1

  # an attempt at getting the fleet names based on occurance of %-sign
  fleetnames.good <- NULL
  if (Ntypes > 1) {
    percentlines <- grep("%", dat)
    for (iline in percentlines) {
      fleetnames <- dat[iline]
      fleetnames <- strsplit(fleetnames, "%")[[1]]
      # strip any white space off the end of the fleetnames
      fleetnames[length(fleetnames)] <- strsplit(fleetnames[length(fleetnames)], "[[:blank:]]+")[[1]][1]
      if (length(fleetnames) == Ntypes) fleetnames.good <- fleetnames
    }
    fleetnames <- fleetnames.good
    if (is.null(fleetnames)) {
      fleetnames <- c(paste("fishery", 1:Nfleet), paste("survey", 1:Nsurveys))
    }
  } else {
    fleetnames <- "fleet1"
  }

  datlist[["fleetnames"]] <- fleetnames
  datlist[["surveytiming"]] <- surveytiming <- allnums[i:(i + Ntypes - 1)]
  i <- i + Ntypes
  datlist[["areas"]] <- areas <- allnums[i:(i + Ntypes - 1)]
  i <- i + Ntypes
  if (verbose) {
    message("areas:", areas)
    message("fleet info:\n", paste0(utils::capture.output(
      data.frame(
        fleet = 1:Ntypes,
        name = fleetnames,
        area = areas,
        timing = surveytiming,
        type = c(rep("FISHERY", Nfleet), rep("SURVEY", Nsurveys))
      )
    ), collapse = "\n"))
  }
  # fleet info
  fleetinfo1 <- data.frame(rbind(surveytiming, areas))
  names(fleetinfo1) <- fleetnames
  fleetinfo1[["input"]] <- c("#_surveytiming", "#_areas")
  datlist[["fleetinfo1"]] <- fleetinfo1

  datlist[["units_of_catch"]] <- units_of_catch <- allnums[i:(i + Nfleet - 1)]
  i <- i + Nfleet
  datlist[["se_log_catch"]] <- se_log_catch <- allnums[i:(i + Nfleet - 1)]
  i <- i + Nfleet
  fleetinfo2 <- data.frame(rbind(units_of_catch, se_log_catch))
  names(fleetinfo2) <- fleetnames[1:Nfleet]
  fleetinfo2[["input"]] <- c("#_units_of_catch", "#_se_log_catch")
  datlist[["fleetinfo2"]] <- fleetinfo2


  # more dimensions
  datlist[["Nsexes"]] <- allnums[i]
  i <- i + 1
  datlist[["Nages"]] <- Nages <- allnums[i]
  i <- i + 1
  datlist[["init_equil"]] <- allnums[i:(i + Nfleet - 1)]
  i <- i + Nfleet


  # catch
  datlist[["N_catch"]] <- N_catch <- allnums[i]
  i <- i + 1
  if (verbose) message("N_catch =", N_catch)
  Nvals <- N_catch * (Nfleet + 2)
  catch <- data.frame(matrix(allnums[i:(i + Nvals - 1)],
    nrow = N_catch, ncol = (Nfleet + 2), byrow = TRUE
  ))
  names(catch) <- c(fleetnames[1:Nfleet], "year", "seas")
  datlist[["catch"]] <- catch
  i <- i + Nvals
  if (verbose) print(catch)

  datlist[["N_cpue"]] <- N_cpue <- allnums[i]
  i <- i + 1
  if (verbose) message("N_cpue =", N_cpue)
  if (N_cpue > 0) {
    CPUEinfo <- data.frame(matrix(c(1:Ntypes, rep(1, Ntypes), rep(0, Ntypes)),
      nrow = Ntypes, ncol = 3, byrow = FALSE
    )) # fill CPUEinfo with defaults
    names(CPUEinfo) <- c("Fleet", "Units", "Errtype")
    CPUE <- data.frame(matrix(
      allnums[i:(i + N_cpue * 5 - 1)],
      nrow = N_cpue, ncol = 5, byrow = TRUE
    ))
    i <- i + N_cpue * 5
    names(CPUE) <- c("year", "seas", "index", "obs", "se_log")
    CPUE <- CPUE[which(CPUE[["obs"]] >= 0), ] # remove negative observations (commented?)
  } else {
    CPUEinfo <- NULL
    CPUE <- NULL
  }
  datlist[["CPUEinfo"]] <- CPUEinfo
  datlist[["CPUE"]] <- CPUE

  # discards
  # datlist[["discard_units"]] <- discard_units <- allnums[i]; i <- i+1
  Dis_type <- allnums[i]
  i <- i + 1
  datlist[["N_discard"]] <- N_discard <- allnums[i]
  i <- i + 1

  if (verbose) message("N_discard =", N_discard)
  if (N_discard > 0) {
    # discard data
    Ncols <- 5
    discard_data <- data.frame(matrix(
      allnums[i:(i + N_discard * Ncols - 1)],
      nrow = N_discard, ncol = Ncols, byrow = TRUE
    ))
    i <- i + N_discard * Ncols
    names(discard_data) <- c("Yr", "Seas", "Flt", "Discard", "Std_in")
    datlist[["discard_data"]] <- discard_data

    datlist[["N_discard_fleets"]] <- N_discard_fleets <- length(unique(discard_data[["Flt"]]))


    datlist[["discard_fleet_info"]] <- data.frame(matrix(c(
      unique(discard_data[["Flt"]]),
      rep(Dis_type, N_discard_fleets), rep(0, N_discard_fleets)
    ),
    nrow = N_discard_fleets, ncol = 3, byrow = FALSE
    )) # fill discard fleet info with defaults
    names(datlist[["discard_fleet_info"]]) <- c("Fleet", "units", "errtype")
  } else {
    datlist[["N_discard_fleets"]] <- 0
    datlist[["discard_data"]] <- NULL
    datlist[["discard_fleet_info"]] <- NULL
  }


  # meanbodywt
  datlist[["N_meanbodywt"]] <- N_meanbodywt <- allnums[i]
  i <- i + 1

  if (N_meanbodywt > 0) {
    Ncols <- 6
    meanbodywt <- data.frame(matrix(
      allnums[i:(i + N_meanbodywt * Ncols - 1)],
      nrow = N_meanbodywt, ncol = Ncols, byrow = TRUE
    ))
    i <- i + N_meanbodywt * Ncols
    names(meanbodywt) <- c("Year", "Seas", "Type", "Partition", "Value", "CV")

    # don't know if this is needed for version 2.00
    # datlist[["DF_for_meanbodywt"]] <- allnums[i]
    # i <- i+1

    datlist[["DF_for_meanbodywt"]] <- NULL
  } else {
    datlist[["DF_for_meanbodywt"]] <- NULL
    meanbodywt <- NULL
  }
  datlist[["meanbodywt"]] <- meanbodywt

  datlist[["lbin_method"]] <- lbin_method <- allnums[i]
  i <- i + 1
  if (verbose) message("lbin_method =", lbin_method)
  if (lbin_method == 2) {
    datlist[["binwidth"]] <- allnums[i]
    i <- i + 1
    datlist[["minimum_size"]] <- allnums[i]
    i <- i + 1
    datlist[["maximum_size"]] <- allnums[i]
    i <- i + 1
  } else {
    datlist[["binwidth"]] <- NA
    datlist[["minimum_size"]] <- NA
    datlist[["maximum_size"]] <- NA
  }
  if (lbin_method == 3) {
    datlist[["N_lbinspop"]] <- N_lbinspop <- allnums[i]
    i <- i + 1
    datlist[["lbin_vector_pop"]] <- allnums[i:(i + N_lbinspop - 1)]
    i <- i + N_lbinspop
    if (verbose) message("N_lbinspop =", N_lbinspop, "\nlbin_vector_pop:\n")
  } else {
    datlist[["N_lbinspop"]] <- NA
    datlist[["lbin_vector_pop"]] <- NA
  }

  datlist[["comp_tail_compression"]] <- allnums[i]
  i <- i + 1
  datlist[["add_to_comp"]] <- allnums[i]
  i <- i + 1
  datlist[["max_combined_age"]] <- allnums[i]
  i <- i + 1

  # length data

  datlist[["N_lbins"]] <- N_lbins <- allnums[i]
  i <- i + 1
  datlist[["lbin_vector"]] <- lbin_vector <- allnums[i:(i + N_lbins - 1)]
  i <- i + N_lbins
  if (verbose) print(lbin_vector)

  datlist[["N_lencomp"]] <- N_lencomp <- allnums[i]
  i <- i + 1

  if (N_lencomp > 0) {
    Ncols <- N_lbins * datlist[["Nsexes"]] + 6
    lencomp <- data.frame(matrix(
      allnums[i:(i + N_lencomp * Ncols - 1)],
      nrow = N_lencomp, ncol = Ncols, byrow = TRUE
    ))
    i <- i + N_lencomp * Ncols
    names(lencomp) <- c(
      "Yr", "Seas", "FltSvy", "Gender", "Part", "Nsamp",
      if (datlist[["Nsexes"]] == 1) {
        paste("l", lbin_vector, sep = "")
      } else {
        NULL
      },
      if (datlist[["Nsexes"]] > 1) {
        c(paste("f", lbin_vector, sep = ""), paste("m", lbin_vector, sep = ""))
      } else {
        NULL
      }
    )
  } else {
    lencomp <- NULL
  }
  datlist[["lencomp"]] <- lencomp

  # age data
  datlist[["N_agebins"]] <- N_agebins <- allnums[i]
  i <- i + 1
  if (verbose) message("N_agebins =", N_agebins, "\n")
  if (N_agebins > 0) {
    agebin_vector <- allnums[i:(i + N_agebins - 1)]
    i <- i + N_agebins
  } else {
    agebin_vector <- NULL
  }
  datlist[["agebin_vector"]] <- agebin_vector

  datlist[["N_ageerror_definitions"]] <- N_ageerror_definitions <- allnums[i]
  i <- i + 1
  if (N_ageerror_definitions > 0) {
    Ncols <- Nages + 1
    ageerror <- data.frame(matrix(
      allnums[i:(i + 2 * N_ageerror_definitions * Ncols - 1)],
      nrow = 2 * N_ageerror_definitions, ncol = Ncols, byrow = TRUE
    ))
    i <- i + 2 * N_ageerror_definitions * Ncols
    names(ageerror) <- paste("age", 0:Nages, sep = "")
  } else {
    ageerror <- NULL
  }
  datlist[["ageerror"]] <- ageerror

  datlist[["N_agecomp"]] <- N_agecomp <- allnums[i]
  i <- i + 1

  datlist[["Lbin_method"]] <- allnums[i]
  i <- i + 1
  datlist[["max_combined_lbin"]] <- allnums[i]
  i <- i + 1

  if (N_agecomp > 0) {
    if (N_agebins == 0) stop("N_agecomp =", N_agecomp, " but N_agebins = 0")
    Ncols <- N_agebins * datlist[["Nsexes"]] + 9
    agecomp <- data.frame(matrix(allnums[i:(i + N_agecomp * Ncols - 1)],
      nrow = N_agecomp, ncol = Ncols, byrow = TRUE
    ))
    i <- i + N_agecomp * Ncols
    names(agecomp) <- c(
      "Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr", "Lbin_lo", "Lbin_hi", "Nsamp",
      if (datlist[["Nsexes"]] == 1) {
        paste("a", agebin_vector, sep = "")
      } else {
        NULL
      },
      if (datlist[["Nsexes"]] > 1) {
        c(paste("f", agebin_vector, sep = ""), paste("m", agebin_vector, sep = ""))
      } else {
        NULL
      }
    )
  } else {
    agecomp <- NULL
  }
  datlist[["agecomp"]] <- agecomp

  # MeanSize_at_Age
  datlist[["N_MeanSize_at_Age_obs"]] <- N_MeanSize_at_Age_obs <- allnums[i]
  i <- i + 1
  if (verbose) message("N_MeanSize_at_Age_obs =", N_MeanSize_at_Age_obs)
  if (N_MeanSize_at_Age_obs > 0) {
    Ncols <- 2 * N_agebins * datlist[["Nsexes"]] + 7
    MeanSize_at_Age_obs <- data.frame(matrix(
      allnums[i:(i + N_MeanSize_at_Age_obs * Ncols - 1)],
      nrow = N_MeanSize_at_Age_obs, ncol = Ncols, byrow = TRUE
    ))
    i <- i + N_MeanSize_at_Age_obs * Ncols
    names(MeanSize_at_Age_obs) <- c(
      "Yr", "Seas", "FltSvy", "Gender", "Part", "AgeErr", "Ignore",
      if (datlist[["Nsexes"]] == 1) {
        paste("a", agebin_vector, sep = "")
      } else {
        NULL
      },
      if (datlist[["Nsexes"]] > 1) {
        c(paste("f", agebin_vector, sep = ""), paste("m", agebin_vector, sep = ""))
      } else {
        NULL
      },
      if (datlist[["Nsexes"]] == 1) {
        paste("N_a", agebin_vector, sep = "")
      } else {
        NULL
      },
      if (datlist[["Nsexes"]] > 1) {
        c(paste("N_f", agebin_vector, sep = ""), paste("N_m", agebin_vector, sep = ""))
      } else {
        NULL
      }
    )
  } else {
    MeanSize_at_Age_obs <- NULL
  }
  datlist[["MeanSize_at_Age_obs"]] <- MeanSize_at_Age_obs

  # other stuff
  datlist[["N_environ_variables"]] <- N_environ_variables <- allnums[i]
  i <- i + 1
  datlist[["N_environ_obs"]] <- N_environ_obs <- allnums[i]
  i <- i + 1
  if (N_environ_obs > 0) {
    Ncols <- 3
    envdat <- data.frame(matrix(
      allnums[i:(i + Ncols * N_environ_obs - 1)],
      nrow = N_environ_obs, ncol = Ncols, byrow = TRUE
    ))
    i <- i + N_environ_obs * Ncols
    names(envdat) <- c("Yr", "Variable", "Value")
  } else {
    envdat <- NULL
  }
  datlist[["envdat"]] <- envdat

  datlist[["N_sizefreq_methods"]] <- N_sizefreq_methods <- allnums[i]
  i <- i + 1
  if (verbose) message("N_sizefreq_methods =", N_sizefreq_methods)
  if (N_sizefreq_methods > 0) {
    # get details of generalized size frequency methods
    datlist[["nbins_per_method"]] <- nbins_per_method <- allnums[i:(i + N_sizefreq_methods - 1)]
    i <- i + N_sizefreq_methods
    datlist[["units_per_method"]] <- units_per_method <- allnums[i:(i + N_sizefreq_methods - 1)]
    i <- i + N_sizefreq_methods
    datlist[["scale_per_method"]] <- scale_per_method <- allnums[i:(i + N_sizefreq_methods - 1)]
    i <- i + N_sizefreq_methods
    datlist[["mincomp_per_method"]] <- mincomp_per_method <- allnums[i:(i + N_sizefreq_methods - 1)]
    i <- i + N_sizefreq_methods
    datlist[["Nobs_per_method"]] <- Nobs_per_method <- allnums[i:(i + N_sizefreq_methods - 1)]
    i <- i + N_sizefreq_methods

    # get list of bin vectors
    sizefreq_bins_list <- list()
    for (imethod in 1:N_sizefreq_methods) {
      sizefreq_bins_list[[imethod]] <- allnums[i:(i + nbins_per_method[imethod] - 1)]
      i <- i + nbins_per_method[imethod]
    }
    datlist[["sizefreq_bins_list"]] <- sizefreq_bins_list
    # read generalized size frequency data
    sizefreq_data_list <- list()
    for (imethod in 1:N_sizefreq_methods) {
      Ncols <- 7 + datlist[["Nsexes"]] * nbins_per_method[imethod]
      Nrows <- Nobs_per_method[imethod]
      sizefreq_data_tmp <- data.frame(matrix(allnums[i:(i + Nrows * Ncols - 1)],
        nrow = Nrows, ncol = Ncols, byrow = TRUE
      ))
      names(sizefreq_data_tmp) <-
        c(
          "Method", "Yr", "Seas", "FltSvy", "Gender", "Part", "Nsamp",
          if (datlist[["Nsexes"]] == 1) {
            paste("a", sizefreq_bins_list[[imethod]], sep = "")
          } else {
            NULL
          },
          if (datlist[["Nsexes"]] > 1) {
            c(
              paste("f", sizefreq_bins_list[[imethod]], sep = ""),
              paste("m", sizefreq_bins_list[[imethod]], sep = "")
            )
          } else {
            NULL
          }
        )
      if (verbose) {
        message("Method =", imethod, "  (first two rows, ten columns):\n")
        print(sizefreq_data_tmp[1:min(Nrows, 2), 1:min(Ncols, 10)])
      }
      if (any(sizefreq_data_tmp[["Method"]] != imethod)) {
        stop(
          "Problem with method in size frequency data:\n",
          "Expecting method: ", imethod, "\n",
          "Read method(s): ", paste(unique(sizefreq_data_tmp[["Method"]]), collapse = ", ")
        )
      }
      sizefreq_data_list[[imethod]] <- sizefreq_data_tmp
      i <- i + Nrows * Ncols
    }
    datlist[["sizefreq_data_list"]] <- sizefreq_data_list
  } else {
    datlist[["nbins_per_method"]] <- NULL
    datlist[["units_per_method"]] <- NULL
    datlist[["scale_per_method"]] <- NULL
    datlist[["mincomp_per_method"]] <- NULL
    datlist[["Nobs_per_method"]] <- NULL
    datlist[["sizefreq_bins_list"]] <- NULL
    datlist[["sizefreq_data_list"]] <- NULL
  }

  datlist[["do_tags"]] <- do_tags <- allnums[i]
  i <- i + 1
  if (verbose) message("do_tags =", do_tags)

  if (do_tags != 0) {
    datlist[["N_tag_groups"]] <- N_tag_groups <- allnums[i]
    i <- i + 1
    datlist[["N_recap_events"]] <- N_recap_events <- allnums[i]
    i <- i + 1
    datlist[["mixing_latency_period"]] <- mixing_latency_period <- allnums[i]
    i <- i + 1
    datlist[["max_periods"]] <- max_periods <- allnums[i]
    i <- i + 1

    # read tag release data
    if (N_tag_groups > 0) {
      Ncols <- 8
      tag_releases <- data.frame(matrix(allnums[i:(i + N_tag_groups * Ncols - 1)], nrow = N_tag_groups, ncol = Ncols, byrow = TRUE))
      i <- i + N_tag_groups * Ncols
      names(tag_releases) <- c("TG", "Area", "Yr", "Season", "tfill", "Gender", "Age", "Nrelease")
      if (verbose) {
        message("Head of tag release data:\n")
        print(head(tag_releases))
      }
    } else {
      tag_releases <- NULL
    }
    datlist[["tag_releases"]] <- tag_releases

    # read tag recapture data
    if (N_recap_events > 0) {
      Ncols <- 5
      tag_recaps <- data.frame(matrix(allnums[i:(i + N_recap_events * Ncols - 1)], nrow = N_recap_events, ncol = Ncols, byrow = TRUE))
      i <- i + N_recap_events * Ncols
      names(tag_recaps) <- c("TG", "Yr", "Season", "Fleet", "Nrecap")
      if (verbose) {
        message("Head of tag recapture data:\n")
        print(head(tag_recaps))
      }
    } else {
      tag_recaps <- NULL
    }
    datlist[["tag_recaps"]] <- tag_recaps
  }

  datlist[["morphcomp_data"]] <- do_morphcomps <- allnums[i]
  i <- i + 1
  if (verbose) message("do_morphcomps =", do_morphcomps)

  if (allnums[i] == 999) {
    if (verbose) message("read of data file 3.00 complete (final value = 999)\n")
    datlist[["eof"]] <- TRUE
  } else {
    message("Error: final value is", allnums[i], " but should be 999\n")
    datlist[["eof"]] <- FALSE
  }

  # Fixes pulled from the read data wrapper function
  # get fleet info
  finfo <- rbind(datlist[["fleetinfo1"]], c(rep(1, datlist[["Nfleet"]]), rep(3, datlist[["Nsurveys"]])))
  finfo <- rbind(finfo, c(datlist[["units_of_catch"]], rep(0, datlist[["Nsurveys"]])))
  rownames(finfo)[3] <- "type"
  rownames(finfo)[4] <- "units"
  finfo <- finfo[, 1:(length(finfo) - 1)]
  finfo <- as.data.frame(t(finfo))
  datlist[["fleetinfo"]] <- finfo
  ## !!! need to add fixes to pop len bins? (see 3.24)

  # return the result
  return(datlist)
}
