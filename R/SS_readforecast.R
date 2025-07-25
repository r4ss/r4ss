#' read forecast file
#'
#' read Stock Synthesis forecast file into list object in R
#'
#'
#' @template file
#' @param Nfleets Number of fleets (not required in 3.30).
#' @param Nareas Number of areas (not required in 3.30).
#' @param nseas number of seasons (not required in 3.30).
#' @template version
#' @param readAll Should the function continue even if Forecast = 0 or -1
#' (at which point SS stops reading)?
#' @template verbose
#' @author Ian G. Taylor, Kelli F. Johnson, Kathryn L. Doering, Nathan R. Vaughan
#' @export
#' @family read/write functions

SS_readforecast <- function(
  file = "forecast.ss",
  Nfleets = NULL,
  Nareas = NULL,
  nseas = NULL,
  version = "3.30",
  readAll = FALSE,
  verbose = TRUE
) {
  # function to read Stock Synthesis forecast files
  if (!(version == "3.24" | version == "3.30" | version == 3.3)) {
    # turns out 3.30 != "3.30" in R
    stop("version must be either 3.24 or 3.30")
  }

  if (version == "3.24") {
    if (is.null(Nfleets) | is.null(Nareas) | is.null(nseas)) {
      stop(
        "version 3.24 must include values for Nfleets, Nareas, and nseas. At least one of these is missing"
      )
    }
  }

  if (verbose) {
    message("running SS_readforecast")
  }
  dat <- readLines(file, warn = FALSE)

  nver <- as.numeric(substring(version, 1, 4))
  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2], " ")[[1]][1]
  if (!is.na(temp) && temp == "Start_time:") {
    dat <- dat[-(1:2)]
  }
  allnums <- NULL
  for (i in seq_along(dat)) {
    # First split between input and comments
    mysplit <- strsplit(dat[i], split = "#")[[1]]
    if (!is.na(mysplit[1])) {
      # split along blank spaces
      mysplit <- strsplit(mysplit[1], split = "[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit != ""]
      # convert to numeric
      nums <- suppressWarnings(as.numeric(mysplit))
      nums <- nums[!is.na(nums)]
      # append new values to allnums vector
      if (length(nums) > 0) {
        allnums <- c(allnums, nums)
      }
    }
  }

  # internally used fun definitions ----
  # Function to add vector to forelist

  add_vec <- function(forelist, length, name, comments = NULL) {
    i <- forelist$".i"
    dat <- forelist$".dat"
    forelist[["temp"]] <- dat[i + 1:length - 1]
    forelist$".i" <- i + length
    if (is.null(comments)) {
      names(forelist[["temp"]]) <- paste0(
        paste0("#_", name, "_", collapse = ""),
        1:length
      )
    } else {
      names(forelist[["temp"]]) <- comments
    }
    if (!is.na(name)) {
      names(forelist)[names(forelist) == "temp"] <- name
    }
    if (verbose) {
      message(
        name,
        ",i=",
        forelist$".i",
        "\n",
        paste0(forelist[[name]], collapse = "\n")
      )
    }
    return(forelist)
  }

  find.index <- function(dat, ind, str) {
    ## Find the first line at position ind or later that
    ## contains the string str and return the index of that
    ## line. If the end of the data is reached, an error
    ## will be shown.
    while (ind < length(dat) & !length(grep(str, dat[ind]))) {
      ind <- ind + 1
    }
    if (ind == length(dat)) {
      stop(
        "SS_readctl_3.30-find.index: Error - ",
        "the value of ",
        str,
        " was not found. ",
        "Check the control file and make sure all ",
        "data frames are correctly formed.\n"
      )
    }
    ind
  }

  # Function to add data as data.frame to forelist
  add_df <- function(
    forelist,
    nrows = NULL,
    ncol,
    col.names,
    name,
    comments = NULL
  ) {
    i <- forelist$".i"
    dat <- forelist$".dat"
    if (is.null(nrows)) {
      end.ind <- find.index(dat, i, "-9999")
      nrow <- as.integer((end.ind - i) / ncol)
      if (nrow == 0) {
        # there isn't any data so just return
        forelist$".i" <- forelist$".i" + ncol
        return(forelist)
      }
    } else {
      nrow <- nrows
    }

    k <- nrow * ncol

    df0 <- as.data.frame(matrix(
      dat[i + 1:k - 1],
      nrow = nrow,
      ncol = ncol,
      byrow = TRUE
    ))
    colnames(df0) <- col.names
    if (is.null(comments)) {
      rownames(df0) <- paste0(paste0("#_", name, collapse = ""), 1:nrow)
    } else {
      rownames(df0) <- comments
    }
    i <- i + k

    if (is.null(nrows)) {
      i <- i + ncol
    } # adjust for -9999 row

    forelist[["temp"]] <- df0
    forelist$".i" <- i
    if (!is.na(name)) {
      names(forelist)[names(forelist) == "temp"] <- name
    }
    if (verbose) {
      message(
        name,
        ",i=",
        forelist$".i",
        "\n",
        paste0(forelist[[which(names(forelist) == name)]], collapse = "\n")
      )
    }
    return(forelist)
  }

  ## function to add an element to forelist
  add_elem <- function(forelist = NA, name) {
    i <- forelist$".i"
    dat <- forelist$".dat"
    forelist[["temp"]] <- dat[i]
    forelist$".i" <- i + 1
    if (!is.na(name)) {
      names(forelist)[names(forelist) == "temp"] <- name
    }
    if (verbose) {
      message(
        name,
        ",i=",
        forelist$".i",
        " ;",
        forelist[[which(names(forelist) == name)]]
      )
    }
    return(forelist)
  }

  ## function to add list  to forelist
  add_list <- function(forelist = NA, name, length, length_each) {
    i <- forelist$".i"
    dat <- forelist$".dat"
    forelist[["temp"]] <- list()
    for (j in 1:length) {
      forelist[["temp"]][[j]] <- dat[i + 1:length_each[j] - 1]
      i <- i + length_each[j]
    }
    forelist$".i" <- i
    if (!is.null(name)) {
      names(forelist)[names(forelist) == "temp"] <- name
    }
    if (verbose) {
      message(name, ",i=", forelist$".i")
    }
    return(forelist)
  }

  # setup ----
  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  forelist <- list()
  forelist$".i" <- i
  forelist$".dat" <- allnums
  forelist[["warnings"]] <- ""
  if (!is.null(nseas)) {
    forelist[["nseas"]] <- as.numeric(nseas)
  }
  if (!is.null(Nfleets)) {
    forelist[["Nfleets"]] <- as.numeric(Nfleets)
  }
  if (!is.null(Nareas)) {
    forelist[["Nareas"]] <- as.numeric(Nareas)
  }
  forelist[["SSversion"]] <- as.numeric(version)
  forelist[["sourcefile"]] <- file
  forelist[["type"]] <- "Stock_Synthesis_forecast_file"

  forelist <- add_elem(forelist, "benchmarks")
  forelist <- add_elem(forelist, "MSY")
  if (forelist[["MSY"]] == 5) {
    forelist <- add_elem(forelist, "MEY_units")
    forelist <- add_df(
      forelist,
      name = "MEY_options",
      ncol = 4,
      col.names = c(
        "fleet",
        "cost_per_F",
        "price_per_mt",
        "adjust_f"
      )
    )
  }
  forelist <- add_elem(forelist, "SPRtarget")
  forelist <- add_elem(forelist, "Btarget")
  if (forelist[["SSversion"]] == 3.24) {
    forelist <- add_vec(forelist, length = 6, name = "Bmark_years")
  } else {
    forelist <- add_vec(forelist, length = 10, name = "Bmark_years")
  }
  if (verbose) {
    message("Benchmark years: ", forelist[["Bmark_years"]])
  }
  forelist <- add_elem(forelist, "Bmark_relF_Basis")
  forelist <- add_elem(forelist, "Forecast")
  if (forelist[["Forecast"]] %in% c(0, -1) & !readAll) {
    if (verbose) {
      message(
        "Forecast is ",
        forelist[["Forecast"]],
        " and input readAll=FALSE so skipping remainder of file"
      )
    }
  } else if (
    forelist[["Forecast"]] %in%
      c(0, -1) &
      readAll &
      ((is.na(forelist$.dat[forelist$.i]) |
        forelist$.dat[forelist$.i] == 999))
  ) {
    # stop reading if forecast 0 or -1 used, and no other lines present
    # (aside from 999), but readAll = TRUE.
    if (verbose) {
      message("Forecast =", forelist[["Forecast"]], "\n")
    }
    warning(
      "readAll selected as TRUE, but lines beyond Forecast are not ",
      "present in the forecasting file, so skipping remainder of ",
      "file"
    )
  } else {
    # continue reading forecast
    if (verbose) {
      message("Forecast =", forelist[["Forecast"]], "\n")
    }
    forelist <- add_elem(forelist, "Nforecastyrs")
    # check for compatible input with forecast option 1.
    if (forelist[["Forecast"]] == 0 & forelist[["Nforecastyrs"]] != 1) {
      if (forelist[["SSversion"]] == 3.3) {
        warning(
          "Forecast = 0 should always be used with 1 forecast year. ",
          "Changing Nforecastyrs to 1. If you would prefer to use 0 years ",
          "of forecast, please use Forecast = -1; if you would like to ",
          " forecast for > 1 year, please select a value of Forecast > 0."
        )
        forelist[["Nforecastyrs"]] <- 1
      }
    }
    forelist <- add_elem(forelist, "F_scalar")
    if (forelist[["SSversion"]] == 3.24) {
      forelist <- add_vec(forelist, length = 4, name = "Fcast_years")
    } else {
      forelist <- add_vec(forelist, length = 6, name = "Fcast_years")
    }
    # -12345 code triggers read of new table input for forecast years
    # implemented in 3.30.22
    if (forelist[["Fcast_years"]][1] == -12345) {
      # need to remove list element before adding new format version
      forelist <- within(forelist, rm(Fcast_years))
      # shift index to account for 6 elements read in previous step
      forelist[[".i"]] <- forelist[[".i"]] - 5
      # add data frame of new-format input
      forelist <- add_df(
        forelist,
        ncol = 4,
        col.names = c("MG_type", "method", "st_year", "end_year"),
        name = "Fcast_years"
      )
    }
    if (verbose) {
      message("Forecast years: ", forelist[["Fcast_years"]])
    }
    # 3.30 models that don't use the new table input above read
    # additional selectivity setting
    if (
      is.vector(forelist[["Fcast_years"]]) &&
        (version == "3.30" | version == 3.3)
    ) {
      forelist <- add_elem(forelist, "Fcast_selex")
      if (verbose) {
        message("Forecast selectivity option: ", forelist[["Fcast_selex"]])
      }
    } else {
      forelist[["Fcast_selex"]] <- NA
    }

    forelist <- add_elem(forelist, "ControlRuleMethod")
    forelist <- add_elem(forelist, "BforconstantF")
    forelist <- add_elem(forelist, "BfornoF")
    forelist <- add_elem(forelist, "Flimitfraction")

    if (forelist[["Flimitfraction"]] < 0) {
      forelist <- add_df(
        forelist,
        ncol = 2,
        col.names = c("year", "fraction"),
        name = "Flimitfraction_m"
      )
    }

    forelist <- add_elem(forelist, "N_forecast_loops")
    forelist <- add_elem(
      forelist,
      "First_forecast_loop_with_stochastic_recruitment"
    )
    forelist <- add_elem(forelist, "fcast_rec_option")
    forelist <- add_elem(forelist, "fcast_rec_val")
    forelist <- add_elem(forelist, "HCR_anchor") # used as of 3.30.24
    # If number is -1 from when not used, use 0 instead to have it use the old
    # hardwired approach
    if (forelist[["HCR_anchor"]] == -1) {
      forelist[["HCR_anchor"]] <- 0
      warning(
        "As of SS3 version 3.30.24, this line that was unused is now",
        " HCR_anchor: 0 or 2 uses unfished benchmark SSB (old hardwired",
        " approach); 1 = virgin SSB; 3 = BMSY. The r4ss function changes",
        " this automatically to 0 but please update accordingly if using",
        " version 3.30.24."
      )
    }
    forelist <- add_elem(forelist, "FirstYear_for_caps_and_allocations")
    forelist <- add_elem(forelist, "stddev_of_log_catch_ratio")
    forelist <- add_elem(forelist, "Do_West_Coast_gfish_rebuilder_output")
    forelist <- add_elem(forelist, "Ydecl")
    forelist <- add_elem(forelist, "Yinit")
    forelist <- add_elem(forelist, "fleet_relative_F")
    forelist <- add_elem(forelist, "basis_for_fcast_catch_tuning")

    if (version == 3.24) {
      if (forelist[["fleet_relative_F"]] == 2) {
        forelist <- add_df(
          forelist,
          nrows = forelist[["nseas"]],
          ncol = forelist[["Nfleets"]],
          col.names = paste0("Fleet ", 1:forelist[["Nfleets"]]),
          name = "vals_fleet_relative_f"
        )
      }
      forelist <- add_vec(
        forelist,
        length = forelist[["Nfleets"]],
        name = "max_totalcatch_by_fleet"
      )
      forelist <- add_vec(
        forelist,
        length = forelist[["Nareas"]],
        name = "max_totalcatch_by_area"
      )
      forelist <- add_vec(
        forelist,
        length = forelist[["Nfleets"]],
        name = "fleet_assignment_to_allocation_group"
      )
      forelist[["N_allocation_groups"]] <- max(forelist[[
        "fleet_assignment_to_allocation_group"
      ]])
      if (forelist[["N_allocation_groups"]] > 0) {
        forelist <- add_vec(
          forelist,
          length = forelist[["N_allocation_groups"]],
          name = "allocation_among_groups"
        )
      } else {
        forelist[["allocation_among_groups"]] <- NULL
      }

      forelist <- add_elem(forelist, "Ncatch")
      forelist <- add_elem(forelist, "InputBasis")
      if (forelist[["Ncatch"]] == 0) {
        forelist[["ForeCatch"]] <- NULL
      } else {
        if (forelist[["InputBasis"]] == -1) {
          forelist <- add_df(
            forelist,
            nrows = forelist[["Ncatch"]],
            ncol = 5,
            col.names = c("year", "seas", "fleet", "catch_or_F", "basis"),
            name = "ForeCatch"
          )
        } else {
          forelist <- add_df(
            forelist,
            nrows = forelist[["Ncatch"]],
            ncol = 4,
            col.names = c("year", "seas", "fleet", "catch_or_F"),
            name = "ForeCatch"
          )
        }
      }
    } else {
      if (forelist[["fleet_relative_F"]] == 2) {
        forelist <- add_df(
          forelist,
          ncol = 3,
          col.names = c("seas", "fleet", "Relative F"),
          name = "vals_fleet_relative_f"
        )
      }
      forelist <- add_df(
        forelist,
        ncol = 2,
        col.names = c("fleet", "max_catch"),
        name = "max_totalcatch_by_fleet"
      )
      forelist <- add_df(
        forelist,
        ncol = 2,
        col.names = c("area", "max_catch"),
        name = "max_totalcatch_by_area"
      )
      forelist <- add_df(
        forelist,
        ncol = 2,
        col.names = c("fleet", "group"),
        name = "fleet_assignment_to_allocation_group"
      )
      if (!is.null(forelist[["fleet_assignment_to_allocation_group"]])) {
        forelist[["N_allocation_groups"]] <- max(forelist[[
          "fleet_assignment_to_allocation_group"
        ]][, 2])
        forelist <- add_df(
          forelist,
          ncol = (forelist[["N_allocation_groups"]] + 1),
          col.names = c(
            "year",
            paste0("group ", 1:forelist[["N_allocation_groups"]])
          ),
          name = "allocation_among_groups"
        )
      } else {
        forelist[["N_allocation_groups"]] <- 0
        forelist[["allocation_among_groups"]] <- NULL
      }
      forelist <- add_elem(forelist, "InputBasis")
      if (forelist[["InputBasis"]] == -1) {
        forelist <- add_df(
          forelist,
          ncol = 5,
          col.names = c("year", "seas", "fleet", "catch_or_F", "basis"),
          name = "ForeCatch"
        )
      } else {
        forelist <- add_df(
          forelist,
          ncol = 4,
          col.names = c("year", "seas", "fleet", "catch_or_F"),
          name = "ForeCatch"
        )
      }
    }
    if (forelist$".dat"[forelist$".i"] == 999) {
      if (verbose) {
        message("read of forecast file complete (final value = 999)\n")
      }
      forelist[["eof"]] <- TRUE
    } else {
      warning(
        "Error: final value is ",
        forelist$".dat"[forelist$".i"],
        " but ",
        "should be 999\n"
      )
      forelist[["eof"]] <- FALSE
    }
  }

  forelist$".dat" <- NULL
  forelist$".i" <- NULL
  return(forelist)
}
