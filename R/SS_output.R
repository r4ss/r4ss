#' A function to create a list object for the output from Stock Synthesis
#'
#' Reads the Report.sso and (optionally) the covar.sso, CompReport.sso and
#' other files produced by Stock Synthesis and formats the important
#' content of these files into a list in the R workspace. A few statistics
#' unavailable elsewhere are taken from the .par file. Summary
#' information and statistics can be returned to the R console or just
#' contained within the list produced by this function.
#'
#'
#' @template dir
#' @param dir.mcmc Optional directory containing MCMC output. This can either be
#' relative to `dir`, such that `file.path(dir, dir.mcmc)`
#' will end up in the right place, or an absolute path.
#' @param repfile Name of the big report file (could be renamed by user).
#' @param compfile Name of the composition report file.
#' @param covarfile Name of the covariance output file.
#' @param forefile Name of the forecast file.
#' @param wtfile Name of the file containing weight at age data.
#' @param warnfile Name of the file containing warnings.
#' @param ncols Deprecated. This value is now calculated automatically.
#' @param forecast Read the forecast-report file?
#' @param warn Read the Warning.sso file?
#' @param covar Read covar.sso?
#' @param readwt Read the weight-at-age file?
#' @template verbose
#' @param printstats Print summary statistics about the output to the R GUI?
#' @param hidewarn Hides some warnings output from the R GUI.
#' @param NoCompOK Allow the function to work without a CompReport file.
#' @param aalmaxbinrange The largest length bin range allowed for composition
#' data to be considered as conditional age-at-length data.
#' @param SpawnOutputLabel An alternative to "Spawning output" for use in
#' figure axis labels and table headers for models that include a fecundity
#' relationship. This provides an option to provide the units, e.g.
#' `SpawnOutputLabel = "Spawning output (trillions of eggs)"`.
#' This needs to be a user input because the units depend on the choice of
#' fecundity parameters which are calculated outside of the SS3 model.
#' @return Many values are returned. Complete list would be quite long, but
#' should probably be created at some point in the future.
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso [SS_plots()]
#' @examples
#' \dontrun{
#' # read model output
#' myreplist <- SS_output(dir = "c:/SS/Simple/")
#' # make a bunch of plots
#' SS_plots(myreplist)
#'
#' # read model output and also read MCMC results (if run), which in
#' # this case would be stored in c:/SS/Simple/mcmc/
#' myreplist <- SS_output(dir = "c:/SS/Simple/", dir.mcmc = "mcmc")
#' }
#'
SS_output <-
  function(dir = "C:/myfiles/mymodels/myrun/",
           dir.mcmc = NULL,
           repfile = "Report.sso",
           compfile = "CompReport.sso",
           covarfile = "covar.sso",
           forefile = "Forecast-report.sso",
           wtfile = "wtatage.ss_new",
           warnfile = "warning.sso",
           ncols = lifecycle::deprecated(),
           forecast = TRUE,
           warn = TRUE,
           covar = TRUE,
           readwt = TRUE,
           verbose = TRUE,
           printstats = TRUE,
           hidewarn = FALSE,
           NoCompOK = TRUE,
           aalmaxbinrange = 4,
           SpawnOutputLabel = "Spawning output") {
    flush.console()

    ###########################################################################
    ## embedded functions: emptytest, match_report_line, match_report_table,
    ##                     file_is_empty
    ###########################################################################

    emptytest <- function(x) {
      # function to help test for empty columns
      sum(!is.na(x) & x == "") / length(x)
    }

    match_report_line <- function(string, obj = rawrep[, 1], substr1 = TRUE) {
      # return a line number from the report file (or other file)
      # substr1 controls whether to compare subsets or the whole line
      match(string, if (substr1) {
        substring(obj, 1, nchar(string))
      } else {
        obj
      })
    }

    match_report_table <- function(string1,
                                   adjust1,
                                   string2 = NULL,
                                   adjust2 = -1,
                                   which_blank = 1,
                                   cols = "nonblank",
                                   matchcol1 = 1,
                                   matchcol2 = 1,
                                   obj = rawrep,
                                   blank_lines = rep_blank_or_hash_lines,
                                   substr1 = TRUE,
                                   substr2 = TRUE,
                                   header = FALSE,
                                   type.convert = FALSE) {
      # extract a table from Report.sso by matching a keyword
      #
      # return a subset of values from the report file (or other file)
      # subset is defined by character strings at the start and end, with integer
      # adjustments of the number of lines to above/below the two strings
      #
      #
      # @param string1 keyword near top of table
      # @param adjust1 integer for number of rows after string1 to start table
      # @param string2 keyword near bottom of table
      # (or NULL to use blank line to end table)
      # @param adjust2 integer for number of rows after string2 to end table
      # (often a negative value)
      # @param which_blank which blank line (after string1) to use as the end
      # of the table (if using string2 = NULL)
      # @param cols which columns to return, can be an integer, a vector, "all",
      # or 'nonblank' (where this last returns all columns with at least one
      # non-blank values in it)
      # @param matchcol1 which column to search for string1
      # @param matchcol2 which column to search for string2
      # @param obj matrix object in which to search (always rawrep so far)
      # @param blank_lines vector of line numbers of obj which are blank
      # (to save the time of replicating this in each function call)
      # @param substr1 allow string1 to be a substring of the text in matchcol1?
      # (It must be start at the beginning regardless)
      # @param substr2 allow string2 to be a substring of the text in matchcol2?
      # (It must be start at the beginning regardless)
      # @param header Is the first row of the table a header?
      # @param apply type.convert() function to the resulting table?
      line1 <- match(
        string1,
        if (substr1) {
          substring(obj[, matchcol1], 1, nchar(string1))
        } else {
          obj[, matchcol1]
        }
      )
      if (is.null(string2)) {
        # get first blank or "#" line after the start
        line2 <- blank_lines[blank_lines > line1][which_blank]
        # if no remaining blank lines, use the end of the file
        if (is.na(line2)) {
          line2 <- nrow(obj)
        }
      } else {
        line2 <- match(
          string2,
          if (substr2) {
            substring(obj[, matchcol2], 1, nchar(string2))
          } else {
            obj[, matchcol2]
          }
        )
      }
      if (is.na(line1) | is.na(line2)) {
        return(NULL)
      }

      if (is.numeric(cols)) {
        out <- obj[(line1 + adjust1):(line2 + adjust2), cols]
      }
      if (cols[1] == "all") {
        out <- obj[(line1 + adjust1):(line2 + adjust2), ]
      }
      if (cols[1] == "nonblank") {
        # returns only columns that contain at least one non-empty value
        out <- obj[(line1 + adjust1):(line2 + adjust2), ]
        out <- out[, apply(out, 2, emptytest) < 1]
      }
      if (header && nrow(out) > 0) {
        out[1, out[1, ] == ""] <- "NoName"
        names(out) <- out[1, ]
        out <- out[-1, ]
      }
      if (type.convert) {
        out <- type.convert(out, as.is = TRUE)
      }
      return(out)
    } # end match_report_table

    df.rename <- function(df, oldnames, newnames) {
      # function to replace names in dataframes
      # added to clean up adaptation to more consistent
      # syntax in Report.sso as of SS version 3.30.01.15.
      if (!is.null(df)) {
        for (iname in seq_along(oldnames)) {
          names(df)[names(df) == oldnames[iname]] <- newnames[iname]
        }
      }
      return(df)
    }

    file_is_empty <- function(file) {
      if (grepl("https:", file)) {
        con <- url(file)
        check <- suppressWarnings(try(
          open.connection(con, open = "rt", timeout = 2),
          silent = TRUE
        )[1])
        suppressWarnings(try(close.connection(con), silent = TRUE))
        return(!is.null(check))
      } else {
        return(!file.exists(file) | file.info(file)[["size"]] <= 0)
      }
    }

    # check inputs
    if (lifecycle::is_present(ncols)) {
      lifecycle::deprecate_warn(
        when = "1.46.0",
        what = "SS_output(ncols)",
        details = "Input 'ncols' no longer needed."
      )
    }

    # check to make sure the first input is in the corect format
    if (!is.character(dir) | length(dir) != 1) {
      stop("Input 'dir' should be a character string for a directory")
    }

    # get info on output files created by Stock Synthesis
    shortrepfile <- repfile
    repfile <- file.path(dir, repfile)

    # figure out which par file to read
    parfile <- get_par_name(dir)

    if (is.na(parfile)) {
      if (!hidewarn) {
        message("Some stats skipped because the .par file not found.")
      }
    }

    # read three rows to get start time and version number from rep file
    if (file_is_empty(repfile)) {
      stop("can't find report file: ", repfile)
    }
    rephead <- readLines(con = repfile, n = 50)

    # warn if SS version used to create rep file is too old or too new for this code
    # note: SS_versionCode is new with V3.20
    # perhaps in the future we will use it to replace SS_versionshort throughout r4ss?
    SS_versionCode <- rephead[grep("#V", rephead)]
    SS_version <- rephead[grep("Stock_Synthesis", rephead)]
    SS_version <- SS_version[substring(SS_version, 1, 2) != "#C"] # remove any version numbering in the comments
    SS_version <- SS_version[1]
    if (substring(SS_version, 1, 2) == "#V") {
      SS_version <- substring(SS_version, 3)
    }
    if (substring(SS_version, 1, 4) == "3.30") {
      SS_versionshort <- "3.30"
      SS_versionNumeric <- as.numeric(SS_versionshort)
    } else {
      # typically something like "SS-V3.24"
      SS_versionshort <- toupper(substr(SS_version, 1, 8))
      SS_versionNumeric <- as.numeric(substring(SS_versionshort, 5))
    }

    SS_versionMax <- 3.30
    SS_versionMin <- 3.24

    # test for version compatibility with this code
    if (SS_versionNumeric < SS_versionMin | SS_versionNumeric > SS_versionMax) {
      warning(
        "This function tested on SS versions 3.24 and 3.30.\n",
        "  You are using ", strsplit(SS_version, split = ";")[[1]][1],
        " which MIGHT NOT WORK with this package."
      )
    } else {
      if (verbose) {
        message(
          "This function tested on SS versions 3.24 and 3.30.\n",
          "  You are using ", strsplit(SS_version, split = ";")[[1]][1],
          " which SHOULD work with this package."
        )
      }
    }

    findtime <- function(lines) {
      # quick function to get model start time from SS output files
      time <- strsplit(lines[grep("ime", lines)], "ime: ")[[1]]
      if (length(time) < 2) {
        return()
      } else {
        return(time[2])
      }
    }
    repfiletime <- findtime(rephead)
    if (verbose) {
      message("Report file time:", repfiletime)
    }

    # time check for CompReport file
    comp <- FALSE
    if (is.null(compfile)) {
      if (verbose) {
        message("Skipping CompReport because 'compfile = NULL'")
      }
    } else {
      compfile <- file.path(dir, compfile)
      if (!file_is_empty(compfile)) {
        # non-NULL compfile input provided and file exists
        comphead <- readLines(con = compfile, n = 30)
        compskip <- grep("Composition_Database", comphead)
        if (length(compskip) == 0) {
          if (verbose) {
            message(
              "No composition data, possibly because detailed output",
              " is turned off in the starter file."
            )
          }
        } else {
          # compend value helps diagnose when no comp data exists in CompReport.sso file.
          compend <- grep(" end ", comphead)
          if (length(compend) == 0) {
            compend <- 999
          }
          comptime <- findtime(comphead)
          if (is.null(comptime) || is.null(repfiletime)) {
            message(
              "problem comparing the file creation times:\n",
              "  Report.sso:", repfiletime, "\n",
              "  CompReport.sso:", comptime, "\n"
            )
          } else {
            if (comptime != repfiletime) {
              message("CompReport time:", comptime, "\n")
              stop(shortrepfile, " and ", compfile, " were from different model runs.")
            }
          }
          comp <- TRUE
        }
      } else {
        # non-NULL compfile input provided and file DOESN'T exist
        if (!is.null(compfile)) {
          if (!NoCompOK) {
            stop(
              "Missing ", compfile,
              ". Change the 'compfile' input, rerun model to get the file,",
              " or change input to 'NoCompOK = TRUE'"
            )
          } else {
            message("Composition file not found: ", compfile)
          }
        }
      }
    } # end check for NULL compfile input

    # read report file
    if (verbose) {
      message("Reading full report file")
    }
    flush.console()

    ncols <- get_ncol(repfile)
    rawrep <- read.table(
      file = repfile, col.names = 1:ncols, fill = TRUE, quote = "",
      colClasses = "character", nrows = -1, comment.char = "",
      blank.lines.skip = FALSE
    )
    # which lines in report file are all blank (either spaces or empty)
    rep_blank_lines <- which(apply(rawrep, 1, emptytest) == 1)
    # which lines in report file have hash in first column and blank after
    rep_hash_lines <- which(rawrep[, 1] == "#" & apply(rawrep[, -1], 1, emptytest) == 1)
    # combine both types (could be modified in the future to focus on just one type
    rep_blank_or_hash_lines <- sort(unique(c(rep_blank_lines, rep_hash_lines)))

    # check empty columns
    # these checks should not be triggered thanks to use of get_ncol() above,
    # added in December 2019
    nonblanks <- apply(rawrep, 2, emptytest) < 1
    maxnonblank <- max(0, (1:ncols)[nonblanks == TRUE])
    if (maxnonblank == ncols) {
      stop(
        "all columns are used and some data may been missed,\n",
        "  increase 'ncols' input above current value (ncols=", ncols, ")"
      )
    }

    # check for revised format to facilitate custom reporting
    # added with 3.30.15.06
    custom <- !is.na(match_report_line(string = "report:1", obj = rawrep[, 2]))

    if (verbose) {
      if ((maxnonblank + 1) == ncols) {
        message("Got all columns using ncols = ", ncols)
      }
      if ((maxnonblank + 1) < ncols) {
        message(
          "Got all columns. To speed code, use ncols = ", maxnonblank + 1,
          " in the future."
        )
      }
      message("Got Report file")
    }
    flush.console()

    # read forecast report file
    # (this function no longer supports reading yield curve from forecast file
    # where it occurred in older SS versions)
    if (forecast) {
      forecastname <- file.path(dir, forefile)
      if (file_is_empty(forecastname)) {
        if (verbose) {
          message("Forecast-report.sso file is missing or empty.")
        }
      } else {
        # read the file
        rawforecast1 <- read.table(
          file = forecastname, col.names = 1:ncols, fill = TRUE, quote = "",
          colClasses = "character", nrows = -1
        )

        # forecast
        grab <- rawforecast1[, 1]
        nforecastyears <- as.numeric(rawforecast1[grab %in% c("N_forecast_yrs:"), 2])
        nforecastyears <- nforecastyears[1]

        # get SPR target
        sprtarg <- as.numeric(rawforecast1[match_report_line(
          "SPR_target",
          rawforecast1[, 1]
        ), 2])

        # starting in SSv3.30.10.00, the Forecast-report file has been restructured
        target_definitions <- grep("_as_target", rawforecast1[, 1], value = TRUE)
        if (length(target_definitions) == 0) {
          # old setup (prior to 3.30.10.00)
          btarg <- as.numeric(rawforecast1[match_report_line(
            "Btarget",
            rawforecast1[, 1]
          ), 2])
        } else {
          # new setup with biomass target
          if ("Ratio_SSB/B0_as_target" %in% target_definitions) {
            btarg <- as.numeric(rawforecast1[match_report_line(
              "Ratio_target",
              rawforecast1[, 1]
            ), 2])
          }
          # new setup with F0.1_as target
          if ("F0.1_as_target" %in% target_definitions) {
            btarg <- -999
          }
        }
      }
    } else {
      if (verbose) {
        message("You skipped the forecast file.")
      }
    }
    if (!exists("btarg")) {
      nforecastyears <- NA
      sprtarg <- -999
      btarg <- -999
      if (verbose) {
        message(
          "  setting SPR target and Biomass target to -999.",
          "  Lines won't be drawn for these targets by SS_plots unless",
          "  'sprtarg' and 'btarg' are provided as inputs."
        )
      }
    }
    # set default minimum biomass thresholds based on typical west coast groundfish
    minbthresh <- -999
    if (!is.na(btarg) & btarg == 0.4) {
      if (verbose) {
        message(
          "Setting minimum biomass threshhold to 0.25",
          "  based on US west coast assumption associated with biomass target of 0.4.",
          "  (can replace or override in SS_plots by setting 'minbthresh')"
        )
      }
      minbthresh <- 0.25 # west coast assumption for non flatfish
    }
    if (!is.na(btarg) & btarg == 0.25) {
      if (verbose) {
        message(
          "Setting minimum biomass threshhold to 0.125",
          "  based on US west coast assumption associated with flatfish target of 0.25.",
          "  (can replace or override in SS_plots by setting 'minbthresh')"
        )
      }
      minbthresh <- 0.125 # west coast assumption for flatfish
    }
    flush.console()

    # check for use of temporary files
    logfile_name <- dir(dir, pattern = ".log$")
    logfile_name <- logfile_name[logfile_name != "fmin.log"]
    if (length(logfile_name) > 1) {
      filetimes <- file.info(file.path(dir, logfile_name))[["mtime"]]
      logfile_name <- logfile_name[filetimes == max(filetimes)]
      if (verbose) {
        message(
          "Multiple files in directory match pattern *.log\n",
          "choosing most recently modified file:", logfile_name, "\n"
        )
      }
    }
    if (length(logfile_name) == 1 &&
      file.info(file.path(dir, logfile_name))[["size"]] > 0) {
      logfile <- readLines(file.path(dir, logfile_name))
      logfile <- grep("^size", logfile, value = TRUE)
      if (length(logfile) == 0) {
        warning(
          logfile_name,
          " does not contain information on the size of temporary files."
        )
        logfile <- NA
      } else {
        logfile <- tidyr::separate(as.data.frame(logfile),
          col = 1,
          into = c("File", "Size"),
          sep = " = "
        )
        names(logfile) <- c("TempFile", "Size")
        logfile[["Size"]] <- as.numeric(logfile[["Size"]])
        maxtemp <- max(logfile[["Size"]])
        if (verbose) {
          if (maxtemp == 0) {
            message(
              "Got log file. There were NO temporary files were written",
              " in this run."
            )
          } else {
            message("Temporary files were written in this run.")
          }
        }
      }
    } else {
      logfile <- NA
      if (verbose) {
        message(
          "No non-empty log file in directory or too many files ",
          " matching pattern *.log"
        )
      }
    }

    # read warnings file
    if (warn) {
      warnname <- file.path(dir, warnfile)
      if (file_is_empty(warnname)) {
        # no warnings.sso file
        message(warnfile, " file not found")
        warnrows <- NA
        warnlines <- NA
      } else {
        # read warning.sso file
        warnlines <- readLines(warnname, warn = FALSE)
        # number of rows isn't equal to number of warnings, just used to
        # detect empty file
        warnrows <- length(warnlines)
        if (verbose && warnrows > 0) {
          message("Got warning file. Final line:", tail(warnlines, 1))
        }
      }
    } else {
      # chose not to read warning.sso file
      if (verbose) {
        message("You skipped the warnings file")
      }
      warnrows <- NA
      warnlines <- NA
    }
    if (verbose) {
      message("Finished reading files")
    }
    flush.console()

    # length selectivity is read earlier than other tables because it was used
    # to get fleet info this can be moved to join rest of selex stuff after
    # SSv3.11 is not supported any more
    sizeselex <- match_report_table("LEN_SELEX", 6, header = TRUE, type.convert = TRUE)
    # update to size selectivity to naming convention associated with 3.30.01.15
    sizeselex <- df.rename(sizeselex,
      oldnames = c("fleet", "year", "seas", "gender", "morph", "label"),
      newnames = c("Fleet", "Yr", "Seas", "Sex", "Morph", "Label")
    )

    ## read DEFINITIONS section (new in SSv3.20)
    ## (which_blank = 2 skips the "#" near the end to include the final table)
    rawdefs <- match_report_table("DEFINITIONS", 1,
      which_blank = 1,
      blank_lines = rep_blank_lines
    )
    # # re-read that section for older models which didn't have a hash
    # if ("LIKELIHOOD" %in% rawdefs[, 1]) {
    #   rawdefs <- match_report_table("DEFINITIONS", 1, which_blank = 1)
    # }

    # four eras for DEFINITIONS section
    # - prior to 3.20: section didn't exist
    #   - these versions not really supported by r4ss, but might work anyway
    # - 3.20 up through 3.24: section was brief with fleet info in rows
    #   - identify by version < 3.30 & presence of DEFINITIONS
    # - 3.30 up through 3.30.11: table of fleet info by column was added
    #   - identify by version >= 3.30, absence of "Jitter"
    # - 3.30.12 to 3.30.20: lots more definitions added
    #   - identify by presence of "Jitter" and "Fleet_names:" in first column
    # - 3.30.21+: fleet info in rows removed, Length_ & Age_comp_error_controls added
    #   - identify by presence of "Jitter" and absence of "Fleet_names:" in first column

    # check for new format for definitions (starting with 3.30.12)
    # ("Jitter" is an indicator of the new format)

    # placeholders for tables added in 3.30.21
    Length_comp_error_controls <- NULL
    Age_comp_error_controls <- NULL
    Size_comp_error_controls <- NULL

    if ("Jitter:" %in% rawdefs[["X1"]]) {
      get.def <- function(string) {
        # function to grab numeric value from 2nd column matching string in 1st column
        row <- grep(string, rawdefs[["X1"]])[1]
        if (length(row) > 0) {
          return(as.numeric(rawdefs[row, 2]))
        } else {
          return(NULL)
        }
      }
      # apply function above to get a bunch of things
      # in some cases, duplicate names are used for backward compatibility
      N_seasons <- nseasons <- get.def("N_seasons")
      N_sub_seasons <- get.def("N_sub_seasons")
      Season_Durations <- seasdurations <- as.numeric(rawdefs[
        grep(
          "Season_Durations",
          rawdefs[["X1"]]
        ),
        1 + 1:nseasons
      ])
      Spawn_month <- spawnmonth <- get.def("Spawn_month")
      Spawn_seas <- spawnseas <- get.def("Spawn_seas")
      Spawn_timing_in_season <- get.def("Spawn_timing_in_season")
      N_areas <- nareas <- get.def("N_areas")
      Start_year <- startyr <- get.def("Start_year")
      End_year <- endyr <- get.def("End_year")
      Retro_year <- get.def("Retro_year")
      N_forecast_yrs <- get.def("N_forecast_yrs")
      N_sexes <- nsexes <- get.def("N_sexes")
      Max_age <- accuage <- get.def("Max_age")
      Empirical_wt_at_age <- get.def("Empirical_wt_at_age")
      N_bio_patterns <- get.def("N_bio_patterns")
      N_platoons <- get.def("N_platoons")
      # following quants added in 3.30.13
      NatMort_option <- get.def("NatMort")
      GrowthModel_option <- get.def("GrowthModel")
      Maturity_option <- get.def("Maturity")
      Fecundity_option <- get.def("Fecundity")
      # end quants added in 3.30.13
      Start_from_par <- get.def("Start_from_par")
      Do_all_priors <- get.def("Do_all_priors")
      Use_softbound <- get.def("Use_softbound")
      N_nudata <- get.def("N_nudata")
      Max_phase <- get.def("Max_phase")
      Current_phase <- get.def("Current_phase")
      Jitter <- get.def("Jitter")
      ALK_tolerance <- get.def("ALK_tolerance")

      # fleetdefs table starts with final "Fleet" in column 1 (within DEFINITIONS)
      fleetdefs <- rawdefs[tail(grep("Fleet", rawdefs[["X1"]]), 1):nrow(rawdefs), ]
      names(fleetdefs) <- fleetdefs[1, ] # set names equal to first row
      fleetdefs <- fleetdefs[-1, ] # remove first row
      # remove any blank columns beyond Fleet_name
      fleetdefs <- fleetdefs[, 1:grep("fleet_name", tolower(names(fleetdefs)))]
      # make values numeric (other than Fleet_name)
      fleetdefs <- type.convert(fleetdefs, as.is = TRUE)

      fleetdefs <- df.rename(fleetdefs,
        oldnames = c("fleet_name"),
        newnames = c("Fleet_name")
      )
      # fleet_type definitions from TPL:
      # 1=fleet with catch; 2=discard only fleet with F;
      # 3=survey(ignore catch); 4=ignore completely
      fleet_type <- fleetdefs[["fleet_type"]]
      fleet_timing <- fleetdefs[["timing"]]
      fleet_area <- fleetdefs[["area"]]
      catch_units <- fleetdefs[["catch_units"]]
      ## equ_catch_se <- fleetdefs[["equ_catch_se"]]
      ## catch_se     <- fleetdefs[["catch_se"]]
      survey_units <- fleetdefs[["survey_units"]]
      survey_error <- fleetdefs[["survey_error"]]
      fleet_ID <- fleetdefs[["Fleet"]]
      IsFishFleet <- fleet_type <= 2 # based on definitions above
      nfishfleets <- sum(IsFishFleet)
      FleetNames <- fleetdefs[["Fleet_name"]]
      nfleets <- max(fleet_ID)

      # process some season info
      seasfracs <- round(12 * cumsum(seasdurations)) / 12
      seasfracs <- seasfracs - seasdurations / 2 # should be mid-point of each season as a fraction of the year

      # end DEFINITIONS elements in 3.30.12-3.30.20

      if ("Length_comp_error_controls" %in% rawdefs[["X1"]]) {
        # read table of length comp error controls (added 3.30.21)
        Length_comp_error_controls <-
          match_report_table("Length_comp_error_controls",
            adjust1 = 1,
            header = TRUE, type.convert = TRUE
          )
        if (nrow(Length_comp_error_controls) > 0) {
          present_Length_comp_error_controls <- TRUE
        }
      }

      # if that table has information in it then proceed with renaming columns
      if (exists("Length_comp_error_controls") & exists("present_Length_comp_error_controls")) {
        # rename "NoName" columns
        names(Length_comp_error_controls)[names(Length_comp_error_controls) == "NoName"] <-
          c("NoName", "Fleet_name")
        # remove extra column with hash symbols
        Length_comp_error_controls <- Length_comp_error_controls |>
          dplyr::select(-NoName)
      }

      if ("Age_comp_error_controls" %in% rawdefs[["X1"]]) {
        # read table of age comp error controls (added 3.30.21)
        Age_comp_error_controls <-
          match_report_table("Age_comp_error_controls",
            adjust1 = 1,
            header = TRUE, type.convert = TRUE
          )
        if (nrow(Age_comp_error_controls) > 0) {
          present_Age_comp_error_controls <- TRUE
        }
      }
      # if that table has information in it then proceed with renaming columns
      if (exists("Age_comp_error_controls") & exists("present_Age_comp_error_controls") > 0) {
        # rename "NoName" columns
        names(Age_comp_error_controls)[names(Age_comp_error_controls) == "NoName"] <-
          c("NoName", "Fleet_name")
        # remove extra column with hash symbols
        Age_comp_error_controls <- Age_comp_error_controls |>
          dplyr::select(-NoName)
      }

      if ("Size_comp_error_controls" %in% rawdefs[["X1"]]) {
        # read table of age comp error controls (added 3.30.21)
        Size_comp_error_controls <-
          match_report_table("Size_comp_error_controls",
            adjust1 = 1,
            header = TRUE, type.convert = TRUE
          ) |>
          dplyr::rename(Sz_method = "#_Sz_method") # remove hash from header
      }
      # end read of 3.30.12+ DEFINITIONS
    } else {
      # old format for DEFINITIONS (up through 3.30.11)

      # get season stuff
      nseasons <- as.numeric(rawdefs[grep("N_seasons", rawdefs[, 1]), 2])
      seasdurations <- as.numeric(rawdefs[grep("Season_Durations", rawdefs[, 1]), 1 + 1:nseasons])
      seasfracs <- round(12 * cumsum(seasdurations)) / 12
      seasfracs <- seasfracs - seasdurations / 2 # should be mid-point of each season as a fraction of the year

      if (SS_versionNumeric >= 3.30) {
        # version 3.3 (fleet info switched from columns to rows starting with 3.30)
        FleetNames <- as.character(rawdefs[grep("fleet_names", rawdefs[["X1"]]), -1])
        FleetNames <- FleetNames[!is.na(FleetNames) & FleetNames != ""]
        # get fleet info
        nfleets <- length(FleetNames)
        fleet_ID <- 1:nfleets
        fleetdefs <- tail(rawdefs, nfleets + 1)
        fleetdefs <- fleetdefs[, apply(rawdefs[-(1:3), ], 2, emptytest) < 1]
        fleetdefs[fleetdefs == ""] <- NA
        if (fleetdefs[1, 1] == "#_rows") { # up to version 3.30.11
          fleetdefs <- fleetdefs[-1, 1:7] # hardwiring dimensions and names
          names(fleetdefs) <- c(
            "fleet_type", "timing", "area", "catch_units",
            "catch_mult", "survey_units", "survey_error"
          )
        } else {
          # additional columns starting with 3.30.12
          # column names are now dynamic
          names(fleetdefs) <- fleetdefs[1, ]
          names(fleetdefs)[1] <- "fleet"
          fleetdefs <- fleetdefs[-1, ]
        }
        fleetdefs <- type.convert(fleetdefs, as.is = TRUE)

        # fleet_type definitions from TPL:
        # 1=fleet with catch; 2=discard only fleet with F;
        # 3=survey(ignore catch); 4=ignore completely
        fleet_type <- fleetdefs[["fleet_type"]]
        fleet_timing <- fleetdefs[["timing"]]
        fleet_area <- fleetdefs[["area"]]
        catch_units <- fleetdefs[["catch_units"]]
        equ_catch_se <- fleetdefs[["equ_catch_se"]]
        catch_se <- fleetdefs[["catch_se"]]
        survey_units <- fleetdefs[["survey_units"]]
        survey_error <- fleetdefs[["survey_error"]]
        IsFishFleet <- fleet_type <= 2 # based on definitions above

        # end of 3.30 - 3.30.11 version of DEFINITIONS
      } else {
        # version 3.20-3.24
        # get fleet info
        fleetdefs <- rawdefs[-(1:3), apply(rawdefs[-(1:3), ], 2, emptytest) < 1]
        fleetdefs[fleetdefs == ""] <- NA
        lab <- fleetdefs[["X1"]]
        fleet_ID <- as.numeric(fleetdefs[grep("fleet_ID", lab), -1])
        names(fleetdefs) <- c("Label", paste("Fleet", fleet_ID, sep = ""))
        FleetNames <- as.character(fleetdefs[grep("fleet_names", lab), -1])
        fleet_area <- as.numeric(fleetdefs[grep("fleet_area", lab), -1])
        catch_units <- as.numeric(fleetdefs[grep("Catch_units", lab), -1])
        catch_error <- as.numeric(fleetdefs[grep("Catch_error", lab), -1])
        survey_units <- as.numeric(fleetdefs[grep("Survey_units", lab), -1])
        survey_error <- as.numeric(fleetdefs[grep("Survey_error", lab), -1])
        IsFishFleet <- !is.na(catch_units)
        nfleets <- length(FleetNames)
      }

      # positions of timeseries section (used in various places below)
      begin <- match_report_line("TIME_SERIES") + 2
      end <- match_report_line("SPR_series") - 2

      # more dimensions
      nfishfleets <- sum(IsFishFleet)
      nsexes <- length(unique(as.numeric(sizeselex[["Sex"]])))
      nareas <- max(as.numeric(rawrep[begin:end, 1]))
      # startyr is the 'initial' year not including VIRG or INIT years
      startyr <- min(as.numeric(rawrep[begin:end, 2])) + 2
      temptime <- rawrep[begin:end, 2:3]
      # endyr is the beginning of the last year of the normal timeseries
      endyr <- max(as.numeric(temptime[temptime[, 2] == "TIME", 1]))
      tempaccu <- as.character(rawrep[match_report_line("Natural_Mortality") + 1, -(1:5)])
      accuage <- max(as.numeric(tempaccu[tempaccu != ""]))
    } # end read of DEFINITIONS

    # compositions
    if (comp) { # skip this stuff if no CompReport.sso file
      # read header section of file to get bin information
      # first, figure out how many columns are needed
      ncols.compfile <- get_ncol(compfile, skip = 3)

      # now read table using the appropriate number of columns
      allbins <- read.table(
        file = compfile, col.names = 1:ncols.compfile, fill = TRUE,
        colClasses = "character", skip = 3, nrows = 25
      )
      # lbins is data length bins
      lbins <- as.numeric(allbins[grep("Size_Bins_dat", allbins[, 1]) + 2, -1])
      lbins <- lbins[!is.na(lbins)]
      nlbins <- length(lbins)
      # lbinspop is Pop_len_mid used for selex and bio quantities
      lbinspop <- as.numeric(allbins[grep("Size_Bins_pop", allbins[, 1]) + 2, -1])
      lbinspop <- lbinspop[!is.na(lbinspop)]
      nlbinspop <- length(lbinspop)
      Lbin_method <- as.numeric(allbins[match_report_line(
        "Method_for_Lbin_definition",
        allbins[, 1]
      ), 2])
      if (compend == compskip + 2) {
        message("It appears that there is no composition data in CompReport.sso")
        comp <- FALSE # turning off switch to function doesn't look for comp data later on
        agebins <- NA
        sizebinlist <- NA
        nagebins <- length(agebins)
      } else {
        # read composition database
        # figure out number of columns based on header row
        col.names <- as.character(read.table(
          file = compfile, skip = compskip,
          nrows = 1, colClasses = "character"
        ))
        rawcompdbase <- read.table(
          file = compfile, col.names = col.names, fill = TRUE,
          colClasses = "character", skip = compskip, nrows = -1
        )
        names(rawcompdbase) <- rawcompdbase[1, ]
        names(rawcompdbase)[names(rawcompdbase) == "Used?"] <- "Used"
        endfile <- grep("End_comp_data", rawcompdbase[, 1])
        compdbase <- rawcompdbase[2:(endfile - 2), ] # subtract header line and last 2 lines

        # update to naming convention associated with current SS version
        # most changes associated with 3.30.12,
        # Nsamp_adj added in 3.30.15
        compdbase <- df.rename(compdbase,
          oldnames = c("Pick_sex", "Pick_gender", "Gender", "N", "Rep"),
          newnames = c("Sexes", "Sexes", "Sex", "Nsamp_adj", "Repl.")
        )

        # remove duplicate rows for unsexed fish
        # (issue was introduced in SS3 version 3.30.20 and discovered
        # after the release of 3.30.21)

        # all values identical except for Cum_obs and Cum_exp
        duplicates <- compdbase |>
          dplyr::select(-Cum_obs, -Cum_exp) |>
          duplicated()
        if (verbose) {
          message(
            "Removing ", sum(duplicates), " out of ", nrow(compdbase),
            " rows in CompReport.sso which are duplicates."
          )
        }
        compdbase <- compdbase[!duplicates, ]
        # done removing duplicates

        # "Sexes" (formerly "Pick_sex" or "Pick_gender"):
        #         0 (unknown), 1 (female), 2 (male), or 3 (females and then males)
        # this is the user input in the data file
        #
        # "Sex" (formerly "Gender"): 1 (unknown or female), or 2 (male)
        # this is a code used internally by SS
        #
        # add new column in code below:
        # "sex": 0 (unknown), 1 (female), or 2 (male)
        # this is the code used by r4ss
        compdbase[["sex"]] <- compdbase[["Sexes"]]
        compdbase[["sex"]][compdbase[["Sexes"]] == 3] <- compdbase[["Sex"]][compdbase[["Sexes"]] == 3]

        # make correction to tag output associated with 3.24f (fixed in later versions)
        if (substr(SS_version, 1, 9) == "SS-V3.24f") {
          if (!hidewarn) {
            message("Correcting for bug in tag data output associated with SSv3.24f\n")
          }
          tag1rows <- compdbase[["Sexes"]] == "TAG1"
          if (any(tag1rows)) {
            tag1 <- compdbase[tag1rows, ]
            tag1new <- tag1
            tag1new[, 4:23] <- tag1new[, 3:22] # shift columns over
            tag1new[["Yr.S"]] <- tag1new[["Yr"]] # move Yr.S
            tag1new[["Yr"]] <- floor(as.numeric(tag1new[["Yr"]])) # turn Yr.S into Yr
            compdbase[tag1rows, ] <- tag1new
          }
        }

        # remove rows within missing observations (beginning of each section)
        compdbase <- compdbase[compdbase[["Obs"]] != "", ]
        # replace underscores with NA
        compdbase[compdbase == "_"] <- NA
        # replace any NA values in the Used? column with "yes".
        compdbase[["Used"]][is.na(compdbase[["Used"]])] <- "yes"
        # add SuprPer column for versions where it didn't exist
        if (!("SuprPer" %in% names(compdbase))) {
          compdbase[["SuprPer"]] <- "No"
        }
        compdbase[["SuprPer"]][is.na(compdbase[["SuprPer"]])] <- "No"
        n <- sum(is.na(compdbase[["Nsamp_adj"]]) &
          compdbase[["Used"]] != "skip" &
          compdbase[["Kind"]] != "TAG2")
        if (n > 0) {
          warning(
            n, " rows from composition database have NA sample size\n",
            "but are not part of a super-period. (Maybe input as N=0?)\n"
          )
        }
        compdbase <- type.convert(compdbase, as.is = TRUE)

        # configure seasons
        if (nseasons > 1) {
          compdbase[["YrSeasName"]] <- paste(floor(compdbase[["Yr"]]), "s", compdbase[["Seas"]], sep = "")
        } else {
          compdbase[["YrSeasName"]] <- compdbase[["Yr"]]
        }

        # starting with SSv3.24a, the Yr.S column is already in the output, otherwise fill it in
        if (!"Yr.S" %in% names(compdbase)) {
          if (any(floor(compdbase[["Yr"]]) != compdbase[["Yr"]])) {
            # in some cases, year is already a decimal number
            compdbase[["Yr.S"]] <- compdbase[["Yr"]]
            compdbase[["Yr"]] <- floor(compdbase[["Yr"]])
          } else {
            # add fraction of season to distinguish between samples
            compdbase[["Yr.S"]] <- compdbase[["Yr"]] + (0.5 / nseasons) * compdbase[["Seas"]]
          }
        }

        # deal with Lbins
        compdbase[["Lbin_range"]] <- compdbase[["Lbin_hi"]] - compdbase[["Lbin_lo"]]
        compdbase[["Lbin_mid"]] <- 0.5 * (compdbase[["Lbin_lo"]] + compdbase[["Lbin_hi"]])

        # divide into objects by kind
        Lbin_range <- compdbase[["Lbin_range"]]
        if (is.null(Lbin_range)) { # if/else required to avoid warning if no comp data at all
          notconditional <- TRUE
          conditional <- FALSE
        } else {
          notconditional <- !is.na(Lbin_range) & Lbin_range > aalmaxbinrange
          conditional <- !is.na(Lbin_range) & Lbin_range <= aalmaxbinrange
        }
        if ("skip" %in% compdbase[["SuprPer"]]) {
          # formatting error in some SS 3.30 versions caused skip to appear in
          # the wrong column, so copy to the right one
          compdbase[["Used"]][compdbase[["SuprPer"]] == "skip"] <- "skip"
          # probability of being a super-period is low, so assigning "No"
          # to assist with identification of ghost comps below
          compdbase[["SuprPer"]][compdbase[["SuprPer"]] == "No"]
        }
        if (SS_versionNumeric >= 3.22) {
          # new designation of ghost fleets from negative samp size to negative fleet
          lendbase <- compdbase[compdbase[["Kind"]] == "LEN" &
            compdbase[["Used"]] != "skip", ]
          sizedbase <- compdbase[compdbase[["Kind"]] == "SIZE" &
            compdbase[["Used"]] != "skip", ]
          agedbase <- compdbase[compdbase[["Kind"]] == "AGE" &
            compdbase[["Used"]] != "skip" & notconditional, ]
          condbase <- compdbase[compdbase[["Kind"]] == "AGE" &
            compdbase[["Used"]] != "skip" & conditional, ]
          morphcompdbase <- compdbase[compdbase[["Kind"]] == "GP%" &
            compdbase[["Used"]] != "skip", ]
        } else {
          # older designation of ghost fleets from negative samp size to negative fleet
          lendbase <- compdbase[compdbase[["Kind"]] == "LEN" &
            (compdbase[["SuprPer"]] == "Sup" |
              (!is.na(compdbase[["Nsamp_adj"]]) & compdbase[["Nsamp_adj"]] > 0)), ]
          sizedbase <- compdbase[compdbase[["Kind"]] == "SIZE" &
            (compdbase[["SuprPer"]] == "Sup" |
              (!is.na(compdbase[["Nsamp_adj"]]) & compdbase[["Nsamp_adj"]] > 0)), ]
          agedbase <- compdbase[compdbase[["Kind"]] == "AGE" &
            (compdbase[["SuprPer"]] == "Sup" |
              (!is.na(compdbase[["Nsamp_adj"]]) & compdbase[["Nsamp_adj"]] > 0)) &
            notconditional, ]
          condbase <- compdbase[compdbase[["Kind"]] == "AGE" &
            (compdbase[["SuprPer"]] == "Sup" |
              (!is.na(compdbase[["Nsamp_adj"]]) & compdbase[["Nsamp_adj"]] > 0)) &
            conditional, ]
        }
        ghostagedbase <- compdbase[compdbase[["Kind"]] == "AGE" &
          compdbase[["Used"]] == "skip" &
          compdbase[["SuprPer"]] == "No" & notconditional, ]
        ghostcondbase <- compdbase[compdbase[["Kind"]] == "AGE" &
          compdbase[["Used"]] == "skip" &
          compdbase[["SuprPer"]] == "No" & conditional, ]
        ghostlendbase <- compdbase[compdbase[["Kind"]] == "LEN" &
          compdbase[["Used"]] == "skip" &
          compdbase[["SuprPer"]] == "No", ]
        compdbase[["Kind"]][compdbase[["Kind"]] == "L@A" & compdbase[["Ageerr"]] < 0] <- "W@A"

        # extra processing for sizedbase
        if (!is.null(sizedbase) && nrow(sizedbase) > 0) {
          sizedbase[["bio.or.num"]] <- c("bio", "num")[sizedbase[["Lbin_lo"]]]
          sizedbase[["units"]] <- c("kg", "lb", "cm", "in")[sizedbase[["Lbin_hi"]]]
          sizedbase[["method"]] <- sizedbase[["Ageerr"]]

          if (any(sizedbase[["units"]] %in% c("lb", "in"))) {
            if (verbose) {
              message(
                "Note: converting bins in generalized size comp data ",
                " in sizedbase back to the original units of lbs or inches."
              )
            }
          }
          # convert bins from kg to lbs when that was the original unit
          sizedbase[["Bin"]][sizedbase[["units"]] == "lb"] <-
            sizedbase[["Bin"]][sizedbase[["units"]] == "lb"] / 0.4536
          # convert bins from cm to inches when that was the original unit
          sizedbase[["Bin"]][sizedbase[["units"]] == "in"] <-
            sizedbase[["Bin"]][sizedbase[["units"]] == "in"] / 2.54

          sizebinlist <- list()
          for (imethod in 1:max(sizedbase[["method"]])) {
            tmp <- sort(unique(sizedbase[["Bin"]][sizedbase[["method"]] == imethod]))
            if (length(tmp) == 0) tmp <- NULL
            sizebinlist[[paste("size_method_", imethod, sep = "")]] <- tmp
          }
        } else {
          sizebinlist <- NA
        }

        if (is.null(compdbase[["Nsamp_adj"]])) {
          good <- TRUE
        } else {
          good <- !is.na(compdbase[["Nsamp_adj"]])
        }
        ladbase <- compdbase[compdbase[["Kind"]] == "L@A" & good, ]
        wadbase <- compdbase[compdbase[["Kind"]] == "W@A" & good, ]
        tagdbase1 <- compdbase[compdbase[["Kind"]] == "TAG1", ]
        tagdbase2 <- compdbase[compdbase[["Kind"]] == "TAG2", ]
        # consider range of bins for conditional age at length data
        if (verbose) {
          message(
            "CompReport file separated by this code as follows",
            " (rows = Ncomps*Nbins):\n",
            if (nrow(lendbase) > 0) {
              paste0(
                "  ", nrow(lendbase),
                " rows of length comp data\n"
              )
            },
            if (nrow(sizedbase) > 0) {
              paste0(
                "  ", nrow(sizedbase),
                " rows of generalized size comp data\n"
              )
            },
            if (nrow(agedbase) > 0) {
              paste0(
                "  ", nrow(agedbase),
                " rows of age comp data\n"
              )
            },
            if (nrow(condbase) > 0) {
              paste0(
                "  ", nrow(condbase),
                " rows of conditional age-at-length data\n"
              )
            },
            if (nrow(ghostagedbase) > 0) {
              paste0(
                "  ", nrow(ghostagedbase),
                " rows of ghost fleet age comp data\n"
              )
            },
            if (nrow(ghostcondbase) > 0) {
              paste0(
                "  ", nrow(ghostcondbase),
                " rows of ghost fleet conditional age-at-length data\n"
              )
            },
            if (nrow(ghostlendbase) > 0) {
              paste0(
                "  ", nrow(ghostlendbase),
                " rows of ghost fleet length comp data\n"
              )
            },
            if (nrow(ladbase) > 0) {
              paste0(
                "  ", nrow(ladbase),
                " rows of mean length at age data\n"
              )
            },
            if (nrow(wadbase) > 0) {
              paste0(
                "  ", nrow(wadbase),
                " rows of mean weight at age data\n"
              )
            },
            if (nrow(tagdbase1) > 0) {
              paste0(
                "  ", nrow(tagdbase1),
                " rows of 'TAG1' comp data\n"
              )
            },
            if (nrow(tagdbase2) > 0) {
              paste0(
                "  ", nrow(tagdbase2),
                " rows of 'TAG2' comp data"
              )
            },
            if (nrow(morphcompdbase) > 0) {
              paste0(
                "  ", nrow(morphcompdbase),
                " rows of morph comp data"
              )
            }
          )
        }
        # convert bin indices to true lengths
        if (nrow(agedbase) > 0) {
          Lbin_ranges <- as.data.frame(table(agedbase[["Lbin_range"]]))
          names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
          if (length(unique(agedbase[["Lbin_range"]])) > 1) {
            warning(
              "different ranges of Lbin_lo to Lbin_hi found in age comps.\n",
              paste(utils::capture.output(print(Lbin_ranges)), collapse = "\n"),
              "\n consider increasing 'aalmaxbinrange' to designate\n",
              "some of these data as conditional age-at-length."
            )
          }
          agebins <- sort(unique(agedbase[["Bin"]][!is.na(agedbase[["Bin"]])]))
        } else {
          if (nrow(condbase) > 0) {
            agebins <- sort(unique(condbase[["Bin"]][!is.na(condbase[["Bin"]])]))
          } else {
            agebins <- NA
          }
        }
        nagebins <- length(agebins)
      }
    } else {
      # if comp option is turned off
      lbins <- NA
      nlbins <- NA

      #### need to get length bins from somewhere
      ## temp <- rawrep[grep("NUMBERS_AT_LENGTH",rawrep[,1])+1,]
      ## lbinspop <- as.numeric(temp[temp!=""][-(1:11)])
      ## nlbinspop <- length(lbinspop)
      ##
      #### if natlen were already defined, it could be
      ## lbinspop <- as.numeric(names(natlen)[-c(1:11)])
      lbinspop <- NA
      nlbinspop <- ncol(sizeselex) - 5 # hopefully this works alright
      agebins <- NA
      nagebins <- NA
      Lbin_method <- 2
      sizebinlist <- NA
    }

    # info on growth morphs (see also section setting mainmorphs below)
    morph_indexing <- match_report_table("MORPH_INDEXING", 1,
      header = TRUE, type.convert = TRUE
    )
    # rename some headers to match output from most recent SS versions
    morph_indexing <- df.rename(morph_indexing,
      oldnames = c("Gpattern", "Bseas", "BirthSeason", "Gender"),
      newnames = c("GP", "BirthSeas", "BirthSeas", "Sex")
    )
    if (!is.null(morph_indexing)) {
      # calculate number of growth patterns
      ngpatterns <- max(morph_indexing[["GP"]])
    } else {
      ngpatterns <- NULL
    }

    if (verbose) {
      message("Finished dimensioning")
    }
    flush.console()

    # stats list: items that are output to the GUI (if printstats==T) for a quick summary of results
    stats <- list()
    stats[["SS_version"]] <- SS_version
    stats[["SS_versionshort"]] <- SS_versionshort
    stats[["SS_versionNumeric"]] <- SS_versionNumeric

    stats[["StartTime"]] <- paste(as.character(match_report_table("StartTime", 0, "StartTime", 0, cols = 1:6)), collapse = " ")
    stats[["RunTime"]] <- paste(as.character(match_report_table("StartTime", 2, "StartTime", 2, cols = 4:9)), collapse = " ")

    # data return object to fill in various things
    returndat <- list()

    # input files
    tempfiles <- match_report_table("Data_File", 0, "Control_File", 0, cols = 1:2)
    stats[["Files_used"]] <- paste(c(tempfiles[1, ], tempfiles[2, ]), collapse = " ")
    returndat[["Data_File"]] <- tempfiles[1, 2]
    returndat[["Control_File"]] <- tempfiles[2, 2]

    # log determinant of the Hessian (previously was from ss.cor file)
    log_det_hessian <- match_report_table("Hessian", 0,
      "Hessian", 0,
      cols = 2
    )
    if (log_det_hessian == "Not") { # first part of "Not requested."
      covar <- FALSE
      log_det_hessian <- NA
    }
    # as.numeric() doesn't give warning if value is NA
    stats[["log_det_hessian"]] <- as.numeric(log_det_hessian)

    # two additional outputs added in 3.30.20
    # (also "total_LogL" which is redundant with value in LIKELIHOOD
    # table read later)
    Final_phase <- match_report_table("Final_phase", 0,
      "Final_phase", 0,
      cols = 2
    )
    if (!is.null(Final_phase)) {
      stats[["Final_phase"]] <- as.numeric(Final_phase)
    }
    N_iterations <- match_report_table("N_iterations", 0,
      "N_iterations", 0,
      cols = 2
    )
    if (!is.null(N_iterations)) {
      stats[["N_iterations"]] <- as.numeric(N_iterations)
    }

    # check warnings
    stats[["Nwarnings"]] <- warnrows
    if (length(warn) > 20) {
      warn <- c(warn[1:20], paste(
        "Note:", length(warn) - 20,
        "additional lines truncated. Look in",
        warnfile,
        "file to see full list."
      ))
    }
    stats[["warnings"]] <- warnlines

    # likelihoods
    rawlike <- match_report_table("LIKELIHOOD", 2, "Fleet:", -2)
    # check for new section added in SS version 3.30.13.04 (2019-05-31)
    laplace_line <- which(rawlike[, 1] == "#_info_for_Laplace_calculations")
    if (length(laplace_line) > 0) {
      rawlike <- rawlike[-laplace_line, ]
    }
    # make numeric, clean up blank values
    like <- data.frame(signif(as.numeric(rawlike[, 2]), digits = 7))
    names(like) <- "values"
    rownames(like) <- rawlike[, 1]
    lambdas <- rawlike[, 3]
    lambdas[lambdas == ""] <- NA
    lambdas <- as.numeric(lambdas)
    like[["lambdas"]] <- lambdas
    # separate new section added in SS version 3.30.13.04 (2019-05-31)
    if (length(laplace_line) > 0) {
      stats[["likelihoods_used"]] <- like[1:(laplace_line - 1), ]
      stats[["likelihoods_laplace"]] <- like[laplace_line:nrow(like), ]
    } else {
      stats[["likelihoods_used"]] <- like
      stats[["likelihoods_laplace"]] <- NULL
    }

    # read fleet-specific likelihoods
    likelihoods_by_fleet <- match_report_table("Fleet:", 0, header = TRUE)
    # there was no space before "Parm_devs_detail" prior to 3.30.15.06
    if (!is.null(likelihoods_by_fleet) &&
      "Parm_devs_detail" %in% likelihoods_by_fleet[, 1]) {
      likelihoods_by_fleet <- match_report_table("Fleet:", 0,
        "Parm_devs_detail", -1,
        header = TRUE
      )
    }

    # clean up fleet-specific likelihoods
    likelihoods_by_fleet[likelihoods_by_fleet == "_"] <- NA
    likelihoods_by_fleet <- type.convert(likelihoods_by_fleet, as.is = TRUE)

    # replace numeric column names with fleet names
    names(likelihoods_by_fleet) <- c("Label", "ALL", FleetNames)
    labs <- likelihoods_by_fleet[["Label"]]

    # removing ":" at the end of likelihood components
    for (irow in seq_along(labs)) {
      labs[irow] <- substr(labs[irow], 1, nchar(labs[irow]) - 1)
    }
    likelihoods_by_fleet[["Label"]] <- labs

    stats[["likelihoods_by_fleet"]] <- likelihoods_by_fleet

    likelihoods_by_tag_group <- match_report_table("Tag_Group:", 0, header = TRUE)
    # check for presence of tag data likelihood which has different column structure
    if (!is.null(likelihoods_by_tag_group)) {
      # clean up tag group likelihoods
      likelihoods_by_tag_group[likelihoods_by_tag_group == "_"] <- NA
      likelihoods_by_tag_group <- type.convert(likelihoods_by_tag_group,
        as.is = TRUE
      )
      # rename columns from numbers to "TagGroup_1", etc.
      names(likelihoods_by_tag_group) <- c(
        "Label", "ALL",
        paste0(
          "TagGroup_",
          names(likelihoods_by_tag_group)[-(1:2)]
        )
      )
      # remove colon from "Tag_Group:"
      likelihoods_by_tag_group[["Label"]][1] <- "Tag_Group"
      stats[["likelihoods_by_tag_group"]] <- likelihoods_by_tag_group
    }

    # read detail on parameters devs (if present, 3.30 only)
    Parm_devs_detail <- match_report_table("Parm_devs_detail", 1,
      header = TRUE, type.convert = TRUE
    )
    stats[["Parm_devs_detail"]] <- Parm_devs_detail

    # parameters
    parameters <- match_report_table("PARAMETERS", 1, header = TRUE)
    parameters <- df.rename(parameters,
      oldnames = c("PR_type", "Prior_Like"),
      newnames = c("Pr_type", "Pr_Like")
    )
    parameters[parameters == "_"] <- NA
    parameters[parameters == " "] <- NA
    parameters[parameters == "1.#INF"] <- Inf # set infinite values equal to R's infinity

    # fix for issue with SSv3.21f
    if (SS_versionNumeric == 3.21) {
      temp <- names(parameters)
      message(
        "Inserting new 13th column heading in parameters section",
        "due to error in Report.sso in SSv3.21f"
      )
      temp <- c(temp[1:12], "PR_type_code", temp[-(1:12)])
      temp <- temp[-length(temp)]
      names(parameters) <- temp
    }
    # fix issue with missing column in dev output
    # associated with at least SS versions 3.30.01 and 3.30.13
    if ("Gradient" %in% names(parameters) &&
      any(parameters[["Gradient"]] %in% c("dev", "F"))) {
      bad <- parameters[["Gradient"]] %in% c("dev", "F")
      parameters[["Pr_type"]][bad] <- parameters[["Gradient"]][bad]
      parameters[["Gradient"]][bad] <- NA
    }
    # make values numeric
    parameters <- type.convert(parameters, as.is = TRUE)

    # convert really old numeric codes to names
    # note that codes used in control file for SS version 3.30 don't match
    # these from earlier models
    # it's possible that SS_output doesn't work for models prior to 3.21, in
    # which case this section could be removed
    if (SS_versionNumeric < 3.21) {
      parameters[["Pr_type_numeric"]] <- parameters[["Pr_type"]]
      parameters[["Pr_type"]][parameters[["Pr_type_numeric"]] == -1] <- "No_prior"
      parameters[["Pr_type"]][parameters[["Pr_type_numeric"]] == 0] <- "Normal"
      parameters[["Pr_type"]][parameters[["Pr_type_numeric"]] == 1] <- "Sym_Beta"
      parameters[["Pr_type"]][parameters[["Pr_type_numeric"]] == 2] <- "Full_Beta"
      parameters[["Pr_type"]][parameters[["Pr_type_numeric"]] == 3] <- "Log_Norm"
      parameters[["Pr_type"]][parameters[["Pr_type_numeric"]] == 4] <- "Log_Norm_adjusted"
    }

    # fix for duplicate parameter labels in 3.30.03.03,
    # not robust to more than 2 growth patterns but probably will be fixed soon
    ParmLabels <- parameters[["Label"]]
    ParmLabels[duplicated(ParmLabels)] <- paste0(ParmLabels[duplicated(ParmLabels)], "_2")
    # end fix
    rownames(parameters) <- ParmLabels

    if (!is.na(parfile)) {
      parline <- read.table(file.path(dir, parfile),
        fill = TRUE,
        comment.char = "", nrows = 1
      )
    } else {
      parline <- matrix(NA, 1, 16)
    }
    stats[["N_estimated_parameters"]] <- parline[1, 6]

    # subset to active parameters only
    pars <- parameters[!is.na(parameters[["Active_Cnt"]]), ]

    if (nrow(pars) > 0) {
      pars[["Afterbound"]] <- ""
      pars[["checkdiff"]] <- pars[["Value"]] - pars[["Min"]]
      pars[["checkdiff2"]] <- pars[["Max"]] - pars[["Value"]]
      pars[["checkdiff3"]] <- abs(pars[["Value"]] - (pars[["Max"]] - (pars[["Max"]] - pars[["Min"]]) / 2))
      pars[["Afterbound"]][pars[["checkdiff"]] < 0.001 | pars[["checkdiff2"]] < 0.001 | pars[["checkdiff2"]] < 0.001] <- "CHECK"
      pars[["Afterbound"]][!pars[["Afterbound"]] %in% "CHECK"] <- "OK"
    }
    stats[["table_of_phases"]] <- table(parameters[["Phase"]])
    # subset columns for printed table of estimated parameters
    estimated_non_dev_parameters <- pars[, names(pars) %in%
      c(
        "Value", "Phase", "Min", "Max", "Init", "Prior", "Gradient", "Pr_type",
        "Pr_SD", "Pr_Like", "Parm_StDev", "Status", "Afterbound"
      )]
    # exclude parameters that represent recdevs or other deviations
    devnames <- c(
      "RecrDev", "InitAge", "ForeRecr",
      "DEVadd", "DEVmult", "DEVrwalk", "DEV_MR_rwalk", "ARDEV"
    )
    # look for rows in table of parameters that have label indicating deviation
    devrows <- NULL
    for (iname in seq_along(devnames)) {
      devrows <- unique(c(devrows, grep(
        devnames[iname],
        rownames(estimated_non_dev_parameters)
      )))
    }
    # remove any dev rows from table
    if (!is.null(devrows) & length(devrows) > 0) {
      estimated_non_dev_parameters <- estimated_non_dev_parameters[-devrows, ]
    }
    # add table to stats that get printed in console
    stats[["estimated_non_dev_parameters"]] <- estimated_non_dev_parameters

    # Semi-parametric (2D-AR1) selectivity parameters
    seldev_pars <- parameters[
      grep("ARDEV", parameters[["Label"]], fixed = TRUE),
      names(parameters) %in% c("Label", "Value")
    ]
    if (nrow(seldev_pars) == 0) {
      # if semi-parametric selectivity IS NOT used
      seldev_pars <- NULL
      seldev_matrix <- NULL
    } else {
      # if semi-parametric selectivity IS used
      if (any(duplicated(FleetNames))) {
        warning(
          "Duplicated fleet names will cause only the semi-parametric",
          " selectivity to be available for the first of the duplicates."
        )
      }
      # parse parameter labels to get info
      # the parameter labels look like like
      # Fishery_ARDEV_y1991_A3 (for age-based selectivity)
      # or
      # Fishery_ARDEV_y1991_Lbin3 (for length-based selectivity)
      #
      # the code below parses those strings to figure out age vs. length,
      # separate the numeric year value and bin number
      seldev_label_info <- strsplit(seldev_pars[["Label"]], split = "_")
      seldev_label_info <- data.frame(do.call(rbind, lapply(seldev_label_info, rbind)))

      # add columns to pars data.frame with info from labels
      seldev_pars[["Fleet"]] <- seldev_label_info[["X1"]]
      yr_col <- grep("^y\\d\\d\\d\\d$", seldev_label_info[1, ])
      # probably always the final column will be one that starts with 
      # A or L (upper or lower case) and ends with a bin number
      type_bin_col <- grep("^[aAlL].*\\d+$", seldev_label_info[1, ])
      seldev_pars[["Year"]] <- as.numeric(substring(seldev_label_info[[yr_col]], 2))
      # note: bin was indicated by "a" for length- and age-based selectivity
      # until early 2020 when separate "A" or "Lbin" codes were used
      seldev_pars[["Type"]] <- ifelse(
        substring(seldev_label_info[[type_bin_col]], 1, 1) %in%
          c("A", "a"),
        yes = "age",
        no = "length"
      )
      # how many non-numeric digits to skip over in parsing bin value
      first_bin_digit <- ifelse(seldev_pars[["Type"]] == "age", 2, 5)
      # parse bin (age or length bin)
      seldev_pars[["Bin"]] <- as.numeric(substring(seldev_label_info[[type_bin_col]], first_bin_digit))
      # remove label column which is redundant with rownames
      seldev_pars <- seldev_pars[, -1]

      # make matrix
      seldev_matrix <- list()
      for (fleet in sort(unique(seldev_pars[["Fleet"]]))) {
        # subset for specific fleet
        seldev_pars_f <- seldev_pars[seldev_pars[["Fleet"]] == fleet, ]
        for (type in unique(seldev_pars_f[["Type"]])) {
          # subset for type (unlikely to have more than 1 per fleet, but safer this way)
          seldev_pars_sub <- seldev_pars_f[seldev_pars_f[["Type"]] == type, ]
          seldev_label <- paste0(fleet, "_", type, "_seldevs")
          seldev_yrs <- sort(unique(seldev_pars_sub[["Year"]]))
          seldev_bins <- sort(unique(seldev_pars_sub[["Bin"]]))
          # create empty matrix with labels on each dimension
          if (type == "length") {
            seldev_matrix[[seldev_label]] <-
              matrix(
                nrow = length(seldev_yrs), ncol = length(seldev_bins),
                dimnames = list(Year = seldev_yrs, Lbin = seldev_bins)
              )
          }
          if (type == "age") {
            seldev_matrix[[seldev_label]] <-
              matrix(
                nrow = length(seldev_yrs), ncol = length(seldev_bins),
                dimnames = list(Year = seldev_yrs, Age = seldev_bins)
              )
          }
          # loop over years and bins to fill in matrix
          for (y in seldev_yrs) {
            for (bin in seldev_bins) {
              seldev_matrix[[seldev_label]][paste(y), paste(bin)] <-
                seldev_pars_sub[["Value"]][seldev_pars_sub[["Year"]] == y & seldev_pars_sub[["Bin"]] == bin][1]
            }
          } # end loop over years
        } # end loop over types
      } # end loop over fleets
    } # end check for semi-parametric selectivity

    # Dirichlet-Multinomial parameters
    # more processing of these parameters is done later in SS_output()
    # after info on the comps has been read
    DM_pars <- parameters[
      grep("ln\\((EffN_mult)|(DM_theta)\\)", parameters[["Label"]]),
      names(parameters) %in% c("Value", "Phase", "Min", "Max")
    ]
    # calculate additional values based on estimate parameter
    # non-log Theta
    DM_pars[["Theta"]] <- exp(DM_pars[["Value"]])
    # Theta ratio related to weighting
    DM_pars$"Theta/(1+Theta)" <- DM_pars[["Theta"]] / (1 + DM_pars[["Theta"]])

    # check the covar.sso file
    # this section moved down within SS_output for 3.30.20 to avoid
    # reading covar if -nohess used
    if (covar) {
      covarfile <- file.path(dir, covarfile)
      if (!file.exists(covarfile)) {
        message("covar file not found, input 'covar' changed to FALSE")
        covar <- FALSE
      } else {
        # time check for CoVar file
        covarhead <- readLines(con = covarfile, n = 10)
        covarskip <- grep("active-i", covarhead) - 1
        covartime <- findtime(covarhead)
        # the conversion to R time class below may no longer be necessary as strings should match
        if (is.null(covartime) || is.null(repfiletime)) {
          message(
            "problem comparing the file creation times:\n",
            "  Report.sso:", repfiletime, "\n",
            "  covar.sso:", covartime
          )
        } else {
          if (covartime != repfiletime) {
            message("covar time:", covartime)
            stop(
              shortrepfile, " and ", covarfile,
              " were from different model runs. Change input to covar=FALSE"
            )
          }
        }

        # covar file exists, but has problems
        nowrite <- grep("do not write", covarhead)
        if (length(nowrite) > 0) {
          warning(
            "covar file contains the warning\n",
            "     '", covarhead[nowrite], "'\n",
            "  input 'covar' changed to FALSE.\n"
          )
          covar <- FALSE
        }
      }
    }

    # read covar.sso file
    if (covar) {
      CoVar <- read.table(covarfile, header = TRUE, colClasses = c(rep("numeric", 4), rep("character", 4), "numeric"), skip = covarskip)
      if (verbose) {
        message("Got covar file.")
      }
      stdtable <- CoVar[CoVar[["Par..j"]] == "Std", c(7, 9, 5)]
      names(stdtable) <- c("name", "std", "type")
      N_estimated_parameters2 <- sum(stdtable[["type"]] == "Par")

      # this section was muddling Derived Quants with Parameters in early version of SSv3.20
      # got work-around pending fix from Rick to use of "Par" vs. "Der" in covar file.
      if (is.na(stats[["N_estimated_parameters"]])) {
        stats[["N_estimated_parameters"]] <- N_estimated_parameters2
      } else {
        if (stats[["N_estimated_parameters"]] != N_estimated_parameters2) {
          warning(
            stats[["N_estimated_parameters"]],
            " estimated parameters indicated by the par file\n  ",
            N_estimated_parameters2,
            " estimated parameters shown in the covar file\n  ",
            "Returning the par file value: ", stats[["N_estimated_parameters"]]
          )
        }
      }
      # check for NA values (see https://github.com/r4ss/r4ss/issues/830)
      if (any(is.na(stdtable[["std"]]))) {
        warning(
          "NA value for parameter uncertainty found in ",
          sum(is.na(stdtable[["std"]])),
          " rows of covar.sso file. ",
          "First par with NA: ",
          stdtable[["name"]][is.na(stdtable[["std"]])]
        )
      }
      Nstd <- sum(stdtable[["std"]] > 0, na.rm = TRUE)
      checkbadrun <- unique(stdtable[["std"]])
      if (length(checkbadrun) == 1) {
        if (checkbadrun %in% c(NA, "NaN", "na")) {
          stop(paste0(
            "No quantities were estimated in the covar file \nand all",
            "estimates of standard deviation are ", checkbadrun, ". \nTry re-running",
            "stock synthesis."
          ))
        }
      }

      if (Nstd <= 1) {
        stop("Too few estimated quantities in covar file (n=", Nstd, "). Change input to covar=FALSE.")
      }
    } else {
      if (verbose) {
        message("You skipped the covar file")
      }
    }
    flush.console()

    # read weight-at-age file
    wtatage <- NULL
    if (readwt) {
      wtfile <- file.path(dir, wtfile)
      wtatage <- SS_readwtatage(file = wtfile, verbose = verbose)
    }

    # read MCMC output
    if (is.null(dir.mcmc)) {
      # if no directory provided, set results to NULL
      mcmc <- NULL
    } else {
      # directory provided, check to make sure it exsists
      dir.mcmc.full <- NULL
      if (dir.exists(dir.mcmc)) {
        dir.mcmc.full <- dir.mcmc
      }
      if (dir.exists(file.path(dir, dir.mcmc))) {
        dir.mcmc.full <- file.path(dir, dir.mcmc)
      }
      # warn if directory doesn't exist
      if (is.null(dir.mcmc.full)) {
        warning(
          "'dir.mcmc' directory not found either as an absolute path ",
          "or relative to the 'dir' input"
        )
        mcmc <- NULL
      } else {
        # check for presence of posteriors file
        if ("posteriors.sso" %in% dir(dir.mcmc.full)) {
          # run function to read posteriors.sso and derived_posteriors.sso
          if (verbose) {
            message("Running 'SSgetMCMC' to get MCMC output")
          }
          mcmc <- SSgetMCMC(dir = dir.mcmc.full)
        } else {
          warning(
            "skipping reading MCMC output because posterior.sso file",
            " not found in \n",
            dir.mcmc.full
          )
          mcmc <- NULL
        }
      }
    }

    # derived quantities
    der <- match_report_table("DERIVED_QUANTITIES", 4, header = TRUE)
    # make older SS output names match current SS output conventions
    der <- df.rename(der, oldnames = "LABEL", newnames = "Label")

    # remove extra row (don't remember why it occurs)
    der <- der[der[["Label"]] != "Bzero_again", ]
    der[der == "_"] <- NA
    der[der == ""] <- NA

    # remove bad rows that were present in 3.30-beta in September 2016
    # (note that spelling differs from "Parm_devs_detail" after likelihood)
    test <- grep("Parm_dev_details", der[["Label"]])
    if (length(test) > 0) {
      der <- der[1:(min(test) - 1), ]
    }
    # convert columns to numeric
    der <- type.convert(der, as.is = TRUE)

    # replace SPB with SSB as changed in SS version 3.30.10.00 (29 Nov. 2017)
    der[["Label"]] <- gsub("SPB_", "SSB_", der[["Label"]], fixed = TRUE)
    # set rownames equal to Label column
    # (skipping any duplicates, such as ln(SPB)_YYYY for models with limited year range)
    rownames(der)[!duplicated(der[["Label"]])] <- der[["Label"]][!duplicated(der[["Label"]])]

    # get management ratio labels from top of DERIVED_QUANTITIES
    managementratiolabels <- match_report_table("DERIVED_QUANTITIES", 1, "DERIVED_QUANTITIES", 3, cols = 1:2)
    names(managementratiolabels) <- c("Ratio", "Label")

    # new message about how forecast selectivity is modeled added in 3.30.06
    # (has impact on read of time-varying parameters below)
    forecast_selectivity <- grep("forecast_selectivity", rawrep[, 1], value = TRUE)
    if (length(forecast_selectivity) == 0) {
      forecast_selectivity <- NA
      offset <- -1
    } else {
      offset <- -2
    }

    # time-varying parameters
    MGparmAdj <- match_report_table("MGparm_By_Year_after_adjustments", 1,
      header = TRUE, type.convert = TRUE
    )
    # make older SS output names match current SS output conventions
    MGparmAdj <- df.rename(MGparmAdj, oldnames = "Year", newnames = "Yr")

    # time-varying size-selectivity parameters
    SelSizeAdj <- match_report_table("selparm(Size)_By_Year_after_adjustments", 2)
    if (is.null(SelSizeAdj) || nrow(SelSizeAdj) <= 2) {
      SelSizeAdj <- NULL
    } else {
      SelSizeAdj <- SelSizeAdj[, apply(SelSizeAdj, 2, emptytest) < 1]
      SelSizeAdj[SelSizeAdj == ""] <- NA
      # make values numeric
      SelSizeAdj <- type.convert(SelSizeAdj, as.is = TRUE)
      # provide column names (first test for extra column added in 3.30.06.02)
      if (rawrep[match_report_line("selparm(Size)_By_Year_after_adjustments") + 1, 3]
      == "Change?") {
        names(SelSizeAdj) <- c(
          "Fleet", "Yr", "Change?",
          paste0("Par", 1:(ncol(SelSizeAdj) - 3))
        )
      } else {
        names(SelSizeAdj) <- c(
          "Fleet", "Yr",
          paste0("Par", 1:(ncol(SelSizeAdj) - 2))
        )
      }
    }

    # time-varying age-selectivity parameters
    SelAgeAdj <- match_report_table("selparm(Age)_By_Year_after_adjustments", 2)
    if (!is.null(SelAgeAdj) && nrow(SelAgeAdj) > 2) {
      SelAgeAdj <- SelAgeAdj[, apply(SelAgeAdj, 2, emptytest) < 1]
      SelAgeAdj[SelAgeAdj == ""] <- NA
      # test for empty table
      if (SelAgeAdj[1, 1] == "RECRUITMENT_DIST") {
        SelAgeAdj <- NA
      } else {
        # make values numeric
        SelAgeAdj <- type.convert(SelAgeAdj, as.is = TRUE)
        names(SelAgeAdj) <- c("Flt", "Yr", paste0("Par", 1:(ncol(SelAgeAdj) - 2)))
        # provide rownames (after testing for extra column added in 3.30.06.02)
        if (rawrep[match_report_line("selparm(Age)_By_Year_after_adjustments") + 1, 3]
        == "Change?") {
          names(SelAgeAdj) <- c(
            "Fleet", "Yr", "Change?",
            paste0("Par", 1:(ncol(SelAgeAdj) - 3))
          )
        } else {
          names(SelAgeAdj) <- c(
            "Fleet", "Yr",
            paste0("Par", 1:(ncol(SelAgeAdj) - 2))
          )
        }
      }
    } else {
      SelAgeAdj <- NULL
    }

    # recruitment distribution
    recruitment_dist <- match_report_table("RECRUITMENT_DIST", 1,
      header = TRUE, type.convert = TRUE
    )
    if (!is.null(recruitment_dist)) {
      # convert to new column header used starting with 3.30.23
      recruitment_dist <- df.rename(recruitment_dist,
        oldnames = c("Frac/sex", "Value"),
        newnames = c("recr_dist_F", "recr_dist_F")
      )
      # calculate first season with recruitment
      first_seas_with_recruits <-
        min(recruitment_dist[["Seas"]][recruitment_dist$"recr_dist_F" > 0])

      # starting in SSv3.24Q there are additional tables
      # (in v3.30 RECRUITMENT_DIST_BENCHMARK was renamed RECRUITMENT_DIST_Bmark
      # and RECRUITMENT_DIST_FORECAST was renamed RECRUITMENT_DIST_endyr)
      recruit_dist_Bmark <- match_report_table("RECRUITMENT_DIST_B", 1,
        header = TRUE, type.convert = TRUE
      )
      if (!is.null(recruit_dist_Bmark)) {
        if (SS_versionNumeric < 3.30) {
          recruit_dist_endyr <- match_report_table("RECRUITMENT_DIST_FORECAST", 1,
            header = TRUE, type.convert = TRUE
          )
          recruit_dist_timeseries <- NULL
        } else {
          recruit_dist_endyr <- match_report_table("RECRUITMENT_DIST_endyr", 1,
            header = TRUE, type.convert = TRUE
          )
          # fix needed for 3.30.19 and 3.30.19.01 (fixed in future versions of SS3)
          if (length(grep("RECRUITMENT_DIST_TIMESERIES", recruit_dist_endyr[["Settle#"]])) == 1) {
            tmp_brk_line <- grep("RECRUITMENT_DIST_TIMESERIES", recruit_dist_endyr[["Settle#"]]) - 1
            recruit_dist_endyr <- recruit_dist_endyr[seq_len(tmp_brk_line), ]
          }
          recruit_dist_timeseries <- match_report_table(
            "RECRUITMENT_DIST_TIMESERIES", 1,
            header = FALSE, type.convert = FALSE
          )
        }
        # rename columns to format used starting with 3.30.23
        recruit_dist_Bmark <- df.rename(recruit_dist_Bmark,
          oldnames = c("Frac/sex", "Value"),
          newnames = c("recr_dist_F", "recr_dist_F")
        )
        recruit_dist_endyr <- df.rename(recruit_dist_endyr,
          oldnames = c("Frac/sex", "Value"),
          newnames = c("recr_dist_F", "recr_dist_F")
        )

        # bundle original and extra tables into a list
        recruitment_dist <- list(
          recruit_dist = recruitment_dist,
          recruit_dist_Bmark = recruit_dist_Bmark,
          recruit_dist_endyr = recruit_dist_endyr,
          recruit_dist_timeseries = recruit_dist_timeseries
        )
      }
    }

    # max gradient
    stats[["maximum_gradient_component"]] <-
      as.numeric(match_report_table("Convergence_Level", 0,
        "Convergence_Level", 0,
        cols = 2
      ))

    # parameters with highest gradients (3.30 only)
    if ("Gradient" %in% names(parameters)) {
      if (any(!is.na(parameters[["Gradient"]]))) {
        # number of gradients to report is 5 (an arbitrary choice),
        # or fewer if fewer than 5 parameters estimated.
        ngrads <- min(5, max(parameters[["Active_Cnt"]], na.rm = TRUE))
        # add highest gradients to table of stats that get printed to the console
        stats[["parameters_with_highest_gradients"]] <-
          head(parameters[
            order(abs(parameters[["Gradient"]]), decreasing = TRUE),
            c("Value", "Gradient")
          ], n = 5)
      }
    }

    # sigma_R
    # accounting for additional Bmsy/Bzero line introduced in 3.24U
    # should be now robust up through 3.24AZ (if that ever gets created)
    if (SS_versionNumeric >= 3.30 |
      substring(SS_version, 1, 9) %in% paste0("SS-V3.24", LETTERS[21:26]) |
      substring(SS_version, 1, 10) %in% paste0("SS-V3.24A", LETTERS)) {
      last_row_index <- 11
    } else {
      last_row_index <- 10
    }
    srhead <- match_report_table("SPAWN_RECRUIT", 0,
      "SPAWN_RECRUIT", last_row_index,
      cols = 1:6
    )
    # account for extra blank line in early 3.30 versions (at least 3.30.01)
    if (all(srhead[7, ] == "")) {
      last_row_index <- 12
      srhead <- match_report_table("SPAWN_RECRUIT", 0,
        "SPAWN_RECRUIT", last_row_index,
        cols = 1:6
      )
    }
    if (is.null(srhead)) {
      # if there's no SPAWN_RECRUIT section (presumably because minimal
      # output was chosen in the starter file)
      rmse_table <- NULL
      breakpoints_for_bias_adjustment_ramp <- NULL
      sigma_R_in <- parameters["SR_sigmaR", "Value"]
    } else {
      # if SPAWN_RECRUIT is present

      # get table of info on root mean squared error of recdevs (rmse)
      rmse_table <- as.data.frame(srhead[-(1:(last_row_index - 1)), 1:5])
      rmse_table <- rmse_table[!grepl("SpawnBio", rmse_table[, 2]), ]
      rmse_table <- type.convert(rmse_table, as.is = TRUE)
      names(rmse_table) <- srhead[last_row_index - 1, 1:5]
      names(rmse_table)[4] <- "RMSE_over_sigmaR"
      row.names(rmse_table) <- NULL

      # info on sigmaR as input or estimated
      sigma_R_in <- as.numeric(srhead[grep("sigmaR", srhead[, 2]), 1])

      # info on recdev method
      if (any(srhead[1, ] == "RecDev_method:")) {
        RecDev_method <- srhead[1, which(srhead[1, ] == "RecDev_method:") + 1] |> as.numeric()
      } else {
        RecDev_method <- NULL
      }

      # Bias adjustment ramp
      biascol <- grep("breakpoints_for_bias", srhead)
      breakpoints_for_bias_adjustment_ramp <- srhead[
        grep("breakpoints_for_bias", srhead[, biascol]), 1:5
      ]
      colnames(breakpoints_for_bias_adjustment_ramp) <- c(
        "last_yr_early",
        "first_yr_full", "last_yr_full", "first_yr_recent", "max_bias_adj"
      )
      rownames(breakpoints_for_bias_adjustment_ramp) <- NULL
    }

    ## Spawner-recruit curve
    # read SPAWN_RECRUIT table
    raw_recruit <- match_report_table("SPAWN_RECRUIT", last_row_index + 1)
    if (!is.null(raw_recruit) && raw_recruit[1, 1] == "S/Rcurve") {
      raw_recruit <- match_report_table("SPAWN_RECRUIT", last_row_index)
    }
    # account for extra blank line in 3.30.01 (and maybe similar versions)
    if (!is.null(raw_recruit) &&
      nrow(raw_recruit) < length(startyr:endyr)) {
      raw_recruit <- match_report_table("SPAWN_RECRUIT", last_row_index + 1,
        which_blank = 2
      )
      if (raw_recruit[1, 1] == "S/Rcurve") {
        raw_recruit <- match_report_table("SPAWN_RECRUIT", last_row_index,
          which_blank = 2
        )
      }
    }

    if (is.null(raw_recruit)) {
      recruit <- NULL
    } else {
      # process SPAWN_RECRUIT table
      names(raw_recruit) <- raw_recruit[1, ]
      raw_recruit[raw_recruit == "_"] <- NA
      raw_recruit <- raw_recruit[-(1:2), ] # remove header rows
      recruit <- raw_recruit[-(1:2), ] # remove rows for Virg and Init
      # temporary change for model that has bad values in dev column
      recruit[["dev"]][recruit[["dev"]] == "-nan(ind)"] <- NA

      # make values numeric
      recruit <- type.convert(recruit, as.is = TRUE)

      # make older SS output names match current SS output conventions
      recruit <- df.rename(recruit,
        oldnames = c("year", "spawn_bio", "adjusted", "biasadj"),
        newnames = c("Yr", "SpawnBio", "bias_adjusted", "biasadjuster")
      )
    }

    # starting in 3.30.11.00, a table with the full spawn recr curve was added
    SPAWN_RECR_CURVE <- NULL
    if (!is.na(match_report_line("Full_Spawn_Recr_Curve"))) {
      SPAWN_RECR_CURVE <- match_report_table("Full_Spawn_Recr_Curve", 1,
        header = TRUE, type.convert = TRUE
      )
    }
    # section was renamed in 3.30.15.06
    if (!is.na(match_report_line("SPAWN_RECR_CURVE"))) {
      SPAWN_RECR_CURVE <- match_report_table("SPAWN_RECR_CURVE", 1,
        header = TRUE, type.convert = TRUE
      )
    }

    ## FIT_LEN_COMPS
    if (SS_versionNumeric >= 3.30) {
      # This section existed but wasn't read prior to 3.30
      fit_len_comps <- match_report_table("FIT_LEN_COMPS", 1, header = TRUE)
    } else {
      fit_len_comps <- NULL
    }
    if (!is.null(dim(fit_len_comps)) && nrow(fit_len_comps) > 0) {
      # replace underscores with NA
      fit_len_comps[fit_len_comps == "_"] <- NA
      # make columns numeric (except "Used", which may contain "skip")
      fit_len_comps <- type.convert(fit_len_comps, as.is = TRUE)
    } else {
      fit_len_comps <- NULL
    }

    # Length_Comp_Fit_Summary
    if (SS_versionNumeric < 3.30) {
      # old way didn't have key word and had parentheses and other issues with column names
      lenntune <- match_report_table("FIT_AGE_COMPS", -(nfleets + 2),
        "FIT_AGE_COMPS", -1,
        cols = 1:10, header = TRUE
      )
      names(lenntune)[10] <- "FleetName"
      # convert underscores
      lenntune[lenntune == "_"] <- NA
      # reorder columns (leaving out sample sizes perhaps to save space)
      lenntune <- lenntune[lenntune[["N"]] > 0, c(10, 1, 4:9)]
      # avoid NA warnings by removing #IND values
      lenntune$"MeaneffN/MeaninputN"[lenntune$"MeaneffN/MeaninputN" == "-1.#IND"] <- NA
      lenntune <- type.convert(lenntune, as.is = TRUE)
      lenntune$"HarMean/MeanInputN" <- lenntune$"HarMean(effN)" / lenntune$"mean(inputN*Adj)"
    } else {
      # new in 3.30 has keyword at top
      lenntune <- match_report_table("Length_Comp_Fit_Summary", 1, header = TRUE)
      if (!is.null(lenntune)) {
        lenntune <- df.rename(lenntune,
          oldnames = c("FleetName", "Factor", "HarMean_effN"),
          newnames = c("Fleet_name", "Data_type", "HarMean")
        )
        if ("Data_type" %in% names(lenntune)) {
          # format starting with 3.30.12 doesn't need adjustment, just convert to numeric
          # ("Factor", introduced in 3.30.12, was renamed "Data_type" in 3.30.20)
          lenntune <- type.convert(lenntune, as.is = TRUE)
        } else {
          # process 3.30 versions prior to 3.30.12
          # reorder columns (leaving out sample sizes perhaps to save space)
          lenntune <- lenntune[lenntune[["Nsamp_adj"]] > 0, ]
          lenntune <- type.convert(lenntune, as.is = TRUE)
          ## new column "Recommend_Var_Adj" in 3.30 now matches calculation below
          # lenntune$"HarMean/MeanInputN" <- lenntune$"HarMean"/lenntune$"mean_inputN*Adj"
          lenntune$"HarMean(effN)/mean(inputN*Adj)" <-
            lenntune$"HarMean" / lenntune$"mean_inputN*Adj"

          # change name to make it clear what the harmonic mean is based on
          lenntune <- df.rename(lenntune,
            oldnames = c("HarMean", "mean_inputN*Adj"),
            newnames = c("HarMean(effN)", "mean(inputN*Adj)")
          )

          # drop distracting column
          lenntune <- lenntune[, names(lenntune) != "mean_effN"]

          # put recommendation and fleetnames at the end
          # (probably a more efficient way to do this)
          end.names <- c("Recommend_Var_Adj", "Fleet_name")
          lenntune <- lenntune[, c(
            which(!names(lenntune) %in% end.names),
            which(names(lenntune) %in% end.names)
          )]
        } # end pre-3.30.12 version of processing Length_Comp_Fit_Summary
      } # end 3.30 version of processing Length_Comp_Fit_Summary
    }
    stats[["Length_Comp_Fit_Summary"]] <- lenntune

    ## FIT_AGE_COMPS
    fit_age_comps <- match_report_table("FIT_AGE_COMPS", 1, header = TRUE)
    # process FIT_AGE_COMPS
    if (!is.null(dim(fit_age_comps)) && nrow(fit_age_comps) > 0) {
      # replace underscores with NA
      fit_age_comps[fit_age_comps == "_"] <- NA
      # make columns numeric (except "Used", which may contain "skip")
      fit_age_comps <- type.convert(fit_age_comps, as.is = TRUE)
    } else {
      fit_age_comps <- NULL
    }

    # Age_Comp_Fit_Summary
    if (SS_versionNumeric < 3.30) {
      # 3.24 and before had no keyword for tuning info below FIT_AGE_COMPS
      # so working backwards from the following section to get it
      agentune <- match_report_table("FIT_SIZE_COMPS", -(nfleets + 2),
        "FIT_SIZE_COMPS", -2,
        cols = 1:10, header = TRUE
      )
    } else {
      # 3.30 version has keyword (if included in output)
      # and requires little processing
      start <- match_report_line("Age_Comp_Fit_Summary")
      if (is.na(start)) {
        agentune <- NULL
      } else {
        if (rawrep[start + 1, 1] == "") {
          adjust1 <- 2
          which_blank <- 2
        } else {
          adjust1 <- 1
          which_blank <- 1
        }
        agentune <- match_report_table("Age_Comp_Fit_Summary",
          adjust1 = adjust1,
          header = TRUE, which_blank = which_blank
        )
      }
    } # end 3.30 version
    agentune <- df.rename(agentune,
      oldnames = c("FleetName", "N", "Factor", "HarMean_effN"),
      newnames = c("Fleet_name", "Nsamp_adj", "Data_type", "HarMean")
    )

    if ("Data_type" %in% names(agentune)) {
      # format starting with 3.30.12 doesn't need adjustment, just
      # convert to numeric
      # ("Factor", introduced in 3.30.12, was renamed "Data_type" in 3.30.20)
      agentune <- type.convert(agentune, as.is = TRUE)
    } else {
      if (!is.null(dim(agentune))) {
        names(agentune)[ncol(agentune)] <- "Fleet_name"
        # convert underscores
        agentune[agentune == "_"] <- NA
        # remove empty rows with NA or zero sample size
        agentune <- agentune[!is.na(agentune[["Nsamp_adj"]]) &
          agentune[["Nsamp_adj"]] > 0, ]
        # avoid NA warnings by removing #IND values
        agentune$"MeaneffN/MeaninputN"[agentune$"MeaneffN/MeaninputN" == "-1.#IND"] <- NA
        agentune <- type.convert(agentune, as.is = TRUE)
        # calculate ratio to be more transparent
        agentune$"HarMean(effN)/mean(inputN*Adj)" <-
          agentune$"HarMean(effN)" / agentune$"mean(inputN*Adj)"

        # calculate recommended value (for length data this is done internally in SS)
        agentune[["Recommend_Var_Adj"]] <-
          agentune[["Var_Adj"]] * agentune$"HarMean(effN)/mean(inputN*Adj)"

        # remove distracting columns (no longer present in recent versions of SS)
        badnames <- c("mean_effN", "Mean(effN/inputN)", "MeaneffN/MeaninputN")
        agentune <- agentune[, !names(agentune) %in% badnames]

        # put fleetnames column at the end (probably a more efficient way to do this)
        agentune <- agentune[, c(
          which(names(agentune) != "Fleet_name"),
          which(names(agentune) == "Fleet_name")
        )]

        # change name to make it clear what's reported and be constent with lengths
        agentune <- df.rename(agentune,
          oldnames = c("Var_Adj"),
          newnames = c("Curr_Var_Adj")
        )
      } else {
        agentune <- NULL
      }
    }
    stats[["Age_Comp_Fit_Summary"]] <- agentune

    ## FIT_SIZE_COMPS
    fit_size_comps <- NULL
    if (SS_versionNumeric >= 3.30) {
      # test for SS version 3.30.12 and beyond
      if (!is.na(match_report_line("FIT_SIZE_COMPS"))) {
        # note that there are hashes in between sub-sections,
        # so using rep_blank_lines instead of default
        # rep_blank_or_hash_lines to find ending
        fit_size_comps <- match_report_table("FIT_SIZE_COMPS", 1,
          header = FALSE,
          blank_lines = rep_blank_lines
        )
        if (!is.null(dim(fit_size_comps)) &&
          nrow(fit_size_comps) > 0 &&
          fit_size_comps[1, 1] != "#_none") {
          # column names
          names(fit_size_comps) <- fit_size_comps[2, ]
          # add new columns for method-specific info
          fit_size_comps[["Method"]] <- NA
          fit_size_comps[["Units"]] <- NA
          fit_size_comps[["Scale"]] <- NA
          fit_size_comps[["Add_to_comp"]] <- NA
          # find the lines with the method-specific info
          method_lines <- grep("#Method:", fit_size_comps[, 1])
          # method info is table to store info from only those lines
          method_info <- fit_size_comps[method_lines, ]

          # find the lines with the fit summary
          if (any(grepl("Size_Comp_Fit_Summary", fit_size_comps[, 1]))) {
            # new header line added in version 3.30.20
            tune_lines <- grep("Size_Comp_Fit_Summary", fit_size_comps[, 1]) + 1
          } else {
            tune_lines <- grep("Factor", fit_size_comps[, 1])
          }

          # place to store fit summary which is split across methods
          sizentune <- NULL
          # loop over methods to fill in new columns
          for (imethod in seq_along(method_lines)) {
            start <- method_lines[imethod]
            if (imethod != length(method_lines)) {
              end <- method_lines[imethod + 1] - 1
            } else {
              end <- nrow(fit_size_comps)
            }
            fit_size_comps[["Method"]][start:end] <- method_info[imethod, 2]
            fit_size_comps[["Units"]][start:end] <- method_info[imethod, 4]
            fit_size_comps[["Scale"]][start:end] <- method_info[imethod, 6]
            fit_size_comps[["Add_to_comp"]][start:end] <- method_info[imethod, 8]

            # split out rows with info on tuning
            sizentune <- rbind(sizentune, fit_size_comps[tune_lines[imethod]:end, ])
          }
          # format sizentune (info on tuning) has been split into
          # a separate data.frame, needs formatting: remove extra columns, change names
          goodcols <- c(
            # grab columns up through Fleet_name + added Method column
            1:grep("name", tolower(sizentune[1, ])),
            grep("Method", names(sizentune))
          )
          # fill in header for Method in first row
          sizentune[1, max(goodcols)] <- "Method"
          sizentune <- sizentune[, goodcols]
          # use first row for names
          names(sizentune) <- sizentune[1, ]
          # rename Factor to Data_type (changed in 3.30.20)
          sizentune <- df.rename(sizentune,
            oldnames = c("Factor", "HarMean_effN"),
            newnames = c("Data_type", "HarMean")
          )
          # subset for rows with single-character value for
          # Data_type (should always be 7 but seems to have been
          # 6 in some earlier models)
          # this should filter out extra header rows
          sizentune <- sizentune[nchar(sizentune[["Data_type"]]) == 1, ]
          # convert to numeric values as needed
          sizentune <- type.convert(sizentune, as.is = TRUE)
          stats[["Size_Comp_Fit_Summary"]] <- sizentune
          # remove extra summary rows of fit_size_comps
          fit_size_comps <- fit_size_comps |>
            dplyr::filter(Fleet_Name %in% FleetNames & Fleet %in% 1:nfleets)
        } # end check for non-empty fit_size_comps
      } else {
        # formatting used for earlier 3.30 versions (prior to 3.30.12)
        fit_size_comps <- match_report_table("FIT_SIZE_COMPS", 1,
          "Size_Comp_Fit_Summary", -(nfleets + 2),
          header = TRUE
        )
      }
    }
    # extra formatting for all versions
    if (!is.null(dim(fit_size_comps)) && nrow(fit_size_comps) > 0) {
      # replace underscores with NA
      fit_size_comps[fit_size_comps == "_"] <- NA
      # make columns numeric (except "Used", which may contain "skip")
      fit_size_comps <- type.convert(fit_size_comps, as.is = TRUE)
    }

    # Size comp effective N tuning check
    # (only available in version 3.30.01.12 and above)
    if (SS_versionNumeric >= 3.30) {
      if (!exists("sizentune")) {
        # if this table hasn't already been parsed from fit_size_comps above
        sizentune <- match_report_table("Size_Comp_Fit_Summary", 1, "OVERALL_COMPS", -1,
          cols = 1:10, header = TRUE
        )
        if (!is.null(dim(sizentune))) {
          sizentune[, 1] <- sizentune[, 10]
          sizentune <- sizentune[sizentune[["Npos"]] > 0, c(1, 3, 4, 5, 6, 8, 9)]
        } else {
          sizentune <- NULL
        }
      }
      stats[["Size_comp_Eff_N_tuning_check"]] <- sizentune
    }

    # placeholders for tables read from data file
    # in versions prior to 3.30.21
    age_data_info <- NULL
    len_data_info <- NULL

    # if D-M parameters present
    if (nrow(DM_pars) > 0) {
      if (!is.null(Length_comp_error_controls) |
        !is.null(Age_comp_error_controls)) {
        # approach used from 3.30.21+ when all info was available in Report.sso
        # (info was added earlier but r4ss didn't switch right away)

        # loop over fleets within each comp database
        # to copy DM sample size over from one table to another
        # surely there are far better ways of doing this with merge
        # or dplyr function

        if (comp) { # only possible if CompReport.sso was read

          # map select columns from fit_len_comps to lendbase
          # (can expand to other columns like MV_T_parm in the future)
          if (nrow(lendbase) > 0) {
            fit_len_comps_select <- fit_len_comps |>
              dplyr::rename(Like_sum = Like) |> # like for vector not bin
              dplyr::select(Fleet, Time, Sexes, Part, Nsamp_DM)
            lendbase <- dplyr::left_join(lendbase, fit_len_comps_select)
          }
          # add info to age comp data
          if (nrow(agedbase) > 0) {
            fit_age_comps_select <- fit_age_comps |>
              dplyr::rename(Like_sum = Like) |> # like for vector not bin
              dplyr::select(Fleet, Time, Sexes, Part, Nsamp_DM)
            agedbase <- dplyr::left_join(agedbase, fit_age_comps_select)
          }
          # add info to conditional age-at-length comp data
          if (nrow(condbase) > 0) {
            fit_cond_age_select <- fit_age_comps |>
              dplyr::rename(Like_sum = Like) |> # like for vector not bin
              dplyr::select(Fleet, Time, Sexes, Part, Nsamp_DM)
            condbase <- dplyr::left_join(condbase, fit_cond_age_select)
          }
          # add info to generalized size comp data
          if (nrow(sizedbase) > 0) {
            fit_size_comps_select <- fit_size_comps |>
              dplyr::rename(Like_sum = Like) |> # like for vector not bin
              dplyr::rename(method = Method) |> # making it match what's in sizedbase
              dplyr::select(Fleet, Time, Sexes, Part, Nsamp_DM, method)
            sizedbase <- dplyr::left_join(sizedbase, fit_size_comps_select)
          }
        } # end test for whether CompReport.sso info is available
        # end approach used starting in 3.30.21
      } else {
        # approach prior to 3.30.21 when info was needed from
        # the data file

        # figure out which fleet uses which parameter,
        # currently (as of SS version 3.30.10.00), requires reading data file
        if (verbose) {
          message("Reading data.ss_new (or data_echo.ss_new) for info on Dirichlet-Multinomial parameters")
        }
        datname <- get_dat_new_name(dir)
        datfile <- SS_readdat(
          file = file.path(dir, datname),
          verbose = verbose,
        )
        # when new data file is empty, find input data file
        if (is.null(datfile)) {
          starter <- SS_readstarter(
            file = file.path(dir, "starter.ss"),
            verbose = verbose
          )
          datfile <- SS_readdat(
            file = file.path(dir, starter[["datfile"]]),
            verbose = verbose, version = "3.30"
          )
        }

        age_data_info <- datfile[["age_info"]]
        len_data_info <- datfile[["len_info"]]
        if (!is.null(age_data_info) & !is.null(len_data_info)) {
          age_data_info[["CompError"]] <- as.numeric(age_data_info[["CompError"]])
          age_data_info[["ParmSelect"]] <- as.numeric(age_data_info[["ParmSelect"]])
          len_data_info[["CompError"]] <- as.numeric(len_data_info[["CompError"]])
          len_data_info[["ParmSelect"]] <- as.numeric(len_data_info[["ParmSelect"]])
          if (!any(age_data_info[["CompError"]] > 0) & !any(len_data_info[["CompError"]] > 0)) {
            stop(
              "Problem with Dirichlet-Multinomial parameters: \n",
              "  Report file indicates parameters exist, but no CompError values\n",
              "  in data.ss_new are > 0."
            )
          }
        } # end check for no Length_ or Age_comp_error_controls tables

        # get Dirichlet-Multinomial parameter values and adjust input N
        # the old way before that info was available in fit_len_comps
        # and fit_age_comps
        get_DM_sample_size <- function(CompError,
                                       f,
                                       sub,
                                       data_info,
                                       dbase) {
          ipar <- data_info[["ParmSelect"]][f]
          if (ipar %in% 1:nrow(DM_pars)) {
            if (CompError == 1) {
              Theta <- DM_pars[["Theta"]][ipar]
            }
            if (CompError == 2) {
              beta <- DM_pars[["Theta"]][ipar]
            }
          } else {
            stop(
              "Issue with Dirichlet-Multinomial parameter:",
              "Fleet = ", f, "and ParmSelect = ", ipar
            )
          }
          if (CompError == 1) {
            Nsamp_DM <-
              1 / (1 + Theta) +
              dbase[["Nsamp_adj"]][sub] * Theta / (1 + Theta)
          }
          if (CompError == 2) {
            Nsamp_DM <-
              dbase[["Nsamp_adj"]][sub] * (1 + beta) /
                (dbase[["Nsamp_adj"]][sub] + beta)
          }
          Nsamp_DM
        } # end get_DM_sample_size()

        if (comp) { # only possible if CompReport.sso was read
          if (nrow(agedbase) > 0) {
            agedbase[["Nsamp_DM"]] <- NA
          }
          if (nrow(lendbase) > 0) {
            lendbase[["Nsamp_DM"]] <- NA
          }
          if (nrow(condbase) > 0) {
            condbase[["Nsamp_DM"]] <- NA
          }

          # loop over fleets within agedbase
          for (f in unique(agedbase[["Fleet"]])) {
            # D-M likelihood for age comps
            if (age_data_info[["CompError"]][f] > 0) {
              sub <- agedbase[["Fleet"]] == f
              agedbase[["Nsamp_DM"]][sub] <-
                get_DM_sample_size(
                  CompError = age_data_info[["CompError"]][f],
                  f = f,
                  sub = sub,
                  data_info = age_data_info,
                  dbase = agedbase
                )
            } # end test for D-M likelihood in age comp
          } # end loop over fleets within agedbase

          # loop over fleets within lendbase
          for (f in unique(lendbase[["Fleet"]])) {
            # D-M likelihood for len comps
            if (len_data_info[["CompError"]][f] > 0) {
              sub <- lendbase[["Fleet"]] == f
              lendbase[["Nsamp_DM"]][sub] <-
                get_DM_sample_size(
                  CompError = len_data_info[["CompError"]][f],
                  f = f,
                  sub = sub,
                  data_info = len_data_info,
                  dbase = lendbase
                )
            } # end test for D-M likelihood in len comp
          } # end loop over fleets within lendbase

          # loop over fleets within condbase
          for (f in unique(condbase[["Fleet"]])) {
            # D-M likelihood for age comps
            if (age_data_info[["CompError"]][f] > 0) {
              sub <- condbase[["Fleet"]] == f
              condbase[["Nsamp_DM"]][sub] <-
                get_DM_sample_size(
                  CompError = age_data_info[["CompError"]][f],
                  f = f,
                  sub = sub,
                  data_info = age_data_info,
                  dbase = condbase
                )
            } # end test for D-M likelihood in age comp
          } # end loop over fleets within condbase
        } # end test for whether CompReport.sso info is available
      } # end processing DM pars & samples prior to 3.30.21
    } # end if DM pars are present

    # get information that will help diagnose jitter coverage and bad bounds
    jitter_info <- parameters[
      !is.na(parameters[["Active_Cnt"]]) &
        !is.na(parameters[["Min"]]),
      c("Value", "Min", "Max", "Init")
    ]
    jitter_info[["sigma"]] <- (jitter_info[["Max"]] - jitter_info[["Min"]]) / (2 * qnorm(.999))
    jitter_info[["CV"]] <- jitter_info[["sigma"]] / jitter_info[["Init"]]
    jitter_info[["InitLocation"]] <- pnorm(
      q = jitter_info[["Init"]],
      mean = (jitter_info[["Max"]] + jitter_info[["Min"]]) / 2,
      sd = jitter_info[["sigma"]]
    )


    if (verbose) {
      message("Finished primary run statistics list")
    }
    flush.console()

    # add stuff to list to return
    if (SS_versionNumeric <= 3.24) {
      returndat[["definitions"]] <- fleetdefs
      returndat[["fleet_ID"]] <- fleet_ID
      returndat[["fleet_area"]] <- fleet_area
      returndat[["catch_units"]] <- catch_units
      returndat[["catch_error"]] <- catch_error
    }
    if (SS_versionNumeric >= 3.30) {
      returndat[["definitions"]] <- fleetdefs
      returndat[["fleet_ID"]] <- fleet_ID
      returndat[["fleet_type"]] <- fleet_type
      returndat[["fleet_timing"]] <- fleet_timing
      returndat[["fleet_area"]] <- fleet_area
      returndat[["catch_units"]] <- catch_units
      if (exists("catch_se")) {
        returndat[["catch_se"]] <- catch_se
        returndat[["equ_catch_se"]] <- equ_catch_se
      } else {
        returndat[["catch_se"]] <- NA
        returndat[["equ_catch_se"]] <- NA
      }
    }

    # simple function to return additional things from the DEFINITIONS
    # section that were added with SS version 3.30.12
    return.def <- function(x) {
      if (exists(x)) {
        get(x)
      } else {
        NULL
      }
    }

    returndat[["mcmc"]] <- mcmc
    returndat[["survey_units"]] <- survey_units
    returndat[["survey_error"]] <- survey_error
    returndat[["IsFishFleet"]] <- IsFishFleet
    returndat[["nfishfleets"]] <- nfishfleets

    returndat[["nfleets"]] <- nfleets
    returndat[["nsexes"]] <- nsexes
    returndat[["ngpatterns"]] <- ngpatterns
    returndat[["lbins"]] <- lbins
    returndat[["Lbin_method"]] <- Lbin_method
    returndat[["nlbins"]] <- nlbins
    returndat[["lbinspop"]] <- lbinspop
    returndat[["nlbinspop"]] <- nlbinspop
    returndat[["sizebinlist"]] <- sizebinlist
    returndat[["age_data_info"]] <- age_data_info
    returndat[["len_data_info"]] <- len_data_info
    returndat[["agebins"]] <- agebins
    returndat[["nagebins"]] <- nagebins
    returndat[["accuage"]] <- accuage
    returndat[["nareas"]] <- nareas
    returndat[["startyr"]] <- startyr
    returndat[["endyr"]] <- endyr
    returndat[["nseasons"]] <- nseasons
    returndat[["seasfracs"]] <- seasfracs
    returndat[["seasdurations"]] <- seasdurations
    returndat[["N_sub_seasons"]] <- return.def("N_sub_seasons")
    returndat[["Spawn_month"]] <- return.def("Spawn_month")
    returndat[["Spawn_seas"]] <- return.def("Spawn_seas")
    returndat[["Spawn_timing_in_season"]] <- return.def("Spawn_timing_in_season")
    returndat[["Retro_year"]] <- return.def("Retro_year")
    returndat[["N_forecast_yrs"]] <- return.def("N_forecast_yrs")
    returndat[["Empirical_wt_at_age"]] <- return.def("Empirical_wt_at_age")
    returndat[["N_bio_patterns"]] <- return.def("N_bio_patterns")
    returndat[["N_platoons"]] <- return.def("N_platoons")
    returndat[["NatMort_option"]] <- return.def("NatMort_option")
    returndat[["GrowthModel_option"]] <- return.def("GrowthModel_option")
    returndat[["Maturity_option"]] <- return.def("Maturity_option")
    returndat[["Fecundity_option"]] <- return.def("Fecundity_option")
    returndat[["Start_from_par"]] <- return.def("Start_from_par")
    returndat[["Do_all_priors"]] <- return.def("Do_all_priors")
    returndat[["Use_softbound"]] <- return.def("Use_softbound")
    returndat[["N_nudata"]] <- return.def("N_nudata")
    returndat[["Max_phase"]] <- return.def("Max_phase")
    returndat[["Current_phase"]] <- return.def("Current_phase")
    returndat[["Jitter"]] <- return.def("Jitter")
    returndat[["ALK_tolerance"]] <- return.def("ALK_tolerance")
    returndat[["Length_comp_error_controls"]] <- Length_comp_error_controls
    returndat[["Age_comp_error_controls"]] <- Age_comp_error_controls
    returndat[["Size_comp_error_controls"]] <- Size_comp_error_controls
    returndat[["nforecastyears"]] <- nforecastyears
    returndat[["morph_indexing"]] <- morph_indexing
    returndat[["MGparmAdj"]] <- MGparmAdj
    returndat[["forecast_selectivity"]] <- forecast_selectivity
    returndat[["SelSizeAdj"]] <- SelSizeAdj
    returndat[["SelAgeAdj"]] <- SelAgeAdj
    returndat[["recruitment_dist"]] <- recruitment_dist
    returndat[["recruit"]] <- recruit
    returndat[["SPAWN_RECR_CURVE"]] <- SPAWN_RECR_CURVE
    returndat[["breakpoints_for_bias_adjustment_ramp"]] <-
      breakpoints_for_bias_adjustment_ramp

    # Static growth
    # note: keyword "BIOLOGY" was not unique enough at some point
    #       but revision on 11 June 2020 seems to be working so far
    # formatting change in 3.30.15.06 puts table one line lower
    biology <- match_report_table("BIOLOGY",
      adjust1 = ifelse(custom, 2, 1),
      header = TRUE, type.convert = TRUE
    )
    # updated BIOLOGY table names based on change July 2022 change
    # https://github.com/nmfs-ost/ss3-source-code/issues/348
    biology <- df.rename(biology,
      oldnames = c("Low", "Mean_Size", "Wt_len", "Wt_len_F", "Mat_len", "Spawn", "Wt_len_M", "Fecundity"),
      newnames = c("Len_lo", "Len_mean", "Wt_F", "Wt_F", "Mat", "Mat*Fec", "Wt_M", "Fec")
    )

    # determine fecundity type
    FecType <- 0
    # get parameter labels
    pl <- parameters[["Label"]]
    FecGrep1 <- grep("Eggs/kg_slope_wt_Fem", pl)
    FecGrep2 <- grep("Eggs_exp_len_Fem", pl)
    FecGrep3 <- grep("Eggs_exp_wt_Fem", pl)
    FecGrep4 <- grep("Eggs_slope_len_Fem", pl)
    FecGrep5 <- grep("Eggs_slope_Wt_Fem", pl)

    if (length(FecGrep1) > 0) {
      FecType <- 1
      FecPar1name <- grep("Eggs/kg_inter_Fem", pl, value = TRUE)[1]
      FecPar2name <- pl[FecGrep1[1]]
    }
    if (length(FecGrep2) > 0) {
      FecType <- 2
      FecPar1name <- grep("Eggs_scalar_Fem", pl, value = TRUE)[1]
      FecPar2name <- pl[FecGrep2[1]]
    }
    if (length(FecGrep3) > 0) {
      FecType <- 3
      FecPar1name <- grep("Eggs_scalar_Fem", pl, value = TRUE)[1]
      FecPar2name <- pl[FecGrep3[1]]
    }
    if (length(FecGrep4) > 0) {
      FecType <- 4
      FecPar1name <- grep("Eggs_intercept_Fem", pl, value = TRUE)[1]
      FecPar2name <- pl[FecGrep4[1]]
    }
    if (length(FecGrep5) > 0) {
      FecType <- 5
      FecPar1name <- grep("Eggs_intercept_Fem", pl, value = TRUE)[1]
      FecPar2name <- pl[FecGrep5[1]]
    }
    if (is.na(lbinspop[1])) {
      lbinspop <- biology[["Len_lo"]][biology[["GP"]] == 1]
    }

    # warning for 3.30 models with multiple growth patterns that have
    # repeat fecundity values, likely to be sorted out in new SS version
    if (length(returndat[["FecPar1"]]) > 1) {
      warning(
        "Plots will only show fecundity and related quantities",
        "for Growth Pattern 1"
      )
      returndat[["FecPar1"]] <- returndat[["FecPar1"]][1]
      returndat[["FecPar2"]] <- returndat[["FecPar2"]][2]
    }

    # cleanup and tests related to biology at length table
    if (!is.null(biology)) {
      # fix for extra header associated with extra column header
      # for single sex models that got fixed in 3.30.16
      if (nsexes == 1 &&
        is.na(biology[["Fec"]][1]) &&
        "Wt_M" %in% names(biology)) {
        # copy Wt_len_M to Fecundity
        biology[["Fec"]] <- biology[["Wt_M"]]
        # remove Wt_len_M
        biology <- biology[, !names(biology) %in% "Wt_M"]
      }

      # test to figure out if fecundity is proportional to spawning biomass
      # check for any mismatch between weight-at-length and fecundity
      returndat[["SpawnOutputUnits"]] <-
        ifelse(!is.null(biology[["Fec"]][1]) &&
          !is.na(biology[["Fec"]][1]) &&
          any(biology[["Wt_F"]] != biology[["Fec"]]),
        "numbers", "biomass"
        )
    }

    # add biology and fecundity varibles to list getting returned
    returndat[["biology"]] <- biology
    returndat[["FecType"]] <- FecType
    returndat[["FecPar1name"]] <- FecPar1name
    returndat[["FecPar2name"]] <- FecPar2name

    returndat[["FecPar1"]] <- parameters[["Value"]][parameters[["Label"]] == FecPar1name]
    returndat[["FecPar2"]] <- parameters[["Value"]][parameters[["Label"]] == FecPar2name]

    # get natural mortality type and vectors of M by age
    adjust1 <- ifelse(custom, 2, 1)
    M_type <- rawrep[match_report_line("Natural_Mortality") + adjust1 - 1, 2]
    M_type <- as.numeric(gsub(
      pattern = ".*([0-9]+)",
      replacement = "\\1",
      x = M_type
    ))

    # in SS 3.30 the number of rows of Natural_Mortality is the product of
    # the number of sexes, growth patterns, settlement events but settlement
    # events didn't exist in 3.24

    # this first table includes all time periods as of 3.30.20
    Natural_Mortality <- match_report_table("Natural_Mortality",
      adjust1 = adjust1,
      header = TRUE,
      type.convert = TRUE
    )
    # the Bmark and endyr tables have been subsumed into the table above
    # in 3.30.20
    Natural_Mortality_Bmark <- match_report_table("Natural_Mortality_Bmark",
      adjust1 = 1,
      header = TRUE,
      type.convert = TRUE
    )
    Natural_Mortality_endyr <- match_report_table("Natural_Mortality_endyr",
      adjust1 = 1,
      header = TRUE,
      type.convert = TRUE
    )
    returndat[["M_type"]] <- M_type
    returndat[["Natural_Mortality"]] <- Natural_Mortality
    returndat[["Natural_Mortality_Bmark"]] <- Natural_Mortality_Bmark
    returndat[["Natural_Mortality_endyr"]] <- Natural_Mortality_endyr

    # get growth parameters
    Growth_Parameters <- match_report_table("Growth_Parameters", 1,
      "Growth_Parameters", 1 + ngpatterns * nsexes,
      header = TRUE, type.convert = TRUE
    )
    returndat[["Growth_Parameters"]] <- Growth_Parameters

    Seas_Effects <- match_report_table("Seas_Effects", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat[["Seas_Effects"]] <- Seas_Effects

    # ending year growth, including pattern for the CV (added in SSv3.22b_Aug3)
    # CVtype will occur on same line or following
    growthCVtype <- match_report_table("Biology_at_age", 0,
      "Biology_at_age", 1,
      header = FALSE
    )
    growthCVtype <- grep("endyr_with_", unlist(growthCVtype), value = TRUE)
    if (length(growthCVtype) > 0) {
      returndat[["growthCVtype"]] <- strsplit(growthCVtype,
        split = "endyr_with_"
      )[[1]][2]
    } else {
      returndat[["growthCVtype"]] <- "unknown"
    }
    # formatting change in 3.30.15.06 puts table one line lower
    growdat <- match_report_table("Biology_at_age",
      adjust1 = ifelse(custom, 2, 1),
      header = TRUE, type.convert = TRUE
    )
    if (!is.null(growdat)) {
      # make older SS output names match current SS output conventions
      growdat <- df.rename(growdat,
        oldnames = c("Gender"),
        newnames = c("Sex")
      )
      # extract a few quantities related to growth morphs/platoons
      # note 16-June-2020: these values don't seem to be used anywhere
      nmorphs <- max(growdat[["Morph"]])
      midmorphs <- c(c(0, nmorphs / nsexes) + ceiling(nmorphs / nsexes / 2))
    }
    returndat[["endgrowth"]] <- growdat
    # test for use of empirical weight-at-age input file (wtatage.ss)
    # should match only "MEAN_BODY_WT(Begin)" or "MEAN_BODY_WT(begin)"
    test <- match_report_table("MEAN_BODY_WT(", 0,
      "MEAN_BODY_WT(", 1,
      header = FALSE
    )
    wtatage_switch <- length(grep("wtatage.ss", test)) > 0
    returndat[["wtatage_switch"]] <- wtatage_switch

    # mean body weight
    mean_body_wt <- match_report_table("MEAN_BODY_WT(begin)", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat[["mean_body_wt"]] <- mean_body_wt

    # get time series of mean length at age
    mean_size <- match_report_table("MEAN_SIZE_TIMESERIES", 1,
      "mean_size_Jan_1", -2,
      cols = 1:(4 + accuage + 1),
      header = TRUE,
      type.convert = TRUE
    )
    # filter values for range of years in time series
    # (may not be needed in more recent SS versions)
    growthvaries <- FALSE
    if (!is.null(mean_size)) {
      if (SS_versionNumeric < 3.30) {
        mean_size <- mean_size[mean_size[["Beg"]] == 1 &
          mean_size[["Yr"]] >= startyr &
          mean_size[["Yr"]] < endyr, ]
      } else {
        mean_size <- mean_size[mean_size[["SubSeas"]] == 1 &
          mean_size[["Yr"]] >= startyr &
          mean_size[["Yr"]] < endyr, ]
      }
      if (nseasons > 1) {
        mean_size <- mean_size[mean_size[["Seas"]] == 1, ]
      }
      # loop over morphs to check for time-varying growth
      # (typically only 1 or 1:2 for females and males)
      for (morph in unique(mean_size[["Morph"]])) {
        # check is based on ages 0 up to accuage-1, because the mean
        # length in the plus group can vary over time as a function of changes
        # in the numbers at age (where fishing down the old fish causes
        # fewer additional ages lumped into that group)
        if (sum(!duplicated(mean_size[
          mean_size[["Morph"]] == morph,
          paste(0:(accuage - 1))
        ])) > 1) {
          growthvaries <- TRUE
        }
      }
      returndat[["growthseries"]] <- mean_size
      returndat[["growthvaries"]] <- growthvaries
    }

    # Length-based selectivity and retention
    if (!forecast) {
      sizeselex <- sizeselex[sizeselex[["Yr"]] <= endyr, ]
    }
    returndat[["sizeselex"]] <- sizeselex

    # Age-based selectivity
    # Updated for 3.30.17 which added an additional row in the AGE_SELEX header
    ageselex <- match_report_table("COMBINED_ALK*selL*selA", 1, header = TRUE)
    # new section added in 3.30.22
    maximum_ASEL2 <- match_report_table(
      "maximum_ASEL2",
      adjust1 = 1,
      header = TRUE,
      type.convert = TRUE
    )

    if (!is.null(ageselex)) {
      # account for additional header row added in March 2021
      # SS commit: 31ae478d1bae53235e14912d8c5c452a62c71adb
      # (not the most efficient way to do this)
      if (any(grepl("COMBINED_ALK", names(ageselex)))) {
        ageselex <- match_report_table("AGE_SELEX", 5, header = TRUE)
      }
      ageselex <- df.rename(ageselex,
        oldnames = c(
          "fleet", "year", "seas", "gender",
          "morph", "label", "factor"
        ),
        newnames = c(
          "Fleet", "Yr", "Seas", "Sex",
          "Morph", "Label", "Factor"
        )
      )
      # filter forecast years from selectivity if no forecast
      # NOTE: maybe refine this in 3.30
      if (!forecast) {
        ageselex <- ageselex[ageselex[["Yr"]] <= endyr, ]
      }
      # make values numeric
      ageselex <- type.convert(ageselex, as.is = TRUE)
    }
    returndat[["ageselex"]] <- ageselex
    returndat[["maximum_ASEL2"]] <- maximum_ASEL2

    # EXPLOITATION
    # read first 20 rows to figure out where meta-data ends
    exploitation_head <- match_report_table("EXPLOITATION", 1,
      "EXPLOITATION", 20,
      header = FALSE
    )
    # check for new header info added in 3.30.13_beta (Feb 2019)
    # "Info:" changed to "NOTE:" with 3.30.23 (Jul 2024)
    if (exploitation_head[1, 1] %in% c("Info:", "NOTE:")) {
      # NOTE: add read of additional header info here
      exploitation <- match_report_table("EXPLOITATION",
        which(exploitation_head[, 1] == "Yr"),
        header = TRUE,
        # using rep_blank_lines instead of default
        # rep_blank_or_hash_lines to find ending because of hash
        blank_lines = rep_blank_lines
      )
      # remove meta-data about fleets (filtered by color in 1st column):
      # "Catchunits:","FleetType:","FleetArea:","FleetID:"
      exploitation <- exploitation[-grep(":", exploitation[, 1]), ]
      # find line with F_method like this "Info: F_Method:=3;.Continuous_F;..."
      # F_method info contains additional information that might be useful elsewhere
      F_method_info <- exploitation_head[grep(
        "F_Method:",
        exploitation_head[, 2]
      ), 2]
      F_method_info <- gsub(
        pattern = ".",
        replacement = " ",
        x = F_method_info,
        fixed = TRUE
      )
      F_method_info <- strsplit(F_method_info,
        split = ";",
        fixed = TRUE
      )[[1]]
      # get numeric value for F_method
      F_method <- as.numeric(strsplit(F_method_info[[1]],
        split = "=",
        fixed = TRUE
      )[[1]][2])
    } else {
      # old format prior to 3.30.13
      exploitation <- match_report_table("EXPLOITATION", 5, header = TRUE)
      # get numeric value for F_method
      F_method <- as.numeric(rawrep[match_report_line("F_Method"), 2])
    }
    returndat[["F_method"]] <- F_method

    if (!is.null(exploitation)) {
      # more processing of exploitation (if present)
      exploitation[exploitation == "_"] <- NA
      # make text numeric
      # "init_yr" not used as of 3.30.13, but must have been in the past
      # "INIT" appears to be used in 3.30.13 and beyond
      exploitation[["Yr"]][exploitation[["Yr"]] %in% c("INIT", "init_yr")] <- startyr - 1
      # make columns numeric
      exploitation <- type.convert(exploitation, as.is = TRUE)
    }
    returndat[["exploitation"]] <- exploitation

    # catch
    catch <- match_report_table("CATCH",
      # comment line added in 3.30.22 needs to be skipped
      adjust1 = ifelse(rawrep[match_report_line("CATCH") + 1, 1] == "#", 2, 1),
      substr1 = FALSE,
      header = TRUE
    )
    # if table is present, then do processing of it
    if (!is.null(catch)) {
      # update to new column names used starting with 3.30.13
      catch <- df.rename(catch,
        oldnames = c("Name", "Yr.frac"),
        newnames = c("Fleet_Name", "Time")
      )
      # fix likelihood associated with 0 catch
      catch[["Like"]][catch[["Like"]] == "-1.#IND"] <- NA
      # change "INIT" or "init" to year value following convention used elsewhere
      catch[["Yr"]][tolower(catch[["Yr"]]) == "init"] <- startyr - 1
      # make columns numeric
      catch <- type.convert(catch, as.is = TRUE)
    }
    returndat[["catch"]] <- catch

    # age associated with summary biomass
    summary_age <- rawrep[match_report_line("TIME_SERIES"), ifelse(custom, 3, 2)]
    summary_age <- as.numeric(substring(summary_age, nchar("BioSmry_age:_") + 1))
    returndat[["summary_age"]] <- summary_age
    # time series
    timeseries <- match_report_table("TIME_SERIES", 1, header = TRUE)
    # temporary fix for 3.30.03.06
    timeseries <- timeseries[timeseries[["Seas"]] != "recruits", ]

    timeseries[timeseries == "_"] <- NA
    timeseries <- type.convert(timeseries, as.is = TRUE)
    ## # sum catches and other quantities across fleets
    ## # commented out pending additional test for more than one fleet with catch,
    ## # without which the apply function has errors
    ## timeseries[["dead_B_sum"]] <- apply(timeseries[,grep("dead(B)",names(timeseries),
    ##                                                 fixed=TRUE)], 1, sum)
    ## timeseries[["dead_N_sum"]] <- apply(timeseries[,grep("dead(N)",names(timeseries),
    ##                                                 fixed=TRUE)], 1, sum)
    ## timeseries[["retain_B_sum"]] <- apply(timeseries[,grep("retain(B)",names(timeseries),
    ##                                                   fixed=TRUE)], 1, sum)
    ## timeseries[["retain_N_sum"]] <- apply(timeseries[,grep("retain(N)",names(timeseries),
    ##                                                   fixed=TRUE)], 1, sum)
    ## timeseries[["sel_B_sum"]] <- apply(timeseries[,grep("sel(B)",names(timeseries),
    ##                                                fixed=TRUE)], 1, sum)
    ## timeseries[["sel_N_sum"]] <- apply(timeseries[,grep("sel(N)",names(timeseries),
    ##                                                fixed=TRUE)], 1, sum)
    ## timeseries[["obs_cat_sum"]] <- apply(timeseries[,grep("obs_cat",names(timeseries),
    ##                                                  fixed=TRUE)], 1, sum)

    returndat[["timeseries"]] <- timeseries

    # get spawning season
    # currently (v3.20b), Spawning Biomass is only calculated
    # in a unique spawning season within the year
    if (!exists("spawnseas")) {
      spawnseas <- unique(timeseries[["Seas"]][!is.na(timeseries[["SpawnBio"]])])

      # problem with spawning season calculation when NA values in SpawnBio
      if (length(spawnseas) == 0) {
        spawnseas <- NA
      }
    }
    returndat[["spawnseas"]] <- spawnseas

    # set mainmorphs as those morphs born in the first season with recruitment
    # and the largest fraction of the platoons (should equal middle platoon when present)
    if (is.null(morph_indexing)) {
      mainmorphs <- NULL
    } else {
      if (SS_versionNumeric >= 3.30) {
        # new "platoon" label
        temp <- morph_indexing[morph_indexing[["BirthSeas"]] ==
          first_seas_with_recruits &
          morph_indexing[["Platoon_Dist"]] ==
            max(morph_indexing[["Platoon_Dist"]]), ]
        mainmorphs <- min(temp[["Index"]][temp[["Sex"]] == 1])
        if (nsexes == 2) {
          mainmorphs <- c(mainmorphs, min(temp[["Index"]][temp[["Sex"]] == 2]))
        }
      }
      if (SS_versionNumeric < 3.30) {
        # old "sub_morph" label
        temp <- morph_indexing[morph_indexing[["BirthSeas"]] ==
          first_seas_with_recruits &
          morph_indexing[["Sub_Morph_Dist"]] ==
            max(morph_indexing[["Sub_Morph_Dist"]]), ]
        mainmorphs <- min(temp[["Index"]][temp[["Sex"]] == 1])
        if (nsexes == 2) {
          mainmorphs <- c(mainmorphs, min(temp[["Index"]][temp[["Sex"]] == 2]))
        }
      }
      if (length(mainmorphs) == 0) {
        warning("Error with morph indexing")
      }
    }
    returndat[["mainmorphs"]] <- mainmorphs

    # get birth seasons as vector of seasons with non-zero recruitment
    birthseas <- sort(unique(timeseries[["Seas"]][timeseries[["Recruit_0"]] > 0]))
    # temporary fix for model with missing Recruit_0 values
    # (so far this has only been seen in one 3.30 model with 2 GPs)
    if (length(birthseas) == 0) {
      birthseas <- sort(unique(morph_indexing[["BirthSeas"]]))
    }
    returndat[["birthseas"]] <- birthseas

    # stats and dimensions
    timeseries[["Yr"]] <- timeseries[["Yr"]] + (timeseries[["Seas"]] - 1) / nseasons
    ts <- timeseries[timeseries[["Yr"]] <= endyr + 1, ]
    tsyears <- ts[["Yr"]][ts[["Seas"]] == 1]

    # Depletion
    tsspaw_bio <- ts[["SpawnBio"]][ts[["Seas"]] == spawnseas & ts[["Area"]] == 1]
    if (nareas > 1) # loop over areas if necessary to sum spawning biomass
      {
        for (a in 2:nareas) {
          tsspaw_bio <- tsspaw_bio + ts[["SpawnBio"]][ts[["Seas"]] == spawnseas &
            ts[["Area"]] == a]
        }
      }
    if (nsexes == 1) {
      tsspaw_bio <- tsspaw_bio / 2
    }
    depletionseries <- tsspaw_bio / tsspaw_bio[1]
    stats[["SBzero"]] <- tsspaw_bio[1]
    stats[["current_depletion"]] <- depletionseries[length(depletionseries)]

    # total landings
    ls <- nrow(ts) - 1
    totretainedmat <- as.matrix(ts[, substr(
      names(ts), 1,
      nchar("retain(B)")
    ) == "retain(B)"])
    ts[["totretained"]] <- 0
    ts[["totretained"]][3:ls] <- rowSums(totretainedmat)[3:ls]

    # total catch
    totcatchmat <- as.matrix(ts[, substr(
      names(ts), 1,
      nchar("enc(B)")
    ) == "enc(B)"])
    ts[["totcatch"]] <- 0
    ts[["totcatch"]][3:ls] <- rowSums(totcatchmat)[3:ls]

    # harvest rates
    if (F_method == 1) {
      stringmatch <- "Hrate:_"
    } else {
      stringmatch <- "F:_"
    }
    Hrates <- as.matrix(ts[, substr(
      names(ts), 1,
      nchar(stringmatch)
    ) == stringmatch])
    fmax <- max(Hrates)

    # depletion basis
    depletion_basis <- as.numeric(rawrep[match_report_line("Depletion_basis"), 2])
    if (is.na(depletion_basis)) {
      # older versions had a different string
      depletion_basis <- as.numeric(rawrep[match_report_line("Depletion_method"), 2])
    }


    if (depletion_basis %in% c(1, 3:4)) {
      if (file.exists(file.path(dir, "starter.ss"))) {
        starter <- SS_readstarter(
          file = file.path(dir, "starter.ss"),
          verbose = verbose
        )
        depletion_multiplier <- starter[["depl_denom_frac"]]
      } else {
        depletion_multiplier <- NULL
      }
    } else {
      depletion_multiplier <- 1
    }

    Bratio_denominator <- rawrep[match_report_line("B_ratio_denominator"), 2]
    if (Bratio_denominator == "no_depletion_basis") {
      Bratio_label <- "no_depletion_basis"
    } else {
      # get depletion_multiplier if no starter file was available
      # will be rounded to nearest % so potentially less accurate than
      # value in the starter file
      if (is.null(depletion_multiplier)) {
        depletion_multiplier <-
          as.numeric(strsplit(Bratio_denominator, "%")[[1]][1]) / 100
      }
      # create Bratio label for use in various plots
      if (grepl(pattern = "100", x = Bratio_denominator)) {
        # exclude 100% if present
        Bratio_label <- paste0(
          "B/",
          substring(Bratio_denominator, 6)
        )
      } else {
        Bratio_label <- paste0(
          "B/(",
          Bratio_denominator,
          ")"
        )
      }
      if (Bratio_label == "B/Virgin_Biomass") {
        Bratio_label <- "B/B_0"
      }
    }
    returndat[["depletion_basis"]] <- depletion_basis
    returndat[["depletion_multiplier"]] <- depletion_multiplier
    returndat[["Bratio_denominator"]] <- Bratio_denominator
    returndat[["Bratio_label"]] <- Bratio_label

    ## discard fractions ###

    # degrees of freedom for T-distribution
    # (or indicator 0, -1, -2 for other distributions)
    if (SS_versionNumeric < 3.20) {
      # old header from 3.11
      DF_discard <- rawrep[match_report_line("DISCARD_OUTPUT"), 3]
      if (length(grep("T_distribution", DF_discard)) > 0) {
        DF_discard <- as.numeric(strsplit(DF_discard, "=_")[[1]][2])
      }
      if (length(grep("_normal_with_Std_in_as_CV", DF_discard)) > 0) {
        DF_discard <- 0
      }
      if (length(grep("_normal_with_Std_in_as_stddev", DF_discard)) > 0) {
        DF_discard <- -1
      }
      if (length(grep("_lognormal", DF_discard)) > 0) {
        DF_discard <- -2
      }
      shift <- 2
      discard_spec <- NULL
    } else { # newer header in 3.20 and beyond
      DF_discard <- NA
      shift <- 1
      # read first 20 lines
      discard_header <- match_report_table(
        "DISCARD_SPECIFICATION", 1,
        "DISCARD_SPECIFICATION", 20
      )
      if (!is.null(discard_header)) {
        # read table of discard info by fleet at bottom of header
        discard_spec <- match_report_table("DISCARD_SPECIFICATION",
          which(discard_header[, 3] == "errtype"),
          header = TRUE, type.convert = TRUE
        )
        discard_spec <- type.convert(discard_spec, as.is = TRUE)
        # not sure under what circumstances this first name wasn't "Fleet" already
        names(discard_spec)[1] <- "Fleet"
      } else {
        discard_spec <- NULL
      }
    }
    # read DISCARD_OUTPUT table
    discard <- match_report_table("DISCARD_OUTPUT", shift, header = TRUE)
    # rerun read of discard with header = FALSE
    # if in SSv3.20b which had missing line break
    if (!is.null(discard) && names(discard)[1] != "Fleet") {
      discard <- match_report_table("DISCARD_OUTPUT", shift, header = FALSE)
      # note that these column names are from 3.20b and have changed since that time
      names(discard) <- c(
        "Fleet", "Yr", "Seas", "Obs", "Exp",
        "Std_in", "Std_use", "Dev"
      )
    }

    # rename columns to standard used with 3.30.13 (starting Feb 14, 2019)
    discard <- df.rename(discard,
      oldnames = c("Name", "Yr.frac"),
      newnames = c("Fleet_Name", "Time")
    )

    # process discard info if table was present
    if (!is.null(discard) && nrow(discard) > 1) {
      discard[discard == "_"] <- NA
      # v3.23 and before had things combined under "Name"
      # which has been renamed above to "Fleet_Name"
      if (SS_versionNumeric <= 3.23) {
        discard <- type.convert(discard, as.is = TRUE)
        if (!"Fleet_Name" %in% names(discard)) {
          discard[["Fleet_Name"]] <- discard[["Fleet"]]
        }
        discard[["Fleet"]] <- NA
        for (i in 1:nrow(discard)) {
          discard[["Fleet"]][i] <- strsplit(discard[["Fleet_Name"]][i], "_")[[1]][1]
          discard[["Fleet_Name"]][i] <- substring(
            discard[["Fleet_Name"]][i],
            nchar(discard[["Fleet"]][i]) + 2
          )
        }
        discard_tuning_info <- NULL # not bothering to support this for 3.23 and before
      } else {
        # v3.24 and beyond has separate columns
        # for fleet number and fleet name
        discard <- type.convert(discard, as.is = TRUE)
        # get info on variance adjustments for discards
        discard_tuning_info <- calc_var_adjust(discard, type = "sd")
      }
    } else {
      discard <- NA # IGT 23-04-2023: not sure why this is NA instead of NULL
      discard_tuning_info <- NULL
    }
    returndat[["discard"]] <- discard
    returndat[["discard_spec"]] <- discard_spec
    returndat[["discard_tuning_info"]] <- discard_tuning_info
    returndat[["DF_discard"]] <- DF_discard

    ## Average body weight observations
    # degrees of freedom for T-distribution
    DF_mnwgt <- rawrep[match_report_line("log(L)_based_on_T_distribution"), 1]
    if (!is.na(DF_mnwgt)) {
      DF_mnwgt <- as.numeric(strsplit(DF_mnwgt, "=_")[[1]][2])
      mnwgt <- match_report_table("MEAN_BODY_WT_OUTPUT", 2, header = TRUE)
      mnwgt <- df.rename(mnwgt,
        oldnames = c("Name"),
        newnames = c("Fleet_Name")
      )
      mnwgt[mnwgt == "_"] <- NA
      # v3.23 and before had things combined under "Name"
      # which has been renamed above to "Fleet_Name"
      if (SS_versionNumeric <= 3.23) {
        mnwgt <- type.convert(mnwgt, as.is = TRUE)
        if (!"Fleet_Name" %in% names(mnwgt)) {
          mnwgt[["Fleet_Name"]] <- mnwgt[["Fleet"]]
        }
        mnwgt[["Fleet"]] <- NA
        for (i in 1:nrow(mnwgt)) {
          mnwgt[["Fleet"]][i] <- strsplit(mnwgt[["Fleet_Name"]][i], "_")[[1]][1]
          mnwgt[["Fleet_Name"]][i] <- substring(
            mnwgt[["Fleet_Name"]][i],
            nchar(mnwgt[["Fleet_Name"]][i]) + 2
          )
        }
        mnwgt_tuning_info <- NULL
      } else { # v3.24 and beyond has separate columns for fleet number and fleet name
        mnwgt <- type.convert(mnwgt, as.is = TRUE)
        # get info on variance adjustments for mean body weight
        mnwgt_tuning_info <- calc_var_adjust(mnwgt, type = "CV")
      }
    } else {
      DF_mnwgt <- NA
      mnwgt <- NA
      mnwgt_tuning_info <- NULL
    }
    returndat[["mnwgt"]] <- mnwgt
    returndat[["mnwgt_tuning_info"]] <- mnwgt_tuning_info
    returndat[["DF_mnwgt"]] <- DF_mnwgt

    # Yield and SPR time-series
    if (!is.na(match_report_line("reports_per_recruit_quantities_using_current_year_biology",
      obj = rawrep[, 2]
    ))
    ) {
      # check for new format starting with 3.30.23
      spr <- match_report_table("SPR_SERIES", 6, header = TRUE)
    } else {
      spr <- match_report_table("SPR_SERIES", 5, header = TRUE)
    }

    # read SPR table again if missing using capitalization prior to 3.30.15.06
    if (is.null(spr)) {
      spr <- match_report_table("SPR_series", 5, header = TRUE)
    }

    if (!is.null(spr)) {
      # clean up SPR output
      # make older SS output names match current SS output conventions
      # note: SPR_std and SPR_report have switched back and forth over the years
      # but as of 3.30.23 is SPR_std
      names(spr) <- gsub(pattern = "SPB", replacement = "SSB", names(spr))
      spr <- df.rename(spr,
        oldnames = c("Year", "spawn_bio", "SPR_report", "Y/R", "F_report"),
        newnames = c("Yr", "SpawnBio", "SPR_std", "YPR", "F_std")
      )
      # additional conversions starting in 3.30.23
      spr <- df.rename(spr,
        oldnames = c("Bio_all", "Bio_Smry", "SSB_unfished", "SSBfished"),
        newnames = c("Bio_all_eq", "Bio_Smry_eq", "SSB_unfished_eq", "SSBfished_eq")
      )

      spr[spr == "_"] <- NA
      spr[spr == "&"] <- NA
      spr[spr == "-1.#IND"] <- NA
      spr <- type.convert(spr, as.is = TRUE)
      stats[["SPRratioLabel"]] <- managementratiolabels[1, 2]
    }

    # Add this section for 3.30.23 separation of the spr table into the spr
    # series and the annual time series
    ann_ts <- match_report_table("ANNUAL_TIME_SERIES",
      adjust1 = 9,
      header = TRUE
    )
    ann_ts[ann_ts == "_"] <- NA
    ann_ts[ann_ts == "&"] <- NA
    ann_ts[ann_ts == "-1.#IND"] <- NA
    ann_ts <- type.convert(ann_ts, as.is = TRUE)

    returndat[["sprseries"]] <- spr
    returndat[["annual_time_series"]] <- ann_ts
    returndat[["managementratiolabels"]] <- managementratiolabels
    returndat[["F_std_basis"]] <- managementratiolabels[["Label"]][2]
    returndat[["SpawnOutputLabel"]] <- SpawnOutputLabel
    returndat[["sprtarg"]] <- sprtarg
    returndat[["btarg"]] <- btarg

    # override minbthresh = 0.25 if it looks like hake
    if (!is.na(btarg) & btarg == 0.4 & startyr == 1966 & sprtarg == 0.4 &
      accuage == 20 & wtatage_switch) {
      if (verbose) {
        message(
          "Setting minimum biomass threshhold to 0.10",
          " because this looks like the Pacific Hake model.",
          " You can replace or override in SS_plots via the",
          " 'minbthresh' input."
        )
      }
      minbthresh <- 0.1 # treaty value for hake
    }
    returndat[["minbthresh"]] <- minbthresh

    # read Kobe plot
    if (length(grep("Kobe_Plot", rawrep[, 1])) != 0) {
      # head of Kobe_Plot section differs by SS version,
      # but I haven't kept track of which is which
      # read first 5 lines to figure out which one is the header
      Kobe_head <- match_report_table("Kobe_Plot", 0, "Kobe_Plot", 5, header = TRUE)
      shift <- grep("^Y(ea)?r", Kobe_head[, 1]) # may be "Year" or "Yr"
      if (length(shift) == 0) {
        # work around for bug in output for 3.24z (and some other versions)
        shift <- grep("MSY_basis:_Y(ea)?r", Kobe_head[, 1])
        if (length(shift) == 0) {
          stop("Bug: r4ss cannot find the start of table for the Kobe plot.")
        }
      }
      Kobe_warn <- NA
      Kobe_MSY_basis <- NA
      if (length(grep("_basis_is_not", Kobe_head[1, 1])) > 0) {
        Kobe_warn <- Kobe_head[1, 1]
      }
      if (length(grep("MSY_basis", Kobe_head[2, 1])) > 0) {
        Kobe_MSY_basis <- Kobe_head[2, 1]
      }
      Kobe <- match_report_table("Kobe_Plot", shift, header = TRUE)
      Kobe[Kobe == "_"] <- NA
      Kobe[Kobe == "1.#INF"] <- NA
      Kobe[Kobe == "-1.#IND"] <- NA
      names(Kobe) <- gsub("/", ".", names(Kobe), fixed = TRUE)
      Kobe[, 1:3] <- lapply(Kobe[, 1:3], as.numeric)
    } else {
      Kobe <- NA
      Kobe_warn <- NA
      Kobe_MSY_basis <- NA
    }
    returndat[["Kobe_warn"]] <- Kobe_warn
    returndat[["Kobe_MSY_basis"]] <- Kobe_MSY_basis
    returndat[["Kobe"]] <- Kobe

    flush.console()


    ## variance and sample size tuning information
    INDEX_1 <- match_report_table("INDEX_1", 1, "INDEX_1", (nfleets + 1), header = TRUE)
    # fill in column name that was missing in SS 3.24 (and perhaps other versions)
    # and replace inconsistent name in some 3.30 versions with standard name
    INDEX_1 <- df.rename(INDEX_1,
      oldnames = c("NoName", "fleetname"),
      newnames = c("Name", "Name")
    )

    # which column of INDEX_1 has number of CPUE values (used in reading INDEX_2)
    if (SS_versionNumeric >= 3.30) {
      ncpue_column <- 11
      INDEX_1 <- match_report_table("INDEX_1", 1, "INDEX_3", -4, header = TRUE)
      # remove any comments at the bottom of table
      INDEX_1 <- INDEX_1[substr(INDEX_1[["Fleet"]], 1, 1) != "#", ]
      # count of observations per index
      ncpue <- sum(as.numeric(INDEX_1[["N"]]), na.rm = TRUE)
    } else {
      ncpue_column <- 11
      ncpue <- sum(as.numeric(rawrep[
        match_report_line("INDEX_1") + 1 + 1:nfleets,
        ncpue_column
      ]))
    }
    # add to list of stuff that gets returned
    returndat[["index_variance_tuning_check"]] <- INDEX_1

    # CPUE/Survey series - will not match if not found
    cpue <- match_report_table("INDEX_2", 1, "INDEX_2", ncpue + 1, header = TRUE)
    # reread to account for note added in 3.30.23.1
    if (!is.null(cpue) && names(cpue)[1] == "NOTE:") {
      cpue <- match_report_table("INDEX_2", 2, "INDEX_2", ncpue + 2, header = TRUE)
    }
    cpue[cpue == "_"] <- NA
    if (length(cpue) > 0) {
      # make older SS output names match current SS output conventions
      # note: "Fleet_name" (formerly "Name") introduced in 3.30.12
      #       and might change as result of discussion on inconsistent use of
      #       similar column names.
      cpue <- df.rename(cpue,
        oldnames = c("Yr.S", "Yr.frac", "Supr_Per", "Name"),
        newnames = c("Time", "Time", "SuprPer", "Fleet_name")
      )
      # process old fleet number/name combo (e.g. "2_SURVEY")
      if (SS_versionNumeric < 3.24) {
        cpue[["Name"]] <- NA
        for (i in 1:nrow(cpue)) {
          cpue[["Fleet"]][i] <- strsplit(cpue[["Fleet"]][i], "_")[[1]][1]
          cpue[["Name"]][i] <- substring(cpue[["Fleet"]][i], nchar(cpue[["Fleet"]][i]) + 2)
        }
      }
      # replace any bad values (were present in at least one 3.24s model)
      if (any(cpue[["Exp"]] == "1.#QNAN")) {
        cpue[["Exp"]][cpue[["Exp"]] == "1.#QNAN"] <- NA
        cpue[["Calc_Q"]][cpue[["Calc_Q"]] == "1.#QNAN"] <- NA
        cpue[["Eff_Q"]][cpue[["Eff_Q"]] == "1.#QNAN"] <- NA
      }

      # work-around for missing SE_input values 3.30.16
      # https://github.com/nmfs-ost/ss3-source-code/issues/169
      # https://github.com/r4ss/r4ss/issues/324
      badrows <- which(cpue[["Use"]] == "")
      if (length(badrows) > 0) {
        # shift columns to the right
        columns <- which(names(cpue) == "SE_input"):which(names(cpue) == "Use")
        cpue[badrows, columns] <- cpue[badrows, columns - 1]
        # add NA value for missing column
        cpue[badrows, "SE_input"] <- NA
      }

      # make columns numeric
      cpue <- type.convert(cpue, as.is = TRUE)
    } else {
      # if INDEX_2 not present
      cpue <- NULL
    }
    returndat[["cpue"]] <- cpue

    # Numbers at age
    natage <- match_report_table("NUMBERS_AT_AGE", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    if (is.null(natage) || nrow(natage) == 0) {
      natage <- NULL
    } else {
      # make older SS output names match current SS output conventions
      natage <- df.rename(natage,
        oldnames = c("Gender", "SubMorph"),
        newnames = c("Sex", "Platoon")
      )
    }
    returndat[["natage"]] <- natage

    # NUMBERS_AT_AGE_Annual with and without fishery
    natage_annual_1_no_fishery <- match_report_table("NUMBERS_AT_AGE_Annual_1", 1,
      header = TRUE, type.convert = TRUE
    )
    natage_annual_2_with_fishery <- match_report_table("NUMBERS_AT_AGE_Annual_2", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat[["natage_annual_1_no_fishery"]] <- natage_annual_1_no_fishery
    returndat[["natage_annual_2_with_fishery"]] <- natage_annual_2_with_fishery

    # Biomass at age (introduced in 3.30)
    batage <- match_report_table("BIOMASS_AT_AGE", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    returndat[["batage"]] <- batage

    # Numbers at length
    col.adjust <- 12
    if (SS_versionNumeric < 3.30) {
      col.adjust <- 11
    }
    # test ending based on text because sections changed within 3.24 series
    natlen <- match_report_table("NUMBERS_AT_LENGTH", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    # make older SS output names match current SS output conventions
    natlen <- df.rename(natlen,
      oldnames = c("Gender", "SubMorph"),
      newnames = c("Sex", "Platoon")
    )
    returndat[["natlen"]] <- natlen

    # Biomass at length (first appeared in version 3.24l, 12-5-2012)
    batlen <- match_report_table("BIOMASS_AT_LENGTH", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    returndat[["batlen"]] <- batlen

    # F at age (first appeared in version 3.30.13, 8-Mar-2019)
    fatage <- match_report_table("F_AT_AGE", 1, header = TRUE, type.convert = TRUE)
    returndat[["fatage"]] <- fatage

    # read discard at age (added with 3.30.12, 29-Aug-2018)
    discard_at_age <- match_report_table("DISCARD_AT_AGE", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat[["discard_at_age"]] <- discard_at_age

    # catch at age
    catage <- match_report_table("CATCH_AT_AGE",
      # skip note added in 3.30.23:
      # "#_NOTE: catage is based on: sel_dead_num = sel * (retain + (1-retain)*discmort)"
      adjust1 = ifelse(is.na(match_report_line("catage", obj = rawrep[, 2])), yes = 1, no = 2),
      header = TRUE, type.convert = TRUE
    )
    returndat[["catage"]] <- catage

    # Movement
    movement <- match_report_table("MOVEMENT", 1, substr1 = FALSE, header = TRUE)
    if (!is.null(movement)) {
      names(movement) <- c(
        names(movement)[1:6],
        paste("age", names(movement)[-(1:6)], sep = "")
      )
      movement <- df.rename(movement,
        oldnames = c("Gpattern"),
        newnames = c("GP")
      )
      for (i in 1:ncol(movement)) {
        movement[, i] <- as.numeric(movement[, i])
      }
    }
    returndat[["movement"]] <- movement

    # tag reporting rates
    tagreportrates <- match_report_table("Reporting_Rates_by_Fishery", 1,
      "See_composition_data_output", -1,
      substr2 = TRUE,
      header = TRUE,
      type.convert = TRUE
    )
    returndat[["tagreportrates"]] <- tagreportrates

    # tag release table
    # (no space after this table before Tags_Alive table)
    tagrelease <- match_report_table("TAG_Recapture", 1,
      "Tags_Alive", -1,
      cols = 1:10
    )
    if (!is.null(tagrelease)) {
      # strip off info from header
      tagfirstperiod <- as.numeric(tagrelease[1, 1])
      tagaccumperiod <- as.numeric(tagrelease[2, 1])
      # remove header and convert to numeric
      names(tagrelease) <- tagrelease[4, ]
      tagrelease <- tagrelease[-(1:4), ]
      tagrelease <- type.convert(tagrelease, as.is = TRUE)
    } else {
      tagrelease <- NULL
      tagfirstperiod <- NULL
      tagaccumperiod <- NULL
    }
    returndat[["tagrelease"]] <- tagrelease
    returndat[["tagfirstperiod"]] <- tagfirstperiod
    returndat[["tagaccumperiod"]] <- tagaccumperiod

    # tags alive
    # (no space after this table before Total_recaptures table)
    tagsalive <- match_report_table(
      "Tags_Alive", 1,
      "Total_recaptures", -1
    )
    if (!is.null(tagsalive)) {
      tagcols <- ncol(tagsalive)
      names(tagsalive) <- c("TG", paste0("period", 0:(tagcols - 2)))
      tagsalive[tagsalive == ""] <- NA
      tagsalive <- type.convert(tagsalive, as.is = TRUE)
    }
    returndat[["tagsalive"]] <- tagsalive

    # total recaptures
    tagtotrecap <- match_report_table("Total_recaptures", 1)
    if (!is.null(tagtotrecap)) {
      tagcols <- ncol(tagtotrecap)
      names(tagtotrecap) <- c("TG", paste0("period", 0:(tagcols - 2)))
      tagtotrecap[tagtotrecap == ""] <- NA
      tagtotrecap <- type.convert(tagtotrecap, as.is = TRUE)
    }
    returndat[["tagtotrecap"]] <- tagtotrecap

    # age-length matrix
    # this section is more complex because of blank lines internally

    # first look for rows like " Seas: 12 Sub_Seas: 2   Morph: 12"
    sdsize_lines <- grep("^sdsize", rawrep[, 1])

    # check for presence of any lines with that string
    if (length(sdsize_lines) > 0) {
      # the section ends with first blank line after the last of the sdsize_lines
      # so count the blanks as 1 greater than those in between the keyword
      # and the last of those sdsize_lines
      # an alternative here would be to modify match_report_table to allow input of a
      # specific line number to end the section
      which_blank <- 1 + length(rep_blank_or_hash_lines[
        rep_blank_or_hash_lines > match_report_line("AGE_LENGTH_KEY") &
          rep_blank_or_hash_lines < max(sdsize_lines)
      ])

      # because of rows like " Seas: 12 Sub_Seas: 2   Morph: 12", the number of columns
      # needs to be at least 6 even if there are fewer ages
      rawALK <- match_report_table("AGE_LENGTH_KEY", 4,
        cols = 1:max(6, accuage + 2),
        header = FALSE,
        which_blank = which_blank
      )
      # confirm that the section is present
      if (length(rawALK) > 1 && # this should filter NULL values
        length(grep("AGE_AGE_KEY", rawALK[, 1])) == 0) {
        morph_col <- 5
        if (SS_versionNumeric < 3.30 &
          length(grep("Sub_Seas", rawALK[, 3])) == 0) {
          morph_col <- 3
        }
        starts <- grep("Morph:", rawALK[, morph_col]) + 2
        ends <- grep("mean", rawALK[, 1]) - 1
        N_ALKs <- length(starts)
        # 3rd dimension should be either nmorphs or nmorphs*(number of Sub_Seas)
        ALK <- array(NA, c(nlbinspop, accuage + 1, N_ALKs))
        dimnames(ALK) <- list(
          Length = rev(lbinspop),
          TrueAge = 0:accuage,
          Matrix = 1:N_ALKs
        )

        # loop over subsections within age-length matrix
        for (i in 1:N_ALKs) {
          # get matrix of values
          ALKtemp <- rawALK[starts[i]:ends[i], 2 + 0:accuage]
          # loop over ages to convert values to numeric
          ALKtemp <- type.convert(ALKtemp, as.is = TRUE)
          # fill in appropriate slice of array
          ALK[, , i] <- as.matrix(ALKtemp)
          # get info on each matrix (such as "Seas: 1 Sub_Seas: 1 Morph: 1")
          Matrix.Info <- rawALK[starts[i] - 2, ]
          # filter out empty elements
          Matrix.Info <- Matrix.Info[Matrix.Info != ""]
          # combine elements to form a label in the dimnames
          dimnames(ALK)[["Matrix"]][i] <- paste(Matrix.Info, collapse = " ")
        }
        returndat[["ALK"]] <- ALK
      } # end check for keyword present
    } # end check for length(sdsize_lines) > 0

    # ageing error matrices
    rawAAK <- match_report_table("AGE_AGE_KEY", 1)
    if (!is.null(rawAAK)) {
      # some SS versions output message,
      # others just had no values resulting in a string with NULL dimension
      if (rawAAK[[1]][1] == "no_age_error_key_used" |
        is.null(dim(rawAAK))) {
        N_ageerror_defs <- 0
      } else {
        starts <- grep("KEY:", rawAAK[, 1])
        N_ageerror_defs <- length(starts)
        if (N_ageerror_defs > 0) {
          # loop over ageing error types to get definitions
          nrowsAAK <- nrow(rawAAK) / N_ageerror_defs - 3
          AAK <- array(NA, c(N_ageerror_defs, nrowsAAK, accuage + 1))
          age_error_mean <- age_error_sd <- data.frame(age = 0:accuage)
          for (i in 1:N_ageerror_defs) {
            AAKtemp <- rawAAK[starts[i] + 2 + 1:nrowsAAK, -1]
            rownames.tmp <- rawAAK[starts[i] + 2 + 1:nrowsAAK, 1]
            AAKtemp <- type.convert(AAKtemp, as.is = TRUE)
            AAK[i, , ] <- as.matrix(AAKtemp)
            age_error_mean[[paste("type", i, sep = "")]] <-
              as.numeric((rawAAK[starts[i] + 1, -1]))
            age_error_sd[[paste("type", i, sep = "")]] <-
              as.numeric((rawAAK[starts[i] + 2, -1]))
          }
          # add names to 3 dimensions of age-age-key
          if (!is.null(AAK)) {
            dimnames(AAK) <- list(
              AgeingErrorType = 1:N_ageerror_defs,
              ObsAgeBin = rownames.tmp,
              TrueAge = 0:accuage
            )
          }
          returndat[["AAK"]] <- AAK
          returndat[["age_error_mean"]] <- age_error_mean
          returndat[["age_error_sd"]] <- age_error_sd
        }
      } # end check for ageing error matrices
      returndat[["N_ageerror_defs"]] <- N_ageerror_defs
    } # end check for NULL output of ageing error info

    # get equilibrium yield for newer versions of SS (some 3.24 and all 3.30),
    # which have SPR/YPR profile in Report.sso
    # (this was previously in Forecast-report.sso, but reading this info
    # is no longer supported for those older versions)
    if (SS_versionNumeric >= 3.30) {
      # 3.30 models have "Finish SPR/YPR profile" followed by some additional comments
      yieldraw <- match_report_table("SPR/YPR_Profile", 1, "Finish", -2)
    } else {
      # 3.24 models and earlier use blank line to end table
      yieldraw <- match_report_table("SPR/YPR_Profile", 1)
    }
    if (!is.null(yieldraw)) {
      names <- yieldraw[1, ]
      names[names == "SSB/Bzero"] <- "Depletion"
      yielddat <- yieldraw[c(2:(as.numeric(length(yieldraw[, 1]) - 1))), ]
      yielddat[yielddat == "-nan(ind)"] <- NA # this value sometimes occurs in 3.30 models
      names(yielddat) <- names
      # remove lines that say "ready for equilcalc" or "ready for loops"
      if ("SPRloop" %in% names) { # column not present in early SS3 versions
        yielddat <- yielddat |> dplyr::filter(SPRloop != "ready")
      }
      yielddat <- type.convert(yielddat, as.is = TRUE)
    } else {
      yielddat <- NA
    }
    returndat[["equil_yield"]] <- yielddat

    # Z at age
    # With_fishery
    # No_fishery_for_Z=M_and_dynamic_Bzero
    Z_at_age <- match_report_table("Z_AT_AGE_Annual_2", 1, header = TRUE)
    if (!is.null(Z_at_age)) {
      Z_at_age[Z_at_age == "_"] <- NA
      # if birth season is not season 1, you can get infinite values
      Z_at_age[Z_at_age == "-1.#INF"] <- NA
      Z_at_age <- type.convert(Z_at_age, as.is = TRUE)
    }
    returndat[["Z_at_age"]] <- Z_at_age

    if (!is.na(match_report_line("Report_Z_by_area_morph_platoon"))) {
      # from 3.30.16.03 onward the old end of the Z_AT_AGE_Annual 1 table
      # doesn't work so should just use the blank line
      # (not available in early versions)
      M_at_age <- match_report_table("Z_AT_AGE_Annual_1", 1, header = TRUE)
    } else {
      # In earlier versions the M at age table ended with comments
      #   Note:  Z calculated as -ln(Nt+1 / Nt)
      #   Note:  Z calculation for maxage not possible, for maxage-1 includes numbers at maxage, so is approximate
      M_at_age <- match_report_table("Z_AT_AGE_Annual_1", 1,
        "-ln(Nt+1", -1,
        matchcol2 = 5,
        header = TRUE
      )
    }
    if (!is.null(M_at_age)) {
      M_at_age[M_at_age == "_"] <- NA
      # if birth season is not season 1, you can get infinite values
      M_at_age[M_at_age == "-1.#INF"] <- NA
      M_at_age <- type.convert(M_at_age, as.is = TRUE)
    }
    returndat[["M_at_age"]] <- M_at_age

    # new section added in SSv3.30.16.03
    if (is.na(match_report_line("Report_Z_by_area_morph_platoon"))) {
      Z_by_area <- NULL
      M_by_area <- NULL
    } else {
      if (!is.na(match_report_line("Report_Z_by_area_morph_platoon_2"))) {
        # format associated with 3.30.19 and beyond (separate tables with/without fishery)
        Z_by_area <- match_report_table("Report_Z_by_area_morph_platoon_2",
          adjust1 = 1,
          header = TRUE,
          type.convert = TRUE
        )
        M_by_area <- match_report_table("Report_Z_by_area_morph_platoon_1",
          adjust1 = 1,
          adjust2 = -3, # remove 2 lines at end ("Note:  Z calculated as -ln(Nt+1 / Nt)")
          header = TRUE,
          type.convert = TRUE
        )
      } else {
        # format associated with 3.30.16.03 to 3.30.18.00 (tables under common header)
        Report_Z_by_area_morph_platoon <-
          match_report_table("Report_Z_by_area_morph_platoon",
            adjust1 = 1,
            header = FALSE
          )
        Z_by_area <- match_report_table("With_fishery",
          adjust1 = 1,
          "No_fishery_for_Z=M",
          adjust2 = -1,
          matchcol1 = 2,
          matchcol2 = 2,
          obj = Report_Z_by_area_morph_platoon,
          header = TRUE,
          type.convert = TRUE
        )
        M_by_area <- match_report_table("No_fishery_for_Z=M",
          blank_lines = nrow(Report_Z_by_area_morph_platoon) + 1,
          adjust1 = 1,
          matchcol1 = 2,
          obj = Report_Z_by_area_morph_platoon,
          header = TRUE,
          type.convert = TRUE
        )
      }
      returndat["Z_by_area"] <- list(Z_by_area)
      returndat["M_by_area"] <- list(M_by_area)
    }

    # Dynamic_Bzero output "with fishery"
    Dynamic_Bzero <- match_report_table("Spawning_Biomass_Report_2", 1)
    # Dynamic_Bzero output "no fishery"
    Dynamic_Bzero2 <- match_report_table("Spawning_Biomass_Report_1", 1)
    if (!is.null(Dynamic_Bzero)) {
      Dynamic_Bzero <- cbind(Dynamic_Bzero, Dynamic_Bzero2[, -(1:2)])
      Dynamic_Bzero <- type.convert(Dynamic_Bzero[-(1:2), ], as.is = TRUE)
      # if (nareas == 1 & ngpatterns == 1) { # for simpler models, do some cleanup
      if (ncol(Dynamic_Bzero) == 4) {
        names(Dynamic_Bzero) <- c("Yr", "Era", "SSB", "SSB_nofishing")
      }
      if (nareas > 1 & !is.null(ngpatterns) && ngpatterns == 1) { # for spatial models, do some cleanup
        names(Dynamic_Bzero) <- c(
          "Yr", "Era", paste0("SSB_area", 1:nareas),
          paste0("SSB_nofishing_area", 1:nareas)
        )
        Dynamic_Bzero[["SSB"]] <- apply(Dynamic_Bzero[, 2 + 1:nareas], 1, sum)
        Dynamic_Bzero[["SSB_nofishing"]] <-
          apply(Dynamic_Bzero[, 2 + nareas + 1:nareas], 1, sum)
      }
    }
    returndat[["Dynamic_Bzero"]] <- Dynamic_Bzero

    # adding stuff to list which gets returned by function
    if (comp) {
      returndat[["comp_data_exists"]] <- TRUE
      returndat[["lendbase"]] <- lendbase
      returndat[["sizedbase"]] <- sizedbase
      returndat[["agedbase"]] <- agedbase
      returndat[["condbase"]] <- condbase
      returndat[["ghostagedbase"]] <- ghostagedbase
      returndat[["ghostcondbase"]] <- ghostcondbase
      returndat[["ghostlendbase"]] <- ghostlendbase
      returndat[["ladbase"]] <- ladbase
      returndat[["wadbase"]] <- wadbase
      returndat[["tagdbase1"]] <- tagdbase1
      returndat[["tagdbase2"]] <- tagdbase2
      returndat[["morphcompdbase"]] <- morphcompdbase
    } else {
      returndat[["comp_data_exists"]] <- FALSE
    }
    # tables on fit to comps and mean age stuff from within Report.sso
    returndat[["len_comp_fit_table"]] <- fit_len_comps
    returndat[["age_comp_fit_table"]] <- fit_age_comps
    returndat[["size_comp_fit_table"]] <- fit_size_comps

    returndat[["derived_quants"]] <- der
    returndat[["parameters"]] <- parameters
    returndat[["Dirichlet_Multinomial_pars"]] <- DM_pars
    returndat[["FleetNames"]] <- FleetNames
    returndat[["repfiletime"]] <- repfiletime

    # type of stock recruit relationship
    SRRtype <- rawrep[match_report_line("SPAWN_RECRUIT"), 3]
    if (!is.na(SRRtype) && SRRtype == "Function:") {
      SRRtype <- as.numeric(rawrep[match_report_line("SPAWN_RECRUIT"), 4])
    }
    returndat[["SRRtype"]] <- SRRtype

    # get "sigma" used by Pacific Council in P-star calculations
    SSB_final_Label <- paste0("SSB_", endyr + 1)
    if (SSB_final_Label %in% der[["Label"]]) {
      SSB_final_EST <- der[["Value"]][der[["Label"]] == SSB_final_Label]
      SSB_final_SD <- der[["StdDev"]][der[["Label"]] == SSB_final_Label]
      returndat[["Pstar_sigma"]] <- sqrt(log((SSB_final_SD / SSB_final_EST)^2 + 1))
    } else {
      returndat[["Pstar_sigma"]] <- NULL
    }
    # get alternative "sigma" based on OFL catch used by Pacific Council
    # (added 23 Sept 2019 based on decision by PFMC SSC)
    OFL_final_Label <- paste0("OFLCatch_", endyr + 1)
    if (OFL_final_Label %in% der[["Label"]]) {
      OFL_final_EST <- der[["Value"]][der[["Label"]] == OFL_final_Label]
      OFL_final_SD <- der[["StdDev"]][der[["Label"]] == OFL_final_Label]
      returndat[["OFL_sigma"]] <- sqrt(log((OFL_final_SD / OFL_final_EST)^2 + 1))
    } else {
      returndat[["OFL_sigma"]] <- NULL
    }

    if (covar) {
      returndat[["CoVar"]] <- CoVar
      returndat[["stdtable"]] <- stdtable
    }

    # extract parameter lines representing annual recruit devs
    recdevEarly <- parameters[substring(parameters[["Label"]], 1, 13) == "Early_RecrDev", ]
    early_initage <- parameters[substring(parameters[["Label"]], 1, 13) == "Early_InitAge", ]
    main_initage <- parameters[substring(parameters[["Label"]], 1, 12) == "Main_InitAge", ]
    recdev <- parameters[substring(parameters[["Label"]], 1, 12) == "Main_RecrDev", ]
    recdevFore <- parameters[substring(parameters[["Label"]], 1, 8) == "ForeRecr", ]
    recdevLate <- parameters[substring(parameters[["Label"]], 1, 12) == "Late_RecrDev", ]

    # empty variable to fill in sections
    recruitpars <- NULL

    # assign "type" label to each one and identify year
    if (nrow(early_initage) > 0) {
      early_initage[["type"]] <- "Early_InitAge"
      early_initage[["Yr"]] <- startyr - as.numeric(substring(early_initage[["Label"]], 15))
      recruitpars <- rbind(recruitpars, early_initage)
    }
    if (nrow(recdevEarly) > 0) {
      recdevEarly[["type"]] <- "Early_RecrDev"
      recdevEarly[["Yr"]] <- as.numeric(substring(recdevEarly[["Label"]], 15))
      recruitpars <- rbind(recruitpars, recdevEarly)
    }
    if (nrow(main_initage) > 0) {
      main_initage[["type"]] <- "Main_InitAge"
      main_initage[["Yr"]] <- startyr - as.numeric(substring(main_initage[["Label"]], 14))
      recruitpars <- rbind(recruitpars, main_initage)
    }
    if (nrow(recdev) > 0) {
      recdev[["type"]] <- "Main_RecrDev"
      recdev[["Yr"]] <- as.numeric(substring(recdev[["Label"]], 14))
      recruitpars <- rbind(recruitpars, recdev)
    }
    if (nrow(recdevFore) > 0) {
      recdevFore[["type"]] <- "ForeRecr"
      recdevFore[["Yr"]] <- as.numeric(substring(recdevFore[["Label"]], 10))
      recruitpars <- rbind(recruitpars, recdevFore)
    }
    if (nrow(recdevLate) > 0) {
      recdevLate[["type"]] <- "Late_RecrDev"
      recdevLate[["Yr"]] <- as.numeric(substring(recdevLate[["Label"]], 14))
      recruitpars <- rbind(recruitpars, recdevLate)
    }

    # sort by year and remove any retain only essential columns
    if (!is.null(recruitpars)) {
      recruitpars <- recruitpars[
        order(recruitpars[["Yr"]]),
        c("Value", "Parm_StDev", "type", "Yr")
      ]
    }

    # add recruitpars to list of stuff that gets returned
    returndat[["recruitpars"]] <- recruitpars

    if (is.null(recruitpars)) {
      sigma_R_info <- NULL
    } else {
      # calculating values related to tuning SigmaR
      sigma_R_info <- data.frame(
        period = c("Main", "Early+Main", "Early+Main+Late"),
        N_devs = 0,
        SD_of_devs = NA,
        Var_of_devs = NA,
        mean_SE = NA,
        mean_SEsquared = NA
      )

      # calculate recdev stats  for Main period
      subset <- recruitpars[["type"]] %in% c("Main_InitAge", "Main_RecrDev")
      within_period <- sigma_R_info[["period"]] == "Main"
      sigma_R_info[["N_devs"]][within_period] <- sum(subset)
      sigma_R_info[["SD_of_devs"]][within_period] <- sd(recruitpars[["Value"]][subset])
      sigma_R_info[["mean_SE"]][within_period] <- mean(recruitpars[["Parm_StDev"]][subset])
      sigma_R_info[["mean_SEsquared"]][within_period] <-
        mean((recruitpars[["Parm_StDev"]][subset])^2)

      # calculate recdev stats  for Early+Main periods
      subset <- recruitpars[["type"]] %in% c(
        "Early_RecrDev", "Early_InitAge",
        "Main_InitAge", "Main_RecrDev"
      )
      within_period <- sigma_R_info[["period"]] == "Early+Main"
      sigma_R_info[["N_devs"]][within_period] <- sum(subset)
      sigma_R_info[["SD_of_devs"]][within_period] <- sd(recruitpars[["Value"]][subset])
      sigma_R_info[["mean_SE"]][within_period] <- mean(recruitpars[["Parm_StDev"]][subset])
      sigma_R_info[["mean_SEsquared"]][within_period] <-
        mean((recruitpars[["Parm_StDev"]][subset])^2)

      # calculate recdev stats for Early+Main+Late periods
      subset <- recruitpars[["type"]] %in% c(
        "Early_RecrDev", "Early_InitAge",
        "Main_InitAge", "Main_RecrDev", "Late_RecrDev"
      )
      within_period <- sigma_R_info[["period"]] == "Early+Main+Late"
      sigma_R_info[["N_devs"]][within_period] <- sum(subset)
      sigma_R_info[["SD_of_devs"]][within_period] <- sd(recruitpars[["Value"]][subset])
      sigma_R_info[["mean_SE"]][within_period] <- mean(recruitpars[["Parm_StDev"]][subset])
      sigma_R_info[["mean_SEsquared"]][within_period] <-
        mean((recruitpars[["Parm_StDev"]][subset])^2)

      # add variance as square of SD
      sigma_R_info[["Var_of_devs"]] <- sigma_R_info[["SD_of_devs"]]^2

      # add sqrt of sum
      sigma_R_info[["sqrt_sum_of_components"]] <- sqrt(sigma_R_info[["Var_of_devs"]] +
        sigma_R_info[["mean_SEsquared"]])
      # ratio of sqrt of sum to sigmaR
      sigma_R_info[["SD_of_devs_over_sigma_R"]] <- sigma_R_info[["SD_of_devs"]] / sigma_R_in
      sigma_R_info[["sqrt_sum_over_sigma_R"]] <- sigma_R_info[["sqrt_sum_of_components"]] / sigma_R_in
      sigma_R_info[["alternative_sigma_R"]] <- sigma_R_in * sigma_R_info[["sqrt_sum_over_sigma_R"]]

      # if there's no uncertainty in the recdevs (probably because of -nohess)
      # then don't report alternative sigma R values
      # could also use [["log_det_hessian"]] as the filter
      sigma_R_info[["alternative_sigma_R"]][sigma_R_info[["mean_SE"]] == 0] <- "needs_Hessian"
    }
    stats[["sigma_R_in"]] <- sigma_R_in
    stats[["sigma_R_info"]] <- sigma_R_info
    stats[["rmse_table"]] <- rmse_table
    stats[["RecDev_method"]] <- RecDev_method

    # process adjustments to recruit devs
    RecrDistpars <- parameters[substring(parameters[["Label"]], 1, 8) == "RecrDist", ]
    returndat[["RecrDistpars"]] <- RecrDistpars

    # adding read of wtatage file
    returndat[["wtatage"]] <- wtatage

    # adding new jitter info table
    returndat[["jitter_info"]] <- jitter_info

    # add list of stats to list that gets returned
    returndat <- c(returndat, stats)

    # add info on semi-parametric selectivity deviations
    returndat[["seldev_pars"]] <- seldev_pars
    returndat[["seldev_matrix"]] <- seldev_matrix

    # print list of statistics
    if (printstats) {
      message("\nStatistics shown below (to turn off, change input to printstats=FALSE)")

      # remove scientific notation (only for display, not returned values,
      # which were added to returndat already)
      stats[["likelihoods_used"]] <- format(stats[["likelihoods_used"]], scientific = 20)
      stats[["estimated_non_dev_parameters"]] <- format(stats[["estimated_non_dev_parameters"]],
        scientific = 20
      )
      print(stats)
    }

    # add log file to list that gets returned
    returndat[["logfile"]] <- logfile


    # return the inputs to this function so they can be used by SS_plots
    # or other functions
    inputs <- list()
    inputs[["dir"]] <- dir
    inputs[["repfile"]] <- repfile
    inputs[["forecast"]] <- forecast
    inputs[["warn"]] <- warn
    inputs[["covar"]] <- covar
    inputs[["verbose"]] <- verbose

    returndat[["inputs"]] <- inputs

    if (verbose) {
      message("completed SS_output")
    }
    invisible(returndat)
  } # end function
