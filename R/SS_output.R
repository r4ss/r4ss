#' A function to create a list object for the output from Stock Synthesis
#'
#' Reads the Report.sso and (optionally) the covar.sso, CompReport.sso and
#' other files produced by Stock Synthesis and formats the important
#' content of these files into a list in the R workspace. A few statistics
#' unavailable elsewhere are taken from the .par and .cor files. Summary
#' information and statistics can be returned to the R console or just
#' contained within the list produced by this function.
#'
#'
#' @param dir Directory containing the Stock Synthesis model output.
#' Forward slashes or double backslashes and quotes are necessary.
#' This can also either be an absolute path or relative to the working
#' directory.
#' @param dir.mcmc Optional directory containing MCMC output. This can either be
#' relative to \code{dir}, such that \code{file.path(dir, dir.mcmc)}
#' will end up in the right place, or an absolute path.
#' @param repfile Name of the big report file (could be renamed by user).
#' @param compfile Name of the composition report file.
#' @param covarfile Name of the covariance output file.
#' @param forefile Name of the forecast file.
#' @param wtfile Name of the file containing weight at age data.
#' @param warnfile Name of the file containing warnings.
#' @param ncols The maximum number of columns in files being read in.  If this
#' value is too big the function runs more slowly, too small and errors will
#' occur.  A warning will be output to the R command line if the value is too
#' small. It should be bigger than the maximum age + 10 and the number of years
#' + 10. The default value is \code{NULL}, which finds the optimum width.
#' @param forecast Read the forecast-report file?
#' @param warn Read the Warning.sso file?
#' @param covar Read covar.sso to get variance information and identify bad
#' correlations?
#' @param readwt Read the weight-at-age file?
#' @param checkcor Check for bad correlations?
#' @param cormax The specified threshold for defining high correlations.  A
#' quantity with any correlation above this value is identified.
#' @param cormin The specified threshold for defining low correlations.  Only
#' quantities with all correlations below this value are identified (to find
#' variables that appear too independent from the model results).
#' @param printhighcor The maximum number of high correlations to print to the
#' R GUI.
#' @param printlowcor The maximum number of low correlations to print to the R
#' GUI.
#' @template verbose
#' @param printstats Print summary statistics about the output to the R GUI?
#' @param hidewarn Hides some warnings output from the R GUI.
#' @param NoCompOK Allow the function to work without a CompReport file.
#' @param aalmaxbinrange The largest length bin range allowed for composition
#' data to be considered as conditional age-at-length data.
#' @return Many values are returned. Complete list would be quite long, but
#' should probably be created at some point in the future.
#' @author Ian Stewart, Ian Taylor
#' @export
#' @seealso \code{\link{SS_plots}}
#' @examples
#'
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
           ncols = NULL,
           forecast = TRUE,
           warn = TRUE,
           covar = TRUE,
           readwt = TRUE,
           checkcor = TRUE,
           cormax = 0.95,
           cormin = 0.01,
           printhighcor = 10,
           printlowcor = 10,
           verbose = TRUE,
           printstats = TRUE,
           hidewarn = FALSE,
           NoCompOK = TRUE,
           aalmaxbinrange = 4) {
    flush.console()

    #################################################################################
    ## embedded functions: emptytest, matchfun and matchfun2
    #################################################################################

    emptytest <- function(x) {
      # function to help test for empty columns
      sum(!is.na(x) & x == "") / length(x)
    }

    matchfun <- function(string, obj = rawrep[, 1], substr1 = TRUE) {
      # return a line number from the report file (or other file)
      # substr1 controls whether to compare subsets or the whole line
      match(string, if (substr1) {
        substring(obj, 1, nchar(string))
      } else {
        obj
      })
    }

    matchfun2 <- function(string1,
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
    } # end matchfun2

    df.rename <- function(df, oldnames, newnames) {
      # function to replace names in dataframes
      # added to clean up adaptation to more consistent
      # syntax in Report.sso as of SS version 3.30.01.15.
      if (!is.null(df)) {
        for (iname in 1:length(oldnames)) {
          names(df)[names(df) == oldnames[iname]] <- newnames[iname]
        }
      }
      return(df)
    }

    # check to make sure the first input is in the corect format
    if (!is.character(dir) | length(dir) != 1) {
      stop("Input 'dir' should be a character string for a directory")
    }

    # get info on output files created by Stock Synthesis
    shortrepfile <- repfile
    repfile <- file.path(dir, repfile)

    parfile <- dir(dir, pattern = ".par$")
    if (length(parfile) > 1) {
      filetimes <- file.info(file.path(dir, parfile))$mtime
      parfile <- parfile[filetimes == max(filetimes)][1]
      if (verbose) {
        message(
          "Multiple files in directory match pattern *.par\n",
          "choosing most recently modified:", parfile
        )
      }
    }
    if (length(parfile) == 0) {
      if (!hidewarn) {
        message("Some stats skipped because the .par file not found.")
      }
      parfile <- NA
    } else {
      parfile <- file.path(dir, parfile)
    }

    # read three rows to get start time and version number from rep file
    if (file.exists(repfile)) {
      if (file.info(repfile)$size > 0) {
        if (verbose) {
          message("Getting header info from:\n  ", repfile)
        }
      } else {
        stop("report file is empty: ", repfile)
      }
    } else {
      stop("can't find report file: ", repfile)
    }
    rephead <- readLines(con = repfile, n = 50)

    # warn if SS version used to create rep file is too old or too new for this code
    # note: SS_versionCode is new with V3.20
    # perhaps in the future we will use it to replace SS_versionshort throughout r4ss?
    SS_versionCode <- rephead[grep("#V", rephead)]
    SS_version <- rephead[grep("Stock_Synthesis", rephead)]
    SS_version <- SS_version[substring(SS_version, 1, 2) != "#C"] # remove any version numbering in the comments
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
    # TRUE/VALSE indicator of custom reporting options (started in 3.30.15.06)
    custom <- length(grep("report:", rephead)) > 0

    corfile <- NA
    if (covar) {
      # .cor file
      if (!is.na(parfile)) {
        corfile <- sub(".par", ".cor", parfile, fixed = TRUE)
        if (!file.exists(corfile)) {
          warning("Some stats skipped because the .cor file not found:", corfile, "\n")
          corfile <- NA
        }
      }
      # CoVar.sso file
      covarfile <- file.path(dir, covarfile)
      if (!file.exists(covarfile)) {
        warning("covar file not found, input 'covar' changed to FALSE")
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

    # time check for CompReport file
    comp <- FALSE
    if (is.null(compfile)) {
      if (verbose) {
        message("Skipping CompReport because 'compfile = NULL'")
      }
    } else {
      if (file.exists(file.path(dir, compfile))) {
        # non-NULL compfile input provided and file exists
        compfile <- file.path(dir, compfile)
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

    if (is.null(ncols)) {
      ncols <- get_ncol(repfile)
    }
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
      temp <- file.info(forecastname)$size
      if (is.na(temp) | temp == 0) {
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
        sprtarg <- as.numeric(rawforecast1[matchfun(
          "SPR_target",
          rawforecast1[, 1]
        ), 2])

        # starting in SSv3.30.10.00, the Forecast-report file has been restructured
        target_definitions <- grep("_as_target", rawforecast1[, 1], value = TRUE)
        if (length(target_definitions) == 0) {
          # old setup (prior to 3.30.10.00)
          btarg <- as.numeric(rawforecast1[matchfun(
            "Btarget",
            rawforecast1[, 1]
          ), 2])
        } else {
          # new setup with biomass target
          if ("Ratio_SSB/B0_as_target" %in% target_definitions) {
            btarg <- as.numeric(rawforecast1[matchfun(
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
          "Setting minimum biomass threshhold to 0.25",
          "  based on US west coast assumption associated with flatfish target of 0.25.",
          "  (can replace or override in SS_plots by setting 'minbthresh')"
        )
      }
      minbthresh <- 0.125 # west coast assumption for flatfish
    }
    flush.console()

    # check for use of temporary files
    logfile <- dir(dir, pattern = ".log$")
    logfile <- logfile[logfile != "fmin.log"]
    if (length(logfile) > 1) {
      filetimes <- file.info(file.path(dir, logfile))$mtime
      logfile <- logfile[filetimes == max(filetimes)]
      if (verbose) {
        message(
          "Multiple files in directory match pattern *.log\n",
          "choosing most recently modified file:", logfile, "\n"
        )
      }
    }
    if (length(logfile) == 1 && file.info(file.path(dir, logfile))$size > 0) {
      logfile <- read.table(file.path(dir, logfile))[, c(4, 6)]
      names(logfile) <- c("TempFile", "Size")
      maxtemp <- max(logfile$Size)
      if (maxtemp == 0) {
        if (verbose) {
          message(
            "Got log file. There were NO temporary files were written",
            " in this run."
          )
        }
      } else {
        if (verbose) {
          message("!warning: temporary files were written in this run:")
          print(logfile)
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
      if (!file.exists(warnname)) {
        message(warnfile, " file not found")
        nwarn <- NA
        warn <- NA
      } else {
        warn <- readLines(warnname, warn = FALSE)
        warnstring <- warn[grep("N warnings: ", warn)]
        if (length(warnstring) > 0) {
          nwarn <- as.numeric(strsplit(warnstring, "N warnings: ")[[1]][2])
          textblock <- ifelse(nwarn > 1,
            paste("were", nwarn, "warnings"),
            paste("was", nwarn, "warning")
          )
          if (verbose) {
            message(
              "Got warning file.",
              " There", textblock, " in ", warnname
            )
          }
        } else {
          message(warnfile, " file is missing the string 'N warnings'")
          nwarn <- NA
        }
      }
    } else {
      if (verbose) {
        message("You skipped the warnings file")
      }
      nwarn <- NA
    }
    if (verbose) {
      message("Finished reading files")
    }
    flush.console()

    # length selectivity is read earlier than other tables because it was used
    # to get fleet info this can be moved to join rest of selex stuff after
    # SSv3.11 is not supported any more
    sizeselex <- matchfun2("LEN_SELEX", 6, header = TRUE, type.convert = TRUE)
    # update to size selectivity to naming convention associated with 3.30.01.15
    sizeselex <- df.rename(sizeselex,
      oldnames = c("fleet", "year", "seas", "gender", "morph", "label"),
      newnames = c("Fleet", "Yr", "Seas", "Sex", "Morph", "Label")
    )

    ## DEFINITIONS section (new in SSv3.20)
    ## (which_blank = 2 skips the "#" near the end to include the final table)
    rawdefs <- matchfun2("DEFINITIONS", 1, which_blank = 2)
    # re-read that section for older models which didn't have a hash
    if ("LIKELIHOOD" %in% rawdefs[, 1]) {
      rawdefs <- matchfun2("DEFINITIONS", 1, which_blank = 1)
    }

    # check for new format for definitions (starting with 3.30.12)
    # ("Jitter" is an indicator of the new format)
    if ("Jitter:" %in% rawdefs$X1) {
      get.def <- function(string) {
        # function to grab numeric value from 2nd column matching string in 1st column
        row <- grep(string, rawdefs$X1)[1]
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
          rawdefs$X1
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

      # table starting with final occurrence of "Fleet" in column 1
      fleetdefs <- rawdefs[tail(grep("Fleet", rawdefs$X1), 1):nrow(rawdefs), ]
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
      fleet_type <- fleetdefs$fleet_type
      fleet_timing <- fleetdefs$timing
      fleet_area <- fleetdefs$area
      catch_units <- fleetdefs$catch_units
      ## equ_catch_se <- fleetdefs$equ_catch_se
      ## catch_se     <- fleetdefs$catch_se
      survey_units <- fleetdefs$survey_units
      survey_error <- fleetdefs$survey_error
      fleet_ID <- fleetdefs$Fleet
      IsFishFleet <- fleet_type <= 2 # based on definitions above
      nfishfleets <- sum(IsFishFleet)
      FleetNames <- fleetdefs$Fleet_name
      nfleets <- max(fleet_ID)

      # process some season info
      seasfracs <- round(12 * cumsum(seasdurations)) / 12
      seasfracs <- seasfracs - seasdurations / 2 # should be mid-point of each season as a fraction of the year

      # end new DEFINITIONS format (starting with 3.30.12)
    } else {
      # old format for DEFINITIONS (up through 3.30.11)

      # get season stuff
      nseasons <- as.numeric(rawdefs[grep("N_seasons", rawdefs[, 1]), 2])
      seasdurations <- as.numeric(rawdefs[grep("Season_Durations", rawdefs[, 1]), 1 + 1:nseasons])
      seasfracs <- round(12 * cumsum(seasdurations)) / 12
      seasfracs <- seasfracs - seasdurations / 2 # should be mid-point of each season as a fraction of the year

      if (SS_versionNumeric >= 3.30) {
        # add read of additions to DEFINITIONS section added with 3.30.12
        # version 3.3 (fleet info switched from columns to rows starting with 3.30)
        FleetNames <- as.character(rawdefs[grep("fleet_names", rawdefs$X1), -1])
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
        fleet_type <- fleetdefs$fleet_type
        fleet_timing <- fleetdefs$timing
        fleet_area <- fleetdefs$area
        catch_units <- fleetdefs$catch_units
        equ_catch_se <- fleetdefs$equ_catch_se
        catch_se <- fleetdefs$catch_se
        survey_units <- fleetdefs$survey_units
        survey_error <- fleetdefs$survey_error
        IsFishFleet <- fleet_type <= 2 # based on definitions above
      } else {
        # version 3.20-3.24
        # get fleet info
        fleetdefs <- rawdefs[-(1:3), apply(rawdefs[-(1:3), ], 2, emptytest) < 1]
        fleetdefs[fleetdefs == ""] <- NA
        lab <- fleetdefs$X1
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
      begin <- matchfun("TIME_SERIES") + 2
      end <- matchfun("SPR_series") - 2

      # more dimensions
      nfishfleets <- sum(IsFishFleet)
      nsexes <- length(unique(as.numeric(sizeselex$Sex)))
      nareas <- max(as.numeric(rawrep[begin:end, 1]))
      # startyr is the 'initial' year not including VIRG or INIT years
      startyr <- min(as.numeric(rawrep[begin:end, 2])) + 2
      temptime <- rawrep[begin:end, 2:3]
      # endyr is the beginning of the last year of the normal timeseries
      endyr <- max(as.numeric(temptime[temptime[, 2] == "TIME", 1]))
      tempaccu <- as.character(rawrep[matchfun("Natural_Mortality") + 1, -(1:5)])
      accuage <- max(as.numeric(tempaccu[tempaccu != ""]))
    } # end read of DEFINITIONS

    # compositions
    if (comp) { # skip this stuff if no CompReport.sso file
      # read header section of file to get bin information
      allbins <- read.table(
        file = compfile, col.names = 1:ncols, fill = TRUE,
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
      Lbin_method <- as.numeric(allbins[matchfun("Method_for_Lbin_definition", allbins[, 1]), 2])
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

        # update to naming convention associated with 3.30.12 (Nsamp_adj added in 3.30.15)
        compdbase <- df.rename(compdbase,
          oldnames = c("Pick_sex", "Pick_gender", "Gender", "N"),
          newnames = c("Sexes", "Sexes", "Sex", "Nsamp_adj")
        )
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
        compdbase$sex <- compdbase$Sexes
        compdbase$sex[compdbase$Sexes == 3] <- compdbase$Sex[compdbase$Sexes == 3]

        # make correction to tag output associated with 3.24f (fixed in later versions)
        if (substr(SS_version, 1, 9) == "SS-V3.24f") {
          if (!hidewarn) {
            message("Correcting for bug in tag data output associated with SSv3.24f\n")
          }
          tag1rows <- compdbase$Sexes == "TAG1"
          if (any(tag1rows)) {
            tag1 <- compdbase[tag1rows, ]
            tag1new <- tag1
            tag1new[, 4:23] <- tag1new[, 3:22] # shift columns over
            tag1new$Yr.S <- tag1new$Yr # move Yr.S
            tag1new$Yr <- floor(as.numeric(tag1new$Yr)) # turn Yr.S into Yr
            compdbase[tag1rows, ] <- tag1new
          }
        }

        # remove rows within missing observations (beginning of each section)
        compdbase <- compdbase[compdbase$Obs != "", ]
        # replace underscores with NA
        compdbase[compdbase == "_"] <- NA
        # replace any NA values in the Used? column with "yes".
        compdbase$Used[is.na(compdbase$Used)] <- "yes"
        # add SuprPer column for versions where it didn't exist
        if (!("SuprPer" %in% names(compdbase))) {
          compdbase$SuprPer <- "No"
        }
        compdbase$SuprPer[is.na(compdbase$SuprPer)] <- "No"
        n <- sum(is.na(compdbase$Nsamp_adj) &
          compdbase$Used != "skip" &
          compdbase$Kind != "TAG2")
        if (n > 0) {
          warning(
            n, " rows from composition database have NA sample size\n",
            "but are not part of a super-period. (Maybe input as N=0?)\n"
          )
        }
        compdbase <- type.convert(compdbase, as.is = TRUE)

        # configure seasons
        if (nseasons > 1) {
          compdbase$YrSeasName <- paste(floor(compdbase$Yr), "s", compdbase$Seas, sep = "")
        } else {
          compdbase$YrSeasName <- compdbase$Yr
        }

        # starting with SSv3.24a, the Yr.S column is already in the output, otherwise fill it in
        if (!"Yr.S" %in% names(compdbase)) {
          if (any(floor(compdbase$Yr) != compdbase$Yr)) {
            # in some cases, year is already a decimal number
            compdbase$Yr.S <- compdbase$Yr
            compdbase$Yr <- floor(compdbase$Yr)
          } else {
            # add fraction of season to distinguish between samples
            compdbase$Yr.S <- compdbase$Yr + (0.5 / nseasons) * compdbase$Seas
          }
        }

        # deal with Lbins
        compdbase$Lbin_range <- compdbase$Lbin_hi - compdbase$Lbin_lo
        compdbase$Lbin_mid <- 0.5 * (compdbase$Lbin_lo + compdbase$Lbin_hi)

        # divide into objects by kind
        Lbin_range <- compdbase$Lbin_range
        if (is.null(Lbin_range)) { # if/else required to avoid warning if no comp data at all
          notconditional <- TRUE
          conditional <- FALSE
        } else {
          notconditional <- !is.na(Lbin_range) & Lbin_range > aalmaxbinrange
          conditional <- !is.na(Lbin_range) & Lbin_range <= aalmaxbinrange
        }
        if ("skip" %in% compdbase$SuprPer) {
          # formatting error in some SS 3.30 versions caused skip to appear in
          # the wrong column, so copy to the right one
          compdbase$Used[compdbase$SuprPer == "skip"] <- "skip"
          # probability of being a super-period is low, so assigning "No"
          # to assist with identification of ghost comps below
          compdbase$SuprPer[compdbase$SuprPer == "No"]
        }
        if (SS_versionNumeric >= 3.22) {
          # new designation of ghost fleets from negative samp size to negative fleet
          lendbase <- compdbase[compdbase$Kind == "LEN" &
            compdbase$Used != "skip", ]
          sizedbase <- compdbase[compdbase$Kind == "SIZE" &
            compdbase$Used != "skip", ]
          agedbase <- compdbase[compdbase$Kind == "AGE" &
            compdbase$Used != "skip" & notconditional, ]
          condbase <- compdbase[compdbase$Kind == "AGE" &
            compdbase$Used != "skip" & conditional, ]
        } else {
          # older designation of ghost fleets from negative samp size to negative fleet
          lendbase <- compdbase[compdbase$Kind == "LEN" &
            (compdbase$SuprPer == "Sup" |
              (!is.na(compdbase$Nsamp_adj) & compdbase$Nsamp_adj > 0)), ]
          sizedbase <- compdbase[compdbase$Kind == "SIZE" &
            (compdbase$SuprPer == "Sup" |
              (!is.na(compdbase$Nsamp_adj) & compdbase$Nsamp_adj > 0)), ]
          agedbase <- compdbase[compdbase$Kind == "AGE" &
            (compdbase$SuprPer == "Sup" |
              (!is.na(compdbase$Nsamp_adj) & compdbase$Nsamp_adj > 0)) &
            notconditional, ]
          condbase <- compdbase[compdbase$Kind == "AGE" &
            (compdbase$SuprPer == "Sup" |
              (!is.na(compdbase$Nsamp_adj) & compdbase$Nsamp_adj > 0)) &
            conditional, ]
        }
        ghostagedbase <- compdbase[compdbase$Kind == "AGE" &
          compdbase$Used == "skip" &
          compdbase$SuprPer == "No" & notconditional, ]
        ghostcondbase <- compdbase[compdbase$Kind == "AGE" &
          compdbase$Used == "skip" &
          compdbase$SuprPer == "No" & conditional, ]
        ghostlendbase <- compdbase[compdbase$Kind == "LEN" &
          compdbase$Used == "skip" &
          compdbase$SuprPer == "No", ]
        compdbase$Kind[compdbase$Kind == "L@A" & compdbase$Ageerr < 0] <- "W@A"

        # extra processing for sizedbase
        if (!is.null(sizedbase) && nrow(sizedbase) > 0) {
          sizedbase$bio.or.num <- c("bio", "num")[sizedbase$Lbin_lo]
          sizedbase$units <- c("kg", "lb", "cm", "in")[sizedbase$Lbin_hi]
          sizedbase$method <- sizedbase$Ageerr

          if (any(sizedbase$units %in% c("lb", "in"))) {
            if (verbose) {
              message(
                "Note: converting bins in generalized size comp data ",
                " in sizedbase back to the original units of lbs or inches."
              )
            }
          }
          # convert bins from kg to lbs when that was the original unit
          sizedbase$Bin[sizedbase$units == "lb"] <-
            sizedbase$Bin[sizedbase$units == "lb"] / 0.4536
          # convert bins from cm to inches when that was the original unit
          sizedbase$Bin[sizedbase$units == "in"] <-
            sizedbase$Bin[sizedbase$units == "in"] / 2.54

          sizebinlist <- list()
          for (imethod in 1:max(sizedbase$method)) {
            tmp <- sort(unique(sizedbase$Bin[sizedbase$method == imethod]))
            if (length(tmp) == 0) tmp <- NULL
            sizebinlist[[paste("size_method_", imethod, sep = "")]] <- tmp
          }
        } else {
          sizebinlist <- NA
        }

        if (is.null(compdbase$Nsamp_adj)) {
          good <- TRUE
        } else {
          good <- !is.na(compdbase$Nsamp_adj)
        }
        ladbase <- compdbase[compdbase$Kind == "L@A" & good, ]
        wadbase <- compdbase[compdbase$Kind == "W@A" & good, ]
        tagdbase1 <- compdbase[compdbase$Kind == "TAG1", ]
        tagdbase2 <- compdbase[compdbase$Kind == "TAG2", ]
        # consider range of bins for conditional age at length data
        if (verbose) {
          message(
            "CompReport file separated by this code as follows",
            " (rows = Ncomps*Nbins):\n",
            "  ", nrow(lendbase), " rows of length comp data,\n",
            "  ", nrow(sizedbase), " rows of generalized size comp data,\n",
            "  ", nrow(agedbase), " rows of age comp data,\n",
            "  ", nrow(condbase), " rows of conditional age-at-length data,\n",
            "  ", nrow(ghostagedbase), " rows of ghost fleet age comp data,\n",
            "  ", nrow(ghostcondbase),
            " rows of ghost fleet conditional age-at-length data,\n",
            "  ", nrow(ghostlendbase),
            " rows of ghost fleet length comp data,\n",
            "  ", nrow(ladbase), " rows of mean length at age data,\n",
            "  ", nrow(wadbase), " rows of mean weight at age data,\n",
            "  ", nrow(tagdbase1), " rows of 'TAG1' comp data, and\n",
            "  ", nrow(tagdbase2), " rows of 'TAG2' comp data."
          )
        }
        # convert bin indices to true lengths
        if (nrow(agedbase) > 0) {
          Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
          names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
          if (length(unique(agedbase$Lbin_range)) > 1) {
            warning(
              "different ranges of Lbin_lo to Lbin_hi found in age comps.\n",
              paste(utils::capture.output(print(Lbin_ranges)), collapse = "\n"),
              "\n consider increasing 'aalmaxbinrange' to designate\n",
              "some of these data as conditional age-at-length."
            )
          }
          agebins <- sort(unique(agedbase$Bin[!is.na(agedbase$Bin)]))
        } else {
          if (nrow(condbase) > 0) {
            agebins <- sort(unique(condbase$Bin[!is.na(condbase$Bin)]))
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
    morph_indexing <- matchfun2("MORPH_INDEXING", 1,
      header = TRUE, type.convert = TRUE
    )
    morph_indexing <- df.rename(morph_indexing,
      oldnames = c("Gpattern", "Bseas", "Gender"),
      newnames = c("GP", "BirthSeas", "Sex")
    )
    if (!is.null(morph_indexing)) {
      # calculate number of growth patterns
      ngpatterns <- max(morph_indexing$GP)
    } else {
      ngpatterns <- NULL
    }

    if (verbose) {
      message("Finished dimensioning")
    }
    flush.console()

    # stats list: items that are output to the GUI (if printstats==T) for a quick summary of results
    stats <- list()
    stats$SS_version <- SS_version
    stats$SS_versionshort <- SS_versionshort
    stats$SS_versionNumeric <- SS_versionNumeric

    stats$StartTime <- paste(as.character(matchfun2("StartTime", 0, "StartTime", 0, cols = 1:6)), collapse = " ")
    stats$RunTime <- paste(as.character(matchfun2("StartTime", 2, "StartTime", 2, cols = 4:9)), collapse = " ")

    # data return object to fill in various things
    returndat <- list()

    # input files
    tempfiles <- matchfun2("Data_File", 0, "Control_File", 0, cols = 1:2)
    stats$Files_used <- paste(c(tempfiles[1, ], tempfiles[2, ]), collapse = " ")
    returndat$Data_File <- tempfiles[1, 2]
    returndat$Control_File <- tempfiles[2, 2]

    # check warnings
    stats$Nwarnings <- nwarn
    if (length(warn) > 20) {
      warn <- c(warn[1:20], paste(
        "Note:", length(warn) - 20,
        "additional lines truncated. Look in",
        warnfile,
        "file to see full list."
      ))
    }
    stats$warnings <- warn

    # likelihoods
    rawlike <- matchfun2("LIKELIHOOD", 2, "Fleet:", -2)
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
    like$lambdas <- lambdas
    # separate new section added in SS version 3.30.13.04 (2019-05-31)
    if (length(laplace_line) > 0) {
      stats$likelihoods_used <- like[1:(laplace_line - 1), ]
      stats$likelihoods_laplace <- like[laplace_line:nrow(like), ]
    } else {
      stats$likelihoods_used <- like
      stats$likelihoods_laplace <- NULL
    }

    # read fleet-specific likelihoods
    likelihoods_by_fleet <- matchfun2("Fleet:", 0, header = TRUE)
    # there was no space before "Parm_devs_detail" prior to 3.30.15.06
    if (!is.null(likelihoods_by_fleet) &&
      "Parm_devs_detail" %in% likelihoods_by_fleet[, 1]) {
      likelihoods_by_fleet <- matchfun2("Fleet:", 0,
        "Parm_devs_detail", -1,
        header = TRUE
      )
    }

    # clean up fleet-specific likelihoods
    likelihoods_by_fleet[likelihoods_by_fleet == "_"] <- NA
    likelihoods_by_fleet <- type.convert(likelihoods_by_fleet, as.is = TRUE)

    # replace numeric column names with fleet names
    names(likelihoods_by_fleet) <- c("Label", "ALL", FleetNames)
    labs <- likelihoods_by_fleet$Label

    # removing ":" at the end of likelihood components
    for (irow in 1:length(labs)) {
      labs[irow] <- substr(labs[irow], 1, nchar(labs[irow]) - 1)
    }
    likelihoods_by_fleet$Label <- labs

    stats$likelihoods_by_fleet <- likelihoods_by_fleet

    likelihoods_by_tag_group <- matchfun2("Tag_Group:", 0, header = TRUE)
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
      likelihoods_by_tag_group$Label[1] <- "Tag_Group"
      stats$likelihoods_by_tag_group <- likelihoods_by_tag_group
    }

    # read detail on parameters devs (if present, 3.30 only)
    Parm_devs_detail <- matchfun2("Parm_devs_detail", 1,
      header = TRUE, type.convert = TRUE
    )
    stats$Parm_devs_detail <- Parm_devs_detail

    # parameters
    parameters <- matchfun2("PARAMETERS", 1, header = TRUE)
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
      any(parameters$Gradient %in% c("dev", "F"))) {
      bad <- parameters$Gradient %in% c("dev", "F")
      parameters$Pr_type[bad] <- parameters$Gradient[bad]
      parameters$Gradient[bad] <- NA
    }
    # make values numeric
    parameters <- type.convert(parameters, as.is = TRUE)

    # convert really old numeric codes to names
    # note that codes used in control file for SS version 3.30 don't match
    # these from earlier models
    # it's possible that SS_output doesn't work for models prior to 3.21, in
    # which case this section could be removed
    if (SS_versionNumeric < 3.21) {
      parameters$Pr_type_numeric <- parameters$Pr_type
      parameters$Pr_type[parameters$Pr_type_numeric == -1] <- "No_prior"
      parameters$Pr_type[parameters$Pr_type_numeric == 0] <- "Normal"
      parameters$Pr_type[parameters$Pr_type_numeric == 1] <- "Sym_Beta"
      parameters$Pr_type[parameters$Pr_type_numeric == 2] <- "Full_Beta"
      parameters$Pr_type[parameters$Pr_type_numeric == 3] <- "Log_Norm"
      parameters$Pr_type[parameters$Pr_type_numeric == 4] <- "Log_Norm_adjusted"
    }

    # fix for duplicate parameter labels in 3.30.03.03,
    # not robust to more than 2 growth patterns but probably will be fixed soon
    ParmLabels <- parameters$Label
    ParmLabels[duplicated(ParmLabels)] <- paste0(ParmLabels[duplicated(ParmLabels)], "_2")
    # end fix
    rownames(parameters) <- ParmLabels

    # names of active parameters
    activepars <- parameters$Label[!is.na(parameters$Active_Cnt)]

    if (!is.na(parfile)) {
      parline <- read.table(parfile, fill = TRUE, comment.char = "", nrows = 1)
    } else {
      parline <- matrix(NA, 1, 16)
    }
    stats$N_estimated_parameters <- parline[1, 6]

    # subset to active parameters only
    pars <- parameters[!is.na(parameters$Active_Cnt), ]

    if (nrow(pars) > 0) {
      pars$Afterbound <- ""
      pars$checkdiff <- pars$Value - pars$Min
      pars$checkdiff2 <- pars$Max - pars$Value
      pars$checkdiff3 <- abs(pars$Value - (pars$Max - (pars$Max - pars$Min) / 2))
      pars$Afterbound[pars$checkdiff < 0.001 | pars$checkdiff2 < 0.001 | pars$checkdiff2 < 0.001] <- "CHECK"
      pars$Afterbound[!pars$Afterbound %in% "CHECK"] <- "OK"
    }
    stats$table_of_phases <- table(parameters$Phase)
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
    for (iname in 1:length(devnames)) {
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
    stats$estimated_non_dev_parameters <- estimated_non_dev_parameters

    # Semi-parametric (2D-AR1) selectivity parameters
    seldev_pars <- parameters[
      grep("ARDEV", parameters$Label, fixed = TRUE),
      names(parameters) %in% c("Label", "Value")
    ]
    if (nrow(seldev_pars) == 0) {
      # if semi-parametric selectivity IS NOT used
      seldev_pars <- NULL
      seldev_matrix <- NULL
    } else {
      # if semi-parametric selectivity IS used

      # parse parameter labels to get info
      seldev_label_info <- strsplit(seldev_pars$Label, split = "_")
      seldev_label_info <- data.frame(do.call(rbind, lapply(seldev_label_info, rbind)))

      # add columns to pars data.frame with info from labels
      seldev_pars$Fleet <- seldev_label_info$X1
      seldev_pars$Year <- as.numeric(substring(seldev_label_info$X3, 2))
      seldev_pars$Type <- ifelse(substring(seldev_label_info$X4, 1, 1) == "a", "age", "length")
      # how many non-numeric digits to skip over in parsing bin value
      first_bin_digit <- ifelse(seldev_pars$Type == "age", 2, 5)
      # parse bin (age or length bin)
      seldev_pars$Bin <- as.numeric(substring(seldev_label_info$X4, first_bin_digit))
      # remove label column which is redundant with rownames
      seldev_pars <- seldev_pars[, -1]

      # make matrix
      seldev_matrix <- list()
      for (fleet in sort(unique(seldev_pars$Fleet))) {
        # subset for specific fleet
        seldev_pars_f <- seldev_pars[seldev_pars$Fleet == fleet, ]
        for (type in unique(seldev_pars_f$Type)) {
          # subset for type (unlikely to have more than 1 per fleet, but safer this way)
          seldev_pars_sub <- seldev_pars_f[seldev_pars_f$Type == type, ]
          seldev_label <- paste0(fleet, "_", type, "_seldevs")
          seldev_yrs <- sort(unique(seldev_pars_sub$Year))
          seldev_bins <- sort(unique(seldev_pars_sub$Bin))
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
                seldev_pars_sub$Value[seldev_pars_sub$Year == y & seldev_pars_sub$Bin == bin]
            }
          } # end loop over years
        } # end loop over types
      } # end loop over fleets
    } # end check for semi-parametric selectivity

    # Dirichlet-Multinomial parameters
    # (new option for comp likelihood that uses these parameters for automated
    #  data weighting)
    DM_pars <- parameters[
      grep("ln(EffN_mult)", parameters$Label, fixed = TRUE),
      names(parameters) %in% c("Value", "Phase", "Min", "Max")
    ]
    DM_pars$Theta <- exp(DM_pars$Value)
    DM_pars$"Theta/(1+Theta)" <- DM_pars$Theta / (1 + DM_pars$Theta)
    # if D-M parameters are present, then do some extra processing steps
    age_data_info <- NULL
    len_data_info <- NULL

    if (nrow(DM_pars) > 0) {
      # save to "stats" list that gets printed to R console
      # (and also added to "returndat" which is returned by this function)
      stats$Dirichlet_Multinomial_pars <- DM_pars

      # figure out which fleet uses which parameter,
      # currently (as of SS version 3.30.10.00), requires reading data file
      if (verbose) {
        message("Reading data.ss_new for info on Dirichlet-Multinomial parameters")
      }
      datfile <- SS_readdat(
        file = file.path(dir, "data.ss_new"),
        verbose = verbose, version = "3.30"
      )
      # deal with case where data file is empty
      if (is.null(datfile)) {
        starter <- SS_readstarter(
          file = file.path(dir, "starter.ss"),
          verbose = verbose
        )
        datfile <- SS_readdat(
          file = file.path(dir, starter$datfile),
          verbose = verbose, version = "3.30"
        )
      }
      age_data_info <- datfile$age_info
      len_data_info <- datfile$len_info
      if (!is.null(age_data_info) & !is.null(len_data_info)) {
        age_data_info$CompError <- as.numeric(age_data_info$CompError)
        age_data_info$ParmSelect <- as.numeric(age_data_info$ParmSelect)
        len_data_info$CompError <- as.numeric(len_data_info$CompError)
        len_data_info$ParmSelect <- as.numeric(len_data_info$ParmSelect)
        if (!any(age_data_info$CompError == 1) & !any(len_data_info$CompError == 1)) {
          stop(
            "Problem Dirichlet-Multinomial parameters: \n",
            "  Report file indicates parameters exist, but no CompError values\n",
            "  in data.ss_new are equal to 1."
          )
        }
      }

      ## get Dirichlet-Multinomial parameter values and adjust input N
      if (comp) { # only possible if CompReport.sso was read
        if (nrow(agedbase) > 0) {
          agedbase$DM_effN <- NA
        }
        if (nrow(lendbase) > 0) {
          lendbase$DM_effN <- NA
        }
        if (nrow(condbase) > 0) {
          condbase$DM_effN <- NA
        }
        # loop over fleets within agedbase
        for (f in unique(agedbase$Fleet)) {
          if (age_data_info$CompError[f] == 1) {
            ipar <- age_data_info$ParmSelect[f]
            if (ipar %in% 1:nrow(DM_pars)) {
              Theta <- DM_pars$Theta[ipar]
            } else {
              stop(
                "Issue with Dirichlet-Multinomial parameter:",
                "Fleet = ", f, "and ParmSelect = ", ipar
              )
            }
            sub <- agedbase$Fleet == f
            agedbase$DM_effN[sub] <-
              1 / (1 + Theta) + agedbase$Nsamp_adj[sub] * Theta / (1 + Theta)
          } # end test for D-M likelihood for this fleet
        } # end loop over fleets within agedbase

        # loop over fleets within lendbase
        for (f in unique(lendbase$Fleet)) {
          if (len_data_info$CompError[f] == 1) {
            ipar <- len_data_info$ParmSelect[f]
            if (ipar %in% 1:nrow(DM_pars)) {
              Theta <- DM_pars$Theta[ipar]
            } else {
              stop(
                "Issue with Dirichlet-Multinomial parameter:",
                "Fleet = ", f, "and ParmSelect = ", ipar
              )
            }
            sub <- lendbase$Fleet == f
            lendbase$DM_effN[sub] <-
              1 / (1 + Theta) + lendbase$Nsamp_adj[sub] * Theta / (1 + Theta)
          } # end test for D-M likelihood for this fleet
        } # end loop over fleets within lendbase

        # loop over fleets within condbase
        for (f in unique(condbase$Fleet)) {
          if (age_data_info$CompError[f] == 1) {
            ipar <- age_data_info$ParmSelect[f]
            if (ipar %in% 1:nrow(DM_pars)) {
              Theta <- DM_pars$Theta[ipar]
            } else {
              stop(
                "Issue with Dirichlet-Multinomial parameter:",
                "Fleet = ", f, "and ParmSelect = ", ipar
              )
            }
            sub <- condbase$Fleet == f
            condbase$DM_effN[sub] <-
              1 / (1 + Theta) + condbase$Nsamp_adj[sub] * Theta / (1 + Theta)
          } # end test for D-M likelihood for this fleet
        } # end loop over fleets within condbase
      } # end test for whether CompReport.sso info is available
    } # end section related to Dirichlet-Multinomial likelihood


    # read covar.sso file
    if (covar) {
      CoVar <- read.table(covarfile, header = TRUE, colClasses = c(rep("numeric", 4), rep("character", 4), "numeric"), skip = covarskip)
      if (verbose) {
        message("Got covar file.")
      }
      stdtable <- CoVar[CoVar$Par..j == "Std", c(7, 9, 5)]
      names(stdtable) <- c("name", "std", "type")
      N_estimated_parameters2 <- sum(stdtable$type == "Par")

      # this section was muddling Derived Quants with Parameters in early version of SSv3.20
      # got work-around pending fix from Rick to use of "Par" vs. "Der" in covar file.
      if (is.na(stats$N_estimated_parameters)) {
        stats$N_estimated_parameters <- N_estimated_parameters2
      } else {
        if (stats$N_estimated_parameters != N_estimated_parameters2) {
          warning(
            stats$N_estimated_parameters,
            "estimated parameters indicated by", parfile, "\n",
            N_estimated_parameters2,
            "estimated parameters shown in", covarfile, "\n",
            "returning the first value:", stats$N_estimated_parameters
          )
          stats$N_estimated_parameters <- stats$N_estimated_parameters
        }
      }
      Nstd <- sum(stdtable$std > 0)
      checkbadrun <- unique(stdtable$std)
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
      if (checkcor == TRUE & stats$N_estimated_parameters > 1) {
        corfilter <- CoVar[CoVar$all.i != CoVar$all.j &
          CoVar$Par..i == "Par" &
          CoVar$Par..j == "Par" &
          CoVar$label.i %in% activepars &
          CoVar$label.j %in% activepars &
          !substr(CoVar$label.i, 1, 8) == "ForeRecr" &
          !substr(CoVar$label.j, 1, 8) == "ForeRecr", ]
        rangecor <- range(abs(corfilter$corr))
        corstats <- list()
        corstats$cormessage1 <- paste("Range of abs(parameter correlations) is", min(rangecor), "to", max(rangecor))
        # search for high or low correlations in covar file
        highcor <- corfilter[abs(corfilter$corr) >= cormax, names(CoVar) %in% c("label.i", "label.j", "corr")]
        lowcorcandidates <- corfilter[abs(corfilter$corr) <= cormin, names(CoVar) %in% c("label.i", "label.j", "corr")]
        lowcortestlist <- data.frame(unique(c(lowcorcandidates$label.i, lowcorcandidates$label.j)))
        lowcortestlist$name <- as.character(lowcortestlist[, 1])
        nlowcor <- 0
        lowcor <- 0
        if (nrow(lowcortestlist) > 0) {
          lowcortestlist$max <- NA
          for (i in 1:length(lowcortestlist[, 1]))
          {
            lowcortestlist$max[i] <- max(corfilter$corr[corfilter$label.i == lowcortestlist$name[i]], corfilter$corr[corfilter$label.j == lowcortestlist$name[i]])
          }
          lowcor <- lowcortestlist[abs(lowcortestlist$max) <= cormin, 2:3]
          nlowcor <- nrow(lowcor)
        }
        nhighcor <- nrow(highcor)
        if (printhighcor > 0) {
          if (nhighcor == 0) textblock <- "No correlations"
          if (nhighcor == 1) textblock <- "1 correlation"
          if (nhighcor > 1) textblock <- paste(nhighcor, "correlations")
          corstats$cormessage2 <- paste(textblock, " above threshold (cormax=", cormax, ")", sep = "")
          if (nhighcor > 0 & nhighcor <= printhighcor) {
            row.names(highcor) <- paste("   ", 1:nhighcor)
            corstats$cormessage3 <- highcor
          }
          if (nhighcor > 0 & nhighcor > printhighcor) {
            highcorsub <- highcor[order(-abs(highcor$corr)), ]
            highcorsub <- highcorsub[1:printhighcor, ]
            row.names(highcorsub) <- paste("   ", 1:printhighcor)
            corstats$cormessage4 <- paste(
              "Highest", printhighcor,
              "parameter correlations above threshold (to print more, increase 'printhighcor' input):"
            )
            corstats$cormessage5 <- highcorsub
          }
        } else {
          corstats$cormessage6 <- "High correlations not reported. To report, change 'printhighcor' input to a positive value."
        }

        if (printlowcor > 0) {
          if (nlowcor == 0) textblock <- "No uncorrelated parameters"
          if (nlowcor == 1) textblock <- "1 uncorrelation"
          if (nlowcor > 1) textblock <- paste(nlowcor, "uncorrelated parameters")
          corstats$cormessage7 <- paste(textblock, " below threshold (cormin=", cormin, ")", sep = "")
          if (nlowcor > 0 & nlowcor <= printlowcor) {
            corstats$cormessage8 <- lowcor
          }
          if (nlowcor > 0 & nlowcor > printlowcor) {
            lowcorsub <- lowcor[order(abs(lowcor$max)), ]
            lowcorsub <- lowcorsub[1:printlowcor, ]
            corstats$cormessage9 <- paste(
              "Lowest", printlowcor,
              "parameters uncorrelations below threshold (to print more, increase 'printlowcor' input):"
            )
            corstats$cormessage10 <- lowcorsub
          }
        } else {
          corstats$cormessage11 <- "Uncorrelated parameters not reported. To report, change 'printlowcor' input to a positive value."
        }
      } else { # if checkcor = FALSE or only 1 estimated parameter
        corstats <- NA
        if (verbose) {
          message("You skipped the correlation check (or have only 1 parameter)")
        }
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
    der <- matchfun2("DERIVED_QUANTITIES", 4, header = TRUE)
    # make older SS output names match current SS output conventions
    der <- df.rename(der, oldnames = "LABEL", newnames = "Label")

    # remove extra row (don't remember why it occurs)
    der <- der[der$Label != "Bzero_again", ]
    der[der == "_"] <- NA
    der[der == ""] <- NA

    # remove bad rows that were present in 3.30-beta in September 2016
    # (note that spelling differs from "Parm_devs_detail" after likelihood)
    test <- grep("Parm_dev_details", der$Label)
    if (length(test) > 0) {
      der <- der[1:(min(test) - 1), ]
    }
    # convert columns to numeric
    der <- type.convert(der, as.is = TRUE)

    # replace SPB with SSB as changed in SS version 3.30.10.00 (29 Nov. 2017)
    der$Label <- gsub("SPB_", "SSB_", der$Label, fixed = TRUE)
    # set rownames equal to Label column
    # (skipping any duplicates, such as ln(SPB)_YYYY for models with limited year range)
    rownames(der)[!duplicated(der$Label)] <- der$Label[!duplicated(der$Label)]

    # get management ratio labels from top of DERIVED_QUANTITIES
    managementratiolabels <- matchfun2("DERIVED_QUANTITIES", 1, "DERIVED_QUANTITIES", 3, cols = 1:2)
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
    MGparmAdj <- matchfun2("MGparm_By_Year_after_adjustments", 1,
      header = TRUE, type.convert = TRUE
    )
    # make older SS output names match current SS output conventions
    MGparmAdj <- df.rename(MGparmAdj, oldnames = "Year", newnames = "Yr")

    # time-varying size-selectivity parameters
    SelSizeAdj <- matchfun2("selparm(Size)_By_Year_after_adjustments", 2)
    if (is.null(SelSizeAdj) || nrow(SelSizeAdj) <= 2) {
      SelSizeAdj <- NULL
    } else {
      SelSizeAdj <- SelSizeAdj[, apply(SelSizeAdj, 2, emptytest) < 1]
      SelSizeAdj[SelSizeAdj == ""] <- NA
      # make values numeric
      SelSizeAdj <- type.convert(SelSizeAdj, as.is = TRUE)
      # provide column names (first test for extra column added in 3.30.06.02)
      if (rawrep[matchfun("selparm(Size)_By_Year_after_adjustments") + 1, 3]
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
    SelAgeAdj <- matchfun2("selparm(Age)_By_Year_after_adjustments", 2)
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
        if (rawrep[matchfun("selparm(Age)_By_Year_after_adjustments") + 1, 3]
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
    recruitment_dist <- matchfun2("RECRUITMENT_DIST", 1,
      header = TRUE, type.convert = TRUE
    )
    if (!is.null(recruitment_dist)) {
      # calculate first season with recruitment
      if ("Frac/sex" %in% names(recruitment_dist)) {
        first_seas_with_recruits <-
          min(recruitment_dist$Seas[recruitment_dist$"Frac/sex" > 0])
      } else {
        first_seas_with_recruits <-
          min(recruitment_dist$Seas[recruitment_dist$Value > 0])
      }
      # starting in SSv3.24Q there are additional tables
      # (in v3.30 RECRUITMENT_DIST_BENCHMARK was renamed RECRUITMENT_DIST_Bmark
      # and RECRUITMENT_DIST_FORECAST was renamed RECRUITMENT_DIST_endyr)
      recruit_dist_Bmark <- matchfun2("RECRUITMENT_DIST_B", 1,
        header = TRUE, type.convert = TRUE
      )
      if (!is.null(recruit_dist_Bmark)) {
        if (SS_versionNumeric < 3.30) {
          recruit_dist_endyr <- matchfun2("RECRUITMENT_DIST_FORECAST", 1,
            header = TRUE, type.convert = TRUE
          )
        } else {
          recruit_dist_endyr <- matchfun2("RECRUITMENT_DIST_endyr", 1,
            header = TRUE, type.convert = TRUE
          )
        }
        # bundle original and extra tables into a list
        recruitment_dist <- list(
          recruit_dist = recruitment_dist,
          recruit_dist_Bmark = recruit_dist_Bmark,
          recruit_dist_endyr = recruit_dist_endyr
        )
      }
    }

    # gradient
    if (covar & !is.na(corfile)) {
      stats$log_det_hessian <- read.table(corfile, nrows = 1)[1, 10]
    }
    stats$maximum_gradient_component <-
      as.numeric(matchfun2("Convergence_Level", 0,
        "Convergence_Level", 0,
        cols = 2
      ))

    # parameters with highest gradients (3.30 only)
    if ("Gradient" %in% names(parameters)) {
      if (any(!is.na(parameters$Gradient))) {
        # number of gradients to report is 5 (an arbitrary choice),
        # or fewer if fewer than 5 parameters estimated.
        ngrads <- min(5, max(parameters$Active_Cnt, na.rm = TRUE))
        # add highest gradients to table of stats that get printed to the console
        stats$parameters_with_highest_gradients <-
          head(parameters[
            order(abs(parameters$Gradient), decreasing = TRUE),
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
    srhead <- matchfun2("SPAWN_RECRUIT", 0,
      "SPAWN_RECRUIT", last_row_index,
      cols = 1:6
    )
    # account for extra blank line in early 3.30 versions (at least 3.30.01)
    if (all(srhead[7, ] == "")) {
      last_row_index <- 12
      srhead <- matchfun2("SPAWN_RECRUIT", 0,
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
      rmse_table <- as.data.frame(srhead[-(1:(last_row_index - 1)), 1:5])
      rmse_table <- rmse_table[!grepl("SpawnBio", rmse_table[, 2]), ]
      rmse_table <- type.convert(rmse_table, as.is = TRUE)
      names(rmse_table) <- srhead[last_row_index - 1, 1:5]
      names(rmse_table)[4] <- "RMSE_over_sigmaR"
      sigma_R_in <- as.numeric(srhead[grep("sigmaR", srhead[, 2]), 1])
      rmse_table <- rmse_table
      row.names(rmse_table) <- NULL

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
    raw_recruit <- matchfun2("SPAWN_RECRUIT", last_row_index + 1)
    if (!is.null(raw_recruit) && raw_recruit[1, 1] == "S/Rcurve") {
      raw_recruit <- matchfun2("SPAWN_RECRUIT", last_row_index)
    }
    # account for extra blank line in 3.30.01 (and maybe similar versions)
    if (!is.null(raw_recruit) &&
      nrow(raw_recruit) < length(startyr:endyr)) {
      raw_recruit <- matchfun2("SPAWN_RECRUIT", last_row_index + 1,
        which_blank = 2
      )
      if (raw_recruit[1, 1] == "S/Rcurve") {
        raw_recruit <- matchfun2("SPAWN_RECRUIT", last_row_index,
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
      recruit$dev[recruit$dev == "-nan(ind)"] <- NA

      # make values numeric
      recruit <- type.convert(recruit, as.is = TRUE)

      # make older SS output names match current SS output conventions
      recruit <- df.rename(recruit,
        oldnames = c("year", "spawn_bio", "adjusted"),
        newnames = c("Yr", "SpawnBio", "bias_adjusted")
      )
    }

    # starting in 3.30.11.00, a table with the full spawn recr curve was added
    SPAWN_RECR_CURVE <- NULL
    if (!is.na(matchfun("Full_Spawn_Recr_Curve"))) {
      SPAWN_RECR_CURVE <- matchfun2("Full_Spawn_Recr_Curve", 1,
        header = TRUE, type.convert = TRUE
      )
    }
    # section was renamed in 3.30.15.06
    if (!is.na(matchfun("SPAWN_RECR_CURVE"))) {
      SPAWN_RECR_CURVE <- matchfun2("SPAWN_RECR_CURVE", 1,
        header = TRUE, type.convert = TRUE
      )
    }

    ## FIT_LEN_COMPS
    if (SS_versionNumeric >= 3.30) {
      # This section hasn't been read by SS_output in the past,
      # not bother adding to models prior to 3.30
      fit_len_comps <- matchfun2("FIT_LEN_COMPS", 1, header = TRUE)
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
      lenntune <- matchfun2("FIT_AGE_COMPS", -(nfleets + 2),
        "FIT_AGE_COMPS", -1,
        cols = 1:10, header = TRUE
      )
      names(lenntune)[10] <- "FleetName"
      # convert underscores
      lenntune[lenntune == "_"] <- NA
      # reorder columns (leaving out sample sizes perhaps to save space)
      lenntune <- lenntune[lenntune$N > 0, c(10, 1, 4:9)]
      # avoid NA warnings by removing #IND values
      lenntune$"MeaneffN/MeaninputN"[lenntune$"MeaneffN/MeaninputN" == "-1.#IND"] <- NA
      lenntune <- type.convert(lenntune, as.is = TRUE)
      lenntune$"HarMean/MeanInputN" <- lenntune$"HarMean(effN)" / lenntune$"mean(inputN*Adj)"
    } else {
      # new in 3.30 has keyword at top
      lenntune <- matchfun2("Length_Comp_Fit_Summary", 1, header = TRUE)
      if (!is.null(lenntune)) {
        lenntune <- df.rename(lenntune,
          oldnames = c("FleetName"),
          newnames = c("Fleet_name")
        )
        if ("Factor" %in% names(lenntune)) {
          # format starting with 3.30.12 doesn't need adjustment, just convert to numeric
          lenntune <- type.convert(lenntune, as.is = TRUE)
        } else {
          # process 3.30 versions prior to 3.30.12
          # reorder columns (leaving out sample sizes perhaps to save space)
          lenntune <- lenntune[lenntune$Nsamp_adj > 0, ]
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
    stats$Length_Comp_Fit_Summary <- lenntune

    ## FIT_AGE_COMPS
    fit_age_comps <- matchfun2("FIT_AGE_COMPS", 1, header = TRUE)
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
      agentune <- matchfun2("FIT_SIZE_COMPS", -(nfleets + 2),
        "FIT_SIZE_COMPS", -2,
        cols = 1:10, header = TRUE
      )
    } else {
      # 3.30 version has keyword (if included in output)
      # and requires little processing
      start <- matchfun("Age_Comp_Fit_Summary")
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
        agentune <- matchfun2("Age_Comp_Fit_Summary",
          adjust1 = adjust1,
          header = TRUE, which_blank = which_blank
        )
      }
    } # end 3.30 version
    agentune <- df.rename(agentune,
      oldnames = c("FleetName", "N"),
      newnames = c("Fleet_name", "Nsamp_adj")
    )

    if ("Factor" %in% names(agentune)) {
      # format starting with 3.30.12 doesn't need adjustment, just convert to numeric
      agentune <- type.convert(agentune, as.is = TRUE)
    } else {
      if (!is.null(dim(agentune))) {
        names(agentune)[ncol(agentune)] <- "Fleet_name"
        # convert underscores
        agentune[agentune == "_"] <- NA
        # remove empty rows with NA or zero sample size
        agentune <- agentune[!is.na(agentune$Nsamp_adj) &
          agentune$Nsamp_adj > 0, ]
        # avoid NA warnings by removing #IND values
        agentune$"MeaneffN/MeaninputN"[agentune$"MeaneffN/MeaninputN" == "-1.#IND"] <- NA
        agentune <- type.convert(agentune, as.is = TRUE)
        # calculate ratio to be more transparent
        agentune$"HarMean(effN)/mean(inputN*Adj)" <-
          agentune$"HarMean(effN)" / agentune$"mean(inputN*Adj)"

        # calculate recommended value (for length data this is done internally in SS)
        agentune$Recommend_Var_Adj <-
          agentune$Var_Adj * agentune$"HarMean(effN)/mean(inputN*Adj)"

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
    stats$Age_Comp_Fit_Summary <- agentune

    ## FIT_SIZE_COMPS
    fit_size_comps <- NULL
    if (SS_versionNumeric >= 3.30) {
      # test for SS version 3.30.12 and beyond which doesn't include
      # the label "Size_Comp_Fit_Summary"
      if (!is.na(matchfun("FIT_SIZE_COMPS"))) {
        # note that there are hashes in between sub-sections,
        # so using rep_blank_lines instead of default
        # rep_blank_or_hash_lines to find ending
        fit_size_comps <- matchfun2("FIT_SIZE_COMPS", 1,
          header = FALSE,
          blank_lines = rep_blank_lines
        )
        if (!is.null(dim(fit_size_comps)) &&
          nrow(fit_size_comps) > 0 &&
          fit_size_comps[1, 1] != "#_none") {
          # column names
          names(fit_size_comps) <- fit_size_comps[2, ]
          # add new columns for method-specific info
          fit_size_comps$Method <- NA
          fit_size_comps$Units <- NA
          fit_size_comps$Scale <- NA
          fit_size_comps$Add_to_comp <- NA
          # find the lines with the method-specific info
          method_lines <- grep("#Method:", fit_size_comps[, 1])
          method_info <- fit_size_comps[method_lines, ]
          tune_lines <- grep("Factor", fit_size_comps[, 1])
          sizentune <- NULL
          # loop over methods to fill in new columns
          for (imethod in 1:length(method_lines)) {
            start <- method_lines[imethod]
            if (imethod != length(method_lines)) {
              end <- method_lines[imethod + 1] - 1
            } else {
              end <- nrow(fit_size_comps)
            }
            fit_size_comps$Method[start:end] <- method_info[imethod, 2]
            fit_size_comps$Units[start:end] <- method_info[imethod, 4]
            fit_size_comps$Scale[start:end] <- method_info[imethod, 6]
            fit_size_comps$Add_to_comp[start:end] <- method_info[imethod, 8]
            # split out rows with info on tuning
            sizentune <- rbind(sizentune, fit_size_comps[tune_lines[imethod]:end, ])
          }
          # format sizentune (info on tuning) has been split into
          # a separate data.frame, needs formatting: remove extra columns, change names
          goodcols <- c(
            1:grep("name", tolower(sizentune[1, ])),
            grep("Method", names(sizentune))
          )
          sizentune[1, max(goodcols)] <- "Method"
          sizentune <- sizentune[, goodcols]
          names(sizentune) <- sizentune[1, ]
          sizentune <- sizentune[sizentune$Factor == 7, ]
          sizentune <- type.convert(sizentune, as.is = TRUE)
          stats$Size_Comp_Fit_Summary <- sizentune
          # format fit_size_comps: remove extra rows, make numeric
          fit_size_comps <- fit_size_comps[fit_size_comps$Fleet_Name %in% FleetNames, ]
        } # end check for non-empty fit_size_comps
      } else {
        # formatting used for earlier 3.30 versions (prior to 3.30.12)
        fit_size_comps <- matchfun2("FIT_SIZE_COMPS", 1,
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
        sizentune <- matchfun2("Size_Comp_Fit_Summary", 1, "OVERALL_COMPS", -1,
          cols = 1:10, header = TRUE
        )
        if (!is.null(dim(sizentune))) {
          sizentune[, 1] <- sizentune[, 10]
          sizentune <- sizentune[sizentune$Npos > 0, c(1, 3, 4, 5, 6, 8, 9)]
        } else {
          sizentune <- NULL
        }
      }
      stats$Size_comp_Eff_N_tuning_check <- sizentune
    }

    # get information that will help diagnose jitter coverage and bad bounds
    jitter_info <- parameters[
      !is.na(parameters$Active_Cnt) &
        !is.na(parameters$Min),
      c("Value", "Min", "Max", "Init")
    ]
    jitter_info$sigma <- (jitter_info$Max - jitter_info$Min) / (2 * qnorm(.999))
    jitter_info$CV <- jitter_info$sigma / jitter_info$Init
    jitter_info$InitLocation <- pnorm(
      q = jitter_info$Init,
      mean = (jitter_info$Max + jitter_info$Min) / 2,
      sd = jitter_info$sigma
    )


    if (verbose) {
      message("Finished primary run statistics list")
    }
    flush.console()

    # add stuff to list to return
    if (SS_versionNumeric <= 3.24) {
      returndat$definitions <- fleetdefs
      returndat$fleet_ID <- fleet_ID
      returndat$fleet_area <- fleet_area
      returndat$catch_units <- catch_units
      returndat$catch_error <- catch_error
    }
    if (SS_versionNumeric >= 3.30) {
      returndat$definitions <- fleetdefs
      returndat$fleet_ID <- fleet_ID
      returndat$fleet_type <- fleet_type
      returndat$fleet_timing <- fleet_timing
      returndat$fleet_area <- fleet_area
      returndat$catch_units <- catch_units
      if (exists("catch_se")) {
        returndat$catch_se <- catch_se
        returndat$equ_catch_se <- equ_catch_se
      } else {
        returndat$catch_se <- NA
        returndat$equ_catch_se <- NA
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

    returndat$mcmc <- mcmc
    returndat$survey_units <- survey_units
    returndat$survey_error <- survey_error
    returndat$IsFishFleet <- IsFishFleet
    returndat$nfishfleets <- nfishfleets

    returndat$nfleets <- nfleets
    returndat$nsexes <- nsexes
    returndat$ngpatterns <- ngpatterns
    returndat$lbins <- lbins
    returndat$Lbin_method <- Lbin_method
    returndat$nlbins <- nlbins
    returndat$lbinspop <- lbinspop
    returndat$nlbinspop <- nlbinspop
    returndat$sizebinlist <- sizebinlist
    returndat$age_data_info <- age_data_info
    returndat$len_data_info <- len_data_info
    returndat$agebins <- agebins
    returndat$nagebins <- nagebins
    returndat$accuage <- accuage
    returndat$nareas <- nareas
    returndat$startyr <- startyr
    returndat$endyr <- endyr
    returndat$nseasons <- nseasons
    returndat$seasfracs <- seasfracs
    returndat$seasdurations <- seasdurations
    returndat$N_sub_seasons <- return.def("N_sub_seasons")
    returndat$Spawn_month <- return.def("Spawn_month")
    returndat$Spawn_seas <- return.def("Spawn_seas")
    returndat$Spawn_timing_in_season <- return.def("Spawn_timing_in_season")
    returndat$Retro_year <- return.def("Retro_year")
    returndat$N_forecast_yrs <- return.def("N_forecast_yrs")
    returndat$Empirical_wt_at_age <- return.def("Empirical_wt_at_age")
    returndat$N_bio_patterns <- return.def("N_bio_patterns")
    returndat$N_platoons <- return.def("N_platoons")
    returndat$NatMort_option <- return.def("NatMort_option")
    returndat$GrowthModel_option <- return.def("GrowthModel_option")
    returndat$Maturity_option <- return.def("Maturity_option")
    returndat$Fecundity_option <- return.def("Fecundity_option")
    returndat$Start_from_par <- return.def("Start_from_par")
    returndat$Do_all_priors <- return.def("Do_all_priors")
    returndat$Use_softbound <- return.def("Use_softbound")
    returndat$N_nudata <- return.def("N_nudata")
    returndat$Max_phase <- return.def("Max_phase")
    returndat$Current_phase <- return.def("Current_phase")
    returndat$Jitter <- return.def("Jitter")
    returndat$ALK_tolerance <- return.def("ALK_tolerance")
    returndat$nforecastyears <- nforecastyears
    returndat$morph_indexing <- morph_indexing
    returndat$MGparmAdj <- MGparmAdj
    returndat$forecast_selectivity <- forecast_selectivity
    returndat$SelSizeAdj <- SelSizeAdj
    returndat$SelAgeAdj <- SelAgeAdj
    returndat$recruitment_dist <- recruitment_dist
    returndat$recruit <- recruit
    returndat$SPAWN_RECR_CURVE <- SPAWN_RECR_CURVE
    returndat$breakpoints_for_bias_adjustment_ramp <-
      breakpoints_for_bias_adjustment_ramp

    # Static growth
    # note: keyword "BIOLOGY" was not unique enough at some point
    #       but revision on 11 June 2020 seems to be working so far
    # formatting change in 3.30.15.06 puts table one line lower
    biology <- matchfun2("BIOLOGY",
      adjust1 = ifelse(custom, 2, 1),
      header = TRUE, type.convert = TRUE
    )

    # determine fecundity type
    FecType <- 0
    # get parameter labels
    pl <- parameters$Label
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
      lbinspop <- biology$Low[biology$GP == 1]
    }

    # add things to list getting returned
    returndat$biology <- biology
    returndat$FecType <- FecType
    returndat$FecPar1name <- FecPar1name
    returndat$FecPar2name <- FecPar2name

    returndat$FecPar1 <- parameters$Value[parameters$Label == FecPar1name]
    returndat$FecPar2 <- parameters$Value[parameters$Label == FecPar2name]

    # warning for 3.30 models with multiple growth patterns that have
    # repeat fecundity values, likely to be sorted out in new SS version
    if (length(returndat$FecPar1) > 1) {
      warning(
        "Plots will only show fecundity and related quantities",
        "for Growth Pattern 1"
      )
      returndat$FecPar1 <- returndat$FecPar1[1]
      returndat$FecPar2 <- returndat$FecPar2[2]
    }

    # simple test to figure out if fecundity is proportional to spawning biomass:
    returndat$SpawnOutputUnits <-
      ifelse(!is.na(biology$Fecundity[1]) &&
        all(biology$Wt_len_F == biology$Fecundity),
      "biomass", "numbers"
      )

    # get natural mortality type and vectors of M by age
    adjust1 <- ifelse(custom, 2, 1)
    M_type <- rawrep[matchfun("Natural_Mortality") + adjust1 - 1, 2]
    M_type <- as.numeric(gsub(
      pattern = ".*([0-9]+)",
      replacement = "\\1",
      x = M_type
    ))
    # in SS 3.30 the number of rows of Natural_Mortality is the product of
    # the number of sexes, growth patterns, settlement events but settlement
    # events didn't exist in 3.24
    M_Parameters <- matchfun2("Natural_Mortality",
      adjust1 = adjust1,
      header = TRUE,
      type.convert = TRUE
    )
    Natural_Mortality_Bmark <- matchfun2("Natural_Mortality_Bmark",
      adjust1 = 1,
      header = TRUE,
      type.convert = TRUE
    )
    Natural_Mortality_endyr <- matchfun2("Natural_Mortality_endyr",
      adjust1 = 1,
      header = TRUE,
      type.convert = TRUE
    )
    returndat$M_type <- M_type
    returndat$Natural_Mortality_Bmark <- Natural_Mortality_Bmark
    returndat$Natural_Mortality_endyr <- Natural_Mortality_endyr

    # get growth parameters
    Growth_Parameters <- matchfun2("Growth_Parameters", 1,
      "Growth_Parameters", 1 + ngpatterns,
      header = TRUE, type.convert = TRUE
    )
    returndat$Growth_Parameters <- Growth_Parameters

    Seas_Effects <- matchfun2("Seas_Effects", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat$Seas_Effects <- Seas_Effects

    # ending year growth, including pattern for the CV (added in SSv3.22b_Aug3)
    # CVtype will occur on same line or following
    growthCVtype <- matchfun2("Biology_at_age", 0,
      "Biology_at_age", 1,
      header = FALSE
    )
    growthCVtype <- grep("endyr_with_", unlist(growthCVtype), value = TRUE)
    if (length(growthCVtype) > 0) {
      returndat$growthCVtype <- strsplit(growthCVtype,
        split = "endyr_with_"
      )[[1]][2]
    } else {
      returndat$growthCVtype <- "unknown"
    }
    # formatting change in 3.30.15.06 puts table one line lower
    growdat <- matchfun2("Biology_at_age",
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
      nmorphs <- max(growdat$Morph)
      midmorphs <- c(c(0, nmorphs / nsexes) + ceiling(nmorphs / nsexes / 2))
    }
    returndat$endgrowth <- growdat

    # test for use of empirical weight-at-age input file (wtatage.ss)
    test <- matchfun2("MEAN_BODY_WT(begin)", 0,
      "MEAN_BODY_WT(begin)", 0,
      header = FALSE
    )
    wtatage_switch <- length(grep("wtatage.ss", test)) > 0
    returndat$wtatage_switch <- wtatage_switch

    # mean body weight
    mean_body_wt <- matchfun2("MEAN_BODY_WT(begin)", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat$mean_body_wt <- mean_body_wt

    # Time-varying growth
    mean_size <- matchfun2("MEAN_SIZE_TIMESERIES", 1,
      "mean_size_Jan_1", -2,
      cols = 1:(4 + accuage + 1),
      header = TRUE,
      type.convert = TRUE
    )
    growthvaries <- FALSE
    if (!is.null(mean_size)) {
      if (SS_versionNumeric < 3.30) {
        mean_size <- mean_size[mean_size$Beg == 1 &
          mean_size$Yr >= startyr &
          mean_size$Yr < endyr, ]
      } else {
        mean_size <- mean_size[mean_size$SubSeas == 1 &
          mean_size$Yr >= startyr &
          mean_size$Yr < endyr, ]
      }
      if (nseasons > 1) {
        mean_size <- mean_size[mean_size$Seas == 1, ]
      }
      if (length(unique(mean_size$Yr)) > 1) {
        growthvaries <- TRUE
      }
      returndat$growthseries <- mean_size
      returndat$growthvaries <- growthvaries
    }

    # Length-based selectivity and retention
    if (!forecast) {
      sizeselex <- sizeselex[sizeselex$Yr <= endyr, ]
    }
    returndat$sizeselex <- sizeselex

    # Age-based selectivity
    ageselex <- matchfun2("AGE_SELEX", 4, header = TRUE)
    if (!is.null(ageselex)) {
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
        ageselex <- ageselex[ageselex$Yr <= endyr, ]
      }
      # make values numeric
      ageselex <- type.convert(ageselex, as.is = TRUE)
    }
    returndat$ageselex <- ageselex

    # EXPLOITATION
    # read first 20 rows to figure out where meta-data ends
    exploitation_head <- matchfun2("EXPLOITATION", 1,
      "EXPLOITATION", 20,
      header = FALSE
    )
    # check for new header info added in 3.30.13_beta (14 Feb. 2019)
    if (exploitation_head[1, 1] == "Info:") {
      # NOTE: add read of additional header info here
      exploitation <- matchfun2("EXPLOITATION",
        which(exploitation_head[, 1] == "Yr"),
        header = TRUE
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
      exploitation <- matchfun2("EXPLOITATION", 5, header = TRUE)
      # get numeric value for F_method
      F_method <- as.numeric(rawrep[matchfun("F_Method"), 2])
    }
    returndat$F_method <- F_method

    if (!is.null(exploitation)) {
      # more processing of exploitation (if present)
      exploitation[exploitation == "_"] <- NA
      # make text numeric
      # "init_yr" not used as of 3.30.13, but must have been in the past
      # "INIT" appears to be used in 3.30.13 and beyond
      exploitation$Yr[exploitation$Yr %in% c("INIT", "init_yr")] <- startyr - 1
      # make columns numeric
      exploitation <- type.convert(exploitation, as.is = TRUE)
    }
    returndat$exploitation <- exploitation

    # catch
    catch <- matchfun2("CATCH", 1, substr1 = FALSE, header = TRUE)
    # if table is present, then do processing of it
    if (!is.null(catch)) {
      # update to new column names used starting with 3.30.13
      catch <- df.rename(catch,
        oldnames = c("Name", "Yr.frac"),
        newnames = c("Fleet_Name", "Time")
      )
      # fix likelihood associated with 0 catch
      catch$Like[catch$Like == "-1.#IND"] <- NA
      # change "INIT" or "init" to year value following convention used elsewhere
      catch$Yr[tolower(catch$Yr) == "init"] <- startyr - 1
      # make columns numeric
      catch <- type.convert(catch, as.is = TRUE)
    }
    returndat$catch <- catch

    # age associated with summary biomass
    summary_age <- rawrep[matchfun("TIME_SERIES"), ifelse(custom, 3, 2)]
    summary_age <- as.numeric(substring(summary_age, nchar("BioSmry_age:_") + 1))
    returndat$summary_age <- summary_age
    # time series
    timeseries <- matchfun2("TIME_SERIES", 1, header = TRUE)
    # temporary fix for 3.30.03.06
    timeseries <- timeseries[timeseries$Seas != "recruits", ]

    timeseries[timeseries == "_"] <- NA
    timeseries <- type.convert(timeseries, as.is = TRUE)
    ## # sum catches and other quantities across fleets
    ## # commented out pending additional test for more than one fleet with catch,
    ## # without which the apply function has errors
    ## timeseries$dead_B_sum <- apply(timeseries[,grep("dead(B)",names(timeseries),
    ##                                                 fixed=TRUE)], 1, sum)
    ## timeseries$dead_N_sum <- apply(timeseries[,grep("dead(N)",names(timeseries),
    ##                                                 fixed=TRUE)], 1, sum)
    ## timeseries$retain_B_sum <- apply(timeseries[,grep("retain(B)",names(timeseries),
    ##                                                   fixed=TRUE)], 1, sum)
    ## timeseries$retain_N_sum <- apply(timeseries[,grep("retain(N)",names(timeseries),
    ##                                                   fixed=TRUE)], 1, sum)
    ## timeseries$sel_B_sum <- apply(timeseries[,grep("sel(B)",names(timeseries),
    ##                                                fixed=TRUE)], 1, sum)
    ## timeseries$sel_N_sum <- apply(timeseries[,grep("sel(N)",names(timeseries),
    ##                                                fixed=TRUE)], 1, sum)
    ## timeseries$obs_cat_sum <- apply(timeseries[,grep("obs_cat",names(timeseries),
    ##                                                  fixed=TRUE)], 1, sum)

    returndat$timeseries <- timeseries

    # get spawning season
    # currently (v3.20b), Spawning Biomass is only calculated
    # in a unique spawning season within the year
    if (!exists("spawnseas")) {
      spawnseas <- unique(timeseries$Seas[!is.na(timeseries$SpawnBio)])

      # problem with spawning season calculation when NA values in SpawnBio
      if (length(spawnseas) == 0) {
        spawnseas <- NA
      }
    }
    returndat$spawnseas <- spawnseas

    # set mainmorphs as those morphs born in the first season with recruitment
    # and the largest fraction of the platoons (should equal middle platoon when present)
    if (is.null(morph_indexing)) {
      mainmorphs <- NULL
    } else {
      if (SS_versionNumeric >= 3.30) {
        # new "platoon" label
        temp <- morph_indexing[morph_indexing$BirthSeas ==
          first_seas_with_recruits &
          morph_indexing$Platoon_Dist ==
            max(morph_indexing$Platoon_Dist), ]
        mainmorphs <- min(temp$Index[temp$Sex == 1])
        if (nsexes == 2) {
          mainmorphs <- c(mainmorphs, min(temp$Index[temp$Sex == 2]))
        }
      }
      if (SS_versionNumeric < 3.30) {
        # old "sub_morph" label
        temp <- morph_indexing[morph_indexing$BirthSeas ==
          first_seas_with_recruits &
          morph_indexing$Sub_Morph_Dist ==
            max(morph_indexing$Sub_Morph_Dist), ]
        mainmorphs <- min(temp$Index[temp$Sex == 1])
        if (nsexes == 2) {
          mainmorphs <- c(mainmorphs, min(temp$Index[temp$Sex == 2]))
        }
      }
      if (length(mainmorphs) == 0) {
        warning("Error with morph indexing")
      }
    }
    returndat$mainmorphs <- mainmorphs

    # get birth seasons as vector of seasons with non-zero recruitment
    birthseas <- sort(unique(timeseries$Seas[timeseries$Recruit_0 > 0]))
    # temporary fix for model with missing Recruit_0 values
    # (so far this has only been seen in one 3.30 model with 2 GPs)
    if (length(birthseas) == 0) {
      birthseas <- sort(unique(morph_indexing$BirthSeas))
    }
    returndat$birthseas <- birthseas

    # stats and dimensions
    timeseries$Yr <- timeseries$Yr + (timeseries$Seas - 1) / nseasons
    ts <- timeseries[timeseries$Yr <= endyr + 1, ]
    tsyears <- ts$Yr[ts$Seas == 1]

    # Depletion
    tsspaw_bio <- ts$SpawnBio[ts$Seas == spawnseas & ts$Area == 1]
    if (nareas > 1) # loop over areas if necessary to sum spawning biomass
      {
        for (a in 2:nareas) {
          tsspaw_bio <- tsspaw_bio + ts$SpawnBio[ts$Seas == spawnseas &
            ts$Area == a]
        }
      }
    if (nsexes == 1) {
      tsspaw_bio <- tsspaw_bio / 2
    }
    depletionseries <- tsspaw_bio / tsspaw_bio[1]
    stats$SBzero <- tsspaw_bio[1]
    stats$current_depletion <- depletionseries[length(depletionseries)]

    # total landings
    ls <- nrow(ts) - 1
    totretainedmat <- as.matrix(ts[, substr(
      names(ts), 1,
      nchar("retain(B)")
    ) == "retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]

    # total catch
    totcatchmat <- as.matrix(ts[, substr(
      names(ts), 1,
      nchar("enc(B)")
    ) == "enc(B)"])
    ts$totcatch <- 0
    ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]

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

    # depletion
    depletion_method <- as.numeric(rawrep[matchfun("Depletion_method"), 2])
    depletion_basis <- rawrep[matchfun("B_ratio_denominator"), 2]
    if (depletion_basis == "no_depletion_basis") {
      depletion_basis <- "none"
    } else {
      depletion_basis <- as.numeric(strsplit(depletion_basis, "%*",
        fixed = TRUE
      )[[1]][1]) / 100
    }
    returndat$depletion_method <- depletion_method
    returndat$depletion_basis <- depletion_basis

    ## discard fractions ###

    # degrees of freedom for T-distribution
    # (or indicator 0, -1, -2 for other distributions)
    if (SS_versionNumeric < 3.20) {
      # old header from 3.11
      DF_discard <- rawrep[matchfun("DISCARD_OUTPUT"), 3]
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
      discard_header <- matchfun2(
        "DISCARD_SPECIFICATION", 1,
        "DISCARD_SPECIFICATION", 20
      )
      if (!is.null(discard_header)) {
        # read table of discard info by fleet at bottom of header
        discard_spec <- matchfun2("DISCARD_SPECIFICATION",
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
    discard <- matchfun2("DISCARD_OUTPUT", shift, header = TRUE)
    # rerun read of discard with header = FALSE
    # if in SSv3.20b which had missing line break
    if (!is.null(discard) && names(discard)[1] != "Fleet") {
      discard <- matchfun2("DISCARD_OUTPUT", shift, header = FALSE)
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
    discard_type <- NA
    if (!is.null(discard) && nrow(discard) > 1) {
      discard[discard == "_"] <- NA
      # v3.23 and before had things combined under "Name"
      # which has been renamed above to "Fleet_Name"
      if (SS_versionNumeric <= 3.23) {
        discard <- type.convert(discard, as.is = TRUE)
        if (!"Fleet_Name" %in% names(discard)) {
          discard$Fleet_Name <- discard$Fleet
        }
        discard$Fleet <- NA
        for (i in 1:nrow(discard)) {
          discard$Fleet[i] <- strsplit(discard$Fleet_Name[i], "_")[[1]][1]
          discard$Fleet_Name[i] <- substring(
            discard$Fleet_Name[i],
            nchar(discard$Fleet[i]) + 2
          )
        }
      } else {
        # v3.24 and beyond has separate columns
        # for fleet number and fleet name
        discard <- type.convert(discard, as.is = TRUE)
      }
    } else {
      discard <- NA
    }
    returndat$discard <- discard
    returndat$discard_type <- discard_type
    returndat$DF_discard <- DF_discard
    returndat$discard_spec <- discard_spec

    ## Average body weight observations
    # degrees of freedom for T-distribution
    DF_mnwgt <- rawrep[matchfun("log(L)_based_on_T_distribution"), 1]
    if (!is.na(DF_mnwgt)) {
      DF_mnwgt <- as.numeric(strsplit(DF_mnwgt, "=_")[[1]][2])
      mnwgt <- matchfun2("MEAN_BODY_WT_OUTPUT", 2, header = TRUE)
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
          mnwgt$Fleet_Name <- mnwgt$Fleet
        }
        mnwgt$Fleet <- NA
        for (i in 1:nrow(mnwgt)) {
          mnwgt$Fleet[i] <- strsplit(mnwgt$Fleet_Name[i], "_")[[1]][1]
          mnwgt$Fleet_Name[i] <- substring(
            mnwgt$Fleet_Name[i],
            nchar(mnwgt$Fleet_Name[i]) + 2
          )
        }
      } else { # v3.24 and beyond has separate columns for fleet number and fleet name
        mnwgt <- type.convert(mnwgt, as.is = TRUE)
      }
    } else {
      DF_mnwgt <- NA
      mnwgt <- NA
    }
    returndat$mnwgt <- mnwgt
    returndat$DF_mnwgt <- DF_mnwgt

    # Yield and SPR time-series
    spr <- matchfun2("SPR_SERIES", 5, header = TRUE)
    # read again if missing using capitalization prior to 3.30.15.06
    if (is.null(spr)) {
      spr <- matchfun2("SPR_series", 5, header = TRUE)
    }

    if (!is.null(spr)) {
      # clean up SPR output
      # make older SS output names match current SS output conventions
      names(spr) <- gsub(pattern = "SPB", replacement = "SSB", names(spr))
      spr <- df.rename(spr,
        oldnames = c("Year", "spawn_bio", "SPR_std", "Y/R", "F_std"),
        newnames = c("Yr", "SpawnBio", "SPR_report", "YPR", "F_report")
      )
      spr[spr == "_"] <- NA
      spr[spr == "&"] <- NA
      spr[spr == "-1.#IND"] <- NA
      spr <- type.convert(spr, as.is = TRUE)
      # spr <- spr[spr$Year <= endyr,]
      spr$spr <- spr$SPR
      stats$last_years_SPR <- spr$spr[nrow(spr)]
      stats$SPRratioLabel <- managementratiolabels[1, 2]
      stats$last_years_SPRratio <- spr$SPR_std[nrow(spr)]
    }
    returndat$sprseries <- spr

    returndat$managementratiolabels <- managementratiolabels
    returndat$F_report_basis <- managementratiolabels$Label[2]
    if (length(grep("%", managementratiolabels$Label[3])) > 0) {
      returndat$B_ratio_denominator <-
        as.numeric(strsplit(managementratiolabels$Label[3], "%")[[1]][1]) / 100
    } else {
      returndat$B_ratio_denominator <- NA
    }
    returndat$sprtarg <- sprtarg
    returndat$btarg <- btarg

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
    returndat$minbthresh <- minbthresh

    # read Kobe plot
    if (length(grep("Kobe_Plot", rawrep[, 1])) != 0) {
      # head of Kobe_Plot section differs by SS version,
      # but I haven't kept track of which is which
      # read first 5 lines
      Kobe_head <- matchfun2("Kobe_Plot", 0, "Kobe_Plot", 5, header = TRUE)
      shift <- grep("^Y", Kobe_head[, 1]) # may be "Year" or "Yr"
      Kobe_warn <- NA
      Kobe_MSY_basis <- NA
      if (length(grep("_basis_is_not", Kobe_head[1, 1])) > 0) {
        Kobe_warn <- Kobe_head[1, 1]
      }
      if (length(grep("MSY_basis", Kobe_head[2, 1])) > 0) {
        Kobe_MSY_basis <- Kobe_head[2, 1]
      }
      Kobe <- matchfun2("Kobe_Plot", shift, header = TRUE)
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
    returndat$Kobe_warn <- Kobe_warn
    returndat$Kobe_MSY_basis <- Kobe_MSY_basis
    returndat$Kobe <- Kobe

    flush.console()


    ## variance and sample size tuning information
    INDEX_1 <- matchfun2("INDEX_1", 1, "INDEX_1", (nfleets + 1), header = TRUE)
    # fill in column name that was missing in SS 3.24 (and perhaps other versions)
    # and replace inconsistent name in some 3.30 versions with standard name
    INDEX_1 <- df.rename(INDEX_1,
      oldnames = c("NoName", "fleetname"),
      newnames = c("Name", "Name")
    )

    # which column of INDEX_1 has number of CPUE values (used in reading INDEX_2)
    if (SS_versionNumeric >= 3.30) {
      ncpue_column <- 11
      INDEX_1 <- matchfun2("INDEX_1", 1, "INDEX_3", -4, header = TRUE)
      # remove any comments at the bottom of table
      INDEX_1 <- INDEX_1[substr(INDEX_1$Fleet, 1, 1) != "#", ]
      # count of observations per index
      ncpue <- sum(as.numeric(INDEX_1$N), na.rm = TRUE)
    } else {
      ncpue_column <- 11
      ncpue <- sum(as.numeric(rawrep[
        matchfun("INDEX_1") + 1 + 1:nfleets,
        ncpue_column
      ]))
    }
    # add to list of stuff that gets returned
    returndat$index_variance_tuning_check <- INDEX_1

    # CPUE/Survey series - will not match if not found
    cpue <- matchfun2("INDEX_2", 1, "INDEX_2", ncpue + 1, header = TRUE)
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
        cpue$Name <- NA
        for (i in 1:nrow(cpue)) {
          cpue$Fleet[i] <- strsplit(cpue$Fleet[i], "_")[[1]][1]
          cpue$Name[i] <- substring(cpue$Fleet[i], nchar(cpue$Fleet[i]) + 2)
        }
      }
      # replace any bad values (were present in at least one 3.24s model)
      if (any(cpue$Exp == "1.#QNAN")) {
        cpue$Exp[cpue$Exp == "1.#QNAN"] <- NA
        cpue$Calc_Q[cpue$Calc_Q == "1.#QNAN"] <- NA
        cpue$Eff_Q[cpue$Eff_Q == "1.#QNAN"] <- NA
      }
      # make columns numeric
      cpue <- type.convert(cpue, as.is = TRUE)
    } else {
      # if INDEX_2 not present
      cpue <- NULL
    }
    returndat$cpue <- cpue

    # Numbers at age
    natage <- matchfun2("NUMBERS_AT_AGE", 1,
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
    returndat$natage <- natage

    # NUMBERS_AT_AGE_Annual with and without fishery
    natage_annual_1_no_fishery <- matchfun2("NUMBERS_AT_AGE_Annual_1", 1,
      header = TRUE, type.convert = TRUE
    )
    natage_annual_2_with_fishery <- matchfun2("NUMBERS_AT_AGE_Annual_2", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat$natage_annual_1_no_fishery <- natage_annual_1_no_fishery
    returndat$natage_annual_2_with_fishery <- natage_annual_2_with_fishery

    # Biomass at age (introduced in 3.30)
    batage <- matchfun2("BIOMASS_AT_AGE", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    returndat$batage <- batage

    # Numbers at length
    col.adjust <- 12
    if (SS_versionNumeric < 3.30) {
      col.adjust <- 11
    }
    # test ending based on text because sections changed within 3.24 series
    natlen <- matchfun2("NUMBERS_AT_LENGTH", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    # make older SS output names match current SS output conventions
    natlen <- df.rename(natlen,
      oldnames = c("Gender", "SubMorph"),
      newnames = c("Sex", "Platoon")
    )
    returndat$natlen <- natlen

    # Biomass at length (first appeared in version 3.24l, 12-5-2012)
    batlen <- matchfun2("BIOMASS_AT_LENGTH", 1,
      substr1 = FALSE,
      header = TRUE, type.convert = TRUE
    )
    returndat$batlen <- batlen

    # F at age (first appeared in version 3.30.13, 8-Mar-2019)
    fatage <- matchfun2("F_AT_AGE", 1, header = TRUE, type.convert = TRUE)
    returndat$fatage <- fatage

    # read discard at age (added with 3.30.12, 29-Aug-2018)
    discard_at_age <- matchfun2("DISCARD_AT_AGE", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat$discard_at_age <- discard_at_age

    # catch at age
    catage <- matchfun2("CATCH_AT_AGE", 1,
      header = TRUE, type.convert = TRUE
    )
    returndat$catage <- catage

    # Movement
    movement <- matchfun2("MOVEMENT", 1, substr1 = FALSE, header = TRUE)
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
    returndat$movement <- movement

    # tag reporting rates
    tagreportrates <- matchfun2("Reporting_Rates_by_Fishery", 1,
      "See_composition_data_output", -1,
      substr2 = TRUE,
      header = TRUE,
      type.convert = TRUE
    )
    returndat$tagreportrates <- tagreportrates

    # tag release table
    # (no space after this table before Tags_Alive table)
    tagrelease <- matchfun2("TAG_Recapture", 1,
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
    returndat$tagrelease <- tagrelease
    returndat$tagfirstperiod <- tagfirstperiod
    returndat$tagaccumperiod <- tagaccumperiod

    # tags alive
    # (no space after this table before Total_recaptures table)
    tagsalive <- matchfun2(
      "Tags_Alive", 1,
      "Total_recaptures", -1
    )
    if (!is.null(tagsalive)) {
      tagcols <- ncol(tagsalive)
      names(tagsalive) <- c("TG", paste0("period", 0:(tagcols - 2)))
      tagsalive[tagsalive == ""] <- NA
      tagsalive <- type.convert(tagsalive, as.is = TRUE)
    }
    returndat$tagsalive <- tagsalive

    # total recaptures
    tagtotrecap <- matchfun2("Total_recaptures", 1)
    if (!is.null(tagtotrecap)) {
      tagcols <- ncol(tagtotrecap)
      names(tagtotrecap) <- c("TG", paste0("period", 0:(tagcols - 2)))
      tagtotrecap[tagtotrecap == ""] <- NA
      tagtotrecap <- type.convert(tagtotrecap, as.is = TRUE)
    }
    returndat$tagtotrecap <- tagtotrecap

    # age-length matrix
    # this section is more complex because of blank lines internally

    # first look for rows like " Seas: 12 Sub_Seas: 2   Morph: 12"
    sdsize_lines <- grep("^sdsize", rawrep[, 1])

    # the section ends with first blank line after the last of the sdsize_lines
    # so count the blanks as 1 greater than those in between the keyword
    # and the last of those sdsize_lines
    # an alternative here would be to modify matchfun2 to allow input of a
    # specific line number to end the section
    which_blank <- 1 + length(rep_blank_or_hash_lines[
                         rep_blank_or_hash_lines > matchfun("AGE_LENGTH_KEY") &
                         rep_blank_or_hash_lines < max(sdsize_lines)])
    
    # because of rows like " Seas: 12 Sub_Seas: 2   Morph: 12", the number of columns
    # needs to be at least 6 even if there are fewer ages
    rawALK <- matchfun2("AGE_LENGTH_KEY", 4,
      cols = 1:max(6, accuage + 2),
      header = FALSE,
      which_blank = which_blank
      )
    # confirm that the section is present
    if (length(sdsize_lines) > 0 &&
        length(rawALK) > 1 && # this should filter NULL values
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
        dimnames(ALK)$Matrix[i] <- paste(Matrix.Info, collapse = " ")
      }
      returndat$ALK <- ALK
    }

    # ageing error matrices
    rawAAK <- matchfun2("AGE_AGE_KEY", 1)
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
          returndat$AAK <- AAK
          returndat$age_error_mean <- age_error_mean
          returndat$age_error_sd <- age_error_sd
        }
      } # end check for ageing error matrices
      returndat$N_ageerror_defs <- N_ageerror_defs
    } # end check for NULL output of ageing error info

    # get equilibrium yield for newer versions of SS (some 3.24 and all 3.30),
    # which have SPR/YPR profile in Report.sso
    # (this was previously in Forecast-report.sso, but reading this info
    # is no longer supported for those older versions)
    if (SS_versionNumeric >= 3.30) {
      # 3.30 models have "Finish SPR/YPR profile" followed by some additional comments
      yieldraw <- matchfun2("SPR/YPR_Profile", 1, "Finish", -2)
    } else {
      # 3.24 models and earlier use blank line to end table
      yieldraw <- matchfun2("SPR/YPR_Profile", 1)
    }
    if (!is.null(yieldraw)) {
      names <- yieldraw[1, ]
      names[names == "SSB/Bzero"] <- "Depletion"
      yielddat <- yieldraw[c(2:(as.numeric(length(yieldraw[, 1]) - 1))), ]
      yielddat[yielddat == "-nan(ind)"] <- NA # this value sometimes occurs in 3.30 models
      names(yielddat) <- names
      yielddat <- type.convert(yielddat, as.is = TRUE)
      yielddat <- yielddat[order(yielddat$Depletion, decreasing = FALSE), ]
    } else {
      yielddat <- NA
    }
    returndat$equil_yield <- yielddat

    # Z at age
    # With_fishery
    # No_fishery_for_Z=M_and_dynamic_Bzero
    Z_at_age <- matchfun2("Z_AT_AGE_Annual_2", 1, header = TRUE)
    if (!is.null(Z_at_age)) {
      Z_at_age[Z_at_age == "_"] <- NA
      # if birth season is not season 1, you can get infinite values
      Z_at_age[Z_at_age == "-1.#INF"] <- NA
      Z_at_age <- type.convert(Z_at_age, as.is = TRUE)
    }
    returndat$Z_at_age <- Z_at_age

    # M at age table ends with comments
    #   Note:  Z calculated as -ln(Nt+1 / Nt)
    #   Note:  Z calculation for maxage not possible, for maxage-1 includes numbers at maxage, so is approximate
    M_at_age <- matchfun2("Z_AT_AGE_Annual_1", 1,
      "-ln(Nt+1", -1,
      matchcol2 = 5,
      header = TRUE
    )
    if (!is.null(Z_at_age)) {
      Z_at_age[Z_at_age == "_"] <- NA
      # if birth season is not season 1, you can get infinite values
      Z_at_age[Z_at_age == "-1.#INF"] <- NA
      Z_at_age <- type.convert(Z_at_age, as.is = TRUE)
    }
    returndat$M_at_age <- M_at_age

    # Dynamic_Bzero output "with fishery"
    Dynamic_Bzero <- matchfun2("Spawning_Biomass_Report_2", 1)
    # Dynamic_Bzero output "no fishery"
    Dynamic_Bzero2 <- matchfun2("Spawning_Biomass_Report_1", 1)
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
        Dynamic_Bzero$SSB <- apply(Dynamic_Bzero[, 2 + 1:nareas], 1, sum)
        Dynamic_Bzero$SSB_nofishing <-
          apply(Dynamic_Bzero[, 2 + nareas + 1:nareas], 1, sum)
      }
    }
    returndat$Dynamic_Bzero <- Dynamic_Bzero

    # adding stuff to list which gets returned by function
    if (comp) {
      returndat$comp_data_exists <- TRUE
      returndat$lendbase <- lendbase
      returndat$sizedbase <- sizedbase
      returndat$agedbase <- agedbase
      returndat$condbase <- condbase
      returndat$ghostagedbase <- ghostagedbase
      returndat$ghostcondbase <- ghostcondbase
      returndat$ghostlendbase <- ghostlendbase
      returndat$ladbase <- ladbase
      returndat$wadbase <- wadbase
      returndat$tagdbase1 <- tagdbase1
      returndat$tagdbase2 <- tagdbase2
    } else {
      returndat$comp_data_exists <- FALSE
    }
    # tables on fit to comps and mean age stuff from within Report.sso
    returndat$len_comp_fit_table <- fit_len_comps
    returndat$age_comp_fit_table <- fit_age_comps
    returndat$size_comp_fit_table <- fit_size_comps

    returndat$derived_quants <- der
    returndat$parameters <- parameters
    returndat$FleetNames <- FleetNames
    returndat$repfiletime <- repfiletime

    # type of stock recruit relationship
    SRRtype <- rawrep[matchfun("SPAWN_RECRUIT"), 3]
    if (!is.na(SRRtype) && SRRtype == "Function:") {
      SRRtype <- as.numeric(rawrep[matchfun("SPAWN_RECRUIT"), 4])
    }
    returndat$SRRtype <- SRRtype

    # get "sigma" used by Pacific Council in P-star calculations
    SSB_final_Label <- paste0("SSB_", endyr + 1)
    if (SSB_final_Label %in% der$Label) {
      SSB_final_EST <- der$Value[der$Label == SSB_final_Label]
      SSB_final_SD <- der$StdDev[der$Label == SSB_final_Label]
      returndat$Pstar_sigma <- sqrt(log((SSB_final_SD / SSB_final_EST)^2 + 1))
    } else {
      returndat$Pstar_sigma <- NULL
    }
    # get alternative "sigma" based on OFL catch used by Pacific Council
    # (added 23 Sept 2019 based on decision by PFMC SSC)
    OFL_final_Label <- paste0("OFLCatch_", endyr + 1)
    if (OFL_final_Label %in% der$Label) {
      OFL_final_EST <- der$Value[der$Label == OFL_final_Label]
      OFL_final_SD <- der$StdDev[der$Label == OFL_final_Label]
      returndat$OFL_sigma <- sqrt(log((OFL_final_SD / OFL_final_EST)^2 + 1))
    } else {
      returndat$OFL_sigma <- NULL
    }

    if (covar) {
      returndat$CoVar <- CoVar
      if (stats$N_estimated_parameters > 1) {
        returndat$highcor <- highcor
        returndat$lowcor <- lowcor
        returndat$corstats <- corstats
      }
      returndat$stdtable <- stdtable
    }

    # extract parameter lines representing annual recruit devs
    recdevEarly <- parameters[substring(parameters$Label, 1, 13) == "Early_RecrDev", ]
    early_initage <- parameters[substring(parameters$Label, 1, 13) == "Early_InitAge", ]
    main_initage <- parameters[substring(parameters$Label, 1, 12) == "Main_InitAge", ]
    recdev <- parameters[substring(parameters$Label, 1, 12) == "Main_RecrDev", ]
    recdevFore <- parameters[substring(parameters$Label, 1, 8) == "ForeRecr", ]
    recdevLate <- parameters[substring(parameters$Label, 1, 12) == "Late_RecrDev", ]

    # empty variable to fill in sections
    recruitpars <- NULL

    # assign "type" label to each one and identify year
    if (nrow(early_initage) > 0) {
      early_initage$type <- "Early_InitAge"
      early_initage$Yr <- startyr - as.numeric(substring(early_initage$Label, 15))
      recruitpars <- rbind(recruitpars, early_initage)
    }
    if (nrow(recdevEarly) > 0) {
      recdevEarly$type <- "Early_RecrDev"
      recdevEarly$Yr <- as.numeric(substring(recdevEarly$Label, 15))
      recruitpars <- rbind(recruitpars, recdevEarly)
    }
    if (nrow(main_initage) > 0) {
      main_initage$type <- "Main_InitAge"
      main_initage$Yr <- startyr - as.numeric(substring(main_initage$Label, 14))
      recruitpars <- rbind(recruitpars, main_initage)
    }
    if (nrow(recdev) > 0) {
      recdev$type <- "Main_RecrDev"
      recdev$Yr <- as.numeric(substring(recdev$Label, 14))
      recruitpars <- rbind(recruitpars, recdev)
    }
    if (nrow(recdevFore) > 0) {
      recdevFore$type <- "ForeRecr"
      recdevFore$Yr <- as.numeric(substring(recdevFore$Label, 10))
      recruitpars <- rbind(recruitpars, recdevFore)
    }
    if (nrow(recdevLate) > 0) {
      recdevLate$type <- "Late_RecrDev"
      recdevLate$Yr <- as.numeric(substring(recdevLate$Label, 14))
      recruitpars <- rbind(recruitpars, recdevLate)
    }

    # sort by year and remove any retain only essential columns
    if (!is.null(recruitpars)) {
      recruitpars <- recruitpars[
        order(recruitpars$Yr),
        c("Value", "Parm_StDev", "type", "Yr")
      ]
    }

    # add recruitpars to list of stuff that gets returned
    returndat$recruitpars <- recruitpars

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
      subset <- recruitpars$type %in% c("Main_InitAge", "Main_RecrDev")
      within_period <- sigma_R_info$period == "Main"
      sigma_R_info$N_devs[within_period] <- sum(subset)
      sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
      sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
      sigma_R_info$mean_SEsquared[within_period] <-
        mean((recruitpars$Parm_StDev[subset])^2)

      # calculate recdev stats  for Early+Main periods
      subset <- recruitpars$type %in% c(
        "Early_RecrDev", "Early_InitAge",
        "Main_InitAge", "Main_RecrDev"
      )
      within_period <- sigma_R_info$period == "Early+Main"
      sigma_R_info$N_devs[within_period] <- sum(subset)
      sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
      sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
      sigma_R_info$mean_SEsquared[within_period] <-
        mean((recruitpars$Parm_StDev[subset])^2)

      # calculate recdev stats for Early+Main+Late periods
      subset <- recruitpars$type %in% c(
        "Early_RecrDev", "Early_InitAge",
        "Main_InitAge", "Main_RecrDev", "Late_RecrDev"
      )
      within_period <- sigma_R_info$period == "Early+Main+Late"
      sigma_R_info$N_devs[within_period] <- sum(subset)
      sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
      sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
      sigma_R_info$mean_SEsquared[within_period] <-
        mean((recruitpars$Parm_StDev[subset])^2)

      # add variance as square of SD
      sigma_R_info$Var_of_devs <- sigma_R_info$SD_of_devs^2

      # add sqrt of sum
      sigma_R_info$sqrt_sum_of_components <- sqrt(sigma_R_info$Var_of_devs +
        sigma_R_info$mean_SEsquared)
      # ratio of sqrt of sum to sigmaR
      sigma_R_info$SD_of_devs_over_sigma_R <- sigma_R_info$SD_of_devs / sigma_R_in
      sigma_R_info$sqrt_sum_over_sigma_R <- sigma_R_info$sqrt_sum_of_components / sigma_R_in
      sigma_R_info$alternative_sigma_R <- sigma_R_in * sigma_R_info$sqrt_sum_over_sigma_R
    }
    stats$sigma_R_in <- sigma_R_in
    stats$sigma_R_info <- sigma_R_info
    stats$rmse_table <- rmse_table

    # process adjustments to recruit devs
    RecrDistpars <- parameters[substring(parameters$Label, 1, 8) == "RecrDist", ]
    returndat$RecrDistpars <- RecrDistpars

    # adding read of wtatage file
    returndat$wtatage <- wtatage

    # adding new jitter info table
    returndat$jitter_info <- jitter_info

    # add list of stats to list that gets returned
    returndat <- c(returndat, stats)

    # add info on semi-parametric selectivity deviations
    returndat$seldev_pars <- seldev_pars
    returndat$seldev_matrix <- seldev_matrix

    # print list of statistics
    if (printstats) {
      message("\nStatistics shown below (to turn off, change input to printstats=FALSE)")

      # remove scientific notation (only for display, not returned values,
      # which were added to returndat already)
      stats$likelihoods_used <- format(stats$likelihoods_used, scientific = 20)
      stats$estimated_non_dev_parameters <- format(stats$estimated_non_dev_parameters,
        scientific = 20
      )
      print(stats)
      if (covar) {
        if (stats$N_estimated_parameters > 1) {
          print(corstats, quote = FALSE)
        } else {
          message("Too few estimated parameters to report correlations.")
        }
      }
    }

    # add log file to list that gets returned
    returndat$logfile <- logfile


    # return the inputs to this function so they can be used by SS_plots
    # or other functions
    inputs <- list()
    inputs$dir <- dir
    inputs$repfile <- repfile
    inputs$forecast <- forecast
    inputs$warn <- warn
    inputs$covar <- covar
    inputs$verbose <- verbose

    returndat$inputs <- inputs

    if (verbose) {
      message("completed SS_output")
    }
    invisible(returndat)
  } # end function
