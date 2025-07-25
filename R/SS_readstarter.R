#' Read Stock Synthesis starter file as a list
#'
#' @template file
#' @template verbose
#' @return A list with one element for each line of input values.
#' List elements containing the name of the control and data file are
#' particularly helpful, i.e., `ctlfile` and `datfile`, respectively.
#' @examples
#' starter_list <- SS_readstarter(
#'   system.file("extdata", "simple_small", "starter.ss",
#'     package = "r4ss"
#'   ),
#'   verbose = FALSE
#' )
#'
#' # The following lines should be TRUE and demonstrate how you can know the
#' # names of the control and data file given information in the starter file.
#' starter_list[["ctlfile"]] == "simple_control.ss"
#' starter_list[["datfile"]] == "simple_data.ss"
#' @author Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson
#' @export
#' @family read/write functions
SS_readstarter <- function(file = "starter.ss", verbose = TRUE) {
  if (verbose) {
    message("running SS_readstarter")
  }

  starter <- readLines(file, warn = FALSE)
  if (length(starter) == 0) {
    stop("The following file was empty: ", file)
  }
  mylist <- list()

  mylist[["sourcefile"]] <- file
  mylist[["type"]] <- "Stock_Synthesis_starter_file"
  mylist[["SSversion"]] <- "3.24 or earlier"

  # get strings for control and data file names
  starter2 <- NULL
  for (i in seq_along(starter)) {
    # get only stuff before # marks
    mysplit <- strsplit(starter[i], split = "#")[[1]][1]
    if (!is.na(mysplit) && length(mysplit) > 0) starter2 <- c(starter2, mysplit)
  }
  strings <- NULL
  for (i in seq_along(starter2)) {
    mysplit <- strsplit(starter2[i], split = "[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit != ""]
    strings <- c(strings, mysplit)
  }
  strings <- strings[is.na(suppressWarnings(as.numeric(strings)))]
  if (length(strings) > 2) {
    warning(
      "Too many strings in starter file?\n",
      "Choosing first 2 of these as data and control file names:\n",
      paste(strings, collapse = "\n")
    )
  }
  mylist[["datfile"]] <- strings[1]
  mylist[["ctlfile"]] <- strings[2]
  if (verbose) {
    message(
      "  data, control files: ",
      mylist[["datfile"]],
      ", ",
      mylist[["ctlfile"]],
      sep = ""
    )
  }

  # get numbers (could be better integrated with function above)
  allnums <- NULL
  for (i in seq_along(starter)) {
    # split apart numbers from text in file
    mysplit <- strsplit(starter[i], split = "[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit != ""]
    nums <- suppressWarnings(as.numeric(mysplit))
    if (sum(is.na(nums)) > 0) {
      maxcol <- min((seq_along(nums))[is.na(nums)]) - 1
    } else {
      maxcol <- length(nums)
    }
    if (maxcol > 0) {
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }

  # go through numerical values and save as elements of a big list
  i <- 1

  mylist[["init_values_src"]] <- allnums[i]
  i <- i + 1
  mylist[["run_display_detail"]] <- allnums[i]
  i <- i + 1
  mylist[["detailed_age_structure"]] <- allnums[i]
  i <- i + 1
  if (mylist[["detailed_age_structure"]] == 3) {
    mylist[["custom_start"]] <- allnums[i]
    i <- i + 1
    end_custom <- which(allnums == -999)
    end_custom <- end_custom[end_custom >= i] # return only vals after i
    end_custom <- end_custom[1] # return first value after i.
    if (end_custom > i) {
      mylist[["custom_add_rm"]] <- allnums[i:(end_custom - 1)]
    }
    i <- end_custom + 1
  }
  mylist[["checkup"]] <- allnums[i]
  i <- i + 1
  mylist[["parmtrace"]] <- allnums[i]
  i <- i + 1
  mylist[["cumreport"]] <- allnums[i]
  i <- i + 1
  mylist[["prior_like"]] <- allnums[i]
  i <- i + 1
  mylist[["soft_bounds"]] <- allnums[i]
  i <- i + 1
  mylist[["N_bootstraps"]] <- allnums[i]
  i <- i + 1
  mylist[["last_estimation_phase"]] <- allnums[i]
  i <- i + 1
  mylist[["MCMCburn"]] <- allnums[i]
  i <- i + 1
  mylist[["MCMCthin"]] <- allnums[i]
  i <- i + 1
  mylist[["jitter_fraction"]] <- allnums[i]
  i <- i + 1
  mylist[["minyr_sdreport"]] <- allnums[i]
  i <- i + 1
  mylist[["maxyr_sdreport"]] <- allnums[i]
  i <- i + 1
  mylist[["N_STD_yrs"]] <- N_STD_yrs <- allnums[i]
  i <- i + 1
  if (N_STD_yrs > 0) {
    mylist[["STD_yr_vec"]] <- allnums[i:(i + N_STD_yrs - 1)]
    i <- i + N_STD_yrs
  }
  mylist[["converge_criterion"]] <- allnums[i]
  i <- i + 1
  if (verbose) {
    message("  converge_criterion = ", mylist[["converge_criterion"]])
  }
  mylist[["retro_yr"]] <- allnums[i]
  i <- i + 1
  mylist[["min_age_summary_bio"]] <- allnums[i]
  i <- i + 1
  mylist[["depl_basis"]] <- allnums[i]
  i <- i + 1
  mylist[["depl_denom_frac"]] <- allnums[i]
  i <- i + 1
  mylist[["SPR_basis"]] <- allnums[i]
  i <- i + 1
  if (verbose) {
    message("  SPR_basis = ", mylist[["SPR_basis"]])
  }
  mylist[["F_std_units"]] <- allnums[i]
  i <- i + 1
  if (
    !is.na(mylist[["F_std_units"]]) &&
      mylist[["F_std_units"]] %in% 4:5
  ) {
    mylist[["F_age_range"]] <- allnums[i]
    i <- i + 1
    mylist[["F_age_range"]][2] <- allnums[i]
    i <- i + 1
  } else {
    mylist[["F_age_range"]] <- NA
    mylist[["F_age_range"]][2] <- NA
  }
  mylist[["F_std_basis"]] <- allnums[i]
  i <- i + 1
  if (verbose) {
    message("  F_std_basis = ", mylist[["F_std_basis"]])
  }

  # last value in vector of numerical values
  i.final <- length(allnums)
  if (i < i.final) {
    # file is probably 3.30
    if (verbose) {
      message("Assuming version 3.30 based on number of numeric values.")
    }
    mylist[["MCMC_output_detail"]] <- allnums[i]
    i <- i + 1
    mylist[["ALK_tolerance"]] <- allnums[i]
    i <- i + 1
    if (verbose) {
      message("  MCMC_output_detail = ", mylist[["MCMC_output_detail"]])
      message("  ALK_tolerance = ", mylist[["ALK_tolerance"]])
    }
  }

  # check final value and define random seed and compatibility check.
  mylist[["final"]] <- final <- allnums[i]
  i <- i + 1
  if (!is.na(final) && final %in% c(3.30, 999)) {
    if (verbose) message("Read of starter file complete. Final value: ", final)
  } else {
    # read seed and check final value
    mylist[["seed"]] <- mylist[["final"]]
    if (verbose) {
      message("Reading a random seed value:", mylist[["seed"]])
    }

    mylist[["final"]] <- final <- allnums[i]
    if (!is.na(final) && final %in% c(1, 0)) {
      mylist[["Compatibility"]] <- mylist[["final"]]
      i <- i + 1
      mylist[["final"]] <- final <- allnums[i]
    }
    if (!is.na(final) && final %in% c(3.30, 999)) {
      if (verbose) {
        message("Read of starter file complete. Final value: ", final)
      }
    } else {
      warning("Final value is ", allnums[i], " but should be 3.30 or 999")
    }
  }
  if (final == 3.30) {
    mylist[["SSversion"]] <- "3.30"
  }
  # all done
  return(mylist)
}
