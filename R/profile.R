#' Deprecated function to run a likelihood profile, renamed to profile().
#'
#' @template deprecated_dots
#' @description
#' `r lifecycle::badge("deprecated")`
#' SS_profile() has been renamed as [profile()]. See
#' https://github.com/r4ss/r4ss/issues/723 for more details.
#'
#' @author Ian G. Taylor
#' @export
#' @seealso [profile()]
SS_profile <- function(...) {
  lifecycle::deprecate_stop(
    when = "4.6.1",
    what = "SS_profile()",
    with = "profile()"
  )
}

#' Run a likelihood profile in Stock Synthesis.
#'
#' Iteratively changes the control file for the chosen parameter. This
#' function was formerly called `SS_profile()`.
#'
#' @template dir
#' @param oldctlfile Source control file. Default = "control.ss_new"
#' @param masterctlfile Deprecated. Use `oldctlfile` instead.
#' @param newctlfile Destination for new control files (must match entry in
#' starter file). Default = "control_modified.ss".
#' @param linenum Line number of parameter to be changed. Can be used instead
#' of `string` or left as NULL. Can be a vector if you are profiling multiple
#' parameters at the same time.
#' @param string String partially matching name of parameter to be changed. Can
#' be used instead of `linenum` or left as NULL. Can be a vector if you are
#' profiling multiple parameters at the same time.
#' @param usepar Use PAR file from previous profile step for starting values?
#' @param globalpar Use global par file (`parfile_original_backup.sso`, which is
#' automatically copied from original `ss.par`) for all runs instead
#' of the par file from each successive run
#' @param parlinenum Line number in par file to change (if usepar = TRUE).
#' Can be a vector if you are profiling multiple parameters at the same time.
#' @param parstring String in par file preceding line number to change as
#' an alternative to parlinenum (only needed if usepar = TRUE).
#' Can be a vector if you are profiling multiple parameters at the same time.
#' @param profilevec Vector of values to profile over. If you are profileing
#' over multiple parameters at the same time this should be a data.frame or
#' matrix with a column for each parameter.
#' @param saveoutput Copy output .sso files to unique names.  Default = TRUE.
#' @param overwrite Overwrite any existing .sso files. Default = TRUE. If FALSE,
#' then some runs may be skipped.
#' @param whichruns Optional vector of run indices to do. This can be used to
#' re-run a subset of the cases in situations where the function was
#' interrupted or some runs fail to converge. Must be a subset of 1:n, where n
#' is the length of profilevec.
#' @param prior_check Check to make sure the starter file is set to include
#' the prior likelihood contribution in the total likelihood.  Default = TRUE.
#' @param read_like Read the table of likelihoods from each model as it finishes.
#' Default = TRUE. Changing to FALSE should allow the function to play through
#' even if something is wrong with reading the table.
#' @template exe
#' @template verbose
#' @param ... Additional arguments passed to [r4ss::run()], such as
#' `extras`, `show_in_console`, and `skipfinished`.
#' @note The starting values used in this profile are not ideal and some models
#' may not converge. Care should be taken in using an automated tool like this,
#' and some models are likely to require rerunning with alternate starting
#' values.
#'
#' Also, someday this function will be improved to work directly with the
#' plotting function [SSplotProfile()], but they don't yet work well
#' together. Thus, even if [profile()] is used, the output should
#' be read using [SSgetoutput()] or by multiple calls to
#' [SS_output()] before sending to [SSplotProfile()].
#' @author Ian G. Taylor, Kathryn L. Doering, Kelli F. Johnson,
#' Chantel R. Wetzel, James T. Thorson
#' @export
#' @seealso [SSgetoutput()],
#' [SS_changepars()], [SS_parlines()]
#' @family run functions
#' @family profile functions
#' @examples
#' \dontrun{
#'
#' ###########################################################################
#' # example profile
#' # (assumes you have an SS3 exe called "ss3.exe" or "ss3" in your PATH)
#' ###########################################################################
#'
#' # directory for "simple_small" example model included with r4ss
#' dir_simple_small <- file.path(
#'   path.package("r4ss"),
#'   file.path("extdata", "simple_small")
#' )
#'
#' # create temporary directory and copy files into it
#' dir_prof <- file.path(tempdir(), "profile")
#' copy_SS_inputs(
#'   dir.old = dir_simple_small,
#'   dir.new = dir_prof,
#'   create.dir = TRUE,
#'   overwrite = TRUE,
#'   copy_par = TRUE,
#'   verbose = TRUE
#' )
#'
#' # the following commands related to starter.ss could be done by hand
#' # read starter file
#' starter <- SS_readstarter(file.path(dir_prof, "starter.ss"))
#' # change control file name in the starter file
#' starter[["ctlfile"]] <- "control_modified.ss"
#' # make sure the prior likelihood is calculated
#' # for non-estimated quantities
#' starter[["prior_like"]] <- 1
#' # write modified starter file
#' SS_writestarter(starter, dir = dir_prof, overwrite = TRUE)
#' # vector of values to profile over
#' h.vec <- seq(0.3, 0.9, .1)
#' Nprofile <- length(h.vec)
#' # run profile command
#' profile <- profile(
#'   dir = dir_prof,
#'   oldctlfile = "control.ss",
#'   newctlfile = "control_modified.ss",
#'   string = "steep", # subset of parameter label
#'   profilevec = h.vec
#' )
#' # read the output files (with names like Report1.sso, Report2.sso, etc.)
#' profilemodels <- SSgetoutput(dirvec = dir_prof, keyvec = 1:Nprofile)
#' # summarize output
#' profilesummary <- SSsummarize(profilemodels)
#'
#' # OPTIONAL COMMANDS TO ADD MODEL WITH PROFILE PARAMETER ESTIMATED
#' # (in the "simple_small" example, steepness is fixed so it doesn't
#' # have any impact)
#' MLEmodel <- SS_output(dir_simple_small, verbose = FALSE, printstats = FALSE)
#' profilemodels[["MLE"]] <- MLEmodel
#' profilesummary <- SSsummarize(profilemodels)
#' # END OPTIONAL COMMANDS
#'
#' # plot profile using summary created above
#' results <- SSplotProfile(profilesummary, # summary object
#'   profile.string = "steep", # substring of profile parameter
#'   profile.label = "Stock-recruit steepness (h)"
#' ) # axis label
#'
#' # make timeseries plots comparing models in profile
#' SSplotComparisons(profilesummary, legendlabels = paste("h =", h.vec))
#'
#' ###########################################################################
#' # example two-dimensional profile
#' # (assumes you have an SS3 exe called "ss3.exe" or "ss3" in your PATH)
#' ###########################################################################
#'
#' dir_simple_small <- file.path(
#'   path.package("r4ss"),
#'   file.path("extdata", "simple_small")
#' )
#'
#' # create temporary directory and copy files into it
#' dir_prof <- file.path(tempdir(), "profile_2D")
#' copy_SS_inputs(
#'   dir.old = dir_simple_small,
#'   dir.new = dir_prof,
#'   create.dir = TRUE,
#'   overwrite = TRUE,
#'   copy_par = TRUE,
#'   verbose = TRUE
#' )
#'
#'
#' # create table of M values for females and males
#' par_table <- expand.grid(
#'   M1vec = c(0.05, 0.10, 0.15),
#'   M2vec = c(0.05, 0.10, 0.15)
#' )
#'
#' # run model once to create control.ss_new with
#' # good starting parameter values
#' # exe is assumed to be in PATH, add "exe" argument if needed
#' run(dir_prof, extras = "-nohess")
#'
#' # run profile using ss_new file as parameter source and
#' # overwriting original control file with new values
#' prof.table <- profile(
#'   dir = dir_prof,
#'   oldctlfile = "control.ss_new",
#'   newctlfile = "control.ss",
#'   string = c("NatM_uniform_Fem_GP_1", "NatM_uniform_Mal_GP_1"),
#'   profilevec = par_table,
#'   extras = "-nohess"
#' )
#'
#' # get model output
#' profilemodels <- SSgetoutput(
#'   dirvec = dir_prof,
#'   keyvec = 1:nrow(par_table), getcovar = FALSE
#' )
#' n <- length(profilemodels)
#' profilesummary <- SSsummarize(profilemodels)
#'
#' # add total likelihood (row 1) to table created above
#' par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])
#'
#' # reshape data frame into a matrix for use with contour
#' like_matrix <- reshape2::acast(
#'   data = par_table,
#'   formula = M1vec ~ M2vec,
#'   value.var = "like"
#' )
#'
#' # look at change relative to the minimum
#' # (shows small change when female and male M are equal,
#' # big change when they are different)
#' like_matrix - min(like_matrix)
#' #         0.05    0.1    0.15
#' # 0.05   6.938 32.710 121.959
#' # 0.1   49.706  0.000  27.678
#' # 0.15 154.897 44.768   5.366
#' }
#'
profile <- function(dir,
                    oldctlfile = "control.ss_new",
                    masterctlfile = lifecycle::deprecated(),
                    newctlfile = "control_modified.ss",
                    linenum = NULL,
                    string = NULL,
                    profilevec = NULL,
                    usepar = FALSE,
                    globalpar = FALSE,
                    parlinenum = NULL,
                    parstring = NULL,
                    saveoutput = TRUE,
                    overwrite = TRUE,
                    whichruns = NULL,
                    prior_check = TRUE,
                    read_like = TRUE,
                    exe = "ss3",
                    verbose = TRUE,
                    ...) {
  # Ensure wd is not changed by the function
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))

  # deprecated variable warnings
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(masterctlfile)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "profile(masterctlfile)",
      with = "profile(oldctlfile)"
    )
    oldctlfile <- masterctlfile
  }

  # check for executable
  check_exe(exe = exe, dir = dir, verbose = verbose)

  # figure out which line to change in control file
  # if not using par file, info still needed to set phase negative in control file
  if (is.null(linenum) & is.null(string)) {
    stop("You should input either 'linenum' or 'string' (but not both)")
  }
  if (!is.null(linenum) & !is.null(string)) {
    stop("You should input either 'linenum' or 'string' (but not both)")
  }
  if (usepar) { # if using par file
    if (is.null(parlinenum) & is.null(parstring)) {
      stop(
        "Using par file. You should input either 'parlinenum' or ",
        "'parstring' (but not both)"
      )
    }
    if (!is.null(parlinenum) & !is.null(parstring)) {
      stop(
        "Using par file. You should input either 'parlinenum' or ",
        "'parstring' (but not both)"
      )
    }
  }

  # count parameters to profile over (typically just 1)
  if (!is.null(linenum)) {
    npars <- length(linenum)
  }
  if (!is.null(string)) {
    npars <- length(string)
  }
  if (usepar) {
    if (!is.null(parlinenum)) {
      npars <- length(parlinenum)
    }
    if (!is.null(parstring)) {
      npars <- length(parstring)
    }
  }
  # not sure what would cause a bad value, but checking for it anyway
  if (is.na(npars) || npars < 1) {
    stop("Problem with the number of parameters to profile over. npars = ", npars)
  }

  # figure out length of profile vec and sort out which runs to do
  if (is.null(profilevec)) {
    stop("Missing input 'profilevec'")
  }
  if (npars == 1) {
    n <- length(profilevec)
  } else {
    if ((!is.data.frame(profilevec) & !is.matrix(profilevec)) ||
      ncol(profilevec) != npars) {
      stop(
        "'profilevec' should be a data.frame or a matrix with ",
        npars, " columns"
      )
    }
    n <- length(profilevec[[1]])
    if (any(unlist(lapply(profilevec, FUN = length)) != n)) {
      stop("Each element in the 'profilevec' list should have length ", n)
    }

    if (verbose) {
      if (!is.null(string)) {
        profilevec_df <- data.frame(profilevec)
        names(profilevec_df) <- string
        message(
          "Profiling over ", npars, " parameters\n",
          paste0(profilevec_df, collapse = "\n")
        )
      }
    }
  }

  # subset runs if requested
  if (is.null(whichruns)) {
    whichruns <- 1:n
  } else {
    if (!all(whichruns %in% 1:n)) {
      stop("input whichruns should be NULL or a subset of 1:", n, "\n", sep = "")
    }
  }
  if (verbose) {
    message(
      "Doing runs: ", paste(whichruns, collapse = ", "),
      ",\n  out of n = ", n
    )
  }

  # places to store convergence and likelihood info
  converged <- rep(NA, n)
  totallike <- rep(NA, n)
  liketable <- NULL

  # change working directory
  if (verbose) {
    message(
      "Changing working directory to ", dir, ",\n",
      " but will be changed back on exit from function."
    )
  }
  setwd(dir)

  # note: std file name is independent of executable name
  stdfile <- file.path(dir, "ss.std")

  # read starter file to get input file names and check various things
  starter.file <- dir()[tolower(dir()) == "starter.ss"]
  if (length(starter.file) == 0) {
    stop("starter.ss not found in", dir)
  }
  starter <- SS_readstarter(starter.file, verbose = FALSE)
  # check for new control file
  if (starter[["ctlfile"]] != newctlfile) {
    stop(
      "starter file should be changed to change\n",
      "'", starter[["ctlfile"]], "' to '", newctlfile, "'"
    )
  }
  # check for prior in likelihood
  if (prior_check & starter[["prior_like"]] == 0) {
    stop(
      "for likelihood profile, you should change the starter file value of\n",
      " 'Include prior likelihood for non-estimated parameters'\n",
      " from 0 to 1 and re-run the estimation.\n"
    )
  }
  # check for consistency in use of par file (part 1)
  if (usepar & starter[["init_values_src"]] == 0) {
    stop(
      "With setting 'usepar=TRUE', change the starter file value",
      " for initial value source from 0 (ctl file) to 1 (par file).\n"
    )
  }
  # check for consistency in use of par file (part 2)
  if (!usepar & starter[["init_values_src"]] == 1) {
    stop(
      "Change the starter file value for initial value source",
      " from 1 (par file) to 0 (par file) or change to",
      " profile(..., usepar = TRUE)."
    )
  }

  # back up par file
  if (usepar) {
    file.copy("ss.par", "parfile_original_backup.sso")
  }

  # run loop over profile values
  for (i in whichruns) {
    # check for presence of ReportN.sso files. If present and overwrite=FALSE,
    # then don't bother running anything
    newrepfile <- paste("Report", i, ".sso", sep = "")
    if (!overwrite & file.exists(newrepfile)) {
      message(
        "skipping profile i=", i, "/", n, " because overwrite=FALSE\n",
        "  and file exists: ", newrepfile
      )
    } else {
      message("running profile i=", i, "/", n)

      # change initial values in the control file
      # this also sets phase negative which is needed even when par file is used
      # dir set as NULL because the wd was already changed to dir earlier in the
      # script.
      if (npars == 1) {
        # get new parameter value
        newvals <- profilevec[i]
      } else {
        # get row as a vector (passing a data.frame to SS_changepars caused error)
        newvals <- as.numeric(profilevec[i, ])
      }
      SS_changepars(
        dir = NULL, ctlfile = oldctlfile, newctlfile = newctlfile,
        linenums = linenum, strings = string,
        newvals = newvals, estimate = FALSE,
        verbose = TRUE, repeat.vals = TRUE
      )

      # read parameter lines of control file
      ctltable_new <- SS_parlines(ctlfile = newctlfile)
      # which parameters are estimated in phase 1
      if (!any(ctltable_new[["PHASE"]] == 1)) {
        warning(
          "At least one parameter needs to be estimated in phase 1.\n",
          "Edit control file to add a parameter\n",
          "which isn't being profiled over to phase 1."
        )
      }

      if (usepar) {
        # alternatively change initial values in the par file
        # read file
        if (globalpar) {
          par <- readLines("parfile_original_backup.sso")
        } else {
          par <- readLines("ss.par")
        }
        # loop over the number of parameters (typically just 1)
        for (ipar in 1:npars) {
          # find value
          if (!is.null(parstring)) {
            parlinenum <- grep(parstring[ipar], par, fixed = TRUE) + 1
          }
          if (length(parlinenum) == 0) {
            stop("Problem with input parstring = '", parstring[ipar], "'")
          }
          parline <- par[parlinenum[ipar]]
          parval <- as.numeric(parline)
          if (is.na(parval)) {
            stop(
              "Problem with parlinenum or parstring for par file.\n",
              "line as read: ", parline
            )
          }
          # replace value
          par[parlinenum[ipar]] <- ifelse(npars > 1,
            profilevec[i, ipar],
            profilevec[i]
          )
        }
        # add new header
        note <- c(
          paste("# New par file created by profile() with the value on line number", linenum),
          paste("# changed from", parval, "to", profilevec[i])
        )
        par <- c(par, "#", note)
        message(paste0(note, collapse = "\n"))
        # write new par file
        writeLines(par, paste0("ss_input_par", i, ".ss"))
        writeLines(par, "ss.par")
      }
      if (file.exists(stdfile)) {
        file.remove(stdfile)
      }
      if (file.exists("Report.sso")) {
        file.remove("Report.sso")
      }

      # run model
      run(dir = dir, verbose = verbose, exe = exe, ...)

      # check for convergence
      converged[i] <- file.exists(stdfile)
      # onegood tracks whether there is at least one non-empty Report file
      onegood <- FALSE
      # look for non-zero report file and read LIKELIHOOD table
      if (read_like && file.exists("Report.sso") &
        file.info("Report.sso")$size > 0) {
        onegood <- TRUE
        # read first 400 lines of Report.sso
        Rep <- readLines("Report.sso", n = 400)
        # calculate range of rows with LIKELIHOOD table
        skip <- grep("LIKELIHOOD", Rep)[2]
        nrows <- grep("Crash_Pen", Rep) - skip - 1
        # read Report again to just get LIKELIHOOD table
        like <- read.table("Report.sso",
          skip = skip,
          nrows = nrows, header = TRUE, fill = TRUE
        )
        liketable <- rbind(liketable, as.numeric(like[["logL.Lambda"]]))
      } else {
        # add a placeholder row of NA values if no good report file
        liketable <- rbind(liketable, rep(NA, 10))
      }

      # rename output files
      if (saveoutput) {
        file.copy("Report.sso", paste("Report", i, ".sso", sep = ""),
          overwrite = overwrite
        )
        file.copy("CompReport.sso", paste("CompReport", i, ".sso", sep = ""),
          overwrite = overwrite
        )
        file.copy("covar.sso", paste("covar", i, ".sso", sep = ""),
          overwrite = overwrite
        )
        file.copy("warning.sso", paste("warning", i, ".sso", sep = ""),
          overwrite = overwrite
        )
        file.copy("admodel.hes", paste("admodel", i, ".hes", sep = ""),
          overwrite = overwrite
        )
        file.copy("ss.par", paste("ss.par_", i, ".sso", sep = ""),
          overwrite = overwrite
        )
      }
    } # end running stuff
  } # end loop of whichruns
  if (onegood) {
    liketable <- as.data.frame(liketable)
    names(liketable) <- like[["Component"]]
    bigtable <- cbind(profilevec[whichruns], converged[whichruns], liketable)
    names(bigtable)[1] <- "Value"
    return(bigtable)
  } else {
    stop("Error: no good Report.sso files created in profile")
  }
} # end function
