#' Run a likelihood profile in Stock Synthesis.
#'
#' Iteratively changes the control file using SS_changepars.
#'
#' @template dir
#' @param masterctlfile Source control file. Default = "control.ss_new"
#' @param newctlfile Destination for new control files (must match entry in
#' starter file). Default = "control_modified.ss".
#' @param linenum Line number of parameter to be changed. Can be used instead
#' of `string` or left as NULL. Can be a vector if you are profiling multiple
#' parameters at the same time.
#' @param string String partially matching name of parameter to be changed. Can
#' be used instead of `linenum` or left as NULL. Can be a vector if you are
#' profiling multiple parameters at the same time.
#' @param usepar Use PAR file from previous profile step for starting values?
#' @param globalpar Use global par file ("parfile_original_backup.sso", which is
#' automatically copied from original `parfile`) for all runs instead
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
#' @param ... Additional arguments passed to r4ss::run(), such as
#' `extras`, and `show_in_console`.
#' @note The starting values used in this profile are not ideal and some models
#' may not converge. Care should be taken in using an automated tool like this,
#' and some models are likely to require rerunning with alternate starting
#' values.
#'
#' Also, someday this function will be improved to work directly with the
#' plotting function [SSplotProfile()], but they don't yet work well
#' together. Thus, even if [SS_profile()] is used, the output should
#' be read using [SSgetoutput()] or by multiple calls to
#' [SS_output()] before sending to [SSplotProfile()].
#' @author Ian Taylor
#' @export
#' @seealso [SSplotProfile()], [SSgetoutput()],
#' [SS_changepars()], [SS_parlines()]
#' @examples
#' \dontrun{
#' # note: don't run this in your main directory
#' # make a copy in case something goes wrong
#' mydir <- "C:/ss/Simple - Copy"
#'
#' # the following commands related to starter.ss could be done by hand
#' # read starter file
#' starter <- SS_readstarter(file.path(mydir, "starter.ss"))
#' # change control file name in the starter file
#' starter[["ctlfile"]] <- "control_modified.ss"
#' # make sure the prior likelihood is calculated
#' # for non-estimated quantities
#' starter[["prior_like"]] <- 1
#' # write modified starter file
#' SS_writestarter(starter, dir = mydir, overwrite = TRUE)
#'
#' # vector of values to profile over
#' h.vec <- seq(0.3, 0.9, .1)
#' Nprofile <- length(h.vec)
#'
#' # run SS_profile command
#' profile <- SS_profile(
#'   dir = mydir, # directory
#'   # "NatM" is a subset of one of the
#'   # parameter labels in control.ss_new
#'   model = "ss",
#'   masterctlfile = "control.ss_new",
#'   newctlfile = "control_modified.ss",
#'   string = "steep",
#'   profilevec = h.vec
#' )
#'
#'
#' # read the output files (with names like Report1.sso, Report2.sso, etc.)
#' profilemodels <- SSgetoutput(dirvec = mydir, keyvec = 1:Nprofile)
#' # summarize output
#' profilesummary <- SSsummarize(profilemodels)
#'
#' # OPTIONAL COMMANDS TO ADD MODEL WITH PROFILE PARAMETER ESTIMATED
#' MLEmodel <- SS_output("C:/ss/SSv3.24l_Dec5/Simple")
#' profilemodels[["MLE"]] <- MLEmodel
#' profilesummary <- SSsummarize(profilemodels)
#' # END OPTIONAL COMMANDS
#'
#' # plot profile using summary created above
#' SSplotProfile(profilesummary, # summary object
#'   profile.string = "steep", # substring of profile parameter
#'   profile.label = "Stock-recruit steepness (h)"
#' ) # axis label
#'
#' # make timeseries plots comparing models in profile
#' SSplotComparisons(profilesummary, legendlabels = paste("h =", h.vec))
#'
#'
#' ###########################################################################
#'
#' # example two-dimensional profile
#' # (e.g. over 2 of the parameters in the low-fecundity stock-recruit function)
#' base_dir <- "c:/mymodel"
#'
#' dir_profile_SR <- file.path(base_dir, "Profiles/Zfrac_and_Beta")
#'
#' # make a grid of values in both dimensions Zfrac and Beta
#' # vector of values to profile over
#' Zfrac_vec <- seq(from = 0.2, to = 0.6, by = 0.1)
#' Beta_vec <- c(0.5, 0.75, 1.0, 1.5, 2.0)
#' par_table <- expand.grid(Zfrac = Zfrac_vec, Beta = Beta_vec)
#' nrow(par_table)
#' ## [1] 25
#' head(par_table)
#' ##   Zfrac Beta
#' ## 1   0.2 0.50
#' ## 2   0.3 0.50
#' ## 3   0.4 0.50
#' ## 4   0.5 0.50
#' ## 5   0.6 0.50
#' ## 6   0.2 0.75
#'
#' # run SS_profile command
#' profile <- SS_profile(
#'   dir = dir_profile_SR, # directory
#'   masterctlfile = "control.ss_new",
#'   newctlfile = "control_modified.ss",
#'   string = c("Zfrac", "Beta"),
#'   profilevec = par_table,
#'   extras = "-nohess" # argument passed to run()
#' )
#'
#' # get model output
#' profilemodels <- SSgetoutput(
#'   dirvec = dir_profile_SR,
#'   keyvec = 1:nrow(par_table), getcovar = FALSE
#' )
#' n <- length(profilemodels)
#' profilesummary <- SSsummarize(profilemodels)
#'
#' # add total likelihood (row 1) to table created above
#' par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])
#'
#' # reshape data frame into a matrix for use with contour
#' like_matrix <- reshape2::acast(par_table, Zfrac ~ Beta, value.var = "like")
#'
#' # make contour plot
#' contour(
#'   x = as.numeric(rownames(like_matrix)),
#'   y = as.numeric(colnames(like_matrix)),
#'   z = like_matrix
#' )
#' }
#'
SS_profile <-
  function(dir,
           masterctlfile = "control.ss_new",
           newctlfile = "control_modified.ss", # must match entry in starter file
           linenum = NULL,
           string = NULL,
           profilevec = NULL,
           usepar = FALSE,
           globalpar = FALSE,
           parlinenum = NULL,
           parstring = NULL,
           dircopy = TRUE,
           saveoutput = TRUE,
           overwrite = TRUE,
           whichruns = NULL,
           prior_check = TRUE,
           read_like = TRUE,
           verbose = TRUE,
           ...) {
    # Ensure wd is not changed by the function
    orig_wd <- getwd()
    on.exit(setwd(orig_wd))

    # check for executable
    check_exe(exe = exe, dir = dir, verbose = verbose)

    # figure out which line to change in control file
    # if not using parfile, info still needed to set phase negative in control file
    if (is.null(linenum) & is.null(string)) {
      stop("You should input either 'linenum' or 'string' (but not both)")
    }
    if (!is.null(linenum) & !is.null(string)) {
      stop("You should input either 'linenum' or 'string' (but not both)")
    }
    if (usepar) { # if using parfile
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

    # std file name is independent of executable name
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
    # check for consistency in use of par file
    if (usepar & starter[["init_values_src"]] == 0) {
      stop(
        "with setting 'usepar=TRUE', you need to change the starter file value\n",
        " for initial value source from 0 (ctl file) to 1 (par file).\n"
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
          dir = NULL, ctlfile = masterctlfile, newctlfile = newctlfile,
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
            paste("# New par file created by SS_profile with the value on line number", linenum),
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
        run(dir = dir, verbose = verbose, ...)

        # check for convergence
        converged[i] <- file.exists(stdfile)
        onegood <- FALSE
        if (read_like && file.exists("Report.sso") &
          file.info("Report.sso")$size > 0) {
          onegood <- TRUE
          Rep <- readLines("Report.sso", n = 200)
          like <- read.table("Report.sso", skip = grep("LIKELIHOOD", Rep)[2] + 0, nrows = 11, header = TRUE, fill = TRUE)
          liketable <- rbind(liketable, as.numeric(like[["logL.Lambda"]]))
        } else {
          liketable <- rbind(liketable, rep(NA, 10))
        }

        if (saveoutput) {
          file.copy("Report.sso", paste("Report", i, ".sso", sep = ""), overwrite = overwrite)
          file.copy("CompReport.sso", paste("CompReport", i, ".sso", sep = ""), overwrite = overwrite)
          file.copy("covar.sso", paste("covar", i, ".sso", sep = ""), overwrite = overwrite)
          file.copy("warning.sso", paste("warning", i, ".sso", sep = ""), overwrite = overwrite)
          file.copy("admodel.hes", paste("admodel", i, ".hes", sep = ""), overwrite = overwrite)
          file.copy(parfile, paste(model, ".par_", i, ".sso", sep = ""), overwrite = overwrite)
        }
      } # end running stuff
    } # end loop of whichruns
    if (onegood) {
      liketable <- as.data.frame(liketable)
      names(liketable) <- like[["Component"]]
      bigtable <- cbind(profilevec, converged, liketable)
      names(bigtable)[1] <- "Value"
      return(bigtable)
    } else {
      stop("Error: no good Report.sso files created in profile")
    }
  } # end function
