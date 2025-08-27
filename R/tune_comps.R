#' Deprecated function to tune composition data, renamed to tune_comps()
#'
#' @template deprecated_dots
#' @description
#' `r lifecycle::badge("deprecated")`
#' SS_tune_comps() has been renamed as [tune_comps()]. See
#' https://github.com/r4ss/r4ss/issues/723 for more details.
#'
#' @author Ian G. Taylor
#' @export
#' @seealso [tune_comps()]
SS_tune_comps <-
  function(...) {
    lifecycle::deprecate_stop(
      when = "4.6.1",
      what = "SS_tune_comps()",
      with = "tune_comps()"
    )
  }

#' Calculate new tunings for length and age compositions and (re)run models
#'
#' Creates a table of values that can be copied into the SS3 control file
#' for SS3 3.30 models to adjust the input sample sizes for length and age
#' compositions based on either the Francis or McAllister-Ianelli tuning or
#' adds the Dirichlet-Multinomial parameters to the necessary files to
#' tune the model using an integrated method.
#' Optionally, this function can automatically add these tunings to the
#' appropriate files and rerun the model for the desired number of iterations.
#'
#' @md
#' @details
#' # `option`
#' ## Francis
#' The Francis approach to data weighting adjusts the input sample sizes using
#' a scalar such that the fit of the expected value is within the uncertainty
#' intervals based on the expected fit given adjusted sample sizes.
#'
#' ## McAllister-Ianelli (MI)
#' Also known as the Harmonic-Mean approach to data weighting, the
#' McAllister-Ianelli weighting approach uses a scalar to adjust the input
#' sample size of composition data based matching the arithmetic mean
#' of the input sample size to the harmonic mean of the effective sample size.
#'
#' ## Dirichlet-Multinomial (DM)
#' The Dirichlet-Multinomial likelihood is an alternative approach that allows
#' the tuning data type to be estimated rather than iteratively tuned.
#' Note that for `option = "DM"` a table of tunings is
#' not created as the DM is not an iterative reweighting option. Instead, each
#' of the fleets with length- and age-composition data will be assigned a DM
#' parameter and the model will be rerun.
#'
#' # SS3 versions
#' ## 3.30.00-3.30.11
#' Recommended_var_adj and other columns were named differently in these
#' early version of SS3. Calculations are thus done internally based on
#' finding the correct column name.
#'
#' ## 3.30.12-3.30.16
#' Starting with SS3 version 3.30.12, the "Length_Comp_Fit_Summary"
#' table in Report.sso is already in the format required to paste into
#' the control file to apply the McAllister-Ianelli tuning. However, this
#' function provides the additional option of the Francis tuning and the
#' ability to compare the two approaches, as well as the functionality to add
#' tunings and rerun the model. The "Age_Comp_Fit_Summary" table in Report.sso
#' is formatted similarly though, though the Recommended_var_adj was wrongly
#' set to 1 for all fleets in SS3 versions 3.30.12 to 3.30.16. Thus, the
#' MI approach is not taken from this recommended column, instead, it is
#' calculated from the harmonic mean and input sample sizes.
#'
#' ## 3.30.20
#' Starting with SS3 version 3.30.20, the Dirichlet-multinomial
#' likelihood was made available for Generalized Size Comp data. As part
#' of this change, the column names were changed for all fit summary
#' tables, to both align the notation among them and also facilitate the
#' future addition of the Multivariate-Tweedie likelihood.
#'
#' @template replist
#' @template fleets
#' @param option Which type of tuning: 'none', 'Francis', 'MI', or 'DM'.
#'  The first option, `none`, will only return information about the
#'  Francis and MI weights that are suggested.
#' @param digits Number of digits to round numbers to.
#' @param write Write suggested tunings to a file saved to the disk called
#'  `suggested_tunings.ss`. This file name is currently hard coded and will
#'  be saved in `dir`.
#' @param niters_tuning The number of times to retune models. Defaults to 0,
#'  where only the tunings should be calculated and the model is not rerun. Note
#'  that for DM, it will be assumed that 0 means not to run the model and
#'  specifying 1 or greater will only run the model once (because DM is not an
#'  iterative retuning method).
#' @param init_run Should the model be run before calculating the tunings?
#'  Defaults to `FALSE`. This run is not counted as an iteration for
#'  `niters_tuning` and will not be used if `option = "DM"`.
#' @template dir
#' @template verbose
#' @param allow_up_tuning Allow tuning values for Francis or MI > 1? Defaults to
#'  FALSE, which caps tuning values at 1.
#' @template exe
#' @param model Deprecated. Use `exe` instead.
#' @template extras
#' @param ... Additional arguments passed to [r4ss::run()], such as
#' `show_in_console`.
#'
#' @return Returns a table that can be copied into the control file.
#' If `write=TRUE` then will write the values to a file
#' (currently hardwired to go in the directory where the model was run
#' and called "suggested_tunings.ss").
#'
#' @author Ian G. Taylor, Kathryn L. Doering
#' @export
#' @family tuning functions
#' @family run functions
#' @references Francis, R.I.C.C. (2011). Data weighting in statistical
#' fisheries stock assessment models. Can. J. Fish. Aquat. Sci. 68: 1124-1138.
#' @examples
#' \dontrun{
#' # Set up the folders ----
#' # Create a temporary directory, feel free to change this location
#' mod_path <- file.path(tempdir(), "simple_mod")
#' # Path to simple model in r4ss and copy files to mod_path
#' example_path <- system.file("extdata", "simple_small", package = "r4ss")
#' # copy model input files
#' copy_SS_inputs(dir.old = example_path, dir.new = mod_path, verbose = FALSE)
#' # copy over the Report file
#' file.copy(
#'   from = file.path(example_path, "Report.sso"),
#'   to = file.path(mod_path, "Report.sso")
#' )
#' # copy comp report file
#' file.copy(
#'   from = file.path(example_path, "CompReport.sso"),
#'   to = file.path(mod_path, "CompReport.sso")
#' )
#' # Use the tune_comps function----
#'
#' # Examples where a model is not run ----
#'
#' # Just get the Francis and MI tables, without running the model. Note that the
#' # model in mod_path needs to already have been run with Stock Synthesis, so
#' # that a report file is available.
#'
#' weight_table <- tune_comps(
#'   dir = mod_path,
#'   option = "none",
#'   verbose = FALSE
#' )
#' # view the weights. Note that the columns New_Francis and New_MI show the
#' # weights, but neither were added to the New_Var_adj column
#' weight_table
#'
#' # Get the Francis and MI tables, but with the Francis weights in the
#' # New_Var_adj column. Note if option = "MI" were used, the output would be
#' # the same except that the New_Var_adj column would contain the MI weights.
#' weight_table_fran <- tune_comps(
#'   dir = mod_path,
#'   option = "Francis",
#'   verbose = FALSE
#' )
#' weight_table_fran
#'
#' # Add Dirichlet-multinomial tuning parameters to the model,
#' # without running it.
#'
#' DM_parm_info <- tune_comps(
#'   option = "DM",
#'   niters_tuning = 0, # 0 means the model will not be run.
#'   dir = mod_path,
#'   verbose = FALSE
#' )
#' # See the Dirichlet parameters added to the model.
#' DM_parm_info[["tuning_table_list"]]
#' # can also look in the data file to see which fleets of comp data now have
#' # DM parameters. The "ParmSelect" column of the len_info and age_info
#' # contains the dirichlet multinomial parameter numbers.
#' dat <- SS_readdat(file.path(mod_path, "simple_data.ss"), verbose = FALSE)
#' dat[["len_info"]]
#' dat[["age_info"]]
#'
#' # Examples where models are run ----
#'
#' # Run MI weighting and allow upweighting for 1 iteration. Assume that an ss
#' # executable called "ss or ss.exe" is available in the mod_path folder.
#' # If the executable is not available, then the call will exit on error.
#' # Note that the Dirichlet mulitnomial parameters will be removed, but any
#' # previous tunings will be retained.
#' tune_info <- tune_comps(
#'   option = "MI",
#'   niters_tuning = 1,
#'   dir = mod_path,
#'   allow_up_tuning = TRUE,
#'   exe = "ss3",
#'   verbose = FALSE
#' )
#' # see the tuning table, and the weights applied to the model.
#' tune_info
#'
#' # Add Dirichlet multinomial paramters and rerun. The function will
#' # automatically remove the MI weighting and add in the DM parameters.
#' # Use extras = "-nohess" when running model to speed up run.
#' DM_parm_info <- tune_comps(
#'   option = "DM",
#'   niters_tuning = 1, # must be 1 or greater to run
#'   dir = mod_path,
#'   extras = "-nohess",
#'   verbose = FALSE
#' )
#' # see the DM parameter estimates
#' DM_parm_info[["tuning_table_list"]]
#'
#' # cleanup ----
#' unlink(mod_path, recursive = TRUE)
#' }
tune_comps <- function(
  replist = NULL,
  fleets = "all",
  option = c("Francis", "MI", "none", "DM"),
  digits = 6,
  write = TRUE,
  niters_tuning = 0,
  init_run = FALSE,
  dir = getwd(),
  exe = "ss3",
  model = lifecycle::deprecated(),
  extras = "",
  allow_up_tuning = FALSE,
  verbose = TRUE,
  ...
) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(model)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "tune_comps(model)",
      with = "tune_comps(exe)"
    )
    exe <- model
  }

  # check inputs
  option <- match.arg(option, several.ok = FALSE)
  # try to read in rep list, if it is null.
  if (is.null(replist)) {
    replist <- try(SS_output(
      dir = dir,
      verbose = FALSE,
      hidewarn = TRUE,
      printstats = FALSE
    ))
    if ("try-error" %in% class(replist)) {
      replist <- NULL
    }
  }
  # this combination of setting won't work:
  if (
    is.null(replist) &
      init_run == FALSE &
      option %in% c("Francis", "MI", "none")
  ) {
    stop(
      "Please specify replist (no report file found) or set init_run == TRUE",
      " when using option Francis, MI, or none"
    )
  }
  # read in model files
  start <- SS_readstarter(file.path(dir, "starter.ss"), verbose = FALSE)
  dat <- SS_readdat(
    file.path(dir, start[["datfile"]]),
    verbose = FALSE,
    section = 1
  )
  ctl <- SS_readctl(
    file.path(dir, start[["ctlfile"]]),
    use_datlist = TRUE,
    datlist = dat,
    verbose = FALSE
  )
  if (fleets[1] == "all") {
    fleets <- seq_len(dat[["Nfleets"]])
  } else {
    if (!all(fleets %in% seq_len(dat[["Nfleets"]]))) {
      fleets <- fleets[fleets %in% seq_len(dat[["Nfleets"]])]
      warning(
        "Not all fleets are included in the model. Changing fleets to ",
        "use only ones in the model: ",
        paste0(fleets, collapse = ", ")
      )
      if (length(fleets) == 0) {
        stop("Please specify fleets used in the model")
      }
    }
  }
  # add check that last_phase is less than max_phase in starter. If not,
  # modify the max phase and send warning.
  # get the highest phase in the model
  last_phase <- get_last_phase(ctl)
  if (last_phase >= start[["last_estimation_phase"]]) {
    warning(
      "The last phase used in the control file, ",
      last_phase,
      ", is higher or the same as the last_estimation_phase in the ",
      "starter file currently set to ",
      start[["last_estimation_phase"]],
      ".",
      "Changing the last_estimation_phase in the starter file to ",
      last_phase + 1,
      "."
    )
    start[["last_estimation_phase"]] <- last_phase + 1
    SS_writestarter(start, dir = dir, verbose = FALSE, overwrite = TRUE)
  }

  # francis, MI ----
  if (option %in% c("none", "Francis", "MI")) {
    if (!is.null(ctl[["dirichlet_parms"]])) {
      if (verbose) {
        message("Removing DM parameters from model")
      }
      # take DM specifications out of data file
      if (!is.null(dat[["len_info"]])) {
        dat[["len_info"]][, "CompError"] <- 0
        dat[["len_info"]][, "ParmSelect"] <- 0
      }
      if (!is.null(dat[["age_info"]])) {
        dat[["age_info"]][, "CompError"] <- 0
        dat[["age_info"]][, "ParmSelect"] <- 0
      }
      ctl[["dirichlet_parms"]] <- NULL
      SS_writectl(
        ctl,
        file.path(dir, start[["ctlfile"]]),
        overwrite = TRUE,
        verbose = FALSE
      )
      SS_writedat(
        dat,
        file.path(dir, start[["datfile"]]),
        overwrite = TRUE,
        verbose = FALSE
      )
    }
    # do an init model run if desired, to get a new replist.
    if (init_run) {
      run(
        dir = dir,
        exe = exe,
        extras = extras,
        skipfinished = FALSE,
        verbose = verbose,
        ...
      )
      suppressWarnings(
        replist <- SS_output(
          dir = dir,
          verbose = FALSE,
          printstats = FALSE,
          covar = !grepl("nohess", extras),
          hidewarn = TRUE
        )
      )
    }
    if (niters_tuning == 0 | option == "none") {
      # calculate the tuning table and rerun
      tuning_table <- get_tuning_table(
        replist = replist,
        fleets = fleets,
        option = option,
        digits = digits,
        write = write,
        verbose = verbose
      )
      return(tuning_table)
    }
    if (niters_tuning > 0) {
      # Use results from the tuning table to rerun the model, if desired.
      weights <- vector("list", length = niters_tuning)
      tuning_table_list <- vector("list", length = niters_tuning)
      for (it in seq_len(niters_tuning)) {
        # 2. get the tunings
        suppressWarnings(
          out <- SS_output(
            dir,
            verbose = FALSE,
            printstats = FALSE,
            covar = !grepl("nohess", extras),
            hidewarn = TRUE
          )
        )
        # construct the variance adjustment
        var_adj <- get_tuning_table(
          replist = out,
          fleets = fleets,
          option = option,
          digits = digits,
          write = write,
          verbose = verbose
        )
        var_adj_unmodified <- var_adj
        var_adj <- var_adj[, 1:3]
        colnames(var_adj) <- c("factor", "fleet", "value")
        if (allow_up_tuning == FALSE) {
          var_adj[["value"]] <- ifelse(
            var_adj[["value"]] > 1,
            1,
            var_adj[["value"]]
          )
        }
        var_adj <- var_adj[var_adj[["fleet"]] %in% fleets, ]
        start <- SS_readstarter(file.path(dir, "starter.ss"), verbose = FALSE)
        dat <- SS_readdat(
          file.path(dir, start[["datfile"]]),
          verbose = FALSE,
          section = 1
        )
        ctl <- SS_readctl(
          file.path(dir, start[["ctlfile"]]),
          use_datlist = TRUE,
          datlist = dat,
          verbose = FALSE
        )
        if ((nrow(var_adj)) > 0) {
          ctl[["DoVar_adjust"]] <- 1
          if (is.null(ctl[["Variance_adjustment_list"]])) {
            # create the list if it does not already exist
            ctl[["Variance_adjustment_list"]] <- var_adj
          } else {
            # leave all var adj intact, unless they match data type and fleet in var_adj.
            cur_var_adj <- ctl[["Variance_adjustment_list"]]
            for (i in seq_len(nrow(var_adj))) {
              tmp_fac <- var_adj[i, "factor"]
              tmp_flt <- var_adj[i, "fleet"]
              tmp_row <- which(
                ctl[["Variance_adjustment_list"]][, "factor"] == tmp_fac &
                  ctl[["Variance_adjustment_list"]][, "fleet"] == tmp_flt
              )
              if (length(tmp_row) == 1) {
                ctl[["Variance_adjustment_list"]][tmp_row, ] <- var_adj[i, ]
              } else if (length(tmp_row) == 0) {
                ctl[["Variance_adjustment_list"]] <- rbind(
                  ctl[["Variance_adjustment_list"]],
                  var_adj[i, ]
                )
              }
              # sanity check. If user recieving this error message, function is not
              # working as developer intended.
              if (length(tmp_row) > 1) {
                stop(
                  "Multiple rows with same data type and fleet in the variance ",
                  "variance adjustment list, which should not be possible. Please",
                  " check that the control file will work with SS3. If still having",
                  " issues, please report your problem: ",
                  "https://github.com/r4ss/r4ss/issues"
                )
              }
            }
          }
        }
        SS_writectl(
          ctl,
          file.path(dir, start[["ctlfile"]]),
          overwrite = TRUE,
          verbose = FALSE
        )
        # 4. run SS again with reweighting
        run(
          dir = dir,
          exe = exe,
          extras = extras,
          skipfinished = FALSE,
          verbose = verbose,
          ...
        )
        # save the weights from the run to a list
        weights[[it]] <- var_adj
        tuning_table_list[[it]] <- var_adj_unmodified
      }
    }
  }
  # DM ----
  if (option == "DM") {
    if (init_run) {
      warning(
        "Init run was TRUE, but option == DM, so no initial run was done.",
        "The model will only be run if niters > 0."
      )
    }
    # determine which fleets specified by user are included in model
    fleets_len <- fleets[fleets %in% unique(dat[["lencomp"]][, "fleet"])]
    fleets_age <- fleets[fleets %in% unique(dat[["agecomp"]][, "fleet"])]

    # 1. specify the parameters in the data file need to do dirichlet MN
    if (!is.null(dat[["lencomp"]])) {
      dat[["len_info"]][fleets_len, "CompError"] <- 1
      # TODO: make this more general so can share params across fleets?
      dat[["len_info"]][fleets_len, "ParmSelect"] <- seq_len(length(fleets_len))
    }
    if (!is.null(dat[["agecomp"]])) {
      dat[["age_info"]][fleets_age, "CompError"] <- 1
      dat[["age_info"]][fleets_age, "ParmSelect"] <-
        (length(fleets_len) + 1):(length(fleets_len) + length(fleets_age))
    }
    npars <- length(fleets_len) + length(fleets_age)
    # get the highest phase in the model
    last_phase <- get_last_phase(ctl)
    # add check that last_phase is less than max_phase in starter. If not,
    # modify the max phase and send warning.
    if (last_phase >= start[["last_estimation_phase"]]) {
      warning(
        "The last phase used in the control file, ",
        last_phase,
        ", is higher or the same as the last_estimation_phase in the ",
        "starter file currently set to ",
        start[["last_estimation_phase"]],
        ".",
        "Changing the last_estimation_phase in the starter file to ",
        last_phase + 1,
        "."
      )
      start[["last_estimation_phase"]] <- last_phase + 1
      SS_writestarter(start, dir = dir, verbose = FALSE, overwrite = TRUE)
    }
    ctl[["dirichlet_parms"]] <- data.frame(
      "LO" = rep(-5, times = npars),
      "HI" = 20,
      "INIT" = 0.5,
      "PRIOR" = 0,
      "PR_SD" = 1.813,
      "PR_type" = 6,
      "PHASE" = last_phase + 1,
      "env_var&link" = 0,
      "dev_link" = 0,
      "dev_minyr" = 0,
      "dev_maxyr" = 0,
      "dev_PH" = 0,
      "Block" = 0,
      "Block_Fxn" = 0
    )

    # remove weights specified through variance adjustment for comps, if any
    if (!is.null(ctl[["Variance_adjustment_list"]])) {
      message("removing composition variance adjustments from model")
      # filter out just data types 4, 5, and 7 for length, age, and size comps
      if (nrow(ctl[["Variance_adjustment_list"]] > 0)) {
        ctl[["Variance_adjustment_list"]] <-
          ctl[["Variance_adjustment_list"]][
            !ctl[["Variance_adjustment_list"]][["factor"]] %in%
              c(4, 5, 7),
          ]
      }
      # remove the list if there's nothing left
      if (nrow(ctl[["Variance_adjustment_list"]]) == 0) {
        ctl[["Variance_adjustment_list"]] <- NULL
        ctl[["DoVar_adjust"]] <- 0
      }
    }
    # Run the model once - look for convergence
    SS_writedat(
      dat,
      file.path(dir, start[["datfile"]]),
      verbose = FALSE,
      overwrite = TRUE
    )
    SS_writectl(
      ctl,
      file.path(dir, start[["ctlfile"]]),
      verbose = FALSE,
      overwrite = TRUE
    )
    if (niters_tuning > 0) {
      run(
        dir = dir,
        exe = exe,
        extras = extras,
        skipfinished = FALSE,
        verbose = verbose,
        ...
      )
      suppressWarnings(
        out <- SS_output(
          dir,
          verbose = FALSE,
          printstats = FALSE,
          covar = !grepl("nohess", extras),
          hidewarn = TRUE
        )
      )
      # figure out what to read in for weights? maybe the DM param ests?
      weights <- out[["Dirichlet_Multinomial_pars"]]
      tuning_table_list <- out[["Dirichlet_Multinomial_pars"]]
    } else {
      # maybe return something besides this weights?
      weights <- ctl[["dirichlet_parms"]]
      tuning_table_list <- ctl[["dirichlet_parms"]]
    }
  }

  # make a plot showing time series of weights
  if (option %in% c("Francis", "MI") & niters_tuning > 1) {
    plot_tunings(weights, dir)
  }

  return_list <- list(
    tuning_table_list = tuning_table_list,
    weights = weights
  )
}


#' Get the tuning table
#'
#' @template replist
#' @param fleets A vector of fleet numbers
#' @param option Which type of tuning: 'none', 'Francis', 'MI', or 'DM'
#' @param digits Number of digits to round numbers to
#' @param write Write suggested tunings to a file 'suggested_tunings.ss'
#' @template verbose
get_tuning_table <- function(
  replist,
  fleets,
  option,
  digits = 6,
  write = TRUE,
  verbose = TRUE
) {
  # check inputs
  # place to store info on data weighting
  tuning_table <- data.frame(
    factor = integer(),
    fleet = integer(),
    Var_adj = double(),
    Hash = character(),
    Old_Var_adj = double(),
    New_Francis = double(),
    New_MI = double(),
    Francis_mult = double(),
    Francis_lo = double(),
    Francis_hi = double(),
    MI_mult = double(),
    Type = character(),
    Name = character(),
    Note = character(),
    stringsAsFactors = FALSE
  )
  # loop over fleets and modify the values for length data
  for (type in c("len", "age", "size")) {
    for (fleet in fleets) {
      if (verbose) {
        message("calculating ", type, " tunings for fleet ", fleet)
      }
      if (type == "len") {
        # table of info from SS3
        tunetable <- replist[["Length_Comp_Fit_Summary"]]
        factor <- 4 # code for Control file
        has_marginal <- fleet %in% replist[["lendbase"]][["Fleet"]]
        has_conditional <- FALSE
      }
      if (type == "age") {
        # table of info from SS3
        tunetable <- replist[["Age_Comp_Fit_Summary"]]
        factor <- 5 # code for Control file
        has_marginal <- fleet %in% replist[["agedbase"]][["Fleet"]]
        has_conditional <- fleet %in% replist[["condbase"]][["Fleet"]]
      }
      if (type == "size") {
        # table of info from SS3
        tunetable <- replist[["Size_Comp_Fit_Summary"]]
        factor <- 7 # code for Control file
        has_marginal <- fleet %in% replist[["sizedbase"]][["Fleet"]]
        has_conditional <- FALSE
      }
      if (has_marginal & has_conditional) {
        warning(
          "fleet",
          fleet,
          "has both conditional ages and marginal ages",
          "\ntuning will be based on conditional ages"
        )
      }
      if (has_marginal | has_conditional) {
        # data is present, calculate stuff
        # Francis_multiplier
        Francis_mult <- NULL
        Francis_lo <- NULL
        Francis_hi <- NULL
        Francis_output <- SSMethod.TA1.8(
          fit = replist,
          type = type,
          fleet = fleet,
          plotit = FALSE,
          printit = FALSE
        )
        if (has_conditional) {
          # run separate function for conditional data
          # (replaces marginal multiplier if present)
          Francis_output <- SSMethod.Cond.TA1.8(
            fit = replist,
            fleet = fleet,
            plotit = FALSE,
            printit = FALSE
          )
        }
        Francis_mult <- Francis_output[1]
        Francis_lo <- Francis_output[2]
        Francis_hi <- Francis_output[3]
        Note <- ""
        if (is.null(Francis_output)) {
          Francis_mult <- NA
          Francis_lo <- NA
          Francis_hi <- NA
          Note <- "No Francis weight"
        }
        # current value
        Curr_Var_Adj <- NA
        if ("Curr_Var_Adj" %in% names(tunetable)) {
          Curr_Var_Adj <- tunetable[["Curr_Var_Adj"]][
            tunetable[["Fleet"]] == fleet
          ]
        }
        if ("Var_Adj" %in% names(tunetable)) {
          Curr_Var_Adj <- tunetable[["Var_Adj"]][tunetable[["Fleet"]] == fleet]
        }
        if (is.na(Curr_Var_Adj)) {
          stop(
            "Model output missing required values, perhaps due to an older version of SS3"
          )
        }

        # McAllister-Ianelli multiplier
        # that will later be multiplied by Curr_Var_Adj to get "New_MI"
        MI_mult <- NA
        if ("HarMean(effN)/mean(inputN*Adj)" %in% names(tunetable)) {
          MI_mult <- tunetable$"HarMean(effN)/mean(inputN*Adj)"[
            tunetable[["Fleet"]] == fleet
          ]
        }
        if ("MeaneffN/MeaninputN" %in% names(tunetable)) {
          MI_mult <- tunetable$"MeaneffN/MeaninputN"[
            tunetable[["Fleet"]] == fleet
          ]
        }
        if ("factor" %in% names(tunetable)) {
          # starting with version 3.30.12
          MI_mult <- tunetable[["Recommend_var_adj"]][
            tunetable[["Fleet"]] == fleet
          ] /
            tunetable[["Curr_Var_Adj"]][tunetable[["Fleet"]] == fleet]
        }
        if (all(c("HarMean", "mean_Nsamp_adj") %in% names(tunetable))) {
          # starting with version 3.30.16?
          MI_mult <-
            tunetable[["HarMean"]][tunetable[["Fleet"]] == fleet] /
            tunetable[["mean_Nsamp_adj"]][tunetable[["Fleet"]] == fleet]
        }
        if (is.na(MI_mult)) {
          stop(
            "Model output missing required values, perhaps due to an older version of SS3"
          )
        }

        # make new row for table
        newrow <-
          data.frame(
            factor = factor,
            fleet = fleet,
            New_Var_adj = NA,
            hash = "#",
            Old_Var_adj = round(Curr_Var_Adj, digits),
            New_Francis = round(Curr_Var_Adj * Francis_mult, digits),
            New_MI = round(Curr_Var_Adj * MI_mult, digits),
            Francis_mult = round(Francis_mult, digits),
            Francis_lo = round(Francis_lo, digits),
            Francis_hi = round(Francis_hi, digits),
            MI_mult = round(MI_mult, digits),
            Type = type,
            Name = replist[["FleetNames"]][fleet],
            Note = Note,
            stringsAsFactors = FALSE
          )

        # add row to existing table
        tuning_table <- rbind(tuning_table, newrow)
      } # end check for data type for this fleet
    } # end loop over fleets
  } # end loop over length or age

  # fill in new variance adjustment based on chosen option
  if (option == "none") {
    tuning_table[["New_Var_adj"]] <- tuning_table[["Old_Var_adj"]]
  }
  if (option == "Francis") {
    tuning_table[["New_Var_adj"]] <- tuning_table[["New_Francis"]]
    NAvals <- is.na(tuning_table[["New_Var_adj"]])
    tuning_table[["New_Var_adj"]][NAvals] <- tuning_table[["New_MI"]][NAvals]
    tuning_table[["Note"]][NAvals] <- paste0(
      tuning_table[["Note"]][NAvals],
      "--using MI value"
    )
  }
  if (option == "MI") {
    tuning_table[["New_Var_adj"]] <- tuning_table[["New_MI"]]
  }
  names(tuning_table)[1] <- "#factor" # add hash to facilitate pasting into Control
  rownames(tuning_table) <- 1:nrow(tuning_table)

  # stuff related to generalized size frequency data
  tunetable_size <- replist[["Size_Comp_Fit_Summary"]]
  if (!is.null(tunetable_size) && "par1" %in% names(tunetable_size)) {
    warning(
      "This function may not work for generalized size composition data ",
      "in models prior to SS3 version 3.30.20."
    )
  }

  # return the results
  if (write) {
    file <- file.path(replist[["inputs"]][["dir"]], "suggested_tuning.ss")
    if (verbose) {
      message("writing to file ", file)
    }
    write.table(tuning_table, file = file, quote = FALSE, row.names = FALSE)
  }
  # return the table
  return(tuning_table)
}

#' Get the highest phase used in the control file
#'
#' @param ctl A control file list read in using `r4ss::SS_readctl`.
#' @author Kathryn L. Doering
get_last_phase <- function(ctl) {
  # read all phases in ctl
  df_vec <- c(
    "MG_parms",
    "MG_parms_tv",
    "MG_parms_seas",
    "SRparm",
    "SR_parms",
    "SR_parms_tv",
    "recr_cycle_pars",
    "init_F",
    "Q_parms",
    "Q_parms_tv",
    "size_selex_parms",
    "size_selex_parms_tv",
    "age_selex_parms",
    "age_selex_parms_tv",
    "dirichlet_parms",
    "pars_2D_AR",
    "TG_loss_init",
    "TG_loss_cronic",
    "TG_overdispersion",
    "TG_Report_fleet",
    "TG_Report_fleet_decay"
  )
  atomic_vec <- c("recdev_phase", "recdev_early_phase", "Fcast_recr_phase")
  phases <- c(
    unlist(lapply(
      df_vec,
      function(x, l) {
        l[[x]][, grep("PHASE|dev_PH", colnames(l[[x]])), drop = FALSE]
      },
      l = ctl
    )),
    unlist(lapply(atomic_vec, function(x, l) l[[x]], l = ctl)),
    ctl[["F_setup2"]][2],
    ctl[["specs_2D_AR"]][, "devphase"]
  )
  last_phase <- ceiling(max(phases)) # round up if not integer value.
}

#' Plot the comp tunings from tune_comps() by iteration
#'
#' @param weights The list of variance adjustment tables created by
#' [tune_comps()]. This can either by the list of both "tuning_table_list"
#' and "weights" returned by [tune_comps()] or just the "weights" list.
#' @param dir Directory in which to save the plot as a png file.
#' If `dir == NULL` then the plot will be created in the default
#' graphics device.
#'
plot_tunings <- function(weights, dir = NULL) {
  # Declare global variables to avoid "no visible binding" errors
  utils::globalVariables(c("iteration", "value", "fleet", "data_type"))

  browser()
  # if the function is called outside of tune_comps() using the
  # output from that function, then extract weights
  if ("tuning_table_list" %in% names(weights)) {
    weights <- weights$weights
  }
  if (length(weights) == 1) {
    stop("tune_comps needs more than one iteration to make a plot")
  }

  # Combine list of dataframes into one long dataframe with iteration column using tidyverse
  weights_df <- purrr::map_dfr(
    seq_along(weights),
    ~ dplyr::mutate(weights[[.x]], iteration = .x)
  ) |>
    dplyr::mutate(
      data_type = factor(
        factor,
        levels = c(1, 2, 3, 4, 5, 6, 7),
        labels = c(
          "Survey CV",
          "Discard SD",
          "Bodywt CV",
          "Len",
          "Age",
          "Size-at-age",
          "Size"
        )
      )
    )

  ylab <- "Sample size multiplier"
  if (any(weights[[1]]$Data_type %in% 1:3)) {
    ylab <- "Variance adjustment"
  }
  p <- ggplot(
    weights_df,
    aes(
      x = iteration,
      y = value,
      color = factor(fleet),
      group = interaction(data_type, fleet),
      shape = data_type
    )
  ) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(x = "Iteration", y = ylab, color = "Fleet", shape = "Data Type") +
    theme_minimal(base_size = 14) +
    expand_limits(y = 0)

  if (!is.null(dir)) {
    ggsave(
      filename = file.path(dir, "tuning_values.png"),
      plot = p,
      width = 6.5,
      height = 6.5,
      units = "in",
      dpi = 300
    )
  }
}
