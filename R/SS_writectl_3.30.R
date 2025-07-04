#' write control file for SS3 version 3.30
#'
#' write Stock Synthesis control file from list object in R which was created
#'   using [SS_readctl()].This function is designed to be called
#'   using [SS_writectl()] and should not be called directly.
#'
#' @param ctllist  List object created by [SS_readctl()].
#' @param outfile Filename for where to write new data file.
#' @template overwrite
#' @template verbose
#' @author Kathryn L. Doering, Yukio Takeuchi, Neil Klaer, Watal M. Iwasaki,
#' Nathan R. Vaughan
#' @export
#' @seealso [SS_readctl()], [SS_readctl_3.30()],[SS_readstarter()],
#' [SS_readforecast()],
#' [SS_writestarter()], [SS_writeforecast()],
#' [SS_writedat()]
#'
SS_writectl_3.30 <- function(
  ctllist,
  outfile,
  overwrite = FALSE,
  verbose = FALSE
) {
  if (verbose) {
    message("Running SS_writectl_3.30\n")
  }
  # input checks
  if (ctllist[["ReadVersion"]] != "3.30") {
    stop(
      "ReadVersion must be '3.30', but is ",
      ctllist[["ReadVersion"]],
      ". ",
      "Please make sure the control file list object is created from a 3.30",
      " SS3 model and not an earlier version."
    )
  }

  if (file.exists(outfile)) {
    if (!overwrite) {
      message("File exists and input 'overwrite'=FALSE:", outfile)
      return()
    } else {
      file.remove(outfile)
    }
  }
  if (verbose) {
    message("Opening connection to ", outfile, "\n")
  }
  zz <- file(outfile, open = "at") # open = "at" means open for appending in text mode.
  on.exit(close(zz)) # Needed in case the function exits early.

  # wl = write a line with a single value to an SS3 control file.
  # @param name The name of the list component in ctllist to find
  # @param comment A comment to put after the value on the line. If no "#" is
  # included at the beginning, then "#_" is added to the beginning of the comment
  # before writing to file.
  wl <- function(name, comment = NULL, con = stdout) {
    # simple function to clean up many repeated commands
    value <- ctllist[names(ctllist) == name]
    if (is.null(comment)) {
      writeLines(paste(value, " #_", name, sep = "", collapse = "_"), con = zz)
    } else {
      if (length(grep(comment, pattern = "^#")) != 0) {
        writeLines(paste(value, comment), con = zz)
      } else {
        writeLines(
          paste(value, " #_", comment, sep = "", collapse = "_"),
          con = zz
        )
      }
    }
  }

  # wl = write a line with a vector of values to an SS3 control file.
  # @param name The name of the list component in ctllist to find
  # @param comment A comment to put after the value on the line. If no "#" is
  # included at the beginning, then "#_" is added to the beginning of the comment
  # before writing to file.
  wl.vector <- function(name, comment = NULL) {
    # simple function to clean up many repeated commands
    value <- ctllist[names(ctllist) == name][[1]]
    if (is.null(comment)) {
      writeLines(
        paste(paste(value, collapse = " "), " #_", name, sep = ""),
        con = zz
      )
    } else {
      writeLines(paste(paste(value, collapse = " "), comment), con = zz)
    }
  }
  # write a line if the values are in a list.
  wl.list <- function(name, comment = NULL, header = NULL) {
    if (!is.null(header)) {
      writeLines(paste0("#_", header), con = zz)
    }
    value <- ctllist[names(ctllist) == name][[1]]
    value1 <- sapply(
      value,
      function(x) {
        paste(paste(x), collapse = " ")
      },
      simplify = TRUE
    )
    writeLines(value1, con = zz)
  }

  ## Internal function to write formatted data.frame
  ## @param cols_to_rm Defaults to NULL. If need to remove any cols, add the
  #    index or indices of the cols here.
  printdf <- function(
    dataframe,
    header = TRUE,
    headerLine = NA,
    cols_to_rm = NULL,
    terminate = FALSE
  ) {
    # function to print data frame with hash mark before first column name
    if (is.character(dataframe)) {
      tmp <- ctllist[names(ctllist) == dataframe]
      if (length(tmp) > 0) {
        dataframe <- tmp[[1]]
      } else {
        dataframe <- NULL
      }
    }
    if (!is.null(dataframe)) {
      # remove columns if desired.
      if (!is.null(cols_to_rm)) {
        dataframe <- dataframe[, -cols_to_rm]
      }
      if (terminate) {
        # add terminator line to end data frame
        newline <- c(-9999, rep(0, ncol(dataframe) - 1))
        dataframe <- rbind(dataframe, newline)
        rownames(dataframe)[nrow(dataframe)] <- "terminator"
      }
      if (header) {
        if (isTRUE(!is.null(dataframe[["PType"]]))) {
          warning(
            "Please remove PType column in parameter dataframe, ",
            "which was deprecated as of r4ss 1.45.0."
          )
          dataframe[["PType"]] <- NULL
        }
        names(dataframe)[1] <- paste("#_", names(dataframe)[1], sep = "")
        writeLines(paste(names(dataframe), collapse = "\t"), con = zz)
      }
      if (!is.na(headerLine)) {
        xxx <- 2
      }
      if (!is.null(rownames(dataframe))) {
        rownames(dataframe) <- sapply(
          rownames(dataframe),
          function(z) {
            ifelse(length(grep(x = z, pattern = "^#")) == 1, z, paste0("#_", z))
          }
        )
        dataframe[["comments"]] <- rownames(dataframe)
      }
      write_fwf4(
        file = zz,
        x = dataframe,
        append = TRUE,
        sep = "\t",
        quote = FALSE,
        rownames = FALSE,
        colnames = FALSE,
        digits = 6
      )
    }
  }

  # internally used commmon values ----
  lng_par_colnames <- c(
    "LO",
    "HI",
    "INIT",
    "PRIOR",
    "PR_SD",
    "PR_type",
    "PHASE",
    "env_var&link",
    "dev_link",
    "dev_minyr",
    "dev_maxyr",
    "dev_PH",
    "Block",
    "Block_Fxn"
  )
  srt_par_colnames <- c(
    "LO",
    "HI",
    "INIT",
    "PRIOR",
    "PR_SD",
    "PR_type",
    "PHASE"
  )

  # Write a header ----
  add_file_header(ctllist, con = zz)

  # Write the contents ----
  # Make comments as consistent with SS3 3.30 as possible.
  # Beginning of ctl file ----

  wl(
    "EmpiricalWAA",
    comment = paste0(
      "# 0 means do not read wtatage.ss; 1 means read and use",
      "wtatage.ss and also read and use growth parameters"
    )
  )
  wl("N_GP", comment = "N_Growth_Patterns") # N_Growth_Patterns
  wl("N_platoon", comment = "N_platoons_Within_GrowthPattern")
  if (ctllist[["N_platoon"]] > 1) {
    # Conditional inputs needed if more than 1 platoon.
    wl("sd_ratio", comment = "Morph_between/within_stdev_ratio")
    wl.vector(
      "submorphdist",
      comment = "# vector_Morphdist_(-1_in_first_val_gives_normal_approx)"
    )
  }
  # Recruitment distribution ----
  wl("recr_dist_method", comment = "# recr_dist_method for parameters")
  wl(
    "recr_global_area",
    comment = paste0(
      "# not yet implemented; Future usage:",
      "Spawner-Recruitment; 1=global; 2=by area"
    )
  )
  wl(
    "recr_dist_read",
    comment = paste0("# number of recruitment settlement assignments ")
  )
  wl("recr_dist_inx", comment = "# unused option")
  writeComment("# for each settlement assignment:", con = zz)
  printdf("recr_dist_pattern")
  writeComment("#", con = zz)
  # Movement ----
  if (ctllist[["N_areas"]] > 1) {
    wl(
      "N_moveDef",
      comment = "#_N_movement_definitions goes here if N_areas > 1"
    )
    if (ctllist[["N_moveDef"]] > 0) {
      wl(
        "firstAgeMove",
        comment = paste0(
          "#_first age that moves (real age at begin of season, ",
          "not integer) also cond on do_migration>0"
        )
      )
      writeComment(
        "move definition for seas, morph, source, dest, age1, age2",
        con = zz
      )
      printdf("moveDef", header = FALSE)
    }
  } else {
    writeComment(
      "#_Cond 0 # N_movement_definitions goes here if N_areas > 1",
      con = zz
    )
    writeComment(
      paste0(
        "#_Cond 1.0 # first age that moves (real age at begin ",
        "of season, not integer) also cond on do_migration>0"
      ),
      con = zz
    )
    writeComment(
      paste0(
        "#_Cond 1 1 1 2 4 10 # example move definition for ",
        "seas=1, morph=1, source=1 dest=2, age1=4, age2=10"
      ),
      con = zz
    )
    writeComment("#", con = zz)
  }
  # Block setup ----
  wl("N_Block_Designs", comment = "#_Nblock_Patterns")
  if (ctllist[["N_Block_Designs"]] > 0) {
    wl.vector("blocks_per_pattern", comment = "#_blocks_per_pattern")
    wl.list("Block_Design", header = "begin and end years of blocks")
  } else {
    writeComment("#_Cond 0 #_blocks_per_pattern", con = zz)
    writeComment("# begin and end years of blocks", con = zz)
  }
  writeComment("#", con = zz)
  # timevary controls ----
  writeComment("# controls for all timevary parameters ", con = zz)
  wl(
    "time_vary_adjust_method",
    comment = paste0(
      "#_env/block/dev_adjust_method for all time-vary parms ",
      "(1=warn relative to base parm bounds; 3=no bound check)"
    )
  )
  writeComment("#", con = zz)
  writeComment("# AUTOGEN", con = zz)
  wl.vector(
    "time_vary_auto_generation",
    comment = paste0(
      "# autogen: 1st element for biology, 2nd for SR, ",
      "3rd for Q, 4th reserved, 5th for selex"
    )
  )
  writeComment(
    paste0(
      "# where: 0 = autogen all time-varying parms; 1 = read ",
      "each time-varying parm line; 2 = read then autogen if ",
      "parm min==-12345"
    ),
    con = zz
  )
  # MG setup ----
  writeComment("#", con = zz)
  writeComment(
    paste0(
      "# setup for M, growth, maturity, fecundity, recruitment",
      " distibution, movement"
    ),
    con = zz
  )
  writeComment("#", con = zz)
  # M setup ----
  wl(
    "natM_type",
    comment = paste0(
      "#_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;",
      "_3=agespecific;_4=agespec_withseasinterpolate;_5=Maunder_M;_6=Age-range_Lorenzen"
    )
  )
  # Following lines depends on the natM_type value.
  if (ctllist[["natM_type"]] == 0) {
    writeComment(
      "#_no additional input for selected M option; read 1P per morph",
      con = zz
    )
  } else if (ctllist[["natM_type"]] == 1) {
    wl("N_natM", comment = "#_N_breakpoints")
    wl.vector("M_ageBreakPoints", comment = "# age(real) at M breakpoints")
  } else if (ctllist[["natM_type"]] == 2) {
    wl(
      "Lorenzen_refage",
      comment = "#_reference age for Lorenzen M; later read 1P per Sex x G Morph"
    )
  } else if (ctllist[["natM_type"]] %in% c(3, 4)) {
    writeComment(" #_Age_natmort_by sex x growthpattern", con = zz)
    printdf("natM")
  } else if (ctllist[["natM_type"]] == 6) {
    wl("Lorenzen_minage", comment = "#_minimum age for Age-range Lorenzen M;")
    wl("Lorenzen_maxage", comment = "#_maximum age for Age-range Lorenzen M;")
    writeComment(" #_later read 1P per Sex x G Morph", con = zz)
  } else {
    stop("natM_type : ", ctllist[["natM_type"]], " is not supported")
  }
  # Growth Setup ----
  wl(
    "GrowthModel",
    comment = paste0(
      "# GrowthModel: 1=vonBert with L1&L2; 2=Richards with ",
      "L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;",
      "5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation"
    )
  )
  wl(
    "Growth_Age_for_L1",
    comment = "#_Age(post-settlement)_for_L1;linear growth below this"
  )
  wl("Growth_Age_for_L2", comment = "#_Growth_Age_for_L2 (999 to use as Linf)")
  wl(
    "Exp_Decay",
    comment = paste0(
      "#_exponential decay for growth above maxage (value ",
      "should approx initial Z; -999 replicates 3.24; -998 to ",
      "not allow growth above maxage)"
    )
  )
  wl("Growth_Placeholder", comment = "#_placeholder for future growth feature")
  # Need the following if statements because there are conditional lines that are
  # necessary for only some growth methods
  if (ctllist[["GrowthModel"]] %in% 3:5) {
    wl("N_ageK", comment = "# number of K multipliers to read")
    wl.vector("Age_K_points", comment = "# ages for K multiplier")
  }
  # Below check added so users can investigate why the ctllist can't be written.
  if (!ctllist[["GrowthModel"]] %in% c(1:5, 8)) {
    stop(
      "The GrowthModel",
      ctllist[["GrowthModel"]],
      "in ctllist ",
      ctllist,
      " is not an option in SS3 3.30. Valid growth options are 1-5 and 8."
    )
  }
  writeComment("#", con = zz)
  wl(
    "SD_add_to_LAA",
    comment = "#_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)"
  )
  wl(
    "CV_Growth_Pattern",
    comment = paste0(
      "#_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); ",
      "2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)"
    )
  )
  # maturity and MG options setup ----
  wl(
    "maturity_option",
    comment = paste0(
      "#_maturity_option:  1=length logistic; 2=age logistic; ",
      "3=read age-maturity matrix by growth_pattern; ",
      "4=read age-fecundity; 5=disabled; 6=read length-maturity"
    )
  )
  # Below check added to help users with troubleshooting
  if (!ctllist[["maturity_option"]] %in% c(1:6)) {
    stop(
      "Invalid maturity option used. ctllist[['maturity_option']] is",
      ctllist[["maturity_option"]],
      ", but must be 1, 2, 3, 4, 5, or 6."
    )
  }
  # Below if statements are lines are conditional on the maturity option chosen
  if (ctllist[["maturity_option"]] %in% c(3, 4)) {
    writeComment("# Age Maturity or Age fecundity:", con = zz)
    printdf("Age_Maturity")
  }
  if (ctllist[["maturity_option"]] == 6) {
    writeComment("# Length Maturity: ", con = zz)
    printdf("Length_Maturity")
  }
  wl("First_Mature_Age", comment = "#_First_Mature_Age")
  wl(
    "fecundity_option",
    comment = paste0(
      "fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;",
      "(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W"
    )
  )
  wl(
    "hermaphroditism_option",
    comment = paste0(
      "#_hermaphroditism option:  0=none; 1=female-to-male ",
      "age-specific fxn; -1=male-to-female age-specific fxn"
    )
  )
  if (!ctllist[["hermaphroditism_option"]] %in% c(0, 1, -1)) {
    stop(
      "Invalid hermaphroditism_option specified in ctllist. Its value is ",
      ctllist[["hermaphroditism_option"]],
      ", but can only be 0, 1, or -1."
    )
  }
  # Below if statement conditional on the hermaphroditism option chosen
  if (ctllist[["hermaphroditism_option"]] %in% c(1, -1)) {
    wl("Herm_season", comment = "# Hermaphro_season ")
    wl("Herm_MalesInSSB", comment = "# Hermaphro_maleSSB")
  }
  wl(
    "parameter_offset_approach",
    comment = paste0(
      "parameter_offset_approach (1=none, 2= M, G, CV_G as ",
      "offset from female-GP1, 3=like SS2 V1.x)"
    )
  )
  # MG parms ----
  writeComment(c("#", "#_growth_parms"), con = zz)
  printdf("MG_parms")

  # MG timevarying parms ----
  if (
    any(ctllist[["MG_parms"]][, c("env_var&link", "dev_link", "Block")] != 0) &
      ctllist[["time_vary_auto_generation"]][1] != 0
  ) {
    writeComment("timevary MG parameters", con = zz)
    printdf("MG_parms_tv")
    writeComment(
      paste0(
        "# info on dev vectors created for MGparms are ",
        "reported with other devs after tag parameter section"
      ),
      con = zz
    )
  } else {
    writeComment("no timevary MG parameters", con = zz)
  }

  # Seasonal effects ----
  writeComment("#", con = zz)
  writeComment("#_seasonal_effects_on_biology_parms", con = zz)
  wl.vector(
    "MGparm_seas_effects",
    comment = paste0(
      "#_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,",
      "Malewtlen1,malewtlen2,L1,K"
    )
  )
  if (sum(ctllist[["MGparm_seas_effects"]]) > 0) {
    printdf("MG_parms_seas")
    writeComment("#", con = zz)
  } else {
    writeComment("#_ LO HI INIT PRIOR PR_SD PR_type PHASE", con = zz)
    writeComment(
      c(
        "#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters",
        "#"
      ),
      con = zz
    )
  }
  # SR -----
  wl(
    "SR_function",
    comment = paste0(
      "Spawner-Recruitment; 2=Ricker (2 parms); 3=std_B-H(2); 4=SCAA(2);",
      "5=Hockey(3); 6=B-H_flattop(2); 7=Survival(3);",
      "8=Shepard(3);",
      "9=Ricker_Power(3);",
      "10=B-H_a,b(4)"
    )
  )
  wl(
    "Use_steep_init_equi",
    comment = "# 0/1 to use steepness in initial equ recruitment calculation"
  )
  wl(
    "Sigma_R_FofCurvature",
    comment = paste0(
      "# future feature: 0/1 to make realized sigmaR a ",
      "function of SR curvature"
    )
  )
  # SR parms ----
  # change column names to match control.ss_new
  colnames(ctllist[["SR_parms"]]) <- c(
    "LO",
    "HI",
    "INIT",
    "PRIOR",
    "PR_SD",
    "PR_type",
    "PHASE",
    "env-var",
    "use_dev",
    "dev_mnyr",
    "dev_mxyr",
    "dev_PH",
    "Block",
    "Blk_Fxn # parm_name"
  )
  # "Blk_Fxn # parm_name" is just to get the parm_name header printed, too.
  printdf("SR_parms")
  # reset column names back.
  colnames(ctllist[["SR_parms"]]) <- lng_par_colnames

  # SR tv parms ----
  if (
    any(ctllist[["SR_parms"]][, c("env_var&link", "dev_link", "Block")] != 0) &
      ctllist[["time_vary_auto_generation"]][2] != 0
  ) {
    writeComment("# timevary SR parameters", con = zz)
    printdf("SR_parms_tv")
  } else {
    writeComment("no timevary SR parameters", con = zz)
  }
  # recdevs ----
  wl(
    "do_recdev",
    comment = paste0(
      "#do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); ",
      "2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; ",
      "dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty"
    )
  )
  wl(
    "MainRdevYrFirst",
    comment = "# first year of main recr_devs; early devs can preceed this era"
  )
  wl(
    "MainRdevYrLast",
    comment = "# last year of main recr_devs; forecast devs start in following year"
  )
  wl("recdev_phase", comment = "recdev phase")
  wl("recdev_adv", comment = "# (0/1) to read 13 advanced options")

  if (ctllist[["recdev_adv"]] == 1) {
    if (verbose) {
      message("Writing 13 advanced SRR options\n")
    }
    wl(
      "recdev_early_start",
      comment = paste0(
        "#_recdev_early_start (0=none; neg value makes relative",
        " to recdev_start)"
      )
    )
    wl("recdev_early_phase", comment = "#_recdev_early_phase")
    wl(
      "Fcast_recr_phase",
      comment = paste0(
        "#_forecast_recruitment phase (incl. late recr) (0 ",
        "value resets to maxphase+1)"
      )
    )
    wl(
      "lambda4Fcast_recr_like",
      comment = "#_lambda for Fcast_recr_like occurring before endyr+1"
    )
    wl(
      "last_early_yr_nobias_adj",
      comment = "#_last_yr_nobias_adj_in_MPD; begin of ramp"
    )
    wl(
      "first_yr_fullbias_adj",
      comment = "#_first_yr_fullbias_adj_in_MPD; begin of plateau"
    )
    wl("last_yr_fullbias_adj", comment = "#_last_yr_fullbias_adj_in_MPD")
    wl(
      "first_recent_yr_nobias_adj",
      comment = paste0(
        "#_end_yr_for_ramp_in_MPD (can be in forecast to shape",
        " ramp, but SS sets bias_adj to 0.0 for fcast yrs)"
      )
    )
    wl(
      "max_bias_adj",
      comment = paste0(
        "#_max_bias_adj_in_MPD (-1 to override ramp and set ",
        "biasadj=1.0 for all estimated recdevs)"
      )
    )
    wl(
      "period_of_cycles_in_recr",
      comment = "#_period of cycles in recruitment (N parms read below)"
    )
    wl("min_rec_dev", comment = "#min rec_dev")
    wl("max_rec_dev", comment = "#max rec_dev")
    wl("N_Read_recdevs", comment = "#_read_recdevs")
    writeComment("end of advanced SR options", con = zz)
    writeComment("#", con = zz)

    if (ctllist[["period_of_cycles_in_recr"]] > 0) {
      printdf("recr_cycle_pars")
    } else {
      writeComment(
        "#_placeholder for full parameter lines for recruitment cycles",
        con = zz
      )
    }
    if (ctllist[["N_Read_recdevs"]] > 0) {
      printdf("recdev_input")
    } else {
      writeComment("# read specified recr devs", con = zz)
      writeComment("#_Yr Input_value", con = zz)
    }
  }
  writeComment("#", con = zz)

  ##
  # F setup ----
  writeComment("#Fishing Mortality info", con = zz)
  wl("F_ballpark", comment = "# F ballpark")
  wl("F_ballpark_year", comment = "# F ballpark year (neg value to disable)")
  wl(
    "F_Method",
    comment = "# F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)"
  )
  wl("maxF", comment = "# max F or harvest rate, depends on F_Method")
  if (ctllist[["F_Method"]] == 2) {
    writeComment(
      "overall start F value; overall phase; N detailed inputs to read",
      con = zz
    )
    wl.vector("F_setup")
    if (ctllist[["F_setup"]][length(ctllist[["F_setup"]])] > 0) {
      printdf("F_setup2")
    }
  }
  if (ctllist[["F_Method"]] == 3) {
    wl(
      "F_iter",
      comment = "# N iterations for tuning F in hybrid method (recommend 3 to 7)"
    )
  }
  if (ctllist[["F_Method"]] == 4) {
    printdf("F_4_Fleet_Parms", terminate = TRUE)
    wl(
      "F_iter",
      comment = "# N iterations for tuning F in hybrid method (recommend 3 to 7)"
    )
  }
  writeComment("#", con = zz)
  if (!is.null(ctllist[["init_F"]])) {
    writeComment("initial_F_parms", con = zz)
    printdf("init_F", cols_to_rm = 8)
  } else {
    writeComment("initial_F_parms; count = 0", con = zz)
  }
  writeComment("#", con = zz)
  # Q setup ----
  writeComment("#_Q_setup for fleets with cpue or survey data", con = zz)
  if (!is.null(ctllist[["Q_options"]])) {
    # There are extra commments with info here in control.ss_new, but exclude for now
    tmp_collength <- length(colnames(ctllist[["Q_options"]]))
    # change column names to match control.ss_new
    colnames(ctllist[["Q_options"]])[tmp_collength] <- "float  #  fleetname"
    printdf("Q_options", terminate = TRUE)
    # change back
    colnames(ctllist[["Q_options"]])[tmp_collength] <- "float"

    writeComment("#_Q_parms(if_any);Qunits_are_ln(q)", con = zz)
    # change column names to match control.ss_new
    colnames(ctllist[["Q_parms"]]) <- c(
      "LO",
      "HI",
      "INIT",
      "PRIOR",
      "PR_SD",
      "PR_type",
      "PHASE",
      "env-var",
      "use_dev",
      "dev_mnyr",
      "dev_mxyr",
      "dev_PH",
      "Block",
      "Blk_Fxn  #  parm_name"
    )
    printdf("Q_parms")
    # change back
    colnames(ctllist[["Q_parms"]]) <- lng_par_colnames
  } else {
    writeLines(text = "-9999 0 0 0 0 0 # terminator", con = zz)
    writeComment("#_Q_parms(if_any);Qunits_are_ln(q)", con = zz)
  }
  # time varying q parm lines -----
  if (
    !is.null(ctllist[["Q_options"]]) &&
      any(ctllist[["Q_parms"]][, c("env_var&link", "dev_link", "Block")] != 0) &
      ctllist[["time_vary_auto_generation"]][3] != 0
  ) {
    writeComment("# timevary Q parameters", con = zz)
    printdf("Q_parms_tv")
    writeComment(
      paste0(
        "# info on dev vectors created for Q parms are ",
        "reported with other devs after tag parameter section"
      ),
      con = zz
    )
    writeComment("#", con = zz)
  } else {
    writeComment(c("#_no timevary Q parameters"), con = zz)
    writeComment("#", con = zz)
  }
  # Size selectivity setup ----
  # change row names to match with 3.30.14 version of control.ss_new
  rownames(ctllist[["size_selex_types"]]) <- paste(
    1:nrow(ctllist[["size_selex_types"]]),
    rownames(ctllist[["size_selex_types"]])
  )
  writeComment("#_size_selex_patterns", con = zz)
  printdf("size_selex_types")
  writeComment("#", con = zz)
  # Age selectivity setup ----
  # change row names to match with 3.30.14 version of control.ss_new
  rownames(ctllist[["age_selex_types"]]) <- paste(
    1:nrow(ctllist[["age_selex_types"]]),
    rownames(ctllist[["age_selex_types"]])
  )
  writeComment("#_age_selex_patterns", con = zz)
  printdf("age_selex_types")
  writeComment("#", con = zz)
  # selectivity parameters ------

  writeComment("SizeSelex", con = zz)
  if (!is.null(ctllist[["size_selex_parms"]])) {
    # change header to match with control.ss_new
    colnames(ctllist[["size_selex_parms"]]) <- c(
      "LO",
      "HI",
      "INIT",
      "PRIOR",
      "PR_SD",
      "PR_type",
      "PHASE",
      "env-var",
      "use_dev",
      "dev_mnyr",
      "dev_mxyr",
      "dev_PH",
      "Block",
      "Blk_Fxn  #  parm_name"
    )
    printdf("size_selex_parms")
    # change back
    colnames(ctllist[["size_selex_parms"]]) <- lng_par_colnames
  } else {
    writeComment("#_No size_selex_parm", con = zz)
  }
  writeComment("AgeSelex", con = zz)
  if (!is.null(ctllist[["age_selex_parms"]])) {
    printdf("age_selex_parms", header = FALSE)
  } else {
    writeComment("#_No age_selex_parm", con = zz)
  }

  # Dirichlet MN pars ----
  if (!is.null(ctllist[["dirichlet_parms"]])) {
    writeComment("#_Dirichlet parameters", con = zz)
    printdf("dirichlet_parms", header = FALSE)
  }

  # TV selectivity pars ----
  # TODO: TV selectivity (devs,env link, and blocks) need to be  implemented in
  # readctl_3.30; then, read parameters here.
  tv_sel_cmt <- FALSE # use to track if any tv selectivity pars have been written
  if (
    any(
      ctllist[["size_selex_parms"]][, c("env_var&link", "dev_link", "Block")] !=
        0
    ) &
      ctllist[["time_vary_auto_generation"]][5] != 0
  ) {
    writeComment("# timevary selex parameters ", con = zz)
    tv_sel_cmt <- TRUE
    printdf("size_selex_parms_tv")
  }
  if (
    any(
      ctllist[["age_selex_parms"]][, c("env_var&link", "dev_link", "Block")] !=
        0
    ) &
      ctllist[["time_vary_auto_generation"]][5] != 0
  ) {
    if (tv_sel_cmt == FALSE) {
      writeComment("# timevary selex parameters ", con = zz)
    }
    tv_sel_cmt <- TRUE
    printdf("age_selex_parms_tv")
  }
  if (tv_sel_cmt == FALSE) {
    # in this case, this means no tv lines written
    writeComment("no timevary selex parameters", con = zz)
    writeComment("#", con = zz)
  } else {
    writeComment(
      paste0(
        "# info on dev vectors created for selex parms are ",
        "reported with other devs after tag parameter section"
      ),
      con = zz
    )
    writeComment("#", con = zz)
  }
  # 2DAR sel ----
  wl(
    "Use_2D_AR1_selectivity",
    comment = "#  use 2D_AR1 selectivity(0/1):  experimental feature"
  )
  if (ctllist[["Use_2D_AR1_selectivity"]] == 0) {
    writeComment("#_no 2D_AR1 selex offset used", con = zz)
  } else if (ctllist[["Use_2D_AR1_selectivity"]] == 1) {
    writeComment(
      "#_specifications for 2D_AR1 and associated parameters",
      con = zz
    )
    for (irow in 1:nrow(ctllist[["specs_2D_AR"]])) {
      printdf(ctllist[["specs_2D_AR"]][irow, ])
      fleet <- ctllist[["specs_2D_AR"]][irow, "fleet"]
      printdf(
        ctllist[["pars_2D_AR"]][
          grepl(
            pattern = paste0("fleet", fleet),
            x = rownames(ctllist[["pars_2D_AR"]])
          ),
        ]
      )
    }
    writeLines(text = "-9999 1 1 1 1 1 1 1 1 1 1 # Terminator ", con = zz)
  } else {
    stop(
      "ctllist[['Use_2D_AR1_selectivity']] has value ",
      ctllist[["Use_2D_AR1_selectivity"]],
      ", but can only have value 0 or 1."
    )
  }
  # Tag model parameters ----
  writeComment("# Tag loss and Tag reporting parameters go next", con = zz)
  wl("TG_custom", comment = "# TG_custom:  0=no read; 1=read if tags exist")
  if (ctllist[["TG_custom"]] == 0) {
    writeComment(
      c(
        "#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters",
        "#"
      ),
      con = zz
    )
  } else if (ctllist[["TG_custom"]] == 1) {
    printdf("TG_Loss_init")
    printdf("TG_Loss_chronic", header = FALSE)
    printdf("TG_overdispersion", header = FALSE)
    printdf("TG_Report_fleet", header = FALSE)
    printdf("TG_Report_fleet_decay", header = FALSE)
  } else {
    stop(
      "ctllist[['TG_custom']] has value ",
      ctllist[["TG_custom"]],
      " but can only",
      "have value 0 or 1."
    )
  }
  # Time varying parameters for tagging, would go here, if implemented.
  if (verbose) {
    warning(
      "Please note that time varying parameters for tagging not yet ",
      "implemented as of SS3 version 3.30.13"
    )
  }
  # Var Adj ----
  writeComment("# Input variance adjustments factors: ", con = zz)
  if (ctllist[["DoVar_adjust"]] == 0) {
    ctllist[["tmp_var"]] <- c(-9999, 1, 0)
    writeComment("#_Data_type Fleet Value", con = zz)
    wl.vector("tmp_var", comment = "# terminator")
  } else if (ctllist[["DoVar_adjust"]] == 1) {
    printdf("Variance_adjustment_list", terminate = TRUE)
  }
  writeComment("#", con = zz)
  # Lambdas ----
  wl("maxlambdaphase", comment = "#_maxlambdaphase")
  wl(
    "sd_offset",
    comment = paste0(
      "#_sd_offset; must be 1 if any growthCV, sigmaR, or ",
      "survey extraSD is an estimated parameter"
    )
  )
  writeComment(
    paste0(
      "# read ",
      ctllist[["N_lambdas"]],
      " changes to default ",
      "Lambdas (default value is 1.0)"
    ),
    con = zz
  )
  # There are some more .ss_new comments here, but not included for now.
  if ((ctllist[["N_lambdas"]] > 0) & (!is.null(ctllist[["lambdas"]]))) {
    if (nrow(ctllist[["lambdas"]]) != ctllist[["N_lambdas"]]) {
      stop(
        "ctllist components N_lambdas and lambdas are not consistent. Please ",
        "make them consistent (i.e., if ctllist[['N_lambdas']] is greater than ",
        "0, ctllist[['N_lambdas']] should equal nrow(ctllist[['lambdas']]))"
      )
    }
    printdf("lambdas", terminate = T)
  } else if ((ctllist[["N_lambdas"]] == 0) & (is.null(ctllist[["lambdas"]]))) {
    # writes terminator line only.
    ctllist[["tmp_var"]] <- c(-9999, rep(0, times = 4))
    wl.vector("tmp_var", comment = "# terminator")
  } else {
    stop(
      "ctllist components N_lambdas and lambdas are not consistent. Please ",
      "make them consistent (i.e., if ctllist[['N_lambdas']] is 0 ,then ",
      "ctllist[['lambdas']] should be NULL; if ctllist[['N_lambdas']] is greater than ",
      "0, ctllist[['N_lambdas']] should equal nrow(ctllist[['lambdas']]))"
    )
  }
  writeComment("#", con = zz)
  # more sd reporting ----
  wl(
    "more_stddev_reporting",
    comment = "# 0/1 read specs for more stddev reporting"
  )

  if (ctllist[["more_stddev_reporting"]] > 0) {
    # break over multiple lines to improve readability.
    ctllist[["tmp_selex_stddev_reporting_specs"]] <- ctllist[[
      "stddev_reporting_specs"
    ]][1:4]
    ctllist[["tmp_growth_stddev_reporting_specs"]] <- ctllist[[
      "stddev_reporting_specs"
    ]][5:6]
    ctllist[["tmp_natage_stddev_reporting_specs"]] <- ctllist[[
      "stddev_reporting_specs"
    ]][7:9]
    if (ctllist[["more_stddev_reporting"]] == 2) {
      ctllist[["tmp_matage_stddev_reporting_specs"]] <-
        ctllist[["stddev_reporting_specs"]][
          10:length(ctllist[["stddev_reporting_specs"]])
        ]
    }
    wl.vector(
      "tmp_selex_stddev_reporting_specs",
      comment = paste0(
        "# selex_fleet, 1=len/2=age/3=both, year, N ",
        "selex bins"
      )
    )
    wl.vector(
      "tmp_growth_stddev_reporting_specs",
      comment = "      # 0 or Growth pattern, N growth ages"
    )
    wl.vector(
      "tmp_natage_stddev_reporting_specs",
      comment = "   # 0 or NatAge_area(-1 for sum), NatAge_yr, N Natages"
    )
    if (!is.null(ctllist[["tmp_matage_stddev_reporting_specs"]])) {
      wl.vector(
        "tmp_matage_stddev_reporting_specs",
        comment = paste0(
          "    # Mortality, Dyn B0 (>3.30.16), SmryBio (>3.30.16) "
        )
      )
    }
    # Selex bin
    if (
      ctllist[["stddev_reporting_specs"]][1] > 0 &
        ctllist[["stddev_reporting_specs"]][4] > 0
    ) {
      wl.vector(
        "stddev_reporting_selex",
        comment = paste0(
          "# vector with selex std bins (-1 in first ",
          "bin to self-generate)"
        )
      )
    }
    # Growth bin
    # not written if empirical weight at age is used.
    if (
      ctllist[["stddev_reporting_specs"]][5] > 0 &
        ctllist[["stddev_reporting_specs"]][6] > 0 &
        ctllist[["EmpiricalWAA"]] == 0
    ) {
      wl.vector(
        "stddev_reporting_growth",
        comment = paste0(
          "# vector with growth std ages picks (-1 in ",
          "first bin to self-generate)"
        )
      )
    }
    # N at age
    if (
      ctllist[["stddev_reporting_specs"]][7] != 0 &
        ctllist[["stddev_reporting_specs"]][9] > 0
    ) {
      wl.vector(
        "stddev_reporting_N_at_A",
        comment = paste0(
          "# vector with NatAge std ages (-1 in first ",
          "bin to self-generate)"
        )
      )
    }
    # M at age
    if (
      ctllist[["more_stddev_reporting"]] == 2 &&
        ctllist[["stddev_reporting_specs"]][11] > 0
    ) {
      wl.vector(
        "stddev_reporting_M_at_A",
        comment = paste0(
          "# vector with MatAge std ages picks (-1 in ",
          "first bin to self-generate)"
        )
      )
    }
  }
  # terminate file ----
  writeComment("#", con = zz)
  writeLines("999", con = zz)

  # cleanup -----
  # options(width=oldwidth,max.print=oldmax.print)
  if (verbose) message("File written to ", outfile, "\n")
}
