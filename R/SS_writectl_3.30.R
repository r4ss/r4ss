#' write control file for SS version 3.30
#'
#' write Stock Synthesis control file from list object in R which was created
#'   using \code{\link{SS_readctl}}.This function is designed to be called 
#'   using \code{\link{SS_writectl}} and should not be called directly.
#'
#' @param ctllist  List object created by \code{\link{SS_readctl}}.
#' @param outfile Filename for where to write new data file.
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param verbose Should there be verbose output while running the file?
#' @author Kathryn Doering, Yukio Takeuchi, Neil Klaer, Watal M. Iwasaki
#' @importFrom gdata write.fwf
#' @export
#' @seealso \code{\link{SS_readctl}}, \code{\link{SS_readctl_3.30}},\code{\link{SS_readstarter}},
#' \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
#' \code{\link{SS_writedat}}
#' 
SS_writectl_3.30 <- function(ctllist, outfile, overwrite, verbose) {
  if(verbose) message("Running SS_writectl_3.30\n")
  
  if(verbose) message("Opening connection to ",outfile,"\n")
  zz <- file(outfile, open="at") #open = "at" means open for appending in text mode.
  on.exit(close(zz)) # Needed in case the function exits early.
  
  # Internally used function definitions -----
  # many of these are identical with functions used by other  
  # SS_write* functions. Perhaps define them in a separate file so they can 
  # be used by any of these functions.But would require passing the file path
  # each time.
  writeComment<-function(text,...) {
    if(length(grep(x = text, pattern = "^#")) != length(text)) {
      text<-paste("#_", text, sep="")
    }
    writeLines(text = text, con = zz, ...)
  }
  # wl = write a line with a single value to an SS control file.
  # @param name The name of the list component in ctllist to find
  # @param comment A comment to put after the value on the line. If no "#" is 
  # included at the beginning, then "#_" is added to the beginning of the comment
  # before writing to file.
  wl <- function(name, comment = NULL, con = stdout) {
    # simple function to clean up many repeated commands
    value = ctllist[names(ctllist) == name]
    if(is.null(comment)) {
      writeLines(paste(value, " #_", name, sep="", collapse = "_"), con = zz)
    } else {
      if(length(grep(comment, pattern = "^#")) != 0) {
        writeLines(paste(value, comment), con = zz)
      } else {
        writeLines(paste(value, " #_", comment, sep = "", collapse = "_"), 
                   con = zz)
      }
    }
  }
  
  # wl = write a line with a vector of values to an SS control file.
  # @param name The name of the list component in ctllist to find
  # @param comment A comment to put after the value on the line. If no "#" is 
  # included at the beginning, then "#_" is added to the beginning of the comment
  # before writing to file.
  wl.vector <- function(name, comment = NULL) {
    # simple function to clean up many repeated commands
    value <- ctllist[names(ctllist) == name][[1]]
    if(is.null(comment)) {
      writeLines(paste(paste(value, collapse = " "), " #_", name, sep = ""), 
                 con = zz)
    } else {
      writeLines(paste(paste(value, collapse = " "), comment), con = zz)
    }
  }
  # write a line if the values are in a list.
  wl.list <- function(name, comment = NULL, header = NULL) {
    if(!is.null(header)) {
      writeLines(paste0("#_", header), con = zz)
    }
    value <-  ctllist[names(ctllist) == name][[1]]
    value1 <- sapply(value, 
                     function(x) {paste(paste(x), collapse = " ")},
                     simplify = TRUE
                     )
    writeLines(value1, con = zz)
  }
  
  ## Internal function to write formatted data.frame
  ## @param cols_to_rm Defaults to NULL. If need to remove any cols, add the
  #    index or indices of the cols here.
  printdf <- function(dataframe, 
                      header = TRUE, 
                      headerLine = NA, 
                      cols_to_rm = NULL,
                      terminate = FALSE ) {
    # function to print data frame with hash mark before first column name
    if(is.character(dataframe)) {
      tmp <- ctllist[names(ctllist) == dataframe]
      if(length(tmp) > 0) {
        dataframe <- tmp[[1]]
      } else {
        dataframe <- NULL
      }
    }
    if(!is.null(dataframe)) {
      # remove columns if desired.
      if(!is.null(cols_to_rm)) {
        dataframe <- dataframe[, -cols_to_rm]
      }
      if (terminate) {
        # add terminator line to end data frame
        newline <- c(-9999, rep(0, ncol(dataframe) - 1))
        dataframe <- rbind(dataframe, newline)
        rownames(dataframe)[nrow(dataframe)] <- "terminator"
      }
      if(header) {
        dataframe$PType <- NULL
        names(dataframe)[1] <- paste("#_", names(dataframe)[1], sep="")
        writeLines(paste(names(dataframe), collapse="\t"), con = zz)
      }
      if(!is.na(headerLine)) xxx <- 2
      if(!is.null(rownames(dataframe))) {
        rownames(dataframe) <- sapply(rownames(dataframe),
          function(z) {
            ifelse(length(grep(x = z, pattern = "^#")) == 1,
            z,
            paste0("#_", z))
          }
        )
        dataframe$comments <- rownames(dataframe)
      }
      write.fwf(file = zz, x = dataframe, append = TRUE, sep = "\t", quote = FALSE,
                       rownames = FALSE, colnames = FALSE, digits = 6)
    }
  }
  
  # Write a header ----
  # Line below included to be consistent with .ss_new files
  writeComment(paste0("#V", ctllist$ReadVersion)) 
  # starter #C means this header will be maintained in control.ss_new file
  #created from a SS model run using this control file.
  writeComment(paste0("#C file created using the SS_writectl function ",
                      "in the R package r4ss"))
  writeComment(paste("#C file write time:", Sys.time()))
  writeComment("#")
  
  # Write the contents ----
  # Make comments as consistent with SS 3.30 as possible.
  # Beginning of ctl file ----
  
  wl("EmpiricalWAA", 
     comment = paste0("# 0 means do not read wtatage.ss; 1 means read and use",
               "wtatage.ss and also read and use growth parameters"))
  wl("N_GP", comment = "N_Growth_Patterns") # N_Growth_Patterns
  wl("N_platoon", comment = "N_platoons_Within_GrowthPattern")
  if(ctllist$N_platoon > 1) { # Conditional inputs needed if more than 1 platoon.
    stop("Multiple platoons are not supported yet by SS_writectl_3.30")
    #TODO: add multiple platoon functionality to SS_writectl_3.30 and
    # SS_readctl_3.30. Then, the stop() above can be removed.
    wl("sd_ratio", comment = "Morph_between/within_stdev_ratio")
    wl("submorphdist", comment = "vector_Morphdist_(-1_in_first_val_gives_normal_approx)")
  }
  # Recruitment distribution ----
  wl("recr_dist_method", comment = "# recr_dist_method for parameters")
  wl("recr_global_area",
     comment = paste0("# Not yet implemented; Future usage:",
                      "Spawner-Recruitment; 1=global; 2=by area"))
  wl("recr_dist_read",
     comment = paste0("# number of recruitment settlement assignments "))
  wl("recr_dist_inx", comment = "# unused option")
  writeComment("# Each settlement assignment:")
  printdf("recr_dist_pattern")
  # Movement ----
  if(ctllist$N_areas > 1) {
    wl("N_moveDef", 
       comment = "#_N_movement_definitions goes here if N_areas > 1")
    wl("firstAgeMove", 
       comment = paste0("#_first age that moves (real age at begin of season, ",
                        "not integer) also cond on do_migration>0"))
    writeComment("move definition for seas, morph, source, dest, age1, age2")
    printdf("moveDef", header = FALSE)
  } else {
    writeComment("#_Cond 0 # N_movement_definitions goes here if N_areas > 1")
    writeComment(paste0("#_Cond 1.0 # first age that moves (real age at begin ",
                        "of season, not integer) also cond on do_migration>0"))
    writeComment(paste0("#_Cond 1 1 1 2 4 10 # example move definition for ",
                        "seas=1, morph=1, source=1 dest=2, age1=4, age2=10"))
    writeComment("#")
  }
  # Block setup ----
  wl("N_Block_Designs", comment = "#_Nblock_Patterns")
  if(ctllist$N_Block_Designs > 0) {
    wl.vector("blocks_per_pattern", comment = "#_blocks_per_pattern")
    wl.list("Block_Design", header = "#_begin and end years of blocks")
  } else {
    writeComment("#_Cond 0 #_blocks_per_pattern")
    writeComment("# begin and end years of blocks")
    writeComment("#")
  }
  # timevary controls ----
  writeComment("# controls for all timevary parameters ")
  wl("time_vary_adjust_method", 
     comment = paste0("#_env/block/dev_adjust_method for all time-vary parms ",
                     "(1=warn relative to base parm bounds; 3=no bound check)"))
  writeComment("# AUTOGEN")
  wl.vector("time_vary_auto_generation", 
            comment = paste0("# autogen: 1st element for biology, 2nd for SR,",
            "3rd for Q, 4th reserved, 5th for selex"))
  writeComment(paste0("# where: 0 = autogen all time-varying parms; 1 = read",
                      "each time-varying parm line; 2 = read then autogen if ",
                      "parm min==-12345"))
  # There are a lot of autogenerated SS lines here that could be added, but 
  # perhaps it is not necessary? The other r4ss write functions typically do not 
  # write out all the autogenerated SS comments.
  # MG setup ----
  writeComment(c("#", "#", "#"))
  writeComment(paste0("# setup for M, growth, maturity, fecundity, recruitment",
                      " distibution, movement"))
  writeComment("#")
  # M setup ----
  wl("natM_type",
     comment= paste0("#_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;",
                     "_3=agespecific;_4=agespec_withseasinterpolate"))
  # Following lines depends on the natM_type value.
  if(ctllist$natM_type == 0) {
    writeComment("#_no additional input for selected M option; read 1P per morph")
  } else if(ctllist$natM_type == 1) {
    wl("N_natM", comment = "#_N_breakpoints")
    wl.vector("M_ageBreakPoints", comment = "# age(real) at M breakpoints")
  } else if(ctllist$natM_type == 2) {
    wl.vector("Lorenzen_refage", 
              comment = "#_reference age for Lorenzen M; read 1P per morph")
  } else if(ctllist$natM_type %in% c(3,4)) {
    writeComment(" #_Age_natmort_by sex x growthpattern")
    printdf("natM")
  } else {
    stop("natM_type : ", ctllist$natM_type, " is not supported")
  }
  #Growth Setup ----
  wl("GrowthModel", 
     comment = paste0("# GrowthModel: 1=vonBert with L1&L2; 2=Richards with ",
     "L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;",
     "5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation"))
  wl("Growth_Age_for_L1", 
     comment = "#_Age(post-settlement)_for_L1;linear growth below this")
  wl("Growth_Age_for_L2", comment = "#_Growth_Age_for_L2 (999 to use as Linf)")
  wl("Exp_Decay",
     comment = paste0("#_exponential decay for growth above maxage (value ",
                      "should approx initial Z; -999 replicates 3.24; -998 to ",
                      "not allow growth above maxage)"))
  wl("Growth_Placeholder", comment= "#_placeholder for future growth feature")
  # Need the following if statements because there are conditional lines that are
  # necessary for only some growth methods
  if (ctllist$GrowthModel == 3) {
    wl("N_ageK", comment = "# number of K multipliers to read")
    wl.vector("Age_K_points", comment = "# ages for K multiplier")
  }
  #TODO: implement reading and writing of these growth options.
  if (ctllist$GrowthModel %in% c(4,5,8)) {
    stop("Cannot write a control file for Growth options 4, 5, or 8 because ",
         "they are not yet implemented in SS_readctl_3.30")
  }
  # Below check added so users can investigate why the ctllist can't be written.
  if (!ctllist$GrowthModel %in% c(1:5,8)) {
    stop("The GrowthModel", ctllist$GrowthModel, "in ctllist ", ctllist, 
         " is not an option in SS 3.30. Valid growth options are 1-5 and 8.")
  }
  writeComment("#")
  wl("SD_add_to_LAA", 
     comment = "#_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)")
  wl("CV_Growth_Pattern",
     comment = paste0("#_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); ",
                      "2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)"))
  
  # maturity and MG options setup ----
  wl("maturity_option", 
     comment = paste0("#_maturity_option:  1=length logistic; 2=age logistic; ",
                      "3=read age-maturity matrix by growth_pattern; ",
                      "4=read age-fecundity; 5=disabled; 6=read length-maturity"
                      ))
  # Below check added to help users with troubleshooting
  if(!ctllist$maturity_option %in% c(1:4,6)) {
    stop("Invalid maturity option used. ctllist$maturity_option is", 
         ctllist$maturity_option, ", but must be 1, 2, 3, 4, or 6.")
  }
  # Below if statements are lines are conditional on the maturity option chosen
  if(ctllist$maturity_option %in% c(3,4)) {
    writeComment("# Age Maturity or Age fecundity:")
    printdf("Age_Maturity")
  }
  if(ctllist$maturity_option == 6) {
    writeComment("# Length Maturity: ")
    printdf("Len_Maturity")
  }
  wl("First_Mature_Age", comment = "#_First_Mature_Age")
  wl("fecundity_option", 
     comment = paste0("fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;",
                      "(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W"))
  wl("hermaphroditism_option", 
     comment = paste0("#_hermaphroditism option:  0=none; 1=female-to-male ",
                      "age-specific fxn; -1=male-to-female age-specific fxn"))
  if (!ctllist$hermaphroditism_option %in% c(0,1,-1)) {
    stop("Invalid hermaphroditism_option specified in ctllist. Its value is ",
         ctllist$hermaphroditism_option, ", but can only be 0, 1, or -1.")
  }
  # Below if statement conditional on the hermaphroditism option chosen
  if (ctllist$hermaphroditism_option %in% c(1,-1)) {
    wl("Herm_season", comment = "# Hermaphro_season ")
    wl("Herm_MalesInSSB", comment = "# Hermaphro_maleSSB")
  }
  wl("parameter_offset_approach",
     comment = paste0("parameter_offset_approach (1=none, 2= M, G, CV_G as ",
                      "offset from female-GP1, 3=like SS2 V1.x)"))
  # MG parms ----
  writeComment(c("#","#_growth_parms"))
  printdf("MG_parms", cols_to_rm = 15) # need to get rid of the last col PType.
  # Time varying MG short parmlines would go next.
  # TODO: Looks like all TV options may not be implemented in the readctl_3.30 
  # function? Need to add this. Implement, then can remove the following stop()
  if(any(ctllist$MG_parms[, c("env_var", "use_dev", "Block")] != 0)) {
    stop("Time varying MG short parameter lines (for environmental links, ",
         "devs, and blocks) cannot be written yet using SS_writectl_3.30")
  }
  # Seasonal effects ----
  writeComment("#")
  writeComment("#_seasonal_effects_on_biology_parms")
  wl.vector("MGparm_seas_effects",
            comment = paste0("#_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,",
                             "Malewtlen1,malewtlen2,L1,K"))
  if(sum(ctllist$MGparm_seas_effects) > 0) {
    printdf("MG_parms_seas")
    writeComment("#")
  } else {
    writeComment("#_ LO HI INIT PRIOR PR_SD PR_type PHASE")
    writeComment(c("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters","#"))
  }
  # SR -----
  wl("SR_function",
     comment = paste0("Spawner-Recruitment; 2=Ricker; 3=std_B-H; 4=SCAA;",
                      "5=Hockey; 6=B-H_flattop; 7=survival_3Parm;",
                      "8=Shepard_3Parm"))
  wl("Use_steep_init_equi",
     comment = "# 0/1 to use steepness in initial equ recruitment calculation")
  wl("Sigma_R_FofCurvature", 
     comment = paste0("# future feature: 0/1 to make realized sigmaR a ",
                      "function of SR curvature"))
  # SR parms ----
  printdf("SRparm")
  #TODO: add time varying SR short lines here, and add code to read them.
  if(any(ctllist$SRparm[,c("env_var", "use_dev", "Block")] != 0)){
    stop("Time varying SR short parameter lines (for environmental links, ",
         "devs, and blocks) cannot be written yet using SS_writectl_3.30")
  }
  # recdevs ----
  wl("do_recdev",
     comment = paste0("#do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); ",
                      "2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; ",
                      "dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty"))
  wl("MainRdevYrFirst",
     comment = "first year of main recr_devs; early devs can preceed this era")
  wl("MainRdevYrLast",
     comment = "last year of main recr_devs; forecast devs start in following year")
  wl("recdev_phase", comment = "recdev phase")
  wl("recdev_adv", comment = "(0/1) to read 13 advanced options")
  
  if(ctllist$recdev_adv == 1) {
    if(verbose) message("Writing 13 advanced SRR options\n")
    wl("recdev_early_start",
       comment = paste0("#_recdev_early_start (0=none; neg value makes relative",
                        " to recdev_start)"))
    wl("recdev_early_phase", comment = "#_recdev_early_phase")
    wl("Fcast_recr_phase",
       comment = paste0("#_forecast_recruitment phase (incl. late recr) (0 ",
                        "value resets to maxphase+1)"))
    wl("lambda4Fcast_recr_like",
       comment = "#_lambda for Fcast_recr_like occurring before endyr+1")
    wl("last_early_yr_nobias_adj", 
       comment = "#_last_yr_nobias_adj_in_MPD; begin of ramp")
    wl("first_yr_fullbias_adj", 
       comment = "#_first_yr_fullbias_adj_in_MPD; begin of plateau")
    wl("last_yr_fullbias_adj", 
       comment = "#_last_yr_fullbias_adj_in_MPD")
    wl("first_recent_yr_nobias_adj", 
       comment = paste0("#_end_yr_for_ramp_in_MPD (can be in forecast to shape",
                        "ramp, but SS sets bias_adj to 0.0 for fcast yrs)"))
    wl("max_bias_adj", 
       comment = paste0("#_max_bias_adj_in_MPD (-1 to override ramp and set ",
                        "biasadj=1.0 for all estimated recdevs)"))
    wl("period_of_cycles_in_recr",
       comment = "#_period of cycles in recruitment (N parms read below)")
    wl("min_rec_dev", comment = "#min rec_dev")
    wl("max_rec_dev", comment = "#max rec_dev")
    wl("N_Read_recdevs", comment = "#_read_recdevs")
    writeComment("#_end of advanced SR options")
    
    #TODO: implement recruitment cycles
    if(ctllist$period_of_cycles_in_recr > 0) {
      stop("Reading full parameters for recr cycles is not yet coded")
    } else {
      writeComment("#_placeholder for full parameter lines for recruitment cycles")
    }
    if(ctllist$N_Read_recdevs > 0) {
      printdf("recdev_input")
    } else {
      writeComment("# read specified recr devs")
      writeComment("#_Yr Input_value")
    }
  }
  
  ##
  # F setup ----
  writeComment("#Fishing Mortality info")
  wl("F_ballpark", comment = "# F ballpark")
  wl("F_ballpark_year", comment = "F ballpark year (neg value to disable)")
  wl("F_Method", 
     comment = "F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)")
  wl("maxF", comment = "max F or harvest rate, depends on F_Method")
  if(ctllist$F_Method == 1) {
    #  Fmethod 1 does not need any additional information
  } else if(ctllist$F_Method == 2) {
    writeComment("overall start F value; overall phase; N detailed inputs to read")
    wl.vector("F_setup", 
              comment = "Intial F value, overall phase, N detailed inputs")
    writeComment("fleet, yr, seas, Fvalue, se, phase")
    if(ctllist$F_setup[length(ctllist$F_setup)] > 0) {
      printdf("F_setup2")
    }
  } else if(ctllist$F_Method == 3) {
    wl("F_iter", 
       comment = "N iterations for tuning F in hybrid method (recommend 3 to 7)")
  }
  
  writeComment(c("#", "#_initial_F_parms"))
  printdf("init_F")
  # Q setup ---- 
  writeComment("#_Q_setup for fleets with cpue or survey data")
  # There are extra commments with info here in control.ss_new, but exclude for now
  printdf("Q_options", terminate = TRUE)
  writeComment("#_Q_parms(if_any);Qunits_are_ln(q)")
  printdf("Q_parms")
  writeComment("#_no timevary Q parameters")
  if(any(ctllist$Q_parms[, c("env_var", "use_dev", "Block")] != 0)) {
    stop("Time varying Q short parameter lines (for environmental links, ",
         "devs, and blocks) cannot be written yet using SS_writectl_3.30")
  }
  # Size selectivity setup ----
  writeComment("#_size_selex_patterns")
  printdf("size_selex_types")
  writeComment("#")
  # Age selectivity setup ----
  writeComment("#_age_selex_types")
  printdf("age_selex_types")
  writeComment("#")
  #selectivity parameters ------
  writeComment("SizeSelex")
  if(!is.null(ctllist$size_selex_parms)) {
    printdf("size_selex_parms")
  } else {
    writeComment("#_No size_selex_parm")
  }
  writeComment("AgeSelex")
  if(!is.null(ctllist$age_selex_parms)) {
    printdf("age_selex_parms")
  } else {
    writeComment("#_No age_selex_parm")
  }
  # TV selectivity parameters
  #TODO: TV selectivity (devs,env link, and blocks) need to be  implemented in 
  # readctl_3.30; then, read parameters here.
  if(any(ctllist$size_selex_parms[, c("env_var", "use_dev", "Block")] != 0)) {
    stop("Time varying Size selex short parameter lines (for environmental links, ",
         "devs, and blocks) cannot be written yet using SS_writectl_3.30")
  }
  if(any(ctllist$age_selex_parms[, c("env_var", "use_dev", "Block")] != 0)) {
    stop("Time varying Age selex short parameter lines (for environmental links, ",
         "devs, and blocks) cannot be written yet using SS_writectl_3.30")
  }
  # 2DAR sel ----
  wl("Use_2D_AR1_selectivity", 
     comment = "#  use 2D_AR1 selectivity(0/1):  experimental feature")
  if (ctllist$Use_2D_AR1_selectivity == 0) {
    writeComment("#_no 2D_AR1 selex offset used")
  } else if (ctllist$Use_2D_AR1_selectivity == 1) {
    stop("SS_writectl_3.30 cannot yet write 2DAR1 selectivity options")
    #TODO: add code here to write 2D_AR1 selectivity options.
  } else {
    stop("ctllist$Use_2D_AR1_selectivity has value ", 
         ctllist$Use_2D_AR1_selectivity, ", but can only have value 0 or 1.")
  }
  # Tag model parameters ----
  writeComment("# Tag loss and Tag reporting parameters go next")
  wl("TG_custom", comment = "TG_custom:  0=no read; 1=read if tags exist")
  if(ctllist$TG_custom == 0) {
    writeComment(c("#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters","#"))
  } else if(ctllist$TG_custom == 1) {
    printdf("TG_Loss_init")
    printdf("TG_Loss_chronic")
    printdf("TG_overdispersion")
    printdf("TG_Report_fleet")
    printdf("TG_Report_fleet_decay")
  } else {
    stop("ctllist$TG_custom has value ", ctllist$TG_custom, " but can only",
         "have value 0 or 1.")
  }
  # Time varying parameters for tagging, would go here, if implemented.
  if(verbose) {
    warning("Please note that time varying parameters for tagging not yet ",
            "implemented as of SS version 3.30.13")
  }
  # Var Adj ----
  # TODO: will need to add to the sS_readctl_3.30 fun so it can read 3.30 var
  # adjustment section correctly if using 
  writeComment("# Input variance adjustments factors: ")
  if(ctllist$DoVar_adjust == 0) {
    ctllist$tmp_var <- c(-9999, 1, 0)
    writeComment("#_Factor Fleet Value")
    wl.vector("tmp_var",comment = "# terminator")
  } else if(ctllist$DoVar_adjust == 1) {
    stop("Variance adjustments for 3.30 are not read correctly by", 
         "SS_writectl_3.30")
  }
  
  # Lambdas ----
  wl("maxlambdaphase", comment = "#_maxlambdaphase")
  wl("sd_offset", 
     comment = paste0("#_sd_offset; must be 1 if any growthCV, sigmaR, or ",
                     "survey extraSD is an estimated parameter"))
  writeComment(paste0("# read ", ctllist$N_lambdas, " changes to default",
                      "Lambdas (default value is 1.0)"))
  # There are some more .ss_new comments here, but not included for now.
  printdf("lambdas", terminate = T)

  # more sd reporting ----
  wl("more_stddev_reporting", 
     comment = " 0/1 read specs for more stddev reporting")
  
  if(ctllist$more_stddev_reporting == 1){
    wl.vector("stddev_reporting_specs",
              comment = paste0("# selex type, len/age, year, N selex bins, ",
                               "Growth pattern, N growth ages, ",
                               "NatAge_area(-1 for all), NatAge_yr, N Natages"))
    # Selex bin
    if(ctllist$stddev_reporting_specs[4] > 0) {
      wl.vector("stddev_reporting_selex",
                comment = paste0("# selex bins to be reported (-1 in first bin", 
                " to self-generate)"))
    }
    # Growth bin
    if(ctllist$stddev_reporting_specs[6] > 0) {
      wl.vector("stddev_reporting_growth",
                comment = paste0("# growth bins to be reported (-1 in first ",
                                 "bin to self-generate)"))
    }
    # N at age
    if(ctllist$stddev_reporting_specs[9] > 0) {
      wl.vector("stddev_reporting_N_at_A",
                comment = "# N@A to be reported (-1 in first bin to self-generate)")
    }
  }else if(ctllist$more_stddev_reporting != 0) {
    stop("ctllist$more_stdev_reporting has value ", 
         ctllist$more_stddev_reporting, " but can only have value 0 or 1.")
  }
  
  # terminate file ----
  writeComment("#")
  writeLines("999", con = zz)
  
  # cleanup -----
  #options(width=oldwidth,max.print=oldmax.print)
  if(verbose) message("File written to ", outfile, "\n")
  }
  
  
