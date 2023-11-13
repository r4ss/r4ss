#' Create executive summary tables from an SS3 Report.sso file
#'
#' Take the output from `SS_output()` and create executive summary .csv files
#' as required by the current Terms of Reference for U.S. West Coast
#' groundfish assessments. Additionally, .csv files of historical catches,
#' time-series, and numbers-at-age are created.
#'
#' @template replist
#' @param plotfolder Directory where a new `tables` directory will be created,
#'   which will be used to store the output from this function. The default is
#'   the dir location where the Report.sso file is located.
#' @param ci_value To calculate confidence intervals, the desired interval must
#'   be specified. The default is 0.95.
#' @param es_only A logical that specifies if only the executive summary tables
#'   should be produced. The default is `FALSE`, which leads to all executive
#'   summary and auxiliary tables being produced (see Return).
#' @template fleetnames
#' @param add_text A single character object, where the default is `"model
#'   area"`. The text will be added to some of the table captions to indicate
#'   what the results apply to. Besides the default, one could use `"base
#'   model"`, `"sub-area model South of Point Conception."`, etc. Just know
#'   that the text will be appended to `"for the"`, and thus, the default text
#'   leads to `"for the model area."`. Another thing to note is that a full
#'   stop is not needed but can be used because a full stop is appended to the
#'   end of the caption if it does not already exist.
#' @param so_units A single character object specifying the unit of measurement
#'   that spawning output is reported in. The default is "millions of eggs".
#'   This text will be used in the table captions. If fecundity is equal to
#'   weight-at-length, then the units are hard-wired to `"mt"` regardless of
#'   what is used within this argument.
#' @param tables Deprecated as of version 1.49.1 because this function only
#'   takes 15 seconds to run and overwriting old tables should not be a problem
#'   if users are modifying the .csv files in a programmatic way. The function
#'   behavior is the same as the previous default behavior where all tables
#'   will be created.
#' @param divide_by_2 A logical allowing the output to be based on single sex
#'   values based on the new sex specification (-1) in SS3 for single sex
#'   models. Default value is `FALSE`. `TRUE` will lead to dividing values by
#'   2.
#' @param endyr Optional input to choose a different ending year for tables,
#'   which could be useful for catch-only updates. The default is `NULL`, which
#'   leads to using the ending year defined in Report.sso.
#' @param adopted_ofl,adopted_abc,adopted_acl Vectors of adopted overfishing
#'   limits (OFL), acceptable biological catch (ABC), and annual catch limits
#'   (ACL) values to be printed in the management performance table. These
#'   vectors *MUST BE* be vectors of length 10. The default of `NULL` leads to
#'   the table being filled in with notes that the values need to be changed.
#' @param forecast_ofl,forecast_abc Optional input vectors for management
#'   adopted OFL and ABC values for table g. These values will overwrite the
#'   OFL and ABC values in the projection table, rather than the model
#'   estimated OFL values. As an example, `c(1500, 1300)` would be viable
#'   input.
#' @param format Deprecated as of version 1.49.1 because most users are now
#'   using LaTeX instead of microsoft word so formatting can be done inside
#'   `sa4ss::es_table_tex()` rather than here. From now on, only .csv files
#'   will be available. The default was `TRUE` but is now essentially
#'   `FALSE`.
#' @param match_digits Deprecated as of version 1.49.1 because this function
#'   just returns an unformatted csv file now.
#' @template verbose
#'
#' @return
#' Individual .csv files for each executive summary table and additional tables
#' (catch, timeseries, numbers-at-age).
#' @author Chantel Wetzel
#' @export
#'
SSexecutivesummary <- function(replist,
                               plotfolder = "default",
                               ci_value = 0.95,
                               es_only = FALSE,
                               fleetnames = NULL,
                               add_text = "model area",
                               so_units = "millions of eggs",
                               tables = lifecycle::deprecated(),
                               divide_by_2 = FALSE,
                               endyr = NULL,
                               adopted_ofl = NULL,
                               adopted_abc = NULL,
                               adopted_acl = NULL,
                               forecast_ofl = NULL,
                               forecast_abc = NULL,
                               format = lifecycle::deprecated(),
                               match_digits = lifecycle::deprecated(),
                               verbose = TRUE) {
  if (!missing(tables)) {
    lifecycle::deprecate_warn(
      when = "1.49.1",
      what = "SSexecutivesummary(tables)"
    )
  }
  if (!missing(format)) {
    lifecycle::deprecate_warn(
      when = "1.49.1",
      what = "SSexecutivesummary(format)"
    )
  }
  if (!missing(match_digits)) {
    lifecycle::deprecate_warn(
      when = "1.49.1",
      what = "SSexecutivesummary(match_digits)"
    )
  }
  csv.dir <- file.path(
    ifelse(
      plotfolder == "default",
      yes = replist[["inputs"]][["dir"]],
      no = plotfolder
    ),
    "tables"
  )

  dir.create(csv.dir, showWarnings = FALSE)
  if (verbose) {
    message("CSV files will be written in:\n", csv.dir)
  }

  # ===========================================================================
  # Function Sections
  # ===========================================================================
  # Function to pull values from the read in report file and calculate the
  # confidence intervals
  Get.Values <- function(replist, label, yrs, ci_value, single = FALSE) {
    dat <- replist[["derived_quants"]]
    if (label == "Main_RecrDev" || label == "Late_RecrDev" || label == "ForeRecr") {
      dat <- replist[["parameters"]]
    }

    if (!single) {
      value <- dat[grep(label, dat[["Label"]]), ]
      value <- value[value[["Label"]] >= paste0(label, "_", yrs[1]) &
        value[["Label"]] <= paste0(label, "_", max(yrs)), ]
      dq <- value[["Value"]]
      ind <- names(value) %in% c("StdDev", "Parm_StDev")
      sd <- value[, ind]
    }

    if (single) {
      value <- dat[grep(label, dat[["Label"]])[1], ]
      dq <- value[["Value"]]
      sd <- value[["StdDev"]]
    }

    if (label == "Recr" || label == "Recr_virgin") {
      # Orig code version - this is the same as SSsummarize below
      low <- exp(log(dq) - qnorm(1 - (1 - ci_value) / 2) * sqrt(log(1 + (sd / dq) * (sd / dq))))
      high <- exp(log(dq) + qnorm(1 - (1 - ci_value) / 2) * sqrt(log(1 + (sd / dq) * (sd / dq))))
      # maia's suggestions
      # issue #537 in github
      # low  <- dq / exp(qnorm(1 - (1 - ci_value) / 2) * dev_sd ) #where dev_sd is the recdev upper interval
      # high <- dq * exp(qnorm(1 - (1 - ci_value) / 2) * dev_sd )
      # SSsummarize
      # sdlog <- sqrt(log(1 + (sd / dq)^2))
      # low  <- qlnorm(p = ((1 - ci_value)/2), meanlog = log(dq), sdlog = sdlog)
      # high <- qlnorm(p = (1 - (1 - ci_value)/2), meanlog = log(dq), sdlog = sdlog)
    }
    if (label != "Recr" && label != "Recr_virgin") {
      low <- dq - qnorm(1 - (1 - ci_value) / 2) * sd
      high <- dq + qnorm(1 - (1 - ci_value) / 2) * sd
    }

    if (!single) {
      # check for length in case filtering results in 0 rows
      # (e.g. no Main_Recrdev within the range of yrs)
      if (length(dq) > 0) {
        return(data.frame(yrs, dq, low, high))
      } else {
        return(NULL)
      }
    }
    if (single) {
      return(data.frame(dq, low, high))
    }
  }


  # ============================================================================
  # Determine the model version and dimensions of the model
  # ============================================================================
  # Need to check how r4ss determines the colname based on SS verion
  sb.name <- "SSB"

  nfleets <- replist[["nfleets"]]
  startyr <- replist[["startyr"]]
  foreyr <- replist[["nforecastyears"]]
  if (is.null(endyr)) {
    endyr <- replist[["endyr"]]
  } else {
    foreyr <- foreyr - (endyr - replist[["endyr"]])
  }
  years <- (endyr - 9):(endyr + 1)
  fore <- (endyr + 1):(endyr + foreyr)
  years_minus_final <- years[1:(length(years) - 1)]
  all <- startyr:max(fore)
  nareas <- replist[["nareas"]]

  # ======================================================================
  # Determine the fleet name fisheries with catch
  # ======================================================================
  fleetnames <- if (is.null(fleetnames) || fleetnames[1] == "default") {
    replist[["FleetNames"]]
  } else {
    fleetnames
  }

  # ======================================================================
  # Find summary age
  # ======================================================================
  smry.age <- replist[["summary_age"]]

  # ======================================================================
  # Two-sex or single-sex model
  # ======================================================================
  if (replist[["nsexes"]] == 1 & !(divide_by_2)) {
    if (verbose) {
      message(
        "Single sex model - ",
        "spawning biomass NOT being divided by a factor of 2."
      )
    }
  }
  nsexes <- replist[["nsexes"]]
  sexfactor <- 1
  if (divide_by_2) {
    sexfactor <- 2
  }

  # ======================================================================
  # Determine the number of growth patterns
  # ======================================================================
  nmorphs <- replist[["ngpatterns"]]

  # ======================================================================
  # Spawning Biomass or Spawning Output?
  # ======================================================================
  if (replist[["SpawnOutputUnits"]] == "numbers") {
    sb.label <- paste0("Spawning Output (", so_units, ")")
    sb.text.name <- "spawning output"
    sb_short <- "SO"
  } else {
    sb.label <- "Spawning Biomass (mt)"
    sb.text.name <- "spawning biomass"
    sb_short <- "SB"
  }

  caption <- tex.label <- filename <- csv_name <- NULL

  # ======================================================================
  # ES Table a  Catches from the fisheries
  # ======================================================================
  if (verbose) {
    message("Creating Table a: Recent catches by fleet")
  }

  catch <- fleet.names <- NULL
  total.catch <- total.dead <- 0

  csv_name <- "a_Catches_ES.csv"
  for (i in 1:nfleets) {
    name <- paste0("retain(B):_", i)
    input.catch <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% years_minus_final, name]
    catch <- cbind(catch, input.catch)

    name <- paste0("dead(B):_", i)
    dead <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% years_minus_final, name]
    if (!is.null(dead)) {
      total.dead <- total.dead + dead
      fleet.names <- c(fleet.names, fleetnames[i])
    }
  }
  total.catch <- apply(catch, 1, sum)


  if (sum(total.catch) != sum(total.dead)) {
    es.a <- data.frame(years_minus_final, catch, total.catch, total.dead)
    colnames(es.a) <- c("Year", paste(fleet.names, "(mt)"), "Total Landings (mt)", "Total Dead (mt)")

    write.csv(es.a, file.path(csv.dir, csv_name), row.names = FALSE)
    caption <- c(
      caption,
      paste0("Recent landings by fleet, total landings summed across fleets, and the total dead catch including discards for the ", add_text, ".")
    )
  } else {
    es.a <- data.frame(years_minus_final, catch, total.catch)
    colnames(es.a) <- c("Year", paste(fleet.names, "(mt)"), "Total Catch (mt)")
    write.csv(es.a, file.path(csv.dir, csv_name), row.names = FALSE)
    caption <- c(
      caption,
      paste0("Recent catches (mt) by fleet and total catch (mt) summed across fleets for the  ", add_text, ".")
    )
  }

  tex.label <- c(tex.label, "removalsES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table b Spawning Biomass and Fraction Unfished
  # ======================================================================
  if (verbose) {
    message(
      "Creating Table b:",
      " Recent spawning biomass/output and fraction unfished"
    )
  }

  ssb <- Get.Values(replist = replist, label = sb.name, years, ci_value)
  if (nsexes == 1) {
    ssb[["dq"]] <- ssb[["dq"]] / sexfactor
    ssb[["low"]] <- ssb[["low"]] / sexfactor
    ssb[["high"]] <- ssb[["high"]] / sexfactor
  }
  fraction_unfished <- Get.Values(
    replist = replist,
    label = "Bratio",
    years,
    ci_value
  )
  es.b <- dplyr::full_join(ssb, fraction_unfished, by = "yrs")
  colnames(es.b) <- c(
    "Year", sb.label, "Lower Interval", "Upper Interval",
    "Fraction Unfished", "Lower Interval", "Upper Interval"
  )

  csv_name <- "b_SSB_ES.csv"
  write.csv(es.b, file = file.path(csv.dir, csv_name), row.names = FALSE)

  caption <- c(
    caption,
    paste0(
      "Estimated recent trend in ", sb.text.name, " and the fraction unfished and the ", round(100 * ci_value, 0),
      " percent intervals for the ", add_text, "."
    )
  )
  tex.label <- c(tex.label, "ssbES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table c Recruitment
  # ======================================================================
  if (verbose) {
    message("Creating Table c: Recent recruitment and deviations")
  }
  # figure out which years for Main, Late, and Forecast recruitmets overlap
  # the years we want
  recdevMain <- replist[["parameters"]][substring(replist[["parameters"]][["Label"]], 1, 12) == "Main_RecrDev", 1:3]
  temp <- toupper(substr(recdevMain[["Label"]], 14, 17))
  main.yrs <- as.numeric(temp[temp %in% years])

  recdevLate <- replist[["parameters"]][substring(replist[["parameters"]][["Label"]], 1, 12) == "Late_RecrDev", 1:3]
  temp <- toupper(substr(recdevLate[["Label"]], 14, 17))
  late.yrs <- as.numeric(temp[temp %in% years])

  recdevFore <- replist[["parameters"]][substring(replist[["parameters"]][["Label"]], 1, 8) == "ForeRecr", 1:3]
  temp <- toupper(substr(recdevFore[["Label"]], 10, 13))
  fore.yrs <- as.numeric(temp[temp %in% years])

  recruits <- Get.Values(replist = replist, label = "Recr", years, ci_value)

  if (length(main.yrs) > 0 | length(late.yrs) > 0 | length(fore.yrs) > 0) {
    # placeholder for devs
    devs <- NULL
    if (length(main.yrs) > 0) {
      recdevs <- Get.Values(replist = replist, label = "Main_RecrDev", yrs = main.yrs, ci_value)
      devs <- recdevs[, c("dq", "low", "high")]
    } else {
      n <- length(years) - length(late.yrs) - length(fore.yrs)
      devs <- data.frame(
        dq = rep(0, n),
        low = rep(0, n),
        high = rep(0, n)
      )
    }

    if (length(late.yrs) > 0) {
      late.recdevs <- Get.Values(replist = replist, label = "Late_RecrDev", yrs = late.yrs, ci_value)
      devs <- rbind(devs, late.recdevs[, c("dq", "low", "high")])
    }

    if (length(fore.yrs) > 0) {
      fore.recdevs <- Get.Values(replist = replist, label = "ForeRecr", yrs = fore.yrs, ci_value)
      if (length(fore.yrs) > 0) {
        devs <- rbind(devs, fore.recdevs[, c("dq", "low", "high")])
      }
    }

    # Zero out the sd for years where devs were not estimated
    devs[is.na(devs)] <- 0

    devs.out <- devs
  } else {
    devs.out <- data.frame(rep(0, length(years)), rep(0, length(years)), rep(0, length(years)))
  }
  es.c <- data.frame(
    years, recruits[["dq"]], recruits[["low"]], recruits[["high"]],
    devs.out[, 1], devs.out[, 2], devs.out[, 3]
  )
  colnames(es.c) <- c(
    "Year", "Recruitment (1,000s)", "Lower Interval", "Upper Interval",
    "Recruitment Deviations", "Lower Interval", "Upper Interval"
  )
  csv_name <- "c_Recr_ES.csv"
  write.csv(es.c, file.path(csv.dir, csv_name), row.names = FALSE)

  caption <- c(
    caption,
    paste0(
      "Estimated recent trend in recruitment (1,000s) and recruitment deviations and the ", round(100 * ci_value, 0),
      " percent intervals for the ", add_text, "."
    )
  )
  tex.label <- c(tex.label, "recrES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table d 1-SPR (%)
  # ======================================================================
  if (verbose) {
    message("Creating Table d: Recent exploitation ")
  }

  spr_type <- replist[["SPRratioLabel"]]
  f_type <- ifelse(replist[["F_report_basis"]] == "_abs_F;_with_F=Exploit(bio)", "Exploitation Rate",
    "Fill in F method"
  )

  if (stringr::str_detect(replist[["SPRratioLabel"]], "%")) {
    spr_label <- paste0(
      substring(replist[["SPRratioLabel"]], 1, 14), " ",
      substring(replist[["SPRratioLabel"]], 16, 17),
      "\\%)"
    )
  } else {
    spr_label <- replist[["SPRratioLabel"]]
  }

  adj.spr <- Get.Values(replist = replist, label = "SPRratio", years_minus_final, ci_value)
  f.value <- Get.Values(replist = replist, label = "F", years_minus_final, ci_value)
  es.d <- data.frame(
    years_minus_final,
    adj.spr[["dq"]], adj.spr[["low"]], adj.spr[["high"]],
    f.value[["dq"]], f.value[["low"]], f.value[["high"]]
  )
  colnames(es.d) <- c(
    "Year", spr_label, "Lower Interval", "Upper Interval",
    f_type, "Lower Interval", "Upper Interval"
  )
  csv_name <- "d_SPR_ES.csv"
  write.csv(es.d, file.path(csv.dir, csv_name), row.names = FALSE)

  caption <- c(
    caption,
    paste0(
      "Estimated recent trend in the ", spr_label, " where SPR is the spawning potential ratio, the exploitation rate, and the ", round(100 * ci_value, 0),
      " percent intervals for the ", add_text, "."
    )
  )
  tex.label <- c(tex.label, "exploitES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table e Reference Point Table
  # ======================================================================
  if (verbose) {
    message("Creating Table e: Reference points ")
  }

  spr <- 100 * replist[["sprtarg"]]
  btarg <- 100 * replist[["btarg"]]

  sb.unfished <- "SSB_unfished"
  smry.unfished <- "SmryBio_unfished"
  recr.unfished <- "Recr_unfished"
  totyield.btgt <- "Dead_Catch_Btgt"
  totyield.spr <- "Dead_Catch_SPR"
  totyield.msy <- "Dead_Catch_MSY"
  f.msy.name <- "Fstd_MSY"
  f.btgt.name <- "Fstd_Btgt"
  f.spr.name <- "Fstd_SPR"

  # if (toupper(substr(replist[["SS_version"]], 10, 11)) < 13){
  if (toupper(substr(replist[["SS_version"]], 6, 7)) < 13) {
    sb.unfished <- "SSB_Unfished"
    smry.unfished <- "SmryBio_Unfished"
    recr.unfished <- "Recr_Unfished"
    totyield.btgt <- "TotYield_Btgt"
    totyield.spr <- "TotYield_SPRtgt"
    totyield.msy <- "TotYield_MSY"
  }

  if (toupper(substr(replist[["SS_version"]], 6, 7)) > 15) {
    f.msy.name <- "annF_MSY"
    f.btgt.name <- "annF_Btgt"
    f.spr.name <- "annF_SPR"
  }

  final.fraction_unfished <- fraction_unfished[dim(fraction_unfished)[1], 2:4]
  ssb.virgin <- Get.Values(replist = replist, label = sb.unfished, years, ci_value, single = TRUE)
  smry.virgin <- Get.Values(replist = replist, label = smry.unfished, years, ci_value, single = TRUE)
  rec.virgin <- Get.Values(replist = replist, label = recr.unfished, years, ci_value, single = TRUE)
  b.target <- Get.Values(replist = replist, label = "SSB_Btgt", years, ci_value, single = TRUE)
  spr.btarg <- Get.Values(replist = replist, label = "SPR_Btgt", years, ci_value, single = TRUE)
  f.btarg <- Get.Values(replist = replist, label = f.btgt.name, years, ci_value, single = TRUE)
  yield.btarg <- Get.Values(replist = replist, label = totyield.btgt, years, ci_value, single = TRUE)
  b.spr <- Get.Values(replist = replist, label = "SSB_SPR", years, ci_value, single = TRUE)
  f.spr <- Get.Values(replist = replist, label = f.spr.name, years, ci_value, single = TRUE)
  yield.spr <- Get.Values(replist = replist, label = totyield.spr, years, ci_value, single = TRUE)
  b.msy <- Get.Values(replist = replist, label = "SSB_MSY", years, ci_value, single = TRUE)
  spr.msy <- Get.Values(replist = replist, label = "SPR_MSY", years, ci_value, single = TRUE)
  f.msy <- Get.Values(replist = replist, label = f.msy.name, years, ci_value, single = TRUE)
  msy <- Get.Values(replist = replist, label = totyield.msy, years, ci_value, single = TRUE)

  # Convert spawning ci_valueities for single-sex models
  if (nsexes == 1) {
    ssb.virgin <- ssb.virgin / sexfactor
    b.target <- b.target / sexfactor
    b.spr <- b.spr / sexfactor
    b.msy <- b.msy / sexfactor
  }

  es.e <- matrix(c(
    ssb.virgin[["dq"]], ssb.virgin[["low"]], ssb.virgin[["high"]],
    smry.virgin[["dq"]], smry.virgin[["low"]], smry.virgin[["high"]],
    rec.virgin[["dq"]], rec.virgin[["low"]], rec.virgin[["high"]],
    ssb[["dq"]][dim(ssb)[1]], ssb[["low"]][dim(ssb)[1]], ssb[["high"]][dim(ssb)[1]],
    final.fraction_unfished[["dq"]], final.fraction_unfished[["low"]], final.fraction_unfished[["high"]],
    "", "", "",
    b.target[["dq"]], b.target[["low"]], b.target[["high"]],
    spr.btarg[["dq"]], spr.btarg[["low"]], spr.btarg[["high"]],
    f.btarg[["dq"]], f.btarg[["low"]], f.btarg[["high"]],
    yield.btarg[["dq"]], yield.btarg[["low"]], yield.btarg[["high"]],
    "", "", "",
    b.spr[["dq"]], b.spr[["low"]], b.spr[["high"]],
    spr / 100, "", "",
    f.spr[["dq"]], f.spr[["low"]], f.spr[["high"]],
    yield.spr[["dq"]], yield.spr[["low"]], yield.spr[["high"]],
    "", "", "",
    b.msy[["dq"]], b.msy[["low"]], b.msy[["high"]],
    spr.msy[["dq"]], spr.msy[["low"]], spr.msy[["high"]],
    f.msy[["dq"]], f.msy[["low"]], f.msy[["high"]],
    msy[["dq"]], msy[["low"]], msy[["high"]]
  ), ncol = 3, byrow = T)

  es.e <- cbind(
    c(
      paste("Unfished", sb.label),
      paste0("Unfished Age ", smry.age, "+ Biomass (mt)"),
      "Unfished Recruitment (R0)",
      paste(years[length(years)], sb.label),
      paste(years[length(years)], "Fraction Unfished"),
      paste0("Reference Points Based ", sb_short, btarg, "\\%"),
      paste0("Proxy ", sb.label, " ", sb_short, btarg, "\\%"),
      paste0("SPR Resulting in ", sb_short, btarg, "\\%"),
      paste0("Exploitation Rate Resulting in ", sb_short, btarg, "\\%"),
      paste0("Yield with SPR Based On ", sb_short, btarg, "\\% (mt)"),
      "Reference Points Based on SPR Proxy for MSY",
      paste0("Proxy ", sb.label, " (SPR", spr, ")"),
      paste0("SPR", spr),
      paste0("Exploitation Rate Corresponding to SPR", spr),
      paste0("Yield with SPR", spr, " at ", sb_short, " SPR (mt)"),
      "Reference Points Based on Estimated MSY Values",
      paste0(sb.label, " at MSY (", sb_short, " MSY)"),
      "SPR MSY",
      "Exploitation Rate Corresponding to SPR MSY",
      "MSY (mt)"
    ),
    es.e
  )
  es.e <- noquote(es.e)
  colnames(es.e) <- c("Reference Points", "Estimate", "Lower Interval", "Upper Interval")
  csv_name <- "e_ReferencePoints_ES.csv"
  write.csv(es.e, file.path(csv.dir, csv_name), row.names = FALSE)

  caption <- c(
    caption,
    paste0(
      "Summary of reference points and management quantities, including estimates of the ", round(100 * ci_value, 0),
      " percent intervals for the ", add_text, "."
    )
  )

  tex.label <- c(tex.label, "referenceES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table f is the historical harvest
  # ======================================================================
  if (verbose) {
    message("Creating Table f: Recent management performance")
  }

  if (is.null(adopted_ofl)) {
    ofl <- rep("fill in", length(years) - 1)
  } else {
    if (length(adopted_ofl) != 12) {
      stop("The adopted_ofl vector needs to have 10 values.")
    }
    ofl <- adopted_ofl
  }

  if (is.null(adopted_abc)) {
    abc <- rep("fill in", length(years) - 1)
  } else {
    if (length(adopted_abc) != 12) {
      stop("The adopted_abc vector needs to have 10 values.")
    }
    abc <- adopted_abc
  }

  if (is.null(adopted_acl)) {
    acl <- rep("fill in", length(years) - 1)
  } else {
    if (length(adopted_acl) != 12) {
      stop("The adopted_acl vector needs to have 10 values.")
    }
    acl <- adopted_acl
  }

  csv_name <- "f_Manage_ES.csv"
  catch <- dead <- total.dead <- 0
  for (i in 1:nfleets) {
    name <- paste0("retain(B):_", i)
    input.catch <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% years_minus_final, name]
    catch <- cbind(catch, input.catch)

    name <- paste0("dead(B):_", i)
    dead <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% years_minus_final, name]
    if (!is.null(dead)) {
      total.dead <- total.dead + dead
    }
  }
  total.catch <- apply(catch, 1, sum)

  if (sum(total.catch) != sum(total.dead)) {
    es.f <- data.frame(years_minus_final, ofl, abc, acl, total.catch, total.dead)
    colnames(es.f) <- c("Year", "OFL (mt)", "ABC (mt)", "ACL (mt)", "Landings (mt)", "Total Mortality (mt)")
    caption <- c(
      caption,
      "Recent trend in the overfishing limits (OFLs), the acceptable biological catches (ABCs), the annual catch limits (ACLs), the total landings, and total mortality all in metric tons (mt)."
    )
  } else {
    es.f <- data.frame(years_minus_final, ofl, abc, acl, total.catch)
    colnames(es.f) <- c("Year", "OFL (mt)", "ABC (mt)", "ACL (mt)", "Catch (mt)")
    caption <- c(
      caption,
      "Recent trend in the overfishing limits (OFL), the acceptable biological catches (ABCs), the annual catch limits (ACLs), and the total catch all in metric tons (mt)."
    )
  }
  write.csv(es.f, file.path(csv.dir, csv_name), row.names = FALSE)

  tex.label <- c(tex.label, "manageES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table g  Predicted forecast values
  # ======================================================================
  if (verbose) {
    message(
      "Creating Table g:",
      " Forecast OFLs, ABCs, Spawning Biomass/Output, and",
      " Fraction Unfished"
    )
  }

  ofl.fore <- Get.Values(replist = replist, label = "OFLCatch", yrs = fore, ci_value)$dq
  abc.fore <- Get.Values(replist = replist, label = "ForeCatch", yrs = fore, ci_value)$dq
  ssb.fore <- Get.Values(replist = replist, label = sb.name, yrs = fore, ci_value)$dq
  fraction_unfished.fore <- Get.Values(replist = replist, label = "Bratio", yrs = fore, ci_value)$dq

  if (!is.null(forecast_ofl)) {
    n <- length(forecast_ofl)
    ofl.fore[1:n] <- forecast_ofl
  }

  if (!is.null(forecast_abc)) {
    n <- length(forecast_abc)
    abc.fore[1:n] <- forecast_abc
  }

  if (nsexes == 1) {
    ssb.fore <- ssb.fore / sexfactor
  }

  smry.fore <- 0
  for (a in 1:nareas) {
    ind <- replist[["timeseries"]][["Area"]] == a & replist[["timeseries"]][["Yr"]] %in% fore
    temp <- replist[["timeseries"]][["Bio_smry"]][ind]
    smry.fore <- smry.fore + temp
  }

  es.g <- data.frame(fore, ofl.fore, abc.fore, smry.fore, ssb.fore, fraction_unfished.fore)

  colnames(es.g) <- c("Year", "Predicted OFL (mt)", "ABC Catch (mt)", paste0("Age ", smry.age, "+ Biomass (mt)"), sb.label, "Fraction Unfished")
  csv_name <- "g_Projections_ES.csv"
  write.csv(es.g, file.path(csv.dir, csv_name), row.names = FALSE)

  caption <- c(
    caption,
    paste0("Projections of potential OFLs (mt), ABCs (mt), estimated ", sb.text.name, ", and fraction unfished.")
  )

  tex.label <- c(tex.label, "projectionES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # ES Table h decision table
  # ======================================================================
  # To be done later
  if (verbose) {
    message("Skipping the decision table (not yet implemented)")
  }


  # ======================================================================
  # ES Table i the summary table
  # ======================================================================
  if (verbose) {
    message("Creating Table i: Summary")
  }

  ind <- length(years) - 1

  catch <- dead <- total.dead <- 0
  for (i in 1:nfleets) {
    name <- paste0("retain(B):_", i)
    input.catch <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% years_minus_final, name]
    catch <- cbind(catch, input.catch)

    name <- paste0("dead(B):_", i)
    dead <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% years_minus_final, name]
    if (!is.null(dead)) {
      total.dead <- total.dead + dead
    }
  }
  total.catch <- apply(catch, 1, sum)
  total.bind <- c(c("Total Catch", total.catch, "NA"), c("Total Dead", total.dead, "NA"))
  if (sum(total.catch) == sum(total.dead)) {
    total.bind <- c("Total Catch", total.catch, "NA")
  }

  spr_type <- replist[["SPRratioLabel"]] # strsplit(base[grep(spr.name,base)]," ")[[1]][3]
  f_type <- ifelse(replist[["F_report_basis"]] == "_abs_F;_with_F=Exploit(bio)", "Exploitation Rate",
    "Fill in F method"
  )
  adj.spr <- Get.Values(replist = replist, label = "SPRratio", years_minus_final, ci_value)
  f.value <- Get.Values(replist = replist, label = "F", years_minus_final, ci_value)

  smry <- smry.fore <- 0
  for (a in 1:nareas) {
    find <- replist[["timeseries"]][["Area"]] == a & replist[["timeseries"]][["Yr"]] %in% years_minus_final
    temp <- replist[["timeseries"]][["Bio_smry"]][find]
    smry <- smry + temp

    find <- replist[["timeseries"]][["Area"]] == a & replist[["timeseries"]][["Yr"]] %in% fore[1]
    temp <- replist[["timeseries"]][["Bio_smry"]][ind]
    smry.fore <- smry.fore + temp
  }
  smry <- c(smry, smry.fore[1])

  ssb <- Get.Values(replist = replist, label = sb.name, years, ci_value)
  if (nsexes == 1) {
    ssb[["dq"]] <- ssb[["dq"]] / sexfactor
    ssb[["low"]] <- ssb[["low"]] / sexfactor
    ssb[["high"]] <- ssb[["high"]] / sexfactor
  }
  fraction_unfished <- Get.Values(replist = replist, label = "Bratio", years, ci_value)
  recruits <- Get.Values(replist = replist, label = "Recr", years, ci_value)

  if (is.null(adopted_ofl)) {
    ofl <- rep("fill in", length(years))
  } else {
    if (length(adopted_ofl) != 12) {
      stop("The adopted_ofl vector needs to have 12 values.")
    }
    if (is.null(forecast_ofl)) {
      ofl <- c(adopted_ofl, "-")
    } else {
      ofl <- c(adopted_ofl, forecast_ofl[1])
    }
  }

  if (is.null(adopted_acl)) {
    acl <- rep("fill in", length(years))
  } else {
    if (length(adopted_acl) != 12) {
      stop("The adopted_acl vector needs to have 12 values.")
    }
    if (is.null(forecast_abc)) {
      acl <- c(adopted_acl, "-")
    } else {
      acl <- c(adopted_acl, forecast_abc[1])
    }
  }

  es.i <- matrix(
    c(
      c("OFL", ofl),
      c("ACL", acl),
      total.bind,
      c(spr_type, c(adj.spr[["dq"]][1:(length(years) - 1)], NA)),
      c(f_type, c(f.value[["dq"]][1:(length(years) - 1)], NA)),
      c(paste0("Age ", smry.age, "+ Biomass (mt)"), smry),
      c(sb.label, ssb[["dq"]]),
      c("Lower Interval", ssb[["low"]]),
      c("Upper Interval", ssb[["high"]]),
      c("Recruits", recruits[["dq"]]),
      c("Lower Interval", recruits[["low"]]),
      c("Upper Interval", recruits[["high"]]),
      c("Fraction Unfished", fraction_unfished[["dq"]]),
      c("Lower Interval", fraction_unfished[["low"]]),
      c("Upper Interval", fraction_unfished[["high"]])
    ),
    ncol = (length(years) + 1), byrow = T
  )
  es.i <- noquote(es.i)
  colnames(es.i) <- c("Quantity", years)
  csv_name <- "i_Summary_ES.csv"
  write.csv(es.i, file.path(csv.dir, csv_name), row.names = FALSE)

  caption <- c(
    caption,
    paste0("Summary of recent estimates and management quantities for the ", add_text, ".")
  )

  tex.label <- c(tex.label, "summaryES")
  filename <- c(filename, csv_name)

  # ======================================================================
  # End executive summary tables
  # ======================================================================
  if (es_only == TRUE) {
    if (verbose) {
      message(
        "Skipping catch, timeseries, and numbers-at-age tables because ",
        "es_only = TRUE"
      )
    }
  }

  if (es_only == FALSE) {
    if (verbose) {
      message("Creating catch table")
    }

    # ======================================================================
    # Total Catch when discards are estimated
    # ======================================================================
    catch <- fleet.names <- NULL
    dead <- total.catch <- total.dead <- 0
    ind <- startyr:endyr
    csv_name <- "Catches_All_Years.csv"

    for (i in 1:nfleets) {
      name <- paste0("retain(B):_", i)
      input.catch <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% ind, name]
      catch <- cbind(catch, input.catch)

      name <- paste0("dead(B):_", i)
      dead <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% ind, name]
      if (!is.null(dead)) {
        total.dead <- total.dead + dead
        fleet.names <- c(fleet.names, fleetnames[i])
      }
    }
    total.catch <- apply(catch, 1, sum)

    if (sum(total.catch) != sum(total.dead)) {
      mortality <- data.frame(ind, catch, total.catch, total.dead)
      colnames(mortality) <- c("Year", paste(fleet.names, "(mt)"), "Total Landings (mt)", "Total Dead (mt)")
      write.csv(mortality, file.path(csv.dir, csv_name), row.names = FALSE)
      caption <- c(caption, paste0("Landings (mt) by fleet for all years, total landings (mt), and total dead catch (mt) summed by year for the ", add_text, "."))
    } else {
      mortality <- data.frame(ind, catch, total.catch)
      colnames(mortality) <- c("Year", paste(fleet.names, "(mt)"), "Total Catch (mt)")
      write.csv(mortality, file.path(csv.dir, csv_name), row.names = FALSE)
      caption <- c(
        caption,
        paste0("Catches (mt) by fleet for all years and total catches (mt) summed by year for the ", add_text, ".")
      )
    }

    tex.label <- c(tex.label, "allcatches")
    filename <- c(filename, csv_name)
  } # end check for es_only == FALSE

  # ======================================================================
  # Time-series Tables
  # ======================================================================
  if (es_only == FALSE) {
    if (verbose) {
      message("Creating time-series table")
    }

    ssb.virgin <- sum(replist[["timeseries"]][replist[["timeseries"]][["Era"]] == "VIRG", "SpawnBio"])

    smry.all <- tot.bio.all <- recruits.all <- ssb.all <- total.dead.all <- 0
    for (a in 1:nareas) {
      find <- replist[["timeseries"]][["Area"]] == a & replist[["timeseries"]][["Yr"]] %in% all

      smry <- replist[["timeseries"]][["Bio_smry"]][find]
      tot.bio <- replist[["timeseries"]][["Bio_all"]][find]
      recruits <- replist[["timeseries"]][["Recruit_0"]][find]
      ssb <- replist[["timeseries"]][["SpawnBio"]][find]

      smry.all <- smry.all + smry
      tot.bio.all <- tot.bio.all + tot.bio
      recruits.all <- recruits.all + recruits
      ssb.all <- ssb.all + ssb
    }

    if (nsexes == 1) {
      ssb.all <- ssb.all / sexfactor
      ssb.virgin <- ssb.virgin / sexfactor
    }
    fraction_unfished.all <- ssb.all / ssb.virgin

    total.dead.all <- 0
    for (i in 1:nfleets) {
      name <- paste0("dead(B):_", i)
      dead <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% all, name]
      if (!is.null(dead)) {
        total.dead.all <- total.dead.all + dead
      }
    }

    expl.all <- total.dead.all / smry.all
    spr_type <- replist[["SPRratioLabel"]]

    if (verbose) {
      message("Catch includes estimated discards for total dead.")
      message(
        "Exploitation = Total dead (including discards) divided by the ",
        "summary biomass."
      )
    }

    # SPRratio may not be reported for all years
    # missing early years may just be for the unfished equilibrium or other reasons
    # missing later years most likely due to starter file setting "max yr for sdreport outputs"

    # First, check to see if there is exploitation in the first model year
    # "ind" should be the number of years (up to 10) at the start of model with 0 total catch
    ind <- 0
    for (z in 1:10) {
      ind <- ind + ifelse(total.dead[z] == 0, 1, break())
    }

    # Get labels that start with SPRratio_
    adj.spr.labels <- grep("SPRratio_", replist[["derived_quants"]][["Label"]], value = TRUE)
    # get year values associated with those labels
    adj.spr.yrs <- as.numeric(substring(adj.spr.labels, first = nchar("SPRratio_") + 1))
    # vector of placeholder NA values for all years
    adj.spr.all <- rep(NA, length(all))
    # replace NA with 0 values for early years which may have had no catch
    if (ind != 0) {
      adj.spr.all[1:ind] <- 0
    }
    # replace placeholders for years with reported SPRratio values
    adj.spr.all[all %in% adj.spr.yrs] <- replist[["derived_quants"]][adj.spr.labels, "Value"]

    ts.table <- data.frame(
      all,
      tot.bio.all,
      ssb.all,
      smry.all,
      fraction_unfished.all,
      recruits.all,
      total.dead.all,
      adj.spr.all,
      expl.all
    )

    colnames(ts.table) <- c(
      "Year", "Total Biomass (mt)", sb.label,
      paste0("Total Biomass ", smry.age, "+ (mt)"), "Fraction Unfished",
      "Age-0 Recruits (1,000s)", "Total Mortality (mt)", spr_type,
      "Exploitation Rate"
    )
    csv_name <- "TimeSeries.csv"
    write.csv(ts.table, file = file.path(csv.dir, csv_name), row.names = FALSE)

    caption <- c(
      caption,
      paste0("Time series of population estimates from the base model for the ", add_text, ".")
    )

    tex.label <- c(tex.label, "timeseries")
    filename <- c(filename, csv_name)
  } # end check for es_only == FALSE


  # ======================================================================
  # Numbers at age
  # ======================================================================
  if (es_only == FALSE) {
    if (verbose) {
      message("Creating numbers-at-age table")
    }

    check <- dim(replist[["natage"]])[2]
    if (is.null(check)) {
      if (verbose) {
        message(
          "Detailed age-structure is not in the report file, double check",
          " settings in the starter file."
        )
      }
    }

    if (!is.null(check)) {
      age0 <- which(names(replist[["natage"]]) == "0")
      get.ages <- age0:check

      if (nsexes == 1) {
        natage <- 0
        for (a in 1:nareas) {
          for (b in 1:nmorphs) {
            ind <- replist[["natage"]][, "Yr"] >= startyr & replist[["natage"]][, "Area"] == a & replist[["natage"]][, "Bio_Pattern"] == b & replist[["natage"]][, "Sex"] == 1 & replist[["natage"]][, "Beg/Mid"] == "B"
            temp <- replist[["natage"]][ind, get.ages]
            natage <- natage + temp
          }
        }

        colnames(natage) <- paste0("Age", 0:(length(get.ages) - 1))
        natage <- data.frame(Year = startyr:max(fore), natage)
        write.csv(natage, file.path(csv.dir, "natage.csv"), row.names = FALSE)
      }

      if (nsexes == 2) {
        natage.f <- natage.m <- 0
        for (a in 1:nareas) {
          for (b in 1:nmorphs) {
            ind <- replist[["natage"]][, "Yr"] >= startyr & replist[["natage"]][, "Area"] == a & replist[["natage"]][, "Bio_Pattern"] == b & replist[["natage"]][, "Sex"] == 1 & replist[["natage"]][, "Beg/Mid"] == "B"
            temp <- replist[["natage"]][ind, get.ages]
            natage.f <- natage.f + temp

            ind <- replist[["natage"]][, "Yr"] >= startyr & replist[["natage"]][, "Area"] == a & replist[["natage"]][, "Bio_Pattern"] == b &
              replist[["natage"]][, "Sex"] == 2 & replist[["natage"]][, "Beg/Mid"] == "B"
            temp <- replist[["natage"]][ind, get.ages]
            natage.m <- natage.m + temp
          }
        }

        colnames(natage.m) <- paste0("Age", 0:(length(get.ages) - 1))
        natage.m <- data.frame(Year = startyr:max(fore), natage.m)
        write.csv(natage.m, file.path(csv.dir, "natage_m.csv"), row.names = FALSE)

        colnames(natage.f) <- paste0("Age", 0:(length(get.ages) - 1))
        natage.f <- data.frame(Year = startyr:max(fore), natage.f)
        write.csv(natage.f, file.path(csv.dir, "natage_f.csv"), row.names = FALSE)
      }
    } # end check for detailed output
  } # end check for es_only = TRUE

  # ======================================================================
  # Biomass at age
  # ======================================================================
  if (es_only == FALSE) {
    if (verbose) {
      message("Creating biomass-at-age table")
    }

    check <- dim(replist[["batage"]])[2]
    if (is.null(check)) {
      if (verbose) {
        message(
          "Detailed age-structure is not in the report file, double check",
          " settings in the starter file."
        )
      }
    }

    if (!is.null(check)) {
      age0 <- which(names(replist[["batage"]]) == "0")
      get.ages <- age0:check

      if (nsexes == 1) {
        batage <- 0
        for (a in 1:nareas) {
          for (b in 1:nmorphs) {
            ind <- replist[["batage"]][, "Yr"] >= startyr & replist[["batage"]][, "Area"] == a & replist[["batage"]][, "Bio_Pattern"] == b & replist[["batage"]][, "Sex"] == 1 & replist[["batage"]][, "Beg/Mid"] == "B"
            temp <- replist[["batage"]][ind, get.ages]
            batage <- batage + temp
          }
        }

        colnames(batage) <- paste0("Age", 0:(length(get.ages) - 1))
        batage <- data.frame(Year = startyr:max(fore), batage)
        write.csv(batage, file.path(csv.dir, "batage.csv"), row.names = FALSE)
      }

      if (nsexes == 2) {
        batage.f <- batage.m <- 0
        for (a in 1:nareas) {
          for (b in 1:nmorphs) {
            ind <- replist[["batage"]][, "Yr"] >= startyr & replist[["batage"]][, "Area"] == a & replist[["batage"]][, "Bio_Pattern"] == b & replist[["batage"]][, "Sex"] == 1 & replist[["batage"]][, "Beg/Mid"] == "B"
            temp <- replist[["batage"]][ind, get.ages]
            batage.f <- batage.f + temp

            ind <- replist[["batage"]][, "Yr"] >= startyr & replist[["batage"]][, "Area"] == a & replist[["batage"]][, "Bio_Pattern"] == b &
              replist[["batage"]][, "Sex"] == 2 & replist[["batage"]][, "Beg/Mid"] == "B"
            temp <- replist[["batage"]][ind, get.ages]
            batage.m <- batage.m + temp
          }
        }

        colnames(batage.m) <- paste0("Age", 0:(length(get.ages) - 1))
        batage.m <- data.frame(Year = startyr:max(fore), batage.m)
        write.csv(batage.m, file.path(csv.dir, "batage_m.csv"), row.names = FALSE)

        colnames(batage.f) <- paste0("Age", 0:(length(get.ages) - 1))
        batage.f <- data.frame(Year = startyr:max(fore), batage.f)
        write.csv(batage.f, file.path(csv.dir, "batage_f.csv"), row.names = FALSE)
      }
    } # end check for detailed output
  } # end check for es_only == FALSE

  # ======================================================================
  # Likelihoods
  # ======================================================================
  if (es_only == FALSE) {
    csv_name <- "likelihoods.csv"

    like <- cbind(
      rownames(replist[["likelihoods_used"]]),
      replist[["likelihoods_used"]][["values"]]
    )
    colnames(like) <- c("Label", "Total")
    like[, 1] <- gsub("\\_", " ", like[, 1])
    write.csv(like, file = file.path(csv.dir, csv_name), row.names = FALSE)

    caption <- c(
      caption,
      "Negative log-likelihood components by data type."
    )
    tex.label <- c(tex.label, "likes")
    filename <- c(filename, csv_name)
  } # end check for es_only == FALSE

  # ======================================================================
  # Write out table with all the captions for each executive summary table
  # ======================================================================

  out_csv <- cbind(caption, NA, tex.label, filename)
  colnames(out_csv) <- c("caption", "altcaption", "label", "filename")
  write.csv(
    out_csv,
    file = file.path(csv.dir, "table_labels.csv"),
    row.names = FALSE
  )
}
