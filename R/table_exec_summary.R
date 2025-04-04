#' Create executive summary tables from an SS3 Report.sso file
#'
#' Take the output from `SS_output()` and create executive summary .rda files
#' as required by the current Terms of Reference for U.S. West Coast
#' groundfish assessments. Additionally, .rda files of historical catches,
#' time-series, and numbers-at-age are created. A CSV file with captions is
#' also created. This function is modified from `SSexecutivesummary()`
#' associated with the adoption of the {asar} template.
#'
#' @template replist
#' @param dir Directory where the .rda files will be written. The default value
#'   is NULL where a table folder will be created where the Report.sso file is
#'   located associated with `replist`.
#' @param ci_value To calculate confidence intervals, the desired interval must
#'   be specified. The default is 0.95.
#' @param fleetnames String of fleet names. Default is NULL which will use the
#'   the model fleet names.
#' @param so_units A single character object specifying the unit of measurement
#'   that spawning output is reported in. The default is "millions of eggs".
#'   This text will be used in the table captions. If fecundity is equal to
#'   weight-at-length, then the units are hard-wired to `"mt"` regardless of
#'   what is used within this argument.
#' @param divide_by_2 A logical allowing the output to be based on single sex
#'   values based on the new sex specification (-1) in SS3 for single sex
#'   models. Default value is `FALSE`. `TRUE` will lead to dividing values by
#'   2.
#' @param endyr Optional input to choose a different ending year for tables,
#'   which could be useful for catch-only updates. The default is `NULL`, which
#'   leads to using the ending year defined in Report.sso.
#' @template verbose
#'
#' @return
#' Individual .rda files containing a list of table and caption
#' @family table functions
#' @author Chantel R. Wetzel, Kelli F. Johnson, Ian G. Taylor
#' @export
#'
table_exec_summary <- function(
    replist,
    dir = NULL,
    ci_value = 0.95,
    fleetnames = NULL,
    so_units = "biomass (mt)",
    divide_by_2 = FALSE,
    endyr = NULL,
    verbose = TRUE) {
  check_replist(replist)

  rda_dir <- file.path(
    ifelse(
      is.null(dir),
      yes = replist[["inputs"]][["dir"]],
      no = dir
    ),
    "tables"
  )
  dir.create(rda_dir, showWarnings = FALSE)
  check_dir(dir = rda_dir, verbose = verbose)
  # write the table to an rda file
  if (verbose) {
    cli::cli_alert_info("writing tables to {rda_dir}")
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
      cli_alert_info(
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
    sb.label <- replist[["SpawnOutputLabel"]]
    sb.text.name <- tolower(sb.label)
    sb_short <- "SO"
  } else {
    sb.label <- "Spawning Biomass (mt)"
    sb.text.name <- "spawning biomass"
    sb_short <- "SB"
  }

  # ======================================================================
  # ES Table a  Catches from the fisheries
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating table of catches by fleet for the last 10 years.")
  }

  catch <- fleet.names <- NULL
  total.catch <- total.dead <- 0
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
    caption <- "Recent landings by fleet, total landings summed across fleets, and the total dead catch including discards."
  } else {
    es.a <- data.frame(years_minus_final, catch, total.catch)
    colnames(es.a) <- c("Year", paste(fleet.names, "(mt)"), "Total Catch (mt)")
    caption <- "Recent catches (mt) by fleet and total catch (mt) summed across fleets."
  }
  catches_es <- list()
  catches_es$table <- es.a
  catches_es$cap <- caption

  tables <- list()
  tables[["catches_es"]] <- catches_es  
  save(catches_es, file = file.path(rda_dir, "catches_es.rda"))

  # ======================================================================
  # ES Table b Spawning Biomass and Fraction Unfished
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info(
      "Creating table of last 10 years of spawning biomass/output and fraction unfished."
    )
  }

  ssb <- get_values(replist = replist, label = sb.name, years, ci_value)
  if (nsexes == 1) {
    ssb[["dq"]] <- ssb[["dq"]] / sexfactor
    ssb[["low"]] <- ssb[["low"]] / sexfactor
    ssb[["high"]] <- ssb[["high"]] / sexfactor
  }
  fraction_unfished <- get_values(
    replist = replist,
    label = "Bratio",
    years,
    ci_value
  )
  es.b <- dplyr::full_join(ssb, fraction_unfished, by = "yrs")
  colnames(es.b) <- c(
    "Year", sb.label, "Lower Interval (mt)", "Upper Interval (mt)",
    "Fraction Unfished", "Lower Interval", "Upper Interval"
  )
  caption <-
    paste0(
      "Estimated recent trend in ", sb.text.name, " and the fraction unfished and the ", round(100 * ci_value, 0),
      " percent intervals."
    )
  ssb_es <- list()
  ssb_es$table <- es.b
  ssb_es$cap <- caption
  tables[["ssb_es"]] <- ssb_es  
  save(ssb_es, file = file.path(rda_dir, "ssb_es.rda"))

  # ======================================================================
  # ES Table c Recruitment
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating table of recent recruitment and deviations.")
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

  recruits <- get_values(replist = replist, label = "Recr", years, ci_value)

  if (length(main.yrs) > 0 | length(late.yrs) > 0 | length(fore.yrs) > 0) {
    # placeholder for devs
    devs <- NULL
    if (length(main.yrs) > 0) {
      recdevs <- get_values(replist = replist, label = "Main_RecrDev", yrs = main.yrs, ci_value)
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
      late.recdevs <- get_values(replist = replist, label = "Late_RecrDev", yrs = late.yrs, ci_value)
      devs <- rbind(devs, late.recdevs[, c("dq", "low", "high")])
    }

    if (length(fore.yrs) > 0) {
      fore.recdevs <- get_values(replist = replist, label = "ForeRecr", yrs = fore.yrs, ci_value)
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
    "Year", "Recruitment (1,000s)", "Lower Interval (1,000s)", "Upper Interval (1,000s)",
    "Recruitment Deviations", "Lower Interval", "Upper Interval"
  )
  recr_es <- list()
  recr_es$table <- es.c
  recr_es$cap <-
    paste0(
      "Estimated recent trend in recruitment (1,000s) and recruitment deviations and the ", round(100 * ci_value, 0),
      " percent intervals."
    )
  tables[["recr_es"]] <- recr_es  
  save(recr_es, file = file.path(rda_dir, "recr_es.rda"))

  # ======================================================================
  # ES Table d 1-SPR (%)
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating table of recent exploitation.")
  }

  spr_type <- replist[["SPRratioLabel"]]
  f_type <- ifelse(replist[["F_std_basis"]] == "_abs_F;_with_F=Exploit(bio)", "Exploitation Rate",
    "Fill in F method"
  )

  if (stringr::str_detect(replist[["SPRratioLabel"]], "%")) {
    spr_label <- paste0(
      substring(replist[["SPRratioLabel"]], 1, 14), " ",
      substring(replist[["SPRratioLabel"]], 16, 17),
      "%)"
    )
  } else {
    spr_label <- replist[["SPRratioLabel"]]
  }

  adj.spr <- get_values(replist = replist, label = "SPRratio", years_minus_final, ci_value)
  f.value <- get_values(replist = replist, label = "F", years_minus_final, ci_value)
  es.d <- data.frame(
    years_minus_final,
    adj.spr[["dq"]], adj.spr[["low"]], adj.spr[["high"]],
    f.value[["dq"]], f.value[["low"]], f.value[["high"]]
  )
  colnames(es.d) <- c(
    "Year", spr_label, "Lower Interval (SPR)", "Upper Interval (SPR)",
    f_type, "Lower Interval (Rate)", "Upper Interval (Rate)"
  )
  spr_es <- list()
  spr_es$table <- es.d
  spr_es$cap <- paste0(
    "Estimated recent trend in the ", spr_label, " where SPR is the spawning potential ratio, the exploitation rate, and the ", round(100 * ci_value, 0),
    " percent intervals."
  )
  tables[["spr_es"]] <- spr_es  
  save(spr_es, file = file.path(rda_dir, "spr_es.rda"))

  # ======================================================================
  # ES Table e Reference Point Table
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating table of reference points.")
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
  ssb.virgin <- get_values(replist = replist, label = sb.unfished, years, ci_value, single = TRUE)
  smry.virgin <- get_values(replist = replist, label = smry.unfished, years, ci_value, single = TRUE)
  rec.virgin <- get_values(replist = replist, label = recr.unfished, years, ci_value, single = TRUE)
  b.target <- get_values(replist = replist, label = "SSB_Btgt", years, ci_value, single = TRUE)
  spr.btarg <- get_values(replist = replist, label = "SPR_Btgt", years, ci_value, single = TRUE)
  f.btarg <- get_values(replist = replist, label = f.btgt.name, years, ci_value, single = TRUE)
  yield.btarg <- get_values(replist = replist, label = totyield.btgt, years, ci_value, single = TRUE)
  b.spr <- get_values(replist = replist, label = "SSB_SPR", years, ci_value, single = TRUE)
  f.spr <- get_values(replist = replist, label = f.spr.name, years, ci_value, single = TRUE)
  yield.spr <- get_values(replist = replist, label = totyield.spr, years, ci_value, single = TRUE)
  b.msy <- get_values(replist = replist, label = "SSB_MSY", years, ci_value, single = TRUE)
  spr.msy <- get_values(replist = replist, label = "SPR_MSY", years, ci_value, single = TRUE)
  f.msy <- get_values(replist = replist, label = f.msy.name, years, ci_value, single = TRUE)
  msy <- get_values(replist = replist, label = totyield.msy, years, ci_value, single = TRUE)

  # Convert spawning ci_values for single-sex models
  if (nsexes == 1) {
    ssb.virgin <- ssb.virgin / sexfactor
    b.target <- b.target / sexfactor
    b.spr <- b.spr / sexfactor
    b.msy <- b.msy / sexfactor
  }

  suppressMessages(
    es.e <- dplyr::bind_rows(
      dplyr::bind_cols(paste("Unfished", sb.label), ssb.virgin[["dq"]], ssb.virgin[["low"]], ssb.virgin[["high"]]),
      dplyr::bind_cols(paste0("Unfished Age ", smry.age, "+ Biomass (mt)"), smry.virgin[["dq"]], smry.virgin[["low"]], smry.virgin[["high"]]),
      dplyr::bind_cols("Unfished Recruitment (R0)", rec.virgin[["dq"]], rec.virgin[["low"]], rec.virgin[["high"]]),
      dplyr::bind_cols(paste(years[length(years)], sb.label), ssb[["dq"]][dim(ssb)[1]], ssb[["low"]][dim(ssb)[1]], ssb[["high"]][dim(ssb)[1]]),
      dplyr::bind_cols(paste(years[length(years)], "Fraction Unfished"), final.fraction_unfished[["dq"]], final.fraction_unfished[["low"]], final.fraction_unfished[["high"]]),
      dplyr::bind_cols(paste0("Reference Points Based ", sb_short, btarg, "%"), NA, NA, NA),
      dplyr::bind_cols(paste0("Proxy ", sb.label, " ", sb_short, btarg, "%"), b.target[["dq"]], b.target[["low"]], b.target[["high"]]),
      dplyr::bind_cols(paste0("SPR Resulting in ", sb_short, btarg, "%"), spr.btarg[["dq"]], spr.btarg[["low"]], spr.btarg[["high"]]),
      dplyr::bind_cols(paste0("Exploitation Rate Resulting in ", sb_short, btarg, "%"), f.btarg[["dq"]], f.btarg[["low"]], f.btarg[["high"]]),
      dplyr::bind_cols(paste0("Yield with SPR Based On ", sb_short, btarg, "% (mt)"), yield.btarg[["dq"]], yield.btarg[["low"]], yield.btarg[["high"]]),
      dplyr::bind_cols("Reference Points Based on SPR Proxy for MSY", NA, NA, NA),
      dplyr::bind_cols(paste0("Proxy ", sb.label, " (SPR", spr, ")"), b.spr[["dq"]], b.spr[["low"]], b.spr[["high"]]),
      dplyr::bind_cols(paste0("SPR", spr), spr / 100, NA, NA),
      dplyr::bind_cols(paste0("Exploitation Rate Corresponding to SPR", spr), f.spr[["dq"]], f.spr[["low"]], f.spr[["high"]]),
      dplyr::bind_cols(paste0("Yield with SPR", spr, " at ", sb_short, " SPR (mt)"), yield.spr[["dq"]], yield.spr[["low"]], yield.spr[["high"]]),
      dplyr::bind_cols("Reference Points Based on Estimated MSY Values", NA, NA, NA),
      dplyr::bind_cols(paste0(sb.label, " at MSY (", sb_short, " MSY)"), b.msy[["dq"]], b.msy[["low"]], b.msy[["high"]]),
      dplyr::bind_cols("SPR MSY", spr.msy[["dq"]], spr.msy[["low"]], spr.msy[["high"]]),
      dplyr::bind_cols("Exploitation Rate Corresponding to SPR MSY", f.msy[["dq"]], f.msy[["low"]], f.msy[["high"]]),
      dplyr::bind_cols("MSY (mt)", msy[["dq"]], msy[["low"]], msy[["high"]])
    )
  )
  colnames(es.e) <- c("Reference Point", "Estimate", "Lower Interval", "Upper Interval")
  reference_points <- list()
  reference_points$table <- es.e
  reference_points$cap <- paste0(
    "Summary of reference points and management quantities, including estimates of the ", round(100 * ci_value, 0),
    " percent intervals."
  )
  tables[["reference_points"]] <- reference_points  
  save(reference_points, file = file.path(rda_dir, "reference_points.rda"))

  # ======================================================================
  # ES Table f is the historical harvest
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating table of recent management performance.")
  }

  ofl <- rep(NA, length(years) - 1)
  abc <- rep(NA, length(years) - 1)
  acl <- rep(NA, length(years) - 1)

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
    caption <- "Recent trend in the overfishing limits (OFLs), the acceptable biological catches (ABCs), the annual catch limits (ACLs), the total landings, and total mortality all in metric tons (mt)."
  } else {
    es.f <- data.frame(years_minus_final, ofl, abc, acl, total.catch)
    colnames(es.f) <- c("Year", "OFL (mt)", "ABC (mt)", "ACL (mt)", "Catch (mt)")
    caption <- "Recent trend in the overfishing limits (OFL), the acceptable biological catches (ABCs), the annual catch limits (ACLs), and the total catch all in metric tons (mt)."
  }
  recent_management <- list()
  recent_management$cap <- caption
  recent_management$table <- es.f
  tables[["recent_management"]] <- recent_management  
  save(recent_management, file = file.path(rda_dir, "recent_management.rda"))

  # ======================================================================
  # ES Table g  Predicted forecast values
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info(
      "Creating table of projected OFLs, ABCs, spawning biomass/output, and fraction unfished."
    )
  }

  ofl.fore <- get_values(replist = replist, label = "OFLCatch", yrs = fore, ci_value)[["dq"]]
  abc.fore <- get_values(replist = replist, label = "ForeCatch", yrs = fore, ci_value)[["dq"]]
  acl.fore <- abc.fore
  buffer <- round(abc.fore / ofl.fore, 3)
  assumed_catch <- c(abc.fore[1:2], rep(NA, length(fore) - 2))
  if (length(ofl.fore) >= 2) {
    ofl.fore[1:2] <- c(NA, NA)
    abc.fore[1:2] <- c(NA, NA)
    acl.fore[1:2] <- c(NA, NA)
    buffer[1:2] <- c(NA, NA)
  }
  ssb.fore <- get_values(replist = replist, label = sb.name, yrs = fore, ci_value)[["dq"]]
  fraction_unfished.fore <- get_values(replist = replist, label = "Bratio", yrs = fore, ci_value)[["dq"]]
  if (any(fraction_unfished.fore < replist$btarg)) {
    replace <- which(fraction_unfished.fore < replist$btarg)
    abc.fore[replace] <- buffer[replace] * ofl.fore[replace]
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
  adopted_ofl <- adopted_acl <- rep(NA, length(fore))
  es.g <- data.frame(
    fore, adopted_ofl, adopted_acl, assumed_catch, ofl.fore, buffer,
    abc.fore, acl.fore, ssb.fore, fraction_unfished.fore
  )
  colnames(es.g) <- c(
    "Year", "Adopted OFL (mt)", "Adopted ACL (mt)", "Assumed Catch (mt)",
    "OFL (mt)", "Buffer", "ABC (mt)", "ACL (mt)", sb.label, "Fraction Unfished"
  )
  projections <- list()
  projections$table <- es.g
  projections$cap <- paste0("Potential OFLs (mt), ABCs (mt), ACLs (mt), the buffer between the OFL and ABC, estimated ", sb.text.name, ", and fraction unfished with
                            adopted OFLs and ACLs and assumed catch for the first two years of the projection period.")
  tables[["projections"]] <- projections  
  save(projections, file = file.path(rda_dir, "projections.rda"))

  # ======================================================================
  # Total catch when discards are estimated
  # ======================================================================
  catch <- fleet.names <- NULL
  dead <- total.catch <- total.dead <- 0
  ind <- startyr:endyr

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
    caption <- paste0("Landings (mt) by fleet for all years, total landings (mt), and total dead catch (mt) summed by year.")
  } else {
    mortality <- data.frame(ind, catch, total.catch)
    colnames(mortality) <- c("Year", paste(fleet.names, "(mt)"), "Total Catch (mt)")
    caption <- paste0("Catches (mt) by fleet for all years and total catches (mt) summed by year.")
  }
  mortality.df <- mortality
  mortality <- list()
  mortality$cap <- caption
  mortality$table <- mortality.df
  tables[["mortality"]] <- mortality  
  save(mortality, file = file.path(rda_dir, "mortality_all_years.rda"))

  # ======================================================================
  # Time-series Tables
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating time series table.")
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
    cli::cli_alert_info(paste(
      "Catch includes estimated discards for total dead.",
      "Exploitation = Total dead (including discards) divided by the",
      "summary biomass."
    ), wrap = TRUE)
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
  time_series <- list()
  time_series$cap <- "Time series of population estimates from the base model."
  time_series$table <- ts.table
  tables[["time_series"]] <- time_series  
  save(time_series, file = file.path(rda_dir, "time_series.rda"))

  # ======================================================================
  # Numbers at age
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating numbers-at-age table.")
  }
  check <- dim(replist[["natage"]])[2]
  if (is.null(check)) {
    if (verbose) {
      cli::cli_alert_info(
        "Detailed age-structure is not in the report file, double check settings in the starter file."
      )
    }
  } else {
    age0 <- which(names(replist[["natage"]]) == "0")
    get.ages <- age0:check
    if (nsexes == 1) {
      natage <- 0
      for (a in 1:nareas) {
        for (b in 1:nmorphs) {
          ind <- replist[["natage"]][, "Yr"] >= startyr &
            replist[["natage"]][, "Area"] == a &
            replist[["natage"]][, "Bio_Pattern"] == b &
            replist[["natage"]][, "Sex"] == 1 &
            replist[["natage"]][, "Beg/Mid"] == "B"
          temp <- replist[["natage"]][ind, get.ages]
          natage <- natage + temp
        }
      }
      colnames(natage) <- paste0("Age", 0:(length(get.ages) - 1))
      natage <- data.frame(Year = startyr:max(fore), natage)
      numbers_at_age <- list()
      numbers_at_age$cap <- "Numbers at age for the base model."
      numbers_at_age$table <- natage
      tables[["numbers_at_age"]] <- numbers_at_age
      save(numbers_at_age, file = file.path(rda_dir, "numbers_at_age.rda"))
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
      numbers_at_age_male <- list()
      numbers_at_age_male$cap <- "Numbers at age for males from the base model."
      numbers_at_age_male$table <- natage.m
      tables[["numbers_at_age_male"]] <- numbers_at_age_male
      save(numbers_at_age_male, file = file.path(rda_dir, "numbers_at_age_male.rda"))

      colnames(natage.f) <- paste0("Age", 0:(length(get.ages) - 1))
      natage.f <- data.frame(Year = startyr:max(fore), natage.f)
      numbers_at_age_female <- list()
      numbers_at_age_female$cap <- "Numbers at age for females from the base model."
      numbers_at_age_female$table <- natage.f
      tables[["numbers_at_age_female"]] <- numbers_at_age_female
      save(numbers_at_age_female, file = file.path(rda_dir, "numbers_at_age_female.rda"))
    }
  } # end check for detailed output

  # ======================================================================
  # Biomass at age
  # ======================================================================
  if (verbose) {
    cli::cli_alert_info("Creating biomass-at-age table.")
  }

  check <- dim(replist[["batage"]])[2]
  if (is.null(check)) {
    if (verbose) {
      cli::cli_alert_info(
        "Detailed age-structure is not in the report file, double check settings in the starter file."
      )
    }
  } else {
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
      biomass_at_age <- list()
      biomass_at_age$cap <- "Biomass at age from the base model."
      biomass_at_age$table <- batage
      tables[["biomass_at_age"]] <- biomass_at_age
      save(biomass_at_age, file = file.path(rda_dir, "biomass_at_age.rda"))
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
      biomass_at_age_male <- list()
      biomass_at_age_male$cap <- "Biomass at age for male from the base model."
      biomass_at_age_male$table <- batage.m
      tables[["biomass_at_age_male"]] <- biomass_at_age_male
      save(biomass_at_age_male, file = file.path(rda_dir, "biomass_at_age_male.rda"))

      colnames(batage.f) <- paste0("Age", 0:(length(get.ages) - 1))
      batage.f <- data.frame(Year = startyr:max(fore), batage.f)
      biomass_at_age_female <- list()
      biomass_at_age_female$cap <- "Biomass at age for female from the base model."
      biomass_at_age_female$table <- batage.m
      tables[["biomass_at_age_female"]] <- biomass_at_age_female
      save(biomass_at_age_female, file = file.path(rda_dir, "biomass_at_age_female.rda"))
    }
  } # end check for detailed output

  return(invisible(tables))
}
