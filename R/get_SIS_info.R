#' Gather information for the NOAA Species Information System (SIS)
#'
#' Processes model results contained in the list created by
#' [SS_output()] in a format that is more convenient for submission
#' to SIS. Currently the results are returned invisibly as a list of two tables
#' and written to a CSV file from which results could be copied into SIS.
#' In the future some more direct link could be explored to avoid the manual
#' copy step.

#' @param model Output from SS_output
#' @param dir Directory in which to write the CSV files.
#' @param writecsv Write results to a CSV file (where the name will have the
#' format "\[stock\]_2019_SIS_info.csv" where `stock`
#' is an additional input
#' @param stock String to prepend id info to filename for CSV file
#' @param assessment_type "Operational" or "Stock Monitoring Updates"
#' (or perhaps additional options as well)
#' @param final_year Year of assessment and reference points
#' (typically will be `model[["endyr"]] + 1`)
#' @param data_year Last year of of timeseries data
#' @param month Month when assessment was conducted (within `final_year`)
#' @param sciencecenter Origin of assessment report
#' @param Mgt_Council Council jurisdiction. Currently only
#' `"PFMC"` (Pacific Fishery Management Council) and `"GM"`
#' (Gulf of Mexico) are the only options.
#' @param SpawnOutputLabel Units for spawning output if not in mt
#' (e.g. "Millions of eggs"). In the future this can be included in the
#' `model` list created by `r4ss::SS_output()`
#' @param contact Name and/or email address for lead author.
#' @param review_result Something like "Full Acceptance"
#' @param catch_input_data Qualitative category for catch input data
#' @param abundance_input_data Qualitative category for abundance input data
#' @param bio_input_data Qualitative category for biological input data
#' @param comp_input_data Qualitative category for size/age composition input
#' data
#' @param ecosystem_linkage Qualitative category for ecosystem linkage
#' @author Ian G. Taylor, Andi Stephens, LaTreese S. Denson
#' @export
#' @seealso [SS_output()]
#' @examples
#' \dontrun{
#' # read the model output
#' model <- SS_output(
#'   dir = system.file("extdata/simple_small", package = "r4ss"),
#'   printstats = FALSE, verbose = FALSE
#' )
#' # run get_SIS_info:
#' info <- get_SIS_info(model, stock = "SimpleExample", month = 1)
#' }
#'
get_SIS_info <- function(model,
                         dir = model[["inputs"]][["dir"]],
                         writecsv = TRUE,
                         stock = "StockName",
                         assessment_type = "Operational",
                         final_year = model[["endyr"]] + 1,
                         data_year = model[["endyr"]],
                         month,
                         sciencecenter = "NWFSC",
                         Mgt_Council = "PFMC",
                         # SpawnOutputLabel is only available in an r4ss branch
                         # https://github.com/r4ss/r4ss/compare/main...spawn_output_label_838
                         # so will be NULL for most models, which is dealt with later
                         SpawnOutputLabel = model[["SpawnOutputLabel"]],
                         contact = "first.last@noaa.gov",
                         review_result = "XXXX",
                         catch_input_data = "XXXX",
                         abundance_input_data = "XXXX",
                         bio_input_data = "XXXX",
                         comp_input_data = "XXXX",
                         ecosystem_linkage = "XXXX") {
  # construct filename
  filename_values <- paste(gsub(" ", "_", stock), final_year,
    "SIS_info_values.csv",
    sep = "_"
  )
  filename_timeseries <- paste(gsub(" ", "_", stock), final_year,
    "SIS_info_timeseries.csv",
    sep = "_"
  )

  message(
    "writing SIS info to CSV files:\n",
    file.path(dir, filename_values), "\n",
    file.path(dir, filename_timeseries)
  )

  # years to report for catch-related quantities
  startyr <- model[["startyr"]]
  years <- startyr:final_year

  # get values from TIME_SERIES table
  ts <- model[["timeseries"]]

  # if check for unsupported model configurations
  if (model[["nseasons"]] > 1) {
    stop("multi-season models are not yet supported")
  }

  # aggregate across areas if needed
  if (model[["nareas"]] > 1) {
    ts.tmp <- ts # copy of table
    ts <- ts.tmp[ts.tmp[["Area"]] == 1, ]
    for (iarea in 2:model[["nareas"]]) {
      ts[, -(1:4)] <- ts[, -(1:4)] + ts.tmp[ts.tmp[["Area"]] == iarea, -(1:4)]
    }
  }

  # calculate fraction unfished
  ts[["FractionUnfished"]] <-
    ts[["SpawnBio"]] / ts[["SpawnBio"]][ts[["Era"]] == "VIRG"]

  # extract columns of interest
  ts_tab <- data.frame(
    Year = ts[["Yr"]],
    Total_Bio = round(ts[["Bio_all"]], 2),
    SpawnBio = round(ts[["SpawnBio"]], 2),
    FractionUnfished = round(ts[["FractionUnfished"]], 3),
    Summary_Bio = round(ts[["Bio_smry"]], 2),
    Recruitment = round(ts[["Recruit_0"]], 2)
  )

  # subset for years of interest and replace potential bad 0 value with NA
  ts_tab <- ts_tab[ts_tab[["Year"]] %in% years, ]

  # calculate total dead catch (aggregated across fleets)
  dead_bio_columns <- grep("dead(B)", names(model[["timeseries"]]), fixed = TRUE)
  dead_N_columns <- grep("dead(N)", names(model[["timeseries"]]), fixed = TRUE)
  if (length(dead_bio_columns) > 1) {
    catch_tab <- data.frame(
      Year = model[["timeseries"]][["Yr"]],
      Catch_bio = apply(model[["timeseries"]][, dead_bio_columns],
        MARGIN = 1, FUN = sum
      ),
      Catch_n = apply(model[["timeseries"]][, dead_N_columns],
        MARGIN = 1, FUN = sum
      )
    )
  }
  if (length(dead_bio_columns) == 1) {
    catch_tab <- data.frame(
      Year = model[["timeseries"]][["Yr"]],
      Catch_bio = model[["timeseries"]][, dead_bio_columns],
      Catch_n = model[["timeseries"]][, dead_N_columns]
    )
  }
  if (length(dead_bio_columns) == 0) {
    stop("No columns matching 'dead(B)' in model[['timeseries'']]")
  }

  # not sure why this is needed, but it is
  catch_tab[["Year"]] <- as.numeric(catch_tab[["Year"]])
  if (model[["nareas"]] > 1) {
    catch_tab <- aggregate(catch_tab[["Catch"]],
      by = list(catch_tab[["Year"]]),
      FUN = sum
    )
    names(catch_tab) <- c("Year", "Catch")
  }
  catch_tab[, -1] <- round(catch_tab[, -1], 2)
  rownames(catch_tab) <- 1:nrow(catch_tab)

  # filter years
  catch_tab <- catch_tab[catch_tab[["Year"]] %in% years, ]

  # calculate proxy-F values (e.g. exploitation rate)
  F_tab <- data.frame(
    Year = years,
    F_values = round(model[["derived_quants"]][paste0("F_", years), "Value"], 4)
  )
  # filter years
  F_tab <- F_tab[F_tab[["Year"]] %in% years, ]

  # SPR-related quantities
  if ("Tot_Exploit" %in% names(model[["sprseries"]])) {
    spr_tab <- model[["sprseries"]][, c("Yr", "SPR_std", "Tot_Exploit", "SPR")]
  } else {
    spr_tab <- model[["sprseries"]][, c("Yr", "SPR")]
    from_ann_ts <- model[["annual_time_series"]][, c("Yr", "SPR_std", "Tot_Exploit")]
    from_ann_ts[["Yr"]] <- from_ann_ts[["year"]]
    from_ann_ts[["Tot_Exploit"]] <- from_ann_ts[["tot_exploit"]]
    spr_tab <- merge(from_ann_ts, spr_tab, by = "Yr")
  }

  names(spr_tab)[1] <- "Year"
  if (model[["SPRratioLabel"]] != "1-SPR") {
    # add 1-SPR if it does not exist as SPR_std already
    spr_tab[["1-SPR"]] <- 1 - spr_tab[["SPR"]]
    spr_tab <- spr_tab[, c("Year", "SPR_std", "Tot_Exploit", "1-SPR")]
  } else {
    spr_tab <- spr_tab[, c("Year", "SPR_std", "Tot_Exploit")]
  }
  # filter years
  spr_tab <- spr_tab[spr_tab[["Year"]] %in% years, ]
  spr_tab[, -1] <- round(spr_tab[, -1], 3)

  # merge columns from time series, catch, F, and SPR together
  tab <- merge(merge(merge(ts_tab, catch_tab), F_tab), spr_tab)
  if (nrow(tab) != length(years) || any(tab[["Year"]] != years)) {
    stop(
      "problem with mismatch of years:\n",
      "range(years): ", range(years), "\n",
      "range(tab[['Year']]): ", range(tab[["Year"]]), "\n"
    )
  }

  # replace NA with 0 in exploitation rate for years with 0 catch
  tab[["F_values"]][!is.na(tab[["Catch_bio"]]) & tab[["Catch_bio"]] == 0] <- 0
  # again for 8th column (SPR-ratio)
  tab[["SPR_std"]][!is.na(tab[["Catch_bio"]]) & tab[["Catch_bio"]] == 0] <- 0

  # replace values with NA for years beyond the data years
  if (Mgt_Council == "PFMC") {
    tab[tab[["Year"]] > data_year, "Catch_bio"] <- NA
    tab[tab[["Year"]] > data_year, "Catch_n"] <- NA
    tab[tab[["Year"]] > data_year, "F_values"] <- NA
    tab[tab[["Year"]] > data_year, "SPR_std"] <- NA
    tab[tab[["Year"]] > data_year, "Tot_Exploit"] <- NA
    if ("1-SPR" %in% names(tab)) {
      tab[tab[["Year"]] > data_year, "1-SPR"] <- NA
    }
  }

  # age for summary biomass
  summary_age <- model[["summary_age"]]
  if (is.null(summary_age)) {
    summary_age <- "X"
  }
  summary_bio_label <- paste0("Age ", summary_age, "+ biomass")

  #######################################

  # create new table of metadata to write as multiple header rows
  header_info <- data.frame(matrix("", ncol = ncol(tab), nrow = 4),
    stringsAsFactors = FALSE
  )
  colnames(header_info) <- names(tab)
  rownames(header_info) <- c("Category", "Primary", "Description", "Unit")

  # putting "year" in the right place
  header_info["Category", "Year"] <- "Year"

  # info on total biomass
  header_info["Category", "Total_Bio"] <- "Abundance"
  header_info["Primary", "Total_Bio"] <- "N"
  header_info["Description", "Total_Bio"] <- "Total Biomass"
  header_info["Unit", "Total_Bio"] <- "Metric Tons"

  # info on summary biomass
  header_info["Category", "Summary_Bio"] <- "Abundance"
  header_info["Primary", "Summary_Bio"] <- "N"
  header_info["Description", "Summary_Bio"] <- summary_bio_label
  header_info["Unit", "Summary_Bio"] <- "Metric Tons"

  # info on spawning biomass
  header_info["Category", "SpawnBio"] <- "Spawners"
  header_info["Primary", "SpawnBio"] <- "Y"

  # SpawnOutputUnits indicating numbers vs biomass switch should work for all
  # models starting with SS3 version 3.30.20 (September 2020)
  if (model[["SpawnOutputUnits"]] == "numbers") {
    header_info["Description", "SpawnBio"] <- "Spawning Output(Eggs)"
    if (is.null(SpawnOutputLabel)) {
      warning("Need to provide a label for the spawning output (e.g. 'millions of eggs')")
      SpawnOutputLabel <- "XXXX eggs"
    }
    header_info["Unit", "SpawnBio"] <- SpawnOutputLabel
    B_basis <- "Stock Reproductive Output"
    B_unit <- SpawnOutputLabel
  } else {
    header_info["Description", "SpawnBio"] <- " Female Mature Spawning Biomass"
    header_info["Unit", "SpawnBio"] <- "Metric Tons"
    B_basis <- "Spawning Stock Biomass"
    B_unit <- "mt"
  }

  # info on fraction unfished
  header_info["Category", "FractionUnfished"] <- "Spawners"
  header_info["Primary", "FractionUnfished"] <- "N"
  header_info["Description", "FractionUnfished"] <- "Female Mature Relative Spawning Biomass SSB/SSB0"
  header_info["Unit", "FractionUnfished"] <- "Rate"

  # info on recruitment
  header_info["Category", "Recruitment"] <- "Recruitment"
  header_info["Primary", "Recruitment"] <- "Y"
  header_info["Description", "Recruitment"] <- "Abundance at Age 0"
  header_info["Unit", "Recruitment"] <- "Number x 1000"

  # info on Catch biomass
  header_info["Category", "Catch_bio"] <- "Catch"
  header_info["Primary", "Catch_bio"] <- "Y"
  header_info["Description", "Catch_bio"] <- "Modeled Total Catch"
  header_info["Unit", "Catch_bio"] <- "Metric Tons"

  # info on Catch numbers
  header_info["Category", "Catch_n"] <- "Catch"
  header_info["Primary", "Catch_n"] <- "N"
  header_info["Description", "Catch_n"] <- "Modeled Total Catch"
  header_info["Unit", "Catch_n"] <- "Number x 1000"

  # check for redundancy between F_values and Tot_Exploit columns
  if (model[["F_std_basis"]] == "_abs_F;_with_F=Exploit(bio)") {
    message(
      "F_YYYY values are redundant with Tot_Exploit column in SPR series, ",
      "excluding from output for SIS."
    )
    # remove redundant F_values column
    header_info <- header_info |> dplyr::select(-F_values)
    tab <- tab |> dplyr::select(-F_values)
  } else {
    # info on Exploitation rate (whatever is chosen for the F_YYYY quants)
    header_info["Category", "F_values"] <- "Fmort"
    header_info["Primary", "F_values"] <-
      dplyr::case_when(
        Mgt_Council == "PFMC" ~ "N", # not used as primary exploitation rate by PFMC
        Mgt_Council == "GM" ~ "XXXX", # fill in value here
        TRUE ~ "XXXX" # any other Mgt_Council
      )
    # clean up F_std_basis (e.g. "_abs_F;_with_F=sum(full_Fs)" to "abs F; with F=sum(full Fs)")
    header_info["Description", "F_values"] <- gsub("_", " ", gsub("^_", "", model[["F_std_basis"]]))

    header_info["Unit", "F_values"] <- "Rate"
  }

  # info on total Exploitation rate (catch / summary bio)
  header_info["Category", "Tot_Exploit"] <- "Fmort"
  header_info["Primary", "Tot_Exploit"] <-
    dplyr::case_when(
      Mgt_Council == "PFMC" ~ "N", # not used as primary exploitation rate by PFMC
      Mgt_Council == "GM" ~ "XXXX", # fill in value here
      TRUE ~ "XXXX" # any other Mgt_Council
    )
  header_info["Description", "Tot_Exploit"] <-
    paste0("Relative Exploitation Rate (Catch/", summary_bio_label, ")")
  header_info["Unit", "Tot_Exploit"] <- "Rate"

  # info on SPR_std (whatever is chosen for SPR ratio)
  header_info["Category", "SPR_std"] <- "Fmort"
  header_info["Primary", "SPR_std"] <-
    dplyr::case_when(
      Mgt_Council == "PFMC" ~ "Y", # often used as primary by PFMC
      Mgt_Council == "GM" ~ "XXXX", # fill in value here
      TRUE ~ "XXXX" # any other Mgt_Council
    )
  header_info["Description", "SPR_std"] <- model[["SPRratioLabel"]]
  header_info["Unit", "SPR_std"] <- "Rate"

  # If SPR_std is not "1-SPR" (e.g. if it's a ratio of 1-SPR to something else)
  # then add column with 1 - SPR
  if (model[["SPRratioLabel"]] != "1-SPR") {
    header_info["Category", "1-SPR"] <- "Fmort"
    header_info["Primary", "1-SPR"] <-
      dplyr::case_when(
        Mgt_Council == "PFMC" ~ "Y", # 1 - SPR is used as primary exploitation rate by PFMC
        Mgt_Council == "GM" ~ "XXXX", # fill in value here
        TRUE ~ "XXXX" # any other Mgt_Council
      )
    # change the value from the SPR_std set above
    header_info["Primary", "SPR_std"] <-
      dplyr::case_when(
        Mgt_Council == "PFMC" ~ "N", # SPR_ratio is not primary if not equal to 1-SPR
        Mgt_Council == "GM" ~ "XXXX", # fill in value here
        TRUE ~ "XXXX" # any other Mgt_Council
      )
    header_info["Description", "1-SPR"] <- "1 - SPR"
    header_info["Unit", "1-SPR"] <- "Rate"
  }

  # add extra column to help with format of CSV file
  tab <- cbind("", tab)

  # end of time series calculations
  ######################################################################
  # start gathering quantities of interest

  if (model[["sprtarg"]] == -999) {
    SPRtarg_text <- "SPR_XX%"
    model[["sprtarg"]] <- NA
    warning("No value for model[['sprtarg']]")
  } else {
    SPRtarg_text <- paste0("SPR", 100 * model[["sprtarg"]], "%") # e.g. SPR50%
  }

  # PFMC Settings (F limit and MSY proxy are in units of 1-SPR)
  if (Mgt_Council == "PFMC") {
    F_limit <- 1 - model[["sprtarg"]]
    F_limit_basis <- paste0("1-SPR_", SPRtarg_text)
    F_msy <- F_limit
    F_msy_basis <- F_limit_basis
  }

  # GM Settings: Using F_SPR target
  if (Mgt_Council == "GM") {
    # Annual F_SPR names changed between versions
    if ("annF_SPR" %in% rownames(model[["derived_quants"]])) {
      F_limit <- round(model[["derived_quants"]]["annF_SPR", "Value"], digits = 3)
    } else {
      F_limit <- round(model[["derived_quants"]]["Fstd_SPRtgt", "Value"], digits = 3)
    }
    F_msy <- F_limit
    F_limit_basis <- paste0("F", 100 * model[["btarg"]], "%")
    F_msy_basis <- paste0("F", 100 * model[["btarg"]], "% as Proxy")
  }


  if (model[["btarg"]] == -999) {
    Btarg_text <- "BXX%"
    model[["btarg"]] <- NA
    warning("No value for model[['btarg']]")
  } else {
    # PFMC Settings
    if (Mgt_Council == "PFMC") {
      Btarg_text <- paste0("B", 100 * model[["btarg"]], "%") # e.g. B40%
    }
    # GM Settings:
    if (Mgt_Council == "GM") {
      Btarg_text <- paste0("SPR", 100 * model[["btarg"]], "%")
    }
  }

  if (model[["minbthresh"]] == -999) {
    MinBthresh_text <- "SSBXX%"
    model[["minbthresh"]] <- NA
    warning("No value for model[['minbthresh']]")
  } else {
    MinBthresh_text <- paste0("SSB", 100 * model[["minbthresh"]], "%") # e.g. B25%
    B_msy_basis <- paste0("SS", Btarg_text)
    B_msy <- model[["SBzero"]] * model[["btarg"]]
    B_limit <- model[["SBzero"]] * model[["minbthresh"]]
  }

  # GM Settings:
  if (Mgt_Council == "GM") {
    MinBthresh_text <- paste0("(1-M)*SPR", 100 * model[["btarg"]], "%") # e.g. B25%
    B_msy_basis <- Btarg_text
    eq_year <- data_year + model[["nforecastyears"]]
    B_msy <- model[["derived_quants"]][["Value"]][which(model[["derived_quants"]][["Label"]] == paste0("SSB_", eq_year))]
    B_limit <- (0.5 * B_msy)
  }

  B_year <- final_year
  ts_tab_short <- data.frame(
    Year = ts[["Yr"]],
    SpawnBio = round(ts[["SpawnBio"]], 2)
  )
  Bcurrent <- ts_tab_short[["SpawnBio"]][ts_tab_short[["Year"]] == final_year]

  # GM Settings:
  if (Mgt_Council == "GM") {
    B_year <- data_year
    Bcurrent <- tab[["SpawnBio"]][tab[["Year"]] == data_year]
  }

  # MSY-proxy labels were changed at some point
  if ("Dead_Catch_SPR" %in% rownames(model[["derived_quants"]])) {
    Yield_at_SPR_target <- round(model[["derived_quants"]]["Dead_Catch_SPR", "Value"], 1)
    Yield_at_B_target <- round(model[["derived_quants"]]["Dead_Catch_Btgt", "Value"], 1)
  } else {
    Yield_at_SPR_target <- round(model[["derived_quants"]]["TotYield_SPRtgt", "Value"], 1)
    Yield_at_B_target <- round(model[["derived_quants"]]["TotYield_Btgt", "Value"], 1)
  }

  if (Mgt_Council == "PFMC") {
    best_F_column <- "1-SPR"
    if (!best_F_column %in% names(tab)) {
      best_F_column <- "SPR_std"
    }
    Best_F_Est <- tab[tab[["Year"]] == data_year, best_F_column]
    F_basis <- "Equilibrium SPR"
    F_unit <- "1-SPR"
  }

  # GM Settings: Best F estimate = geometric mean of last three data years
  if (Mgt_Council == "GM") {
    Best_F_Est <- round(mean(tab[["F_values"]][tab[["Year"]] %in% c((data_year - 2):data_year)]), 2)
    F_basis <- paste0("Total_Catch/Total_Biomass_Geometric_Mean_", (data_year - 2), "-", data_year)
    F_unit <- "Exploitation Rate"
  }

  # SS3 version
  SS_version <- strsplit(model[["SS_version"]], split = ";")[[1]][1]
  gsub("-safe", "", SS_version)
  gsub("-opt", "", SS_version)

  stock_level_to_MSY <- Bcurrent / B_msy
  if (stock_level_to_MSY > 1) {
    stock_level_to_MSY <- "ABOVE"
  } else if (stock_level_to_MSY < 0.90) {
    stock_level_to_MSY <- "BELOW"
  } else if (stock_level_to_MSY > 0.90 & stock_level_to_MSY < 1) {
    stock_level_to_MSY <- "NEAR"
  } else {
    stock_level_to_MSY <- "UNKNOWN"
  }


  # make big 2-column table of info about each model
  info_tab <- c(
    paste0("SAIP:,", "https://spo.nmfs.noaa.gov/sites/default/files/TMSPO183.pdf"),
    "",
    paste0("Stock / Entity Name,", stock),
    paste0("Assessment Type,", assessment_type),
    paste0("Ensemble/Multimodel Approach,", "NA"),
    # sprintf command in line below converts month 9 to "09" (for example)
    paste0("Asmt Year and Month,", final_year, ".", sprintf("%02d", month)),
    paste0("Last Data Year,", data_year),
    paste0("Asmt Model Category,", "6 - Statistical Catch-at-Age (SS, ASAP, AMAK, BAM, MultifancL< CASAL)"),
    paste0("Asmt Model Category,", "SS"),
    paste0("Model Version,", SS_version),
    paste0("Lead Lab,", sciencecenter),
    paste0("Point of Contact (assessment lead),", contact),
    paste0("Review Result,", review_result),
    paste0("Catch Input Data,", catch_input_data),
    paste0("Abundance Input Data,", abundance_input_data),
    paste0("Biological Input Data,", bio_input_data),
    paste0("Size/Age Composition Input Data,", comp_input_data),
    paste0("Ecosystem Linkage,", ecosystem_linkage),
    "",
    "Fishing Mortality Estimates",
    paste0("F Year, ", data_year),
    paste0("Min F Estimate, ", ""),
    paste0("Max F Estimate, ", ""),
    paste0("Best F Estimate, ", Best_F_Est),
    paste0("F Basis, ", F_basis),
    paste0("F Unit, ", F_unit),
    "",
    "Domestic Fishing Mortality Derive Management Quantities",
    paste0("Flimit, ", F_limit),
    paste0("Flimit Basis, ", F_limit_basis),
    paste0("FMSY, ", F_msy),
    paste0("FMSY Basis, ", F_msy_basis),
    paste0("F target, ", ""),
    paste0("F target Basis, ", ""),
    paste0("F/F_limit, ", round(Best_F_Est / F_limit, 3)),
    paste0("F/F_MSY, ", round(Best_F_Est / F_msy, 3)),
    paste0("F/F_target, ", ""),
    "",
    "Biomass Estimates",
    paste0("B Year, ", B_year),
    paste0("Min B. Estimate, ", ""),
    paste0("Max. B Estimate, ", ""),
    paste0("Best B Estimate, ", round(Bcurrent, 1)),
    paste0("B Basis, ", B_basis),
    paste0("B Unit, ", B_unit),
    paste0("MSY, ", Yield_at_B_target),
    paste0("MSY Unit, ", "mt"),
    "",
    "Domestic Biomass Derived Management Quantities",
    paste0("Blimit, ", round(B_limit, 1)),
    paste0("Blimit Basis, ", MinBthresh_text),
    paste0("BMSY, ", round(B_msy, 1)),
    paste0("BMSY Basis, ", B_msy_basis), # e.g. SSB40%
    paste0("B/Blimit, ", round((Bcurrent / B_limit), 3)),
    paste0("B/Bmsy, ", round((Bcurrent / B_msy), 3)),
    paste0("Stock Level (relative) to BMSY, ", stock_level_to_MSY),
    "",
    ""
  )

  if (writecsv) {
    # write 2-column table with quantities of interest
    write.table(info_tab,
      file = file.path(dir, filename_values),
      quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE
    )

    # write header rows for time series table
    write.table(header_info,
      file = file.path(dir, filename_timeseries),
      quote = FALSE, sep = ",", row.names = TRUE, col.names = FALSE,
      append = FALSE
    )
    # add time series values (suppressing warning about 'appending column names to file')
    suppressWarnings(
      write.table(tab,
        file = file.path(dir, filename_timeseries),
        quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE,
        append = TRUE
      )
    )
  }

  # return the list
  invisible(list(info = info_tab, timeseries = tab, header_info = header_info))
}
