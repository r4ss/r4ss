#' Gather information for the NOAA Species Information System (SIS)
#'
#' Processes model results contained in the list created by
#' \code{\link{SS_output}} in a format that is more convenient for submission
#' to SIS. Currently the results are returned invisibly as a list of two tables
#' and written to a CSV file from which results could be copied into SIS.
#' In the future some more direct link could be explored to avoid the manual
#' copy step.

#' @param model Output from SS_output
#' @param dir Directory where the file will be written
#' @param writecsv Write results to a CSV file (where the name will have the
#' format "[species]_2019_SIS_info.csv" where \code{species}
#' is an additional input
#' @param stock String to prepend id info to filename for CSV file
#' @param final_year Year for reference points and final year of timeseries
#' (typically will be model$endyr + 1)
#' @author Ian G. Taylor, Andi Stephens
#' @export
#' @seealso \code{\link{SS_output}}
#' @examples
#'
#'   \dontrun{
#' # directory with the model output
#' mydir <- file.path(path.package("r4ss"), "extdata/simple_3.30.13")
#' # read the model output
#' model <- SS_output(dir = mydir)
#' # run get_SIS_info:
#' info <- get_SIS_info(model, stock = "SimpleExample")
#'   }


get_SIS_info <- function(model, dir = NULL, writecsv = TRUE,
                         stock = "StockName", final_year = 2019){

  # directory to write file
  if(is.null(dir)){
    dir <- model$inputs$dir
  }

  # construct filename
  if (is.null(final_year)) {
    final_year <- model$endyr + 1
  }
  filename_values <- paste(gsub(" ", "_", stock), final_year,
                           "SIS_info_values.csv", sep="_")
  filename_timeseries <- paste(gsub(" ", "_", stock), final_year,
                               "SIS_info_timeseries.csv", sep="_")

  message("writing SIS info to CSV files:\n",
          file.path(dir, filename_values ), "\n",
          file.path(dir, filename_timeseries ))

  # years to report for catch-related quantities
  startyr <- model$startyr
  years <- startyr:final_year

  # get values from TIME_SERIES table
  ts <- model$timeseries

  # if check for unsupported model configurations
  if(model$nseasons > 1){
    stop("multi-season models are not yet supported")
  }

  # aggregate across areas if needed
  if(model$nareas > 1){
    ts.tmp <- ts # copy of table
    ts <- ts.tmp[ts.tmp$Area == 1,]
    for(iarea in 2:model$nareas){
      ts[,-(1:4)] <- ts[,-(1:4)] + ts.tmp[ts.tmp$Area == iarea,-(1:4)]
    }
  }

  # calculate fraction unfished
  ts$FractionUnfished <-
    ts$SpawnBio / ts$SpawnBio[ts$Era == "VIRG"]

  # extract columns of interest
  ts_tab <- data.frame(Year = ts$Yr,
                       Total_Bio = round(ts$Bio_all),
                       SpawnBio = round(ts$SpawnBio),
                       FractionUnfished = round(ts$FractionUnfished, 3),
                       Summary_Bio = round(ts$Bio_smry),
                       Recruitment = round(ts$Recruit_0))

  # subset for years of interst and replace potential bad 0 value with NA
  ts_tab <- ts_tab[ts_tab$Year %in% years,]
  if(ts_tab$Total_Bio[ts_tab$Year == final_year] == 0){
    ts_tab$Total_Bio[ts_tab$Year == final_year] <- NA
  }

  # calculate total dead catch (aggregated across fleets)
  dead_bio_columns <- grep("dead(B)", names(model$timeseries), fixed = TRUE)
  if(length(dead_bio_columns) > 1){
    catch_tab <- data.frame(Year = model$timeseries$Yr,
                            Catch = apply(model$timeseries[,dead_bio_columns],
                                MARGIN = 1, FUN = sum))
  }
  if(length(dead_bio_columns) == 1){
    catch_tab <- data.frame(Year = model$timeseries$Yr,
                            Catch = model$timeseries[,dead_bio_columns])
  }
  if(length(dead_bio_columns) == 0){
    stop("No columns matching 'dead(B)' in model$timeseries")
  }

  # not sure why this is needed, but it is
  catch_tab$Year <- as.numeric(catch_tab$Year)
  if(model$nareas > 1){
    catch_tab <- aggregate(catch_tab$Catch,
                           by = list(catch_tab$Year),
                           FUN = sum)
    names(catch_tab) <- c("Year", "Catch")
  }
  catch_tab$Catch <- round(catch_tab$Catch, 0)
  rownames(catch_tab) <- 1:nrow(catch_tab)

  # filter years and set forecast values to NA
  catch_tab <- catch_tab[catch_tab$Year %in% years,]
  catch_tab[catch_tab$Year == final_year, -1] <- NA

  # calculate proxy-F values (e.g. exploitation rate)
  F_tab <- data.frame(Year = years,
                      Exploit_rate = round(model$derived_quants[paste0("F_", years), "Value"], 2))
  if(length(grep("Exploit(bio)", model$F_report_basis, fixed = TRUE)) == 0){
    warning("Problem with units of F, value is not exploitation rate")
  }
  # filter years and set forecast values to NA
  F_tab <- F_tab[F_tab$Year %in% years,]
  F_tab[F_tab$Year == final_year, -1] <- NA

  # SPR-related quantities
  spr_tab <- model$sprseries[, c("Yr", "SPR_report", "SPR")]
  # add 1-SPR (may have already existed as SPR_report
  spr_tab$"1-SPR" <- 1 - spr_tab$SPR
  names(spr_tab)[1] <- "Year"
  spr_tab <- spr_tab[,c("Year","SPR_report", "1-SPR")]
  # filter years and set forecast values to NA
  spr_tab <- spr_tab[spr_tab$Year %in% years,]
  spr_tab[spr_tab$Year == final_year, -1] <- NA
  spr_tab[,-1] <- round(spr_tab[,-1], 3)

  # merge columns from time series, catch, F, and SPR together
  tab <- merge(merge(merge(ts_tab, catch_tab), F_tab), spr_tab)
  if(nrow(tab) != length(years) || any(tab$Year != years)){
    stop("problem with mismatch of years:\n",
         "range(years): ", range(years), "\n",
         "range(tab$Year): ", range(tab$Year), "\n")
  }

  # replace NA with 0 in exploitation rate for years with 0 catch
  tab$Exploit_rate[!is.na(tab$Catch) & tab$Catch == 0] <- 0
  # again for 8th column (SPR-ratio)
  tab[!is.na(tab$Catch) & tab$Catch == 0, 8] <- 0

  # age for summary biomass
  summary_age <- model$summary_age
  if(is.null(summary_age)){
    summary_age <- "X"
  }
  summary_bio_label <- paste0("Age ", summary_age, "+ biomass")

  #######################################

  # create new table of metadata to write as multiple header rows
  header_info <- data.frame(matrix("", ncol = ncol(tab), nrow = 4),
                         stringsAsFactors = FALSE)
  colnames(header_info) <- names(tab)
  rownames(header_info) <- c("Category", "Primary", "Description", "Unit")

  # putting "year" in the right place
  header_info["Category",   "Year"] <- "Year"

  # info on total biomass
  header_info["Category",   "Total_Bio"] <- "Abundance"
  header_info["Primary",    "Total_Bio"] <- "N"
  header_info["Description","Total_Bio"] <- "Total Biomass"
  header_info["Unit",       "Total_Bio"] <- "Metric Tons"

  # info on summary biomass
  header_info["Category",   "Summary_Bio"] <- "Abundance"
  header_info["Primary",    "Summary_Bio"] <- "N"
  header_info["Description","Summary_Bio"] <- summary_bio_label
  header_info["Unit",       "Summary_Bio"] <- "Metric Tons"

  # info on spawning biomass
  header_info["Category",   "SpawnBio"] <- "Spawners"
  header_info["Primary",    "SpawnBio"] <- "Y"
  if(model$SpawnOutputUnits == "numbers"){
    header_info["Description","SpawnBio"] <- "Spawning Output, Eggs"
    header_info["Unit",       "SpawnBio"] <- "XXXX"
  }else{
    header_info["Description","SpawnBio"] <- "Spawning Biomass"
    header_info["Unit",       "SpawnBio"] <- "Metric Tons"
  }

  # info on fraction unfished
  header_info["Category",   "FractionUnfished"] <- "Spawners"
  header_info["Primary",    "FractionUnfished"] <- "N"
  header_info["Description","FractionUnfished"] <- "Female Mature Relative Spawning Biomass SSB/SSB0"
  header_info["Unit",       "FractionUnfished"] <- "Rate"

  # info on recruitment
  header_info["Category",   "Recruitment"] <- "Recruitment"
  header_info["Primary",    "Recruitment"] <- "Y"
  header_info["Description","Recruitment"] <- "Abundance at Age 0"
  header_info["Unit",       "Recruitment"] <- "Number x 1000"

  # info on Catch
  header_info["Category",   "Catch"] <- "Catch"
  header_info["Primary",    "Catch"] <- "Y"
  header_info["Description","Catch"] <- "Modeled Total Catch"
  header_info["Unit",       "Catch"] <- "Metric Tons"

  # info on Exploitation rate
  header_info["Category",   "Exploit_rate"] <- "Fmort"
  header_info["Primary",    "Exploit_rate"] <- "N"
  header_info["Description","Exploit_rate"] <-
    paste0("Relative Exploitation Rate (Catch/", summary_bio_label, ")")
  header_info["Unit",       "Exploit_rate"] <- "Rate"

  # info on SPR_report
  header_info["Category",   "SPR_report"] <- "Fmort"
  header_info["Primary",    "SPR_report"] <- "Y"
  header_info["Description","SPR_report"] <- model$SPRratioLabel
  header_info["Unit",       "SPR_report"] <- "Rate"

  # info on 1 - SPR
  header_info["Category",   "1-SPR"] <- "Fmort"
  header_info["Primary",    "1-SPR"] <- "N"
  header_info["Description","1-SPR"] <- "1 - SPR"
  header_info["Unit",       "1-SPR"] <- "Rate"

  # add extra column to help with format of CSV file
  tab <- cbind("", tab)

  # end of time series calculations
  ######################################################################
  # start gathering quantities of interest

  if(model$sprtarg == -999){
    SPRtarg_text <- "SPR_XX%"
    model$sprtarg <- NA
    warning('No value for model$sprtarg')
  }else{
    SPRtarg_text <- paste0("SPR", 100*model$sprtarg, "%") # e.g. SPR50%
  }

  if(model$btarg == -999){
    Btarg_text <- "BXX%"
    model$btarg <- NA
    warning('No value for model$btarg')
  }else{
    Btarg_text <- paste0("B", 100*model$btarg, "%") # e.g. B40%
  }

  if(model$minbthresh == -999){
    MinBthresh_text <- "BXX%"
    model$minbthresh <- NA
    warning('No value for model$minbthresh')
  }else{
    MinBthresh_text <- paste0("B", 100*model$minbthresh, "%") # e.g. B25%
  }

  Bcurrent <- tab$SpawnBio[tab$Year == final_year]

  # MSY-proxy labels were changed at some point
  if("Dead_Catch_SPR" %in% rownames(model$derived_quants)){
    Yield_at_SPR_target <- round(model$derived_quants["Dead_Catch_SPR", "Value"])
    Yield_at_B_target <- round(model$derived_quants["Dead_Catch_Btgt", "Value"])
  }else{
    Yield_at_SPR_target <- round(model$derived_quants["TotYield_SPRtgt", "Value"])
    Yield_at_B_target <- round(model$derived_quants["TotYield_Btgt", "Value"])
  }

  # SS version
  SS_version <- strsplit(model$SS_version, split = ";")[[1]][1]
  gsub("-safe", "", SS_version)
  gsub("-opt", "", SS_version)

  # make big 2-column table of info about each model
  info_tab <- c(paste0("SAIP:,", "https://spo.nmfs.noaa.gov/sites/default/files/TMSPO183.pdf"),
                "",
                paste0("Stock / Entity Name,", stock),
                paste0("Assessment Type,", "X"),
                paste0("Ensemble/Multimodel Approach,", "NA"),
                paste0("Asmt Year and Month,", "XXXX.XX"),
                paste0("Last Data Year,", final_year - 1),
                paste0("Asmt Model Category,", "6 - Statistical Catch-at-Age (SS, ASAP, AMAK, BAM, MultifancL< CASAL)"),
                paste0("Asmt Model Category,", "SS"),
                paste0("Model Version,", SS_version),
                paste0("Lead Lab,", "NWFSC"),
                paste0("Point of Contact (assessment lead),", "XXXX"),
                paste0("Review Result,", "XXXX"),
                paste0("Catch Input Data,", "XXXX"),
                paste0("Abundance Input Data,", "XXXX"),
                paste0("Biological Input Data,", "XXXX"),
                paste0("Size/Age Composition Input Data,", "XXXX"),
                paste0("Ecosystem Linkage,", "XXXX"),
                "",

                "Fishing Mortality Estimates",
                paste0("F Year, ",          final_year - 1),
                paste0("Min F Estimate, ",  ""),
                paste0("Max F Estimate, ",  ""),
                paste0("Best F Estimate, ", tab$"1-SPR"[tab$Year == final_year - 1]),
                paste0("F Basis, ",         "Equilibrium SPR"),
                paste0("F Unit, ",          "1-SPR"),
                "",

                "Domestic Fishing Mortality Derive Management Quantities",
                paste0("Flimit, ",          1 - model$sprtarg),
                paste0("Flimit Basis, ",    "1-SPR_", SPRtarg_text),
                paste0("FMSY, ",            1 - model$sprtarg),
                paste0("FMSY Basis, ",      "1-SPR_",SPRtarg_text),
                paste0("F target,",         ""),
                paste0("F target Basis,",   ""),
                paste0("MSY, ",             Yield_at_SPR_target),
                paste0("MSY Unit,",         "mt"),
                paste0("MSY Basis, ",     "Yield with ", SPRtarg_text,
                       " at SB_", SPRtarg_text),
                "",

                "Biomass Estimates",
                paste0("[Unfished Spawning Biomass], ", model$SBzero),
                paste0("B Year, ",          final_year),
                paste0("Min B. Estimate, ", ""),
                paste0("Max. B Estimate, ", ""),
                paste0("Best B Estimate, ", Bcurrent),
                paste0("B Basis, ",         "Spawning Biomass / Female mature biomass"),
                paste0("B Unit, ",          "mt"),
                paste0("MSY, ",             Yield_at_B_target),
                paste0("MSY Unit, ",        "mt"),
                # paste0("[MSY Basis], ",     "Yield with SPR_",
                #        Btarg_text," at ",   Btarg_text, "(mt)"),
                # paste0("[Depletion], ",
                #        round(model$derived_quants[paste0("Bratio_", final_year),
                #                                   "Value"], 3)),
                "",

                "Domestic Biomass Derived Management Quantities",
                paste0("Blimit, ",          model$SBzero*model$minbthresh),
                paste0("Blimit Basis, ",    MinBthresh_text),
                paste0("BMSY, ",            model$SBzero*model$btarg),
                paste0("BMSY Basis, ",      "SS", Btarg_text), # e.g. SSB40%
                paste0("B/Blimit, ",        Bcurrent/(model$SBzero*model$minbthresh)),
                paste0("B/Bmsy, ",          Bcurrent/(model$SBzero*model$btarg)),
                paste0("Stock Level (relative) to BMSY, ", "Above"),
                # paste0("age ", summary_age, "+ summary biomass, ",
                #        model$timeseries$Bio_smry[model$timeseries$Yr == final_year - 1]),
                "",
                "")

  if(writecsv){
    # write 2-column table with quantities of interest
    write.table(info_tab, file = file.path(dir, filename_values),
                quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)

    # write header rows for time series table
    write.table(header_info, file = file.path(dir, filename_timeseries),
                quote = FALSE, sep = ",", row.names = TRUE, col.names = FALSE,
                append = FALSE)
    # add time series values (suppressing warning about 'appending column names to file')
    suppressWarnings(
        write.table(tab, file = file.path(dir, filename_timeseries),
                    quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE,
                    append = TRUE)
    )
  }

  # return the list
  invisible(list(info = info_tab, timeseries = tab, header_info = header_info))
}
