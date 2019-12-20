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
#' @param species String to prepend id info to filename for CSV file
#' @param endyr Year for reference points and timeseries
#' @author Ian G. Taylor, Andi Stephens
#' @export
#' @seealso \code{\link{SS_output}}

get_SIS_info <- function(model, dir = NULL, writecsv = TRUE,
                         species = "NoName", endyr = NULL){

  # directory to write file
  if(is.null(dir)){
    dir <- model$inputs$dir
  }

  # construct filename
  if (is.null(endyr)) {
    endyr <- model$endyr
  }
  filename <- paste(species, endyr, "SIS_info.csv", sep="_")

  message("writing SIS info to CSV file:\n",
          file.path(dir, filename ))

  # years to report for catch-related quantities
  startyr <- model$startyr
  years <- startyr:(endyr + 1)

  # get values from TIME_SERIES table
  ts <- model$timeseries

  # aggregate across areas if needed
  if(model$nareas > 1){
    ts.tmp <- ts # copy of table
    ts <- ts.tmp[ts.tmp$Area == 1,]
    for(iarea in 2:model$nareas){
      ts[,-(1:4)] <- ts[,-(1:4)] + ts.tmp[ts.tmp$Area == iarea,-(1:4)]
    }
  }
  tab <- data.frame(Year = ts$Yr,
                    Total_Bio = round(ts$Bio_all),
                    Spawning_Bio = round(ts$SpawnBio),
                    Summary_Bio_Age_Xplus = round(ts$Bio_smry),
                    Rel_Spawning_Bio = round(ts$SpawnBio / ts$SpawnBio[ts$Era == "VIRG"], 3),
                    Recruitment = round(ts$Recruit_0))
  ts_tab <- tab[tab$Year %in% years,]
  if(tab$Total_Bio[tab$Year == endyr + 1] == 0){
    tab$Total_Bio[tab$Year == endyr + 1] <- NA
  }

  # calculate total catch (aggregated across fleets)
  catch_tab <- aggregate(model$catch$kill_bio, by = list(model$catch$Yr),
                         FUN = function(x){round(sum(x))})
  names(catch_tab) <- c("Year","Catch")
  # filter years and set forecast values to NA
  catch_tab <- catch_tab[catch_tab$Year %in% years,]
  catch_tab[catch_tab$Year == endyr+1, -1] <- NA

  if (endyr <= model$endyr) {
    # add NA value for endyr + 1 if not present (this was the case in SS version 3.30.12)
    if(endyr+1 %in% catch_tab$Year){
      catch_tab <- rbind(catch_tab, data.frame(Year = endyr + 1, Catch = NA))
    }
  } else {
    forecatch = data.frame(model$derived_quants[grepl("ForeCatch",model$derived_quants$Label),1:2])
    names(forecatch) = c("Year","Catch")
    forecatch$Year = gsub("ForeCatch_", "", forecatch$Year)
    forecatch = forecatch[forecatch$Year < (endyr+2),]
    forecatch$Catch[nrow(forecatch)] = NA
    catch_tab = rbind(catch_tab, forecatch)
  } # end if-else

  # calculate proxy-F values (e.g. exploitation rate)
  F_tab <- data.frame(Year = years,
                      Exploit_rate = round(model$derived_quants[paste0("F_", years), "Value"], 2))
  if(length(grep("Exploit(bio)", model$F_report_basis, fixed = TRUE)) == 0){
    warning("Problem with units of F, value is not exploitation rate")
  }
  # filter years and set forecast values to NA
  F_tab <- F_tab[F_tab$Year %in% years,]
  F_tab[F_tab$Year == endyr+1, -1] <- NA

  # SPR-related quantities
  spr_tab <- model$sprseries[, c("Yr", "SPR")]
  # add 1-SPR (may have already existed as SPR_report
  spr_tab$"1-SPR" <- 1 - spr_tab$SPR
  names(spr_tab)[1] <- "Year"
  spr_tab <- spr_tab[,c("Year","1-SPR","SPR")]
  # filter years and set forecast values to NA
  spr_tab <- spr_tab[spr_tab$Year %in% years,]
  spr_tab[spr_tab$Year == endyr+1, -1] <- NA
  spr_tab[,-1] <- round(spr_tab[,-1], 3)

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

  # add extra column to help with format of CSV file
  tab <- cbind("", tab)

  # quantities of interest
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
  
  Bcurrent <- tab$Spawning_Bio[tab$Year == endyr + 1]

  # MSY-proxy labels were changed at some point
  if("Dead_Catch_SPR" %in% rownames(model$derived_quants)){
    Yield_at_SPR_target <- round(model$derived_quants["Dead_Catch_SPR", "Value"])
    Yield_at_B_target <- round(model$derived_quants["Dead_Catch_Btgt", "Value"])
  }else{
    Yield_at_SPR_target <- round(model$derived_quants["TotYield_SPRtgt", "Value"])
    Yield_at_B_target <- round(model$derived_quants["TotYield_Btgt", "Value"])
  }

  # age for summary biomass
  summary_age <- model$summary_age
  if(is.null(summary_age)){
    summary_age <- "X"
  }

  # SS version
  SS_version <- strsplit(model$SS_version, split = ";")[[1]][1]
  gsub("-safe", "", SS_version)
  gsub("-opt", "", SS_version)
  
  # make big 2-column table of info about each model
  info_tab <- c(paste0("SAIP:,", "https://spo.nmfs.noaa.gov/sites/default/files/TMSPO183.pdf"),
                "",
                paste0("Stock / Entity Name,", species),
                paste0("Assessment Type,", "X"),
                paste0("Ensemble/Multimodel Approach,", "NA"),
                paste0("Asmt Year and Month,", "XXXX.XX"),
                paste0("Last Data Year,", endyr),
                paste0("Asmt Model Category,", "6 - Statistical Catch-at-Age (SS, ASAP, AMAK, BAM, MultifancL< CASAL)"),
                paste0("Asmt Model Category,", "SS"),
                paste0("Model Version,", SS_version),
                paste0("Lead Lab,", "NWFSC"),
                paste0("Point of Contact (assessment lead),", "XXXX"),
                paste0("Review Result,", "Accepted"),
                paste0("Catch Input Data,", "XXXX"),
                paste0("Abundance Input Data,", "XXXX"),
                paste0("Biological Input Data,", "XXXX"),
                paste0("Size/Age Composition Input Data,", "XXXX"),
                paste0("Ecosystem Linkage,", "XXXX"),
                "",

                "Fishing Mortality Estimates",
                paste0("F Year, ",          endyr),
                paste0("Min F Estimate, ",  "not required and typically not entered"),
                paste0("Max F Estimate, ",  "not required and typically not entered"),
                paste0("Best F Estimate, ", tab$"1-SPR"[tab$Year == endyr]),
                paste0("F Basis, ",         "Equilibrium SPR"),
                paste0("F Unit, ",          "1-SPR"),
                "",

                "Domestic Fishing Mortality Derive Management Quantities",
                paste0("Flimit, ",          1 - model$sprtarg),
                paste0("Flimit Basis, ",    "1-SPR_", SPRtarg_text),
                paste0("FMSY, ",            1 - model$sprtarg),
                paste0("FMSY Basis, ",      "1-SPR_",SPRtarg_text),
                paste0("F target,",         "not required and typically not entered"),
                paste0("F target Basis,",   "not required and typically not entered"),
                paste0("MSY, ",             Yield_at_SPR_target),
                paste0("MSY Unit,",         "mt"),
                paste0("[MSY Basis], ",     "Yield with ", SPRtarg_text,
                       " at SB_", SPRtarg_text),
                "",

                "Biomass Estimates",
                paste0("[Unfished Spawning Biomass], ", model$SBzero),
                paste0("B Year, ",          endyr + 1),
                paste0("Min B. Estimate, ", NA),
                paste0("Max. B Estimate, ", NA),
                paste0("Best B Estimate, ", Bcurrent),
                paste0("B Basis, ",         "Spawning Biomass / Female mature biomass"),
                paste0("B unit, ",          "mt"),
                paste0("MSY, ",             Yield_at_B_target),
                paste0("MSY Unit, ",        "mt"),
                paste0("[MSY Basis], ",     "Yield with SPR_",
                       Btarg_text," at ",   Btarg_text, "(mt)"),
                paste0("[Depletion], ",
                       round(model$derived_quants[paste0("Bratio_", endyr+1),
                                                  "Value"], 3)),
                "",

                "Domestic Biomass Derived Management Quantities",
                paste0("Blimit, ",          model$SBzero*model$minbthresh),
                paste0("Blimit Basis, ",    MinBthresh_text),
                paste0("BMSY, ",            model$SBzero*model$btarg),
                paste0("BMSY Basis, ",      "SS", Btarg_text), # e.g. SSB40%
                paste0("B/Blimit, ",        Bcurrent/(model$SBzero*model$minbthresh)),
                paste0("B/Bmsy, ",          Bcurrent/(model$SBzero*model$btarg)),
                paste0("Stock Level (relative) to BMSY, ", "Above"),
                paste0("age ", summary_age, "+ summary biomass, ",
                       model$timeseries$Bio_smry[model$timeseries$Yr == endyr]),
                "",
                "")

  # write 2-column table at the top of the file
  write.table(info_tab, file = file.path(dir, filename),
              quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  # add time series table with more columns (suppressing warning about
  # 'appending column names to file')
  suppressWarnings(
      write.table(tab, file = file.path(dir, filename),
                  quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE,
                  append = TRUE)
  )

  invisible(list(info_tab, tab))
}
