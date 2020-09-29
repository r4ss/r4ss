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
#' @param final_year Year of assessment and reference points
#' (typically will be model$endyr + 1)
#' @param data_year Last year of of timeseries data 
#' @param sciencecenter Origin of assessment report
#' @param Mgt_Council Council jurisdiction. Currently the only option oustside of the default is Gulf of Mexico (`"GM"`)
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
                         stock = "StockName", final_year = 2019, data_year = NULL, sciencecenter="NWFSC", Mgt_Council="NA"){

  # directory to write file
  if(is.null(dir)){
    dir <- model$inputs$dir
  }

  
  if (is.null(data_year)) {
    data_year <- model$endyr
  }
  
  # construct filename
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
                       Total_Bio = round(ts$Bio_all,2),
                       SpawnBio = round(ts$SpawnBio,2),
                       FractionUnfished = round(ts$FractionUnfished, 3), 
                       Summary_Bio = round(ts$Bio_smry,2),
                       Recruitment = round(ts$Recruit_0,2))

  # subset for years of interest and replace potential bad 0 value with NA
  ts_tab <- ts_tab[ts_tab$Year %in% years,]
  # if(ts_tab$Total_Bio[ts_tab$Year == final_year] == 0){
  #  ts_tab$Total_Bio[ts_tab$Year == final_year] <- NA
  # }
  

  # calculate total dead catch (aggregated across fleets)
  dead_bio_columns <- grep("dead(B)", names(model$timeseries), fixed = TRUE)
  dead_N_columns <- grep("dead(N)", names(model$timeseries), fixed = TRUE)  
  if(length(dead_bio_columns) > 1){
    catch_tab <- data.frame(Year = model$timeseries$Yr,
                            Catch_bio = apply(model$timeseries[,dead_bio_columns],
                                              MARGIN = 1, FUN = sum),
                            Catch_n = apply(model$timeseries[,dead_N_columns],
                                            MARGIN = 1, FUN = sum)
    )
  }
  if(length(dead_bio_columns) == 1){
    catch_tab <- data.frame(Year = model$timeseries$Yr,
                            Catch_bio = model$timeseries[,dead_bio_columns],
                            Catch_n = model$timeseries[,dead_N_columns])
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
  catch_tab[,-1] <- round(catch_tab[,-1], 2)
  rownames(catch_tab) <- 1:nrow(catch_tab)

  # filter years and set forecast values to NA
  catch_tab <- catch_tab[catch_tab$Year %in% years,]
  # catch_tab[catch_tab$Year == final_year, -1] <- NA

  # calculate proxy-F values (e.g. exploitation rate)
  F_tab <- data.frame(Year = years,
                      Exploit_rate = round(model$derived_quants[paste0("F_", years), "Value"], 4))
  if(length(grep("Exploit(bio)", model$F_report_basis, fixed = TRUE)) == 0){
    warning("Problem with units of F, value is not exploitation rate")
  }
  # filter years and set forecast values to NA
  F_tab <- F_tab[F_tab$Year %in% years,]
  # F_tab[F_tab$Year == final_year, -1] <- NA

  # SPR-related quantities
  spr_tab <- model$sprseries[, c("Yr", "SPR_report","Tot_Exploit","SPR")]
  names(spr_tab)[1] <- "Year"
  if(model$SPRratioLabel!="1-SPR"){
    # add 1-SPR if it does not exist as SPR_report already
    spr_tab$"1-SPR" <- 1 - spr_tab$SPR
    spr_tab <- spr_tab[,c("Year","SPR_report","Tot_Exploit","1-SPR")]
  }else{
    spr_tab <- spr_tab[,c("Year","SPR_report","Tot_Exploit")]
  }
  # filter years and set forecast values to NA
  spr_tab <- spr_tab[spr_tab$Year %in% years,]
  # spr_tab[spr_tab$Year == final_year, -1] <- NA
  spr_tab[,-1] <- round(spr_tab[,-1], 3)

  # merge columns from time series, catch, F, and SPR together
  tab <- merge(merge(merge(ts_tab, catch_tab), F_tab), spr_tab)
  if(nrow(tab) != length(years) || any(tab$Year != years)){
    stop("problem with mismatch of years:\n",
         "range(years): ", range(years), "\n",
         "range(tab$Year): ", range(tab$Year), "\n")
  }

  # replace NA with 0 in exploitation rate for years with 0 catch
  tab$Exploit_rate[!is.na(tab$Catch_bio) & tab$Catch_bio == 0] <- 0
  # again for 8th column (SPR-ratio)
  tab$SPR_report[!is.na(tab$Catch_bio) & tab$Catch_bio == 0] <- 0

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
  
  # numbers vs biomass switch should work for all models 
  # ran using SS after 8/28/20
  if(model$SpawnOutputUnits == "numbers"){ 
    header_info["Description","SpawnBio"] <- "Spawning Output(Eggs)" 
    header_info["Unit",       "SpawnBio"] <- "XXXX"
  }else{
    header_info["Description","SpawnBio"] <- " Female Mature Spawning Biomass"
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
  
  # info on Catch biomass
  header_info["Category",   "Catch_bio"] <- "Catch"
  header_info["Primary",    "Catch_bio"] <- "Y"
  header_info["Description","Catch_bio"] <- "Modeled Total Catch"
  header_info["Unit",       "Catch_bio"] <- "Metric Tons"
  
  # info on Catch numbers
  header_info["Category",   "Catch_n"] <- "Catch"
  header_info["Primary",    "Catch_n"] <- "N"
  header_info["Description","Catch_n"] <- "Modeled Total Catch"
  header_info["Unit",       "Catch_n"] <- "Number x 1000"
  
  # info on Exploitation rate
  header_info["Category",   "Exploit_rate"] <- "Fmort"
  header_info["Primary",    "Exploit_rate"] <- "XXX"
  header_info["Description","Exploit_rate"] <- strsplit(model$F_report_basis, ";")[[1]][1]
  header_info["Unit",       "Exploit_rate"] <- "Rate"
  
  # info on total Exploitation rate
  header_info["Category",   "Tot_Exploit"] <- "Fmort"
  header_info["Primary",    "Tot_Exploit"] <- "XXX"
  header_info["Description","Tot_Exploit"] <-
    paste0("Relative Exploitation Rate (Catch/", summary_bio_label, ")")
  header_info["Unit",       "Tot_Exploit"] <- "Rate"
  
  # info on SPR_report
  header_info["Category",   "SPR_report"] <- "Fmort"
  header_info["Primary",    "SPR_report"] <- "XXX"
  header_info["Description","SPR_report"] <- model$SPRratioLabel
  header_info["Unit",       "SPR_report"] <- "Rate"
  
  # Only needs to be added if SPR_report is not 1-spr
  # info on 1 - SPR
  if(model$SPRratioLabel!="1-SPR"){ 
    header_info["Category",   "1-SPR"] <- "Fmort"
    header_info["Primary",    "1-SPR"] <- "XXX"
    header_info["Description","1-SPR"] <- "1 - SPR"
    header_info["Unit",       "1-SPR"] <- "Rate"}
  
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

  F_limit = 1-model$sprtarg
  F_limit_basis = paste0("1-SPR_", SPRtarg_text)
  F_msy = F_limit
  F_msy_basis = F_limit_basis
  
  # GM Settings: Using F_SPR target
  if(Mgt_Council=="GM"){
    
    #Annual FSPR names changed between versions
    if("annF_SPR" %in% rownames(model$derived_quants)){
      F_limit <- round(model$derived_quants["annF_SPR", "Value"],digits=3)
      }else{
        F_limit <- round(model$derived_quants["Fstd_SPRtgt", "Value"],digits=3)
        }
    F_msy = F_limit
    F_limit_basis = paste0("F",100*model$btarg, "%")
    F_msy_basis = paste0("F",100*model$btarg, "% as Proxy")
    }
  

  if(model$btarg == -999){
    Btarg_text <- "BXX%"
    model$btarg <- NA
    warning('No value for model$btarg')
  }else{
    Btarg_text <- paste0("B", 100*model$btarg, "%") # e.g. B40%
	# GM Settings:
	if(Mgt_Council=="GM"){
      Btarg_text <- paste0("SPR", 100*model$btarg, "%")
    }
  }

  if(model$minbthresh == -999){
    MinBthresh_text <- "BXX%"
    model$minbthresh <- NA
    warning('No value for model$minbthresh')
  }else{
    MinBthresh_text <- paste0("B", 100*model$minbthresh, "%") # e.g. B25%
	B_msy_basis = paste0("SS", Btarg_text)
    B_msy = round(model$SBzero*model$btarg)
    B_limit = round(model$SBzero*model$minbthresh)
  }

  # GM Settings:
  if(Mgt_Council=="GM"){
    MinBthresh_text <- paste0("(1-M)*SPR", 100*model$btarg, "%") # e.g. B25%
    B_msy_basis = Btarg_text
    eq_year = data_year+model$N_forecast_yrs
    B_msy = round(model$derived_quants$Value[which(model$derived_quants$Label==paste0("SSB_",eq_year))])
    B_limit = round((0.5*B_msy))
    }

  B_year = final_year
  ts_tab_short <- data.frame(Year = ts$Yr,
                             SpawnBio = round(ts$SpawnBio,2))
  Bcurrent <- round(ts_tab_short$SpawnBio[ts_tab_short$Year == final_year])
  
  # GM Settings:
  if(Mgt_Council == "GM"){
    B_year = data_year
    Bcurrent <- round(tab$SpawnBio[tab$Year == data_year])
  }

  # MSY-proxy labels were changed at some point
  if("Dead_Catch_SPR" %in% rownames(model$derived_quants)){
    Yield_at_SPR_target <- round(model$derived_quants["Dead_Catch_SPR", "Value"])
    Yield_at_B_target <- round(model$derived_quants["Dead_Catch_Btgt", "Value"])
  }else{
    Yield_at_SPR_target <- round(model$derived_quants["TotYield_SPRtgt", "Value"])
    Yield_at_B_target <- round(model$derived_quants["TotYield_Btgt", "Value"])
  }
  
  Best_F_Est = tab$"1-SPR"[tab$Year == data_year]
  F_basis = "Equilibrium SPR"
  F_unit = "1-SPR"
  
  # GM Settings: Best F estimate = geometric mean of last three data years
  if(Mgt_Council=="GM"){
    Best_F_Est = round(mean(tab$"Exploit_rate"[tab$Year %in% c((data_year-2):data_year)]),2)
    F_basis = paste0("Total_Catch/Total_Biomass_Geometric_Mean_",(data_year-2),"-",data_year)
    F_unit = "Exploitation Rate"
    }

  # SS version
  SS_version <- strsplit(model$SS_version, split = ";")[[1]][1]
  gsub("-safe", "", SS_version)
  gsub("-opt", "", SS_version)
  
  stock_level_to_MSY = Bcurrent/B_msy
  if(stock_level_to_MSY>1){stock_level_to_MSY = "ABOVE"
  }else if (stock_level_to_MSY<0.90){ stock_level_to_MSY = "BELOW"
  }else if(stock_level_to_MSY>0.90 & stock_level_to_MSY<1){stock_level_to_MSY = "NEAR"
  }else{stock_level_to_MSY = "UNKNOWN"}




  # make big 2-column table of info about each model
  info_tab <- c(paste0("SAIP:,", "https://spo.nmfs.noaa.gov/sites/default/files/TMSPO183.pdf"),
                "",
                paste0("Stock / Entity Name,", stock),
                paste0("Assessment Type,", "X"),
                paste0("Ensemble/Multimodel Approach,", "NA"),
                paste0("Asmt Year and Month,", final_year, ".XX"),
                paste0("Last Data Year,", data_year),
                paste0("Asmt Model Category,", "6 - Statistical Catch-at-Age (SS, ASAP, AMAK, BAM, MultifancL< CASAL)"),
                paste0("Asmt Model Category,", "SS"),
                paste0("Model Version,", SS_version),
                paste0("Lead Lab,", sciencecenter),
                paste0("Point of Contact (assessment lead),", "XXXX"),
                paste0("Review Result,", "XXXX"),
                paste0("Catch Input Data,", "XXXX"),
                paste0("Abundance Input Data,", "XXXX"),
                paste0("Biological Input Data,", "XXXX"),
                paste0("Size/Age Composition Input Data,", "XXXX"),
                paste0("Ecosystem Linkage,", "XXXX"),
                "",

                "Fishing Mortality Estimates",
                paste0("F Year, ",          data_year),
                paste0("Min F Estimate, ",  ""),
                paste0("Max F Estimate, ",  ""),
                paste0("Best F Estimate, ", Best_F_Est),
                paste0("F Basis, ",         F_basis),
                paste0("F Unit, ",          F_unit),
                "",

                "Domestic Fishing Mortality Derive Management Quantities",
                paste0("Flimit, ",          F_limit),
                paste0("Flimit Basis, ",    F_limit_basis),
                paste0("FMSY, ",            F_msy),
                paste0("FMSY Basis, ",      F_msy_basis),
                paste0("F target,",         ""),
                paste0("F target Basis,",   ""),
                #paste0("MSY, ",             Yield_at_SPR_target), # These lines have moved to the next section
                #paste0("MSY Unit,",         "mt"),
                #paste0("MSY Basis, ",     "Yield with ", SPRtarg_text,
                #      " at SB_", SPRtarg_text),
                "",

                "Biomass Estimates",
                paste0("[Unfished Spawning Biomass], ", model$SBzero), # no longer a requested value
                paste0("B Year, ",          B_year),
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
                paste0("Blimit, ",          B_limit),
                paste0("Blimit Basis, ",    MinBthresh_text),
                paste0("BMSY, ",            B_msy),
                paste0("BMSY Basis, ",      B_msy_basis), # e.g. SSB40%
                paste0("B/Blimit, ",        round((Bcurrent/B_limit),2)),
                paste0("B/Bmsy, ",          round((Bcurrent/B_msy), 3)),
                paste0("Stock Level (relative) to BMSY, ", stock_level_to_MSY),
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
