get_SIS_info <- function(model){

  # years to report for catch-related quantities
  startyr <- model$startyr
  endyr <- model$endyr
  years <- startyr:(endyr + 1)
  
  # get values from TIME_SERIES table
  ts <- model$timeseries
  tab <- data.frame(Year = ts$Yr,
                    Total_Bio = round(ts$Bio_all),
                    Spawning_Bio = round(ts$SpawnBio),
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
  # add NA value for endyr + 1 if not present (this was the case in SS version 3.30.12)
  if(endyr+1 %in% catch_tab$Year){
    catch_tab <- rbind(catch_tab, data.frame(Year = endyr + 1, Catch = NA))
  }

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
  spr_tab <- model$sprseries[, c("Yr", "SPR_report", "SPR")]
  names(spr_tab) <- c("Year", model$SPRratioLabel, "SPR")
  # add 1-SPR
  spr_tab$"1-SPR" <- 1 - spr_tab$SPR
  # re-order columns
  spr_tab <- spr_tab[,c(1,2,4,3)]
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
  SPRtarg_text <- paste0("SPR", 100*model$sprtarg, "%") # e.g. SPR50%
  Btarg_text <- paste0("B", 100*model$btarg, "%") # e.g. B40%
  MinBthresh_text <- paste0("B", 100*model$minbthresh, "%") # e.g. B25%
  Bcurrent <- tab$Spawning_Bio[tab$Year == endyr + 1]
  
  info_tab <- c("Fishing Mortality Estimates",
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
                paste0("MSY, ",             round(model$derived_quants["Dead_Catch_SPR",
                                                                       "Value"])),
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
                paste0("MSY, ",             round(model$derived_quants["Dead_Catch_Btgt",
                                                                       "Value"])),
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
                paste0("age X+summary biomass, ",
                       model$timeseries$Bio_smry[model$timeseries$Yr == endyr]),
                "",
                "")

  write.table(info_tab, file='c:/github/r4ss/ignored/test.csv',
              quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)

  write.table(tab, file='c:/github/r4ss/ignored/test.csv',
              quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE,
              append = TRUE)

  return(list(info_tab, tab))
}
