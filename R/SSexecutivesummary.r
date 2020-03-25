#' A function to create a executive summary tables from an SS Report.sso file
#'
#' Takes the output from SS_output and creates executive summary tables
#' as required by the current Terms of Reference for US West Coast
#' groundfish stock. Additionally, historical catches, time-series and numbers-at-ages tables are created.
#'
#' @param replist Object name that holds output from the SS_output function.
#' @param plotfolder Directory where the 'tables' directory will be created.
#' The default is the dir location where the Report.sso file is located.
#' @param ci_value To calculate confidence intervals, default is set at 0.95
#' @param es_only TRUE/FALSE switch to produce only the executive summary tables
#' will be produced, default is FALSE which will return all executive summary
#' tables, historical catches, and numbers-at-ages
#' @param tables Which tables to produce (default is everything). Note: some
#' tables depend on calculations related to previous tables, so will fail
#' if requested on their own (e.g. Table 'f' can't be created
#' without also creating Table 'a')
#' @param divide_by_2 This will allow the user to calculate single sex values
#' based on the new sex specification (-1) in SS for single sex models. Default value is FALSE.
#' TRUE will divide by 2.
#' @param endyr Optional input to choose a different ending year for tables
#' (could be useful for catch-only updates)
#' @param adopted_ofl Vector of adopted ofl values to be printed in the mangagement performance
#' table. This should be a vector of 10 values. 
#' @param adopted_abc Vector of adopted abc values to be printed in the mangagement performance
#' table. This should be a vector of 10 values. 
#' @param adopted_acl Vector of adopted acl values to be printed in the mangagement performance
#' table. This should be a vector of 10 values. 
#' @param forecast_ofl Optional input vector for management adopted OFL values for table g. These values
#' will be overwrite the OFL values in the projection table, rather than the model estimated
#' OFL values. Example input: c(1500, 1300)
#' @param forecast_abc Optional input vector for management adopted ABC values for table g. These values
#' will be overwrite the ABC values in the projection table, rather than the model estimated
#' ABC values. Example input: c(1500, 1300)
#' @param verbose Return updates of function progress to the R console?
#' @return Individual csv files for each executive summary table and additional tables (catch, timeseries, numbers-at-age).
#' @author Chantel Wetzel
#' @export
#'
SSexecutivesummary <- function (replist, 
                                plotfolder = 'default', 
                                ci_value = 0.95,
                                es_only = FALSE, 
                                tables = c('a','b','c','d','e','f','g','h','i','catch', 'timeseries', 'numbers'),
                                divide_by_2 = FALSE,
                                endyr = NULL,
                                adopted_ofl = NULL,
                                adopted_abc = NULL,
                                adopted_acl = NULL,
                                forecast_ofl = NULL,
                                forecast_abc = NULL,
                                verbose = TRUE) 
{

  # Make sure table.dir contains the report file
  if(is.null(replist)){
    stop("The input 'replist' should refer to an R object created by the function 'SS_output'.")
  }

  if (plotfolder == 'default') { csv.dir = paste0(replist$inputs$dir,"/tables/") }
  if (plotfolder != 'default') { csv.dir = paste0(plotfolder,"/tables/")}

  dir.create(csv.dir, showWarnings = FALSE)
  if(verbose){
    message("CSV files will be written in:\n", csv.dir)
  }
  
  #=============================================================================
  # Function Sections
  #=============================================================================
  print.numeric  <- function(x, digits) {
    formatC(x, digits = digits, format = "f")
  }

  comma          <- function(x, digits=0) {
    formatC(x, big.mark=",", digits, format = "f")
  }

  # Function to pull values from the read in report file and calculate the confidence intervals
  Get.Values <- function(replist, label, yrs, ci_value, single = FALSE){

    dat = replist$derived_quants
    if (label == "Main_RecrDev" || label == "Late_RecrDev" || label == "ForeRecr") {
      dat = replist$parameters
    }

    if(!single){
      value = dat[grep(label,dat$Label),]
      value = value[value$Label >= paste0(label, '_', yrs[1]) &
                    value$Label <= paste0(label, '_', max(yrs)),]
      dq = value$Value
      ind = names(value) %in% c("StdDev", "Parm_StDev")
      sd = value[,ind]
    }

    if(single){
      value = dat[grep(label, dat$Label)[1],]
      dq = value$Value
      sd = value$StdDev
    }

    if(label == " Recr" || label == "Recr_virgin"){
      low = exp(log(dq) - qnorm(1-(1-ci_value)/2) * sqrt(log(1 + (sd/dq) * (sd/dq))))
      high= exp(log(dq) + qnorm(1-(1-ci_value)/2) * sqrt(log(1 + (sd/dq) * (sd/dq))))
    }
    if(label != " Recr" && label != "Recr_virgin"){
      low = dq - qnorm(1-(1-ci_value)/2)*sd
      high= dq + qnorm(1-(1-ci_value)/2)*sd
    }

    if (!single) { return(data.frame(yrs, dq, low, high)) }
    if ( single) { return(data.frame(dq, low, high)) }
  }


  #============================================================================
  # Determine the model version and dimensions of the model
  #============================================================================

  # Need to check how r4ss determines the colname based on SS verion
  sb.name = "SSB"

  nfleets <- replist$nfleets
  startyr <- replist$startyr 
  foreyr  <- replist$nforecastyears 
  if(is.null(endyr)) { 
    endyr   <- replist$endyr 
  } else {
    foreyr <- foreyr - (endyr - replist$endyr)
  }
  hist    <- (endyr - 11):(endyr + 1)
  fore    <- (endyr + 1):(endyr + foreyr)
  all     <- startyr:max(fore)
  nareas  <- replist$nareas

  #======================================================================
  # Determine the fleet name and number for fisheries with catch
  #======================================================================
  names <- replist$FleetNames 
  fleet.num <- replist$fleet_ID 

  #======================================================================
  # Find summary age
  #======================================================================
  smry.age  <-  replist$summary_age

  #======================================================================
  # Two-sex or single-sex model
  #======================================================================
  if (replist$nsexes == 1 & !(divide_by_2)) {
    if(verbose){
      message("Single sex model - spawning biomass NOT beind divided by a factor of 2.") }
  }
  nsexes <- replist$nsexes
  sexfactor <- 1
  if (divide_by_2) { sexfactor <- 2}

  #======================================================================
  # Determine the number of growth patterns
  #======================================================================
  nmorphs <- replist$ngpatterns 

  #======================================================================
  # Spawning Biomass or Spawning Output?
  #======================================================================  
  sb.label = if(replist$SpawnOutputUnits == 'numbers'){
    "Spawning Output"
  } else{
    "Spawning Biomass (mt)"
  }

  #======================================================================
  #ES Table a  Catches from the fisheries
  #======================================================================
  if('a' %in% tables){
    if(verbose){
      message("Creating Table a: Recent catches by fleet")
    }

    catch = fleet.names = NULL
    total.catch = total.dead = 0

    for (i in 1:nfleets){
      name = paste0("retain(B):_",i)
      input.catch = replist$timeseries[replist$timeseries$Yr %in% hist[1:(length(hist)-1)], name]
      catch = cbind(catch, input.catch)

      name = paste0("dead(B):_",i)
      dead = replist$timeseries[replist$timeseries$Yr %in% hist[1:(length(hist)-1)], name]
      if (!is.null(dead)){ 
        total.dead = total.dead + dead
        fleet.names = c(fleet.names, replist$FleetNames[i]) }
    } 
    total.catch = apply(catch, 1, sum)     


    if(sum(total.catch) != sum(total.dead)){
      es.a = data.frame(hist[1:(length(hist)-1)], comma(catch, digits = 2), comma(total.catch, digits = 2), comma(total.dead, digits = 2))
      colnames(es.a) = c("Years", fleet.names, "Total Catch", "Total Dead")
      write.csv(es.a, paste0(csv.dir, "/a_Catches_ES.csv"), row.names = FALSE)
    } else {
      es.a = data.frame(hist[1:(length(hist)-1)], comma(catch, digits = 2), comma(total.catch, digits = 2))
      colnames(es.a) = c("Years", fleet.names, "Total Catch")
      write.csv(es.a, paste0(csv.dir, "/a_Catches_ES.csv"), row.names = FALSE)      
    }

  } # end check for 'a' %in% tables

  #======================================================================
  #ES Table b Spawning Biomass and Depletion
  #======================================================================
  if('b' %in% tables){
    if(verbose){
      message("Creating Table b: Recent spawning biomass/output and depletion")
    }
    
    ssb =  Get.Values(replist = replist, label = sb.name, hist, ci_value )
    if (nsexes == 1) { ssb$dq = ssb$dq / sexfactor ; ssb$low = ssb$low / sexfactor ; ssb$high = ssb$high / sexfactor }
    depl = Get.Values(replist = replist, label = "Bratio" , hist, ci_value )
    for (i in 1:length(hist)){ dig = ifelse(ssb[i,2] < 100, 1, 0)}
    es.b =  data.frame(hist,
                       comma(ssb$dq,digits = dig), paste0(comma(ssb$low,digits = dig), "\u2013", comma(ssb$high,digits = dig)),
                       print(100*depl$dq, digits = 1), paste0(print(100*depl$low,digits = 1), "\u2013", print(100*depl$high,digits = 1)))
    colnames(es.b) = c("Years", sb.label, "95% Asymptotic Interval", "Estimated Depletion (%)", "95% Asymptotic Interval")

    write.csv(es.b, file.path(csv.dir, "b_SSB_ES.csv"), row.names = FALSE)

  } # end check for 'b' %in% tables

  #======================================================================
  #ES Table c Recruitment
  #======================================================================
  if('c' %in% tables){
    if(verbose){
      message("Creating Table c: Recent recruitment and deviations")
    }

    recdevMain   <- replist$parameters[substring(replist$parameters$Label,1,12)=="Main_RecrDev", 1:3]
    recdevLate   <- replist$parameters[substring(replist$parameters$Label,1,12)=="Late_RecrDev", 1:3]
    temp         <- toupper(substr(recdevLate$Label,14,17))
    late.yrs     <- as.numeric(temp)

    recdevFore   <- replist$parameters[substring(replist$parameters$Label,1, 8)=="ForeRecr", 1:3]
    temp         <- toupper(substr(recdevFore$Label,10,13))
    fore.yrs     <- as.numeric(temp)
    ind          <- fore.yrs <= max(hist)
    fore.yrs     <- fore.yrs[ind]

    end          <- ifelse(length(late.yrs) == 0, fore.yrs - 1, late.yrs - 1)

    recruits     <- Get.Values(replist = replist, label = "Recr" , hist, ci_value )

    if (dim(recdevMain)[1] != 0){
      recdevs      = Get.Values(replist = replist, label = "Main_RecrDev", yrs = hist[1]:end, ci_value )
      devs = cbind(recdevs$dq, recdevs$low, recdevs$high)

      if (length(late.yrs) > 0 ){
        late.recdevs = Get.Values(replist = replist, label = "Late_RecrDev", yrs = late.yrs, ci_value )
        devs = cbind(c(recdevs$dq, late.recdevs$dq), c(recdevs$low, late.recdevs$low), c(recdevs$high, late.recdevs$high))
      }

      if(length(fore.yrs) > 0){
        fore.recdevs = Get.Values(replist = replist, label = "ForeRecr", yrs = fore.yrs, ci_value )
        if (length(late.yrs) > 0){
          devs = cbind(c(recdevs$dq, late.recdevs$dq, fore.recdevs$dq),
              c(recdevs$low, late.recdevs$low, fore.recdevs$low),
              c(recdevs$high, late.recdevs$high, fore.recdevs$high))
        }

        if (length(late.yrs) == 0){
          devs = cbind(c(recdevs$dq,    fore.recdevs$dq),
              c(recdevs$low,   fore.recdevs$low),
              c(recdevs$high,  fore.recdevs$high))
        }

      }
      # Zero out the sd for years where devs were not estimated
      devs.out = data.frame(print(devs[,1], digits = 3), paste0(print(devs[,2],digits = 3), "\u2013", print(devs[,3], digits = 3)))
    }

    if (dim(recdevMain)[1] == 0) { devs.out = data.frame(rep(0, length(hist)), rep(0, length(hist))) }
    for (i in 1:length(hist)){ dig = ifelse(recruits[i,2] < 100, 1, 0)}

    es.c = data.frame(hist,
                      comma(recruits$dq, dig), paste0(comma(recruits$low, dig), "\u2013", comma(recruits$high, dig)),
                      devs.out )

    colnames(es.c) = c("Years", "Recruitment", "95% Asymptotic Interval", "Recruitment Deviations", "95% Asymptotic Interval")

    write.csv(es.c, file.path(csv.dir, "c_Recr_ES.csv"), row.names = FALSE)

  } # end check for 'c' %in% tables

  #======================================================================
  #ES Table d 1-SPR (%)
  #======================================================================
  if('d' %in% tables){
    if(verbose){
      message("Creating Table d: Recent exploitation ")
    }

    spr_type = replist$SPRratioLabel 
    f_type   = ifelse(replist$F_report_basis == "_abs_F;_with_F=Exploit(bio)", "Exploitation Rate",
                      "Fill in F method")


    adj.spr = Get.Values(replist = replist, label = "SPRratio" , hist[1:(length(hist)-1)], ci_value)
    f.value = Get.Values(replist = replist, label = "F" , hist[1:(length(hist)-1)], ci_value)
    es.d = data.frame(hist[1:(length(hist)-1)],
           print(adj.spr$dq*100,2), paste0(print(adj.spr$low*100,2), "\u2013", print(adj.spr$high*100,2)),
           print(f.value$dq,4),     paste0(print(f.value$low, 4),     "\u2013", print(f.value$high, 4)))
    colnames(es.d) = c("Years", paste0("Estimated ", spr_type, " (%)"), "95% Asymptotic Interval", 
                        f_type, "95% Asymptotic Interval")

    write.csv(es.d, file.path(csv.dir, "d_SPR_ES.csv"), row.names = FALSE)

  } # end check for 'd' %in% tables
  
  #======================================================================
  #ES Table e Reference Point Table
  #======================================================================
  if('e' %in% tables){
    if(verbose){
      message("Creating Table e: Reference points ")
    }

    spr   <- 100*replist$sprtarg
    btarg <- 100*replist$btarg 

    sb.unfished   = "SSB_unfished"
    smry.unfished = "SmryBio_unfished"
    recr.unfished = "Recr_unfished"
    totyield.btgt = "Dead_Catch_Btgt"
    totyield.spr  = "Dead_Catch_SPR"
    totyield.msy  = "Dead_Catch_MSY"

    if (toupper(substr(replist$SS_version, 10, 11)) < 13){
      sb.unfished   = "SSB_Unfished"
      smry.unfished = "SmryBio_Unfished"
      recr.unfished = "Recr_Unfished"
      totyield.btgt    = "TotYield_Btgt"
      totyield.spr     = "TotYield_SPRtgt"
      totyield.msy     = "TotYield_MSY"
    } 

    final.depl = 100*depl[dim(depl)[1],2:4]
    ssb.virgin = Get.Values(replist = replist, label = sb.unfished,   hist, ci_value, single = TRUE)
    smry.virgin= Get.Values(replist = replist, label = smry.unfished, hist, ci_value, single = TRUE)
    rec.virgin = Get.Values(replist = replist, label = recr.unfished, hist, ci_value, single = TRUE)
    b.target   = Get.Values(replist = replist, label = "SSB_Btgt",    hist, ci_value, single = TRUE)
    spr.btarg  = Get.Values(replist = replist, label = "SPR_Btgt",    hist, ci_value, single = TRUE)
    f.btarg    = Get.Values(replist = replist, label = "Fstd_Btgt",   hist, ci_value, single = TRUE)
    yield.btarg= Get.Values(replist = replist, label = totyield.btgt, hist, ci_value, single = TRUE)
    b.spr      = Get.Values(replist = replist, label = "SSB_SPR",     hist, ci_value, single = TRUE)
    f.spr      = Get.Values(replist = replist, label = "Fstd_SPR",    hist, ci_value, single = TRUE)
    yield.spr  = Get.Values(replist = replist, label = totyield.spr,  hist, ci_value, single = TRUE)
    b.msy      = Get.Values(replist = replist, label = "SSB_MSY",     hist, ci_value, single = TRUE)
    spr.msy    = Get.Values(replist = replist, label = "SPR_MSY",     hist, ci_value, single = TRUE)
    f.msy      = Get.Values(replist = replist, label = "Fstd_MSY",    hist, ci_value, single = TRUE)
    msy        = Get.Values(replist = replist, label = totyield.msy,  hist, ci_value, single = TRUE)

    # Convert spawning ci_valueities for single-sex models
    if (nsexes == 1){
      ssb.virgin = ssb.virgin / sexfactor
      b.target = b.target / sexfactor
      b.spr = b.spr / sexfactor
      b.msy = b.msy / sexfactor
    }

    es.e =  matrix(c(
        comma(ssb.virgin$dq,       dig),  paste0(comma(ssb.virgin$low,      dig), "\u2013", comma(ssb.virgin$high,      dig)),
        comma(smry.virgin$dq,      dig),  paste0(comma(smry.virgin$low,     dig), "\u2013", comma(smry.virgin$high,     dig)),
        comma(rec.virgin$dq,       dig),  paste0(comma(rec.virgin$low,      dig), "\u2013", comma(rec.virgin$high,      dig)),
        comma(ssb$dq[dim(ssb)[1]], dig),  paste0(comma(ssb$low[dim(ssb)[1]],dig), "\u2013", comma(ssb$high[dim(ssb)[1]],dig)),
        print(final.depl$dq,         2),  paste0(print(final.depl$low,      2),   "\u2013", print(final.depl$high,      2)),
        "",    "",
        comma(b.target$dq,     dig),      paste0(comma(b.target$low,    dig),     "\u2013", comma(b.target$high,      dig)),
        print(spr.btarg$dq,    3),        paste0(print(spr.btarg$low,     3),     "\u2013", print(spr.btarg$high,      3)),
        print(f.btarg$dq,      3),        paste0(print(f.btarg$low,       3),     "\u2013", print(f.btarg$high,      3)),
        comma(yield.btarg$dq,  dig),      paste0(comma(yield.btarg$low, dig),     "\u2013", comma(yield.btarg$high, dig)),
        "",    "",
        comma(b.spr$dq,        dig),      paste0(comma(b.spr$low,       dig),     "\u2013", comma(b.spr$high,     dig)),
        print(spr,              3),       " NA ",
        print(f.spr$dq,          3),      paste0(print(f.spr$low,        3),      "\u2013", print(f.spr$high,          3)),
        comma(yield.spr$dq, dig),         paste0(comma(yield.spr$low, dig),       "\u2013", comma(yield.spr$high,    dig)),
        "",    "",
        comma(b.msy$dq,        dig),      paste0(comma(b.msy$low,    dig),        "\u2013", comma(b.msy$high,        dig)),
        print(spr.msy$dq,      3),        paste0(print(spr.msy$low,   3),         "\u2013", print(spr.msy$high,      3)),
        print(f.msy$dq,          3),      paste0(print(f.msy$low,     3),         "\u2013", print(f.msy$high,          3)),
        comma(msy$dq,         dig),       paste0(comma(msy$low,        dig),      "\u2013", comma(msy$high,        dig))
    ), ncol=2, byrow=T )

    es.e = noquote(es.e)

    colnames(es.e) = c("Estimate", "95% Asymptotic Interval")
    rownames(es.e) = c(paste("Unfished", sb.label),
                paste0("Unfished Age ", smry.age, "+ Biomass (mt)"),               
                "Unfished Recruitment (R0)",
                paste0(sb.label, " (", hist[length(hist)], ")"),
                paste0("Depletion ", "(", hist[length(hist)], ")"),
                paste0("Reference Points Based SB", btarg, "%"),
                paste0("Proxy ", sb.label, "(SB",btarg, "%)"),
                paste0("SPR resulting in SB", btarg, "%"),
                paste0("Exploitation Rate Resulting in SB", btarg, "%"),
                paste0("Yield with SPR Based On SB", btarg, "% (mt)"),
                "Reference Points based on SPR proxy for MSY",
                paste0("Proxy ", sb.label, " (SPR", spr, ")"),
                paste0("SPR", spr),
                paste0("Exploitation rate corresponding to SPR", spr),
                paste0("Yield with SPR", spr, " at SB SPR (mt)"),
                "Reference points based on estimated MSY values",
                paste0(sb.label, " at MSY (SB MSY)"),
                "SPR MSY",
                "Exploitation rate corresponding to SPR MSY",
                "MSY (mt)")

    write.csv(es.e, file.path(csv.dir, "e_ReferencePoints_ES.csv"))

  } # end check for 'e' %in% tables

  
  #======================================================================
  # ES Table f is the historical harvest
  #======================================================================
  if('f' %in% tables){
    if(verbose){
      message("Creating Table f: Recent management performance")
    }
    
    if(is.null(adopted_ofl)){
      ofl = rep("fill_in", length(hist)-1)
    } else {
      if(length(adopted_ofl) != 12) { stop("The adopted_ofl vector needs to have 10 values.") }
      ofl = adopted_ofl
    }

    if(is.null(adopted_abc)){
      abc = rep("fill_in", length(hist)-1)
    } else {
      if(length(adopted_abc) != 12) { stop("The adopted_abc vector needs to have 10 values.") }
      abc = adopted_abc
    }

    if(is.null(adopted_acl)){
      acl = rep("fill_in", length(hist)-1)
    } else {
      if(length(adopted_acl) != 12) { stop("The adopted_acl vector needs to have 10 values.") }
      acl = adopted_acl
    }

    catch = dead = total.dead = 0
    for (i in 1:nfleets){
      name = paste0("retain(B):_",i)
      input.catch = replist$timeseries[replist$timeseries$Yr %in% hist[1:(length(hist)-1)], name]
      catch = cbind(catch, input.catch)

      name = paste0("dead(B):_",i)
      dead = replist$timeseries[replist$timeseries$Yr %in% hist[1:(length(hist)-1)], name]
      if (!is.null(dead)){ total.dead = total.dead + dead }
    } 
    total.catch = apply(catch, 1, sum) 
    catch = comma(total.catch, digits = 2)
    dead  = comma(total.dead,  digits = 2)

    if(sum(total.catch) != sum(total.dead)){
      es.f = data.frame(hist[1:(length(hist)-1)], ofl, abc, acl, catch, dead)
      colnames(es.f) = c("Years", "OFL", "ABC", "ACL", "Landings", "Total Dead")
    } else {
      es.f = data.frame(hist[1:(length(hist)-1)], ofl, abc, acl, catch)
      colnames(es.f) = c("Years", "OFL", "ABC", "ACL", "Landings")
    }
    write.csv(es.f, file.path(csv.dir, "f_Manage_ES.csv"), row.names = FALSE)

  } # end check for 'f' %in% tables

  #======================================================================
  #ES Table g  Predicted forecast values
  #======================================================================
  if('g' %in% tables){
    if(verbose){
      message("Creating Table g: Forecast OFLs, ABCs, Spawning Biomass/Output, and Depletion")
    }
    
    ofl.fore =  Get.Values(replist = replist, label = "OFLCatch" ,  yrs = fore, ci_value)$dq
    abc.fore =  Get.Values(replist = replist, label = "ForeCatch" , yrs = fore, ci_value)$dq
    ssb.fore  = Get.Values(replist = replist, label =  sb.name,     yrs = fore, ci_value)$dq
    depl.fore = Get.Values(replist = replist, label = "Bratio",     yrs = fore, ci_value)$dq

    if(!is.null(forecast_ofl)){
      n = length(forecast_ofl)
      ofl.fore[1:n] = forecast_ofl }

    if(!is.null(forecast_abc)){
      n = length(forecast_abc)
      abc.fore[1:n] = forecast_abc }

    if (nsexes == 1) {
      ssb.fore$dq = ssb.fore$dq / sexfactor
      ssb.fore$low = ssb.fore$low / sexfactor
      ssb.fore$high = ssb.fore$high / sexfactor
    }

    smry.fore = 0
    for(a in 1:nareas){
      ind = replist$timeseries$Area == a & replist$timeseries$Yr %in% fore 
      temp = replist$timeseries$Bio_smry[ind]
      smry.fore = smry.fore + temp
    }

    es.g = data.frame(fore,
           comma(ofl.fore, 2),
           comma(abc.fore, 2),
           comma(smry.fore,   2),
           comma(ssb.fore, 2),
           print(depl.fore*100,2))

    colnames(es.g) = c("Year", "Predicted OFL (mt)", "ABC Catch (mt)", paste0("Age ", smry.age, "+ Biomass (mt)"), sb.label, "Depletion (%)")

    write.csv(es.g, file.path(csv.dir, "g_Projections_ES.csv"), row.names = FALSE)

  } # end check for 'g' %in% tables

  #======================================================================
  #ES Table h decision table
  #======================================================================
  # To be done later
  if('h' %in% tables){
    if(verbose){
      message("Skipping the decision table (not yet implemented)")
    }
  }
  

  #======================================================================
  #ES Table i the summary table
  #======================================================================
  if('i' %in% tables){
    if(verbose){
      message("Creating Table i: Summary")
    }

    ind = length(hist)-1

    catch = dead = total.dead = 0
    for (i in 1:nfleets){
      name = paste0("retain(B):_",i)
      input.catch = replist$timeseries[replist$timeseries$Yr %in% hist[1:(length(hist)-1)], name]
      catch = cbind(catch, input.catch)

      name = paste0("dead(B):_",i)
      dead = replist$timeseries[replist$timeseries$Yr %in% hist[1:(length(hist)-1)], name]
      if (!is.null(dead)){ total.dead = total.dead + dead }
    } 
    total.catch = apply(catch, 1, sum) 
    total.bind = c(c("Total Catch", total.catch, "NA"), c("Total Dead", total.dead, "NA"))
    if(sum(total.catch) == sum(total.dead)) { 
      total.bind = c("Total Catch", total.catch, "NA")
    }

    spr_type = replist$SPRratioLabel #strsplit(base[grep(spr.name,base)]," ")[[1]][3]
    f_type   = ifelse(replist$F_report_basis == "_abs_F;_with_F=Exploit(bio)", "Exploitation Rate",
                      "Fill in F method")
    adj.spr = Get.Values(replist = replist, label = "SPRratio" , hist[1:(length(hist)-1)], ci_value)
    f.value = Get.Values(replist = replist, label = "F" , hist[1:(length(hist)-1)], ci_value)

    smry = smry.fore = 0
    for(a in 1:nareas){
      find = replist$timeseries$Area == a & replist$timeseries$Yr %in% hist[1:(length(hist)-1)] 
      temp = replist$timeseries$Bio_smry[find]
      smry = smry + temp

      find = replist$timeseries$Area == a & replist$timeseries$Yr %in% fore[1] 
      temp = replist$timeseries$Bio_smry[ind]
      smry.fore = smry.fore + temp
    }  
    smry = c(smry, smry.fore[1])

    ssb =  Get.Values(replist = replist, label = sb.name, hist, ci_value )
    if (nsexes == 1) { ssb$dq = ssb$dq / sexfactor ; ssb$low = ssb$low / sexfactor ; ssb$high = ssb$high / sexfactor }
    depl = Get.Values(replist = replist, label = "Bratio" , hist, ci_value )
    for (i in 1:length(hist)){ dig = ifelse(ssb[i,2] < 100, 1, 0)}
    recruits = Get.Values(replist = replist, label = "Recr" , hist, ci_value )

    if(is.null(adopted_ofl)){
      ofl = rep("fill_in", length(hist))
    } else {
      if(length(adopted_ofl) != 12) { stop("The adopted_ofl vector needs to have 12 values.") }
      if(is.null(forecast_ofl)) { 
        ofl = c(adopted_ofl, "-") 
      } else {
        ofl = c(adopted_ofl, forecast_ofl[1]) 
      }
    }

    if(is.null(adopted_acl)){
      acl = rep("fill_in", length(hist))
    } else {
      if(length(adopted_acl) != 12) { stop("The adopted_acl vector needs to have 12 values.") }
      if(is.null(forecast_abc)) { 
        acl = c(adopted_acl, "-") 
      } else {
        acl = c(adopted_acl, forecast_abc[1]) 
      }
    }

    es.i = matrix(c(
           c("OFL", ofl),
           c("ACL", acl),
           total.bind,
           c(spr_type, c(print(adj.spr$dq[1:(length(hist)-1)],2), "NA")),
           c(f_type, c(print(f.value$dq[1:(length(hist)-1)],2), "NA")),
           c(paste0("Age ", smry.age, "+ Biomass (mt)"), comma(smry,   dig)),
           c(sb.label, comma(ssb$dq, dig)),
           c("95% CI", paste0(comma(ssb$low, dig), "\u2013", comma(ssb$high, dig))),
           c("Recruits", comma(recruits$dq, dig)),
           c("95% CI", paste0(comma(recruits$low, dig), "\u2013", comma(recruits$high, dig))),
           c("Depletion (%)", print(depl$dq*100, 1)),
           c("95% CI", paste0(print(depl$low*100,1), "\u2013", print(depl$high*100,1)))),
           ncol = (length(hist)+1), byrow = T)

    es.i = noquote(es.i)
    colnames(es.i) = c("Quantity", hist)

    write.csv(es.i, file.path(csv.dir, "i_Summary_ES.csv"), row.names = FALSE)

  } # end check for 'i' %in% tables

  #======================================================================
  #End executive summary tables
  #======================================================================
  
  if (es_only == TRUE){
    if(verbose){
      message("Skipping catch, timeseries, and numbers-at-age tables because es_only = TRUE")
    }
  }

  if (es_only == FALSE & 'catch' %in% tables){
    if(verbose){
      message("Creating catch table")
    }
    
    #======================================================================
    # Total Catch when discards are estimated
    #======================================================================
    catch = fleet.names =  NULL
    dead = total.catch = total.dead = 0
    ind = startyr:endyr

    for (i in 1:nfleets){
      name = paste0("retain(B):_",i)
      input.catch = replist$timeseries[replist$timeseries$Yr %in% ind, name]
      catch = cbind(catch, input.catch)

      name = paste0("dead(B):_",i)
      dead = replist$timeseries[replist$timeseries$Yr %in% ind, name]
      if (!is.null(dead)){ 
        total.dead = total.dead + dead
        fleet.names = c(fleet.names, replist$FleetNames[i]) }
    } 
    total.catch = apply(catch, 1, sum) 

    if(sum(total.catch) != sum(total.dead)){
      mortality = data.frame(ind, comma(catch, digits = 2), comma(total.catch, digits = 2), comma(total.dead, digits = 2))
      colnames(mortality) = c("Years", fleet.names, "Total Catch", "Total Dead")
      write.csv(mortality, paste0(csv.dir, "/_Catches_All_Years.csv"), row.names = FALSE)      
    } else {
      mortality = data.frame(ind, comma(catch, digits = 2), comma(total.catch, digits = 2))
      colnames(mortality) = c("Years", fleet.names, "Total Catch")
      write.csv(mortality, paste0(csv.dir, "/_Catches_All_Years.csv"), row.names = FALSE)
    }

  } # end check for es_only = TRUE & 'catch' %in% tables

  #======================================================================
  #Time-series Tables
  #======================================================================
  if (es_only == FALSE & 'timeseries' %in% tables){
    if(verbose){
      message("Creating time-series table")
    }

    ssb.virgin = sum(replist$timeseries[replist$timeseries$Era == "VIRG", "SpawnBio"])
    
    smry.all = tot.bio.all = recruits.all = ssb.all = total.dead.all = 0
    for (a in 1:nareas){
      find = replist$timeseries$Area == a & replist$timeseries$Yr %in% all

      smry     = replist$timeseries$Bio_smry[find]
      tot.bio  = replist$timeseries$Bio_all[find]    
      recruits = replist$timeseries$Recruit_0[find]
      ssb      = replist$timeseries$SpawnBio[find]

      smry.all = smry.all + smry      
      tot.bio.all = tot.bio.all + tot.bio
      recruits.all = recruits.all + recruits
      ssb.all = ssb.all + ssb      
    }
  
    if (nsexes == 1) { ssb.all = ssb.all / sexfactor; ssb.virgin = ssb.virgin / sexfactor}
    depl.all = ssb.all / ssb.virgin

    total.dead.all = 0
    for (i in 1:nfleets){
      name = paste0("dead(B):_",i)
      dead = replist$timeseries[replist$timeseries$Yr %in% all, name]
      if (!is.null(dead)){ 
        total.dead.all = total.dead.all + dead }
    } 
    
    expl.all = total.dead.all / smry.all
    spr_type = replist$SPRratioLabel

    if (verbose){
      message("Catch includes estimated discards for total dead.")
      message("Exploitation = Total dead (including discards) divided by the summary biomass.")
    }

    # Check to see if there is exploitation in the first model year
    ind = 0
    for(z in 1:10) { ind = ind + ifelse(total.dead[z] == 0, 1, break()) } 
    adj.spr.all = replist$derived_quants[grep("SPRratio_",replist$derived_quants$Label),"Value"] 
    if(ind != 0) { adj.spr.all = c(rep(0, ind), adj.spr.all)}
  
    ts.table = data.frame(all,
          comma(tot.bio.all,0),
          comma(ssb.all,0),
          comma(smry.all,0),
          print(depl.all*100,1),
          comma(recruits.all,0),
          total.dead.all,
          print(adj.spr.all,3),
          expl.all)
    
    colnames(ts.table) = c("Year", "Total Biomass (mt)", sb.label, 
      paste0("Total Biomass ", smry.age ," (mt)"), "Depletion (%)", 
      "Age-0 Recruits", "Total Catch (mt)", spr_type, "Exploitation Rate")
    write.csv(ts.table, file = paste0(csv.dir,"TimeSeries.csv"), row.names = FALSE) 
  }  

  #======================================================================
  #Numbers at age
  #======================================================================
  if (es_only == FALSE & 'numbers' %in% tables){
    if(verbose){
      message("Creating numbers-at-age table")
    }
    
    check = dim(replist$natage)[2] 
    if (is.null(check)) {
      "Detailed age-structure is not in the report file, double check settings in the starter file."
    }

    if (!is.null(check)){
      age0 = which(names(replist$natage) == "0")
      get.ages = age0:check 

      if (nsexes == 1) {
        natage = 0
        for(a in 1:nareas){
          for(b in 1:nmorphs){
            ind = replist$natage[,"Yr"] >= startyr & replist$natage[,"Area"] == a  & replist$natage[,"Bio_Pattern"] == b & replist$natage[,"Sex"] == 1 & replist$natage[,"Beg/Mid"] == "B" 
            temp = replist$natage[ind, get.ages]
            natage = natage + temp
          }
        }

        colnames(natage) <- paste0("Age ", 0:(length(get.ages)-1))
        natage <- data.frame(Year = startyr:max(fore), natage)
        write.csv(natage, file.path(csv.dir, "_natage.csv"), row.names = FALSE)
      }

      if (nsexes == 2) {
        natage.f = natage.m = 0
        for(a in 1:nareas){
          for (b in 1:nmorphs){
            ind = replist$natage[,"Yr"] >= startyr & replist$natage[,"Area"] == a & replist$natage[,"Bio_Pattern"] == b & replist$natage[,"Sex"] == 1 & replist$natage[,"Beg/Mid"] == "B"
            temp = replist$natage[ind, get.ages]
            natage.f = natage.f + temp

            ind = replist$natage[,"Yr"] >= startyr & replist$natage[,"Area"] == a & replist$natage[,"Bio_Pattern"] == b &
            replist$natage[,"Sex"] == 2 & replist$natage[,"Beg/Mid"] == "B"
            temp = replist$natage[ind, get.ages]
            natage.m = natage.m + temp

          }
        }

        colnames(natage.m) <- paste0("Age", 0:(length(get.ages)-1))
        natage.m <- data.frame(Year = startyr:max(fore), natage.m)
        write.csv(natage.m, file.path(csv.dir, "_natage_m.csv"), row.names = FALSE)
  
        colnames(natage.f) <- paste0("Age ", 0:(length(get.ages)-1))
        natage.f <- data.frame(Year = startyr:max(fore), natage.f)
        write.csv(natage.f, file.path(csv.dir, "_natage.f.csv"), row.names = FALSE)
      }
    } # end check for detailed output
  } # end check for es_only = TRUE & 'numbers' %in% tables

}
