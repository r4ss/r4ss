SS_decision_table_stuff <- function(replist, yrs=2017:2026){
  # function for getting values for decision tables
  # not yet cleaned up for inclusion in r4ss

  # needs to be able to aggregate across areas for spatial models
  if(replist$nareas > 1){
    warning("You probably need to aggregate function output across areas")
  }
  # subset timeseries
  ts <- replist$timeseries[replist$timeseries$Yr %in% yrs,]
  # note that new $dead_B_sum quantity can be used in future versions
  catch <- apply(ts[,grep("dead(B)",names(ts),fixed=TRUE)], 1, sum)
  yr <- ts$Yr
  # get spawning biomass
  SpawnBio <- round(ts$SpawnBio,1)
  # get depletion (this calc is independent of Bratio definition)
  SpawnBioVirg <- replist$timeseries$SpawnBio[replist$timeseries$Era=="VIRG"]
  dep <- round(SpawnBio/SpawnBioVirg,3)
  # get summary biomass (not currently reported)
  Bio_smry <- ts$Bio_smry
  # combine stuff
  #stuff <- data.frame(yr=yr[ts$Area==1], catch, dep, SpawnBio, Bio_smry)
  stuff <- data.frame(yr, catch, SpawnBio, dep)
  return(stuff)
}



if(FALSE){
  # example use
  (dt <- SS_decision_table_stuff(base))
  ##        yr    catch SpawnBio   dep
  ## 3591 2017 18.82745     38.1 0.585
  ## 3592 2018 18.40211     37.3 0.573
  ## 3593 2019 18.01635     36.6 0.562
  ## 3594 2020 17.66874     35.9 0.551
  ## 3595 2021 17.35720     35.3 0.542
  ## 3596 2022 17.07926     34.8 0.535
  ## 3597 2023 16.83214     34.3 0.527
  ## 3598 2024 16.61281     33.9 0.521
  
}
