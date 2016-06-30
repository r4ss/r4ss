#' read forecast file
#' 
#' read Stock Synthesis forecast file into list object in R
#' 
#' 
#' @param file Filename either with full path or relative to working directory.
#' @param Nfleets Number of fleets.
#' @param Nareas Number of areas.
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_readstarter}}, \code{\link{SS_readdat}},
#' \code{\link{SS_readctl}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},
#' \code{\link{SS_writectl}}
SS_readforecast <-  function(file='forecast.ss', Nfleets, Nareas, verbose=TRUE){
  # function to read Stock Synthesis forecast files
  if(verbose) cat("running SS_readsforecast\n")
  forecast <- readLines(file,warn=F)
  mylist <- list()

  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_forecast_file"
  mylist$SSversion <- "SSv3.21_or_later"

  # get numbers (could be better integrated with function above)
  allnums <- NULL
  for(i in 1:length(forecast)){
      # split apart numbers from text in file
      mysplit <- strsplit(forecast[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      nums <- suppressWarnings(as.numeric(mysplit))
      if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
      else maxcol <- length(nums)
      if(maxcol > 0){
          nums <- nums[1:maxcol]
          allnums <- c(allnums, nums)
      }
  }

  # go through numerical values and save as elements of a big list
  i <- 1
  mylist$benchmarks <- allnums[i]; i <- i+1
  mylist$MSY <- allnums[i]; i <- i+1
  mylist$SPRtarget <- allnums[i]; i <- i+1
  mylist$Btarget <- allnums[i]; i <- i+1
  mylist$Bmark_years <- allnums[i:(i+5)]; i <- i+6
  mylist$Bmark_relF_Basis <- allnums[i]; i <- i+1
  mylist$Forecast <- allnums[i]; i <- i+1
  mylist$Nforecastyrs <- allnums[i]; i <- i+1
  mylist$F_scalar <- allnums[i]; i <- i+1
  mylist$Fcast_years <- allnums[i:(i+3)]; i <- i+4
  mylist$ControlRuleMethod <- allnums[i]; i <- i+1
  mylist$BforconstantF <- allnums[i]; i <- i+1
  mylist$BfornoF <- allnums[i]; i <- i+1
  mylist$Flimitfraction <- allnums[i]; i <- i+1
  mylist$N_forecast_loops <- allnums[i]; i <- i+1
  mylist$First_forecast_loop_with_stochastic_recruitment <- allnums[i]; i <- i+1
  mylist$Forecast_loop_control_3 <- allnums[i]; i <- i+1
  mylist$Forecast_loop_control_4 <- allnums[i]; i <- i+1
  mylist$Forecast_loop_control_5 <- allnums[i]; i <- i+1
  mylist$FirstYear_for_caps_and_allocations <- allnums[i]; i <- i+1
  mylist$stddev_of_log_catch_ratio <- allnums[i]; i <- i+1
  mylist$Do_West_Coast_gfish_rebuilder_output <- allnums[i]; i <- i+1
  mylist$Ydecl <- allnums[i]; i <- i+1
  mylist$Yinit <- allnums[i]; i <- i+1
  mylist$fleet_relative_F <- allnums[i]; i <- i+1
  if(mylist$fleet_relative_F==2) stop("SS_readforecast doesn't yet support option 2 for 'fleet relative F'")
  mylist$basis_for_fcast_catch_tuning <- allnums[i]; i <- i+1
  mylist$max_totalcatch_by_fleet <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  if(verbose) cat("  max_totalcatch_by_fleet =",mylist$max_totalcatch_by_fleet,"\n")
  mylist$max_totalcatch_by_area <- allnums[i:(i+Nareas-1)]; i <- i+Nareas
  if(verbose) cat("  max_totalcatch_by_area =",mylist$max_totalcatch_by_area,"\n")
  mylist$fleet_assignment_to_allocation_group <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  # allocation groups
  if(verbose) cat("  fleet_assignment_to_allocation_group =",mylist$fleet_assignment_to_allocation_group,"\n")
  if(any(mylist$fleet_assignment_to_allocation_group!=0)){
    mylist$N_allocation_groups <- max(mylist$fleet_assignment_to_allocation_group)
    mylist$allocation_among_groups <- allnums[i:(i+mylist$N_allocation_groups-1)]; i <- i+mylist$N_allocation_groups
  }else{
    mylist$N_allocation_groups <- 0
    mylist$allocation_among_groups <- NULL
  }
  mylist$Ncatch <- Ncatch <- allnums[i]; i <- i+1
  mylist$InputBasis <- allnums[i]; i <- i+1
  # forcast catch levels
  if(Ncatch==0){
    ForeCatch <- NULL
  }else{
    ForeCatch <- data.frame(matrix(
                                   allnums[i:(i+Ncatch*4-1)],nrow=Ncatch,ncol=4,byrow=TRUE))
    i <- i+Ncatch*4
    names(ForeCatch) <- c("Year","Seas","Fleet","Catch_or_F")
    if(verbose){
      cat("  Catch inputs (Ncatch =",Ncatch,"\n")
      print(ForeCatch)
    }
  }
  
  mylist$ForeCatch <- ForeCatch
  # check final value
  if(allnums[i]==999){
    if(verbose) cat("read of forecast file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # all done
  return(mylist)
}
