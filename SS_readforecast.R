SS_readforecast <-  function(file='forecast.ss', Nfleets=NULL, verbose=TRUE){
  # function to read Stock Synthesis forecast files
  if(verbose) cat("running SS_readsforecast\n")
  cat("This function needs updating for SSv3.21\n")
  forecast <- readLines(file,warn=F)
  mylist <- list()

  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_forecast_file"
  mylist$SSversion <- "SSv3.20_or_later"

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

  mylist$Forecast <- allnums[i]; i <- i+1
  mylist$first_selex_yr <- allnums[i]; i <- i+1
  mylist$last_selex_yr <- allnums[i]; i <- i+1
  mylist$benchmarks <- allnums[i]; i <- i+1
  mylist$MSY <- allnums[i]; i <- i+1
  mylist$SPRtarget <- allnums[i]; i <- i+1
  mylist$Btarget <- allnums[i]; i <- i+1
  mylist$Nforecastyrs <- allnums[i]; i <- i+1
  mylist$AdvancedOptions <- allnums[i]; i <- i+1
  if(mylist$AdvancedOptions==1){ # go through 10 advanced options
    mylist$RebuilderOutput <- allnums[i]; i <- i+1
    mylist$Ydecl <- allnums[i]; i <- i+1
    mylist$Yinit <- allnums[i]; i <- i+1
    mylist$ControlRuleMethod <- allnums[i]; i <- i+1
    mylist$BforconstantF <- allnums[i]; i <- i+1
    mylist$BfornoF <- allnums[i]; i <- i+1
    mylist$Flimitfraction <- allnums[i]; i <- i+1
    mylist$MaxCatchBasis <- allnums[i]; i <- i+1
    mylist$ImplementError <- allnums[i]; i <- i+1
    mylist$ErrorStdDev <- allnums[i]; i <- i+1
  } # end advanced options
  mylist$FleetAllocation <- allnums[i]; i <- i+1
  if(mylist$FleetAllocation==2){
    if(is.null(Nfleets)){
      cat("Fleet allocation switch = 2, so input 'Nfleets' is needed for function SS_readforecast\n")
      return()
    }else{
      mylist$FleetAllocationVec <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
    }
  }
  mylist$Ncatch <- Ncatch <- allnums[i]; i <- i+1
  # forcast catch levels
  if(Ncatch>0){
    mylist$InputBasis <- allnums[i]; i <- i+1
    ForeCatch <- data.frame(matrix(
      allnums[i:(i+Ncatch*4-1)],nrow=Ncatch,ncol=4,byrow=TRUE))
    names(ForeCatch) <- c("Year","Seas","Fleet","Catch")
  }else{
    ForeCatch <- NULL
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
