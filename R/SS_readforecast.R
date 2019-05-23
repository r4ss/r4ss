#' read forecast file
#'
#' read Stock Synthesis forecast file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param Nfleets Number of fleets (not required in 3.30).
#' @param Nareas Number of areas (not required in 3.30).
#' @param nseas number of seasons (not required in 3.30).
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
#' @param readAll Should the function continue even if Forecast=0
#' (at which point SS stops reading)
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_readstarter}}, \code{\link{SS_readdat}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},

SS_readforecast <-  function(file='forecast.ss', Nfleets, Nareas, nseas,
                             version="3.30", readAll=FALSE, verbose=TRUE){
  # function to read Stock Synthesis forecast files
  if(!(version=="3.24" | version=="3.30" | version==3.3)){
    # turns out 3.30 != "3.30" in R
    stop('version must be either 3.24 or 3.30')
  }

  if(verbose) cat("running SS_readforecast\n")
  forecast <- readLines(file,warn=F)

  mylist <- list()
  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_forecast_file"
  mylist$SSversion <- version

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
  if(version==3.24){
    mylist$Bmark_years <- allnums[i:(i+5)]; i <- i+6
  }else{
    mylist$Bmark_years <- allnums[i:(i+9)]; i <- i+10
  }
  if(verbose){
    cat("Benchmark years: ", mylist$Bmark_years, "\n")
  }
  mylist$Bmark_relF_Basis <- allnums[i]; i <- i+1
  mylist$Forecast <- Forecast <- allnums[i]; i <- i+1
  # test for 0 value of Forecast and only continue if non-zero or readAll
  if(Forecast==0 & !readAll){
    if(verbose){
      cat("Forecast=0 and input readAll=FALSE so skipping remainder of file\n")
    }
  }else{
    if(verbose){
      cat("Forecast =", Forecast, "\n")
    }
    mylist$Nforecastyrs <- allnums[i]; i <- i+1
    mylist$F_scalar <- allnums[i]; i <- i+1
    if(version==3.24){
      mylist$Fcast_years <- allnums[i:(i+3)]; i <- i+4
    }else{
      mylist$Fcast_years <- allnums[i:(i+5)]; i <- i+6
    }
    if(verbose){
      cat("Forecast years: ", mylist$Fcast_years, "\n")
    }
    mylist$Fcast_selex <- NA
    if(version=="3.30" | version==3.3){
      mylist$Fcast_selex <- allnums[i]; i <- i+1 # not present in early 3.30 versions
      if(verbose){
        cat("Forecast selectivity option: ", mylist$Fcast_selex, "\n")
      }
    }
    
    mylist$ControlRuleMethod <- allnums[i]; i <- i+1
    mylist$BforconstantF <- allnums[i]; i <- i+1
    mylist$BfornoF <- allnums[i]; i <- i+1
    mylist$Flimitfraction <- allnums[i]; i <- i+1
    if (mylist$Flimitfraction < 0) {
      ii <- i
      while (allnums[ii] > 0) ii <- ii + 1
      mylist$Flimitfraction_m <- data.frame(matrix(allnums[i:(ii + 1)], 
        ncol = 2, byrow = TRUE))
      colnames(mylist$Flimitfraction_m) <- c("Year", "Fraction")
      i <- ii + 2
      remove(ii)
    }
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
    if(mylist$fleet_relative_F==2){
      stop("SS_readforecast doesn't yet support option 2 for 'fleet relative F'")
    }
    mylist$basis_for_fcast_catch_tuning <- allnums[i]; i <- i+1
    if(version==3.24){
      if(verbose){
        cat('reading section on fleet- and area-specific inputs based on 3.24 format\n')
      }
      # the following section is somewhat different between 3.24 and 3.30
      mylist$max_totalcatch_by_fleet <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
      if(verbose) cat("  max_totalcatch_by_fleet =",mylist$max_totalcatch_by_fleet,"\n")
      mylist$max_totalcatch_by_area <- allnums[i:(i+Nareas-1)]; i <- i+Nareas
      if(verbose) cat("  max_totalcatch_by_area =",mylist$max_totalcatch_by_area,"\n")
      mylist$fleet_assignment_to_allocation_group <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
      # allocation groups
      if(verbose) cat("  fleet_assignment_to_allocation_group =",mylist$fleet_assignment_to_allocation_group,"\n")
      if(any(mylist$fleet_assignment_to_allocation_group!=0)){
        mylist$N_allocation_groups <- max(mylist$fleet_assignment_to_allocation_group)
        allocation_among_groups <- allnums[i:(i+mylist$N_allocation_groups*nseas-1)]; i <- i+mylist$N_allocation_groups*nseas
        mylist$allocation_among_groups<-
          as.data.frame(t(array(data=allocation_among_groups,dim=c(mylist$N_allocation_groups,nseas))))
        colnames(mylist$allocation_among_groups)<-paste0("Grp",1:mylist$N_allocation_groups) 
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
          cat("  Catch inputs (Ncatch = ",Ncatch,")\n", sep="")
          print(ForeCatch)
        }
      }
    }
    if(version=="3.30" | version==3.3){
      if(verbose){
        cat('reading section on fleet- and area-specific inputs based on 3.30 format\n')
      }
      # check for any catch caps or allocation groups
      # (indicated by additional values rather than something like the following lines:
      ## # enter list of fleet number and max for fleets with max annual catch; terminate with fleet=-9999
      ## -9999 -1
      ## # enter list of area ID and max annual catch; terminate with area=-9999
      ## -9999 -1
      ## # enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999
      ## -9999 -1
      if(any(allnums[i+c(0,2,4)] != -9999)){
        stop("sorry, SS_readforecast doesn't yet work for 3.30 models with catch caps or allocation groups")
      }
      i <- i+6 # increment indicator past section on caps and allocations

      # NULL variables that may be needed for SS_writeforecast
      mylist$max_totalcatch_by_fleet <- NULL
      mylist$max_totalcatch_by_area <- NULL
      mylist$fleet_assignment_to_allocation_group <- NULL
      mylist$N_allocation_groups <- 0
      mylist$allocation_among_groups <- NULL

      mylist$InputBasis <- allnums[i]; i <- i+1

      # forcast catch levels
      if(allnums[i]==-9999){
        ForeCatch <- NULL
        i <- i+4
      }else{
        # offset from current position in vector to ending point
        all9999 <- which(allnums == -9999)
        ForeCatch.end <- min(all9999[all9999 > i])-1
        Nvals <- (ForeCatch.end - i + 1)
        # even final line starting with -9999 needs to have 4 values
        # so number of values should always be evenly divisible by 4
        if(Nvals %% 4 != 0){
          stop("Error in read of input forecast catch.\n",
               "Number of values should be a multiple of 4.\n",
               "Values:\n", paste(allnums[i:ForeCatch.end], collapse="\n"))
        }
        ForeCatch <- data.frame(matrix(
            allnums[i:ForeCatch.end], nrow=Nvals/4, ncol=4, byrow=TRUE))
        # increment index
        # (+5 to skip over -9999 and 3 placeholders at end of input matrix)
        i <- ForeCatch.end + 5
        names(ForeCatch) <- c("Year","Seas","Fleet","Catch_or_F")
        if(verbose){
          cat("  Catch inputs (Ncatch = ",Nvals/4,")\n", sep="")
          print(ForeCatch)
        }
      }
    }
    mylist$ForeCatch <- ForeCatch
    # check final value
    if(allnums[i]==999){
      if(verbose) cat("read of forecast file complete (final value = 999)\n")
    }else{
      cat("Error: final value is", allnums[i]," but should be 999\n")
    }
  } # end read of additional values beyond "Forecast" input
  # all done
  return(mylist)
}
