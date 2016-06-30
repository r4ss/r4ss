#' read data file from SS version 3.30 (function still in development)
#'
#' Read Stock Synthesis (version 3.30) data file into list object in R.
#' This function was formerly called SS_readdat. That name is now used
#' for a wrapper function that calls either SS_readdat_3.24 or SS_readdat_3.30
#' (and potentially additional functions in the future).
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar
#' @export
#' @seealso \code{\link{SS_readdat}}, \code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readctl}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},
#' \code{\link{SS_writectl}}
SS_readdat_3.30 <- function(file,verbose=TRUE,echoall=FALSE,section=NULL){
  # function to read Stock Synthesis data files

  if(verbose) cat("running SS_readdat_3.30\n")

  # stop function since it's not yet complete
  stop("SS_readdat_3.30 is still in development")

  #everything below needs to be revised to work with SSv3.30
  dat <- readLines(file,warn=FALSE)

  # split apart any bootstrap or expected value sections in data.ss_new
  if(!is.null(section)){
    Nsections <- as.numeric(substring(dat[grep("Number_of_datafiles",dat)],24))
    if(!section %in% 1:Nsections) stop("The 'section' input should be within the 'Number_of_datafiles' in a data.ss_new file.\n")
    if(section==1){
      end <- grep("#_expected values with no error added",dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[grep("#_observed data:",dat):end]
    }
    if(section==2){
      end <- grep("#_bootstrap file: 1",dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[grep("#_expected values with no error added",dat):end]
    }
    if(section>=3){
      start <- grep(paste("#_bootstrap file:",section-2),dat)
      end <- grep(paste("#_bootstrap file:",section-1),dat)
      if(length(end)==0) end <- length(dat)
      dat <- dat[start:end]
    }
  }

  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2]," ")[[1]][1]
  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in 1:length(dat)){
    # split along blank spaces
    mysplit <- strsplit(dat[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    # if final value is a number is followed immediately by a pound ("1#"),
    # this needs to be split
    nvals <- length(mysplit)
    if(nvals>0) mysplit[nvals] <- strsplit(mysplit[nvals],"#")[[1]][1]
    # convert to numeric
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
    else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  datlist <- list()

  # specifications
  datlist$sourcefile <- file
  datlist$type <- "Stock_Synthesis_data_file"
  datlist$SSversion <- NULL # "SSv3.21"

  if(verbose) cat("SSversion =",datlist$SSversion,"\n")

  # model dimensions
  datlist$styr  <- allnums[i]; i <- i+1
  datlist$endyr <- allnums[i]; i <- i+1
  datlist$nseas <- nseas <- allnums[i]; i <- i+1
  datlist$months_per_seas <- allnums[i:(i+nseas-1)]; i <- i+nseas
  datlist$spawn_seas <- allnums[i]; i <- i+1
  datlist$Ngenders <- Ngenders <- allnums[i]; i <- i+1
  datlist$Nages <- Nages <- allnums[i]; i <- i+1
  datlist$N_areas <- allnums[i]; i <- i+1
  # no longer separation of fleets and surveys
  datlist$Nfleets <- Nfleets <- allnums[i]; i <- i+1

  ### Ian, continue here (line below is temporary just to avoid warning about
  ### "no visible binding for global variable 'fleetnames'"
  fleetnames <- paste0("Fleet", Nfleets)
  
  #if(verbose) cat("fleetnames:",fleetnames,'\n')
  datlist$fleetnames <- fleetnames
  datlist$surveytiming <- surveytiming <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  datlist$areas <- areas <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  if(verbose){
    cat("areas:",areas,'\n')
    cat("fleet info:\n")
    ## print(data.frame(fleet  = 1:Nfleets,
    ##                  name   = fleetnames,
    ##                  area   = areas,
    ##                  timing = surveytiming,
    ##                  type   = c(rep("FISHERY",Nfleets), rep("SURVEY",Nsurveys))))
  }
  # fleet info
  fleetinfo1 <- data.frame(rbind(surveytiming,areas))
  names(fleetinfo1) <- fleetnames
  fleetinfo1$input <- c("#_surveytiming","#_areas")
  datlist$fleetinfo1 <- fleetinfo1
  ## if(verbose){
  ##   cat("fleetinfo1:\n")
  ##   print(t(fleetinfo1))
  ## }

  datlist$units_of_catch <- units_of_catch <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  datlist$se_log_catch <- se_log_catch <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  fleetinfo2 <- data.frame(rbind(units_of_catch,se_log_catch))
  names(fleetinfo2) <- fleetnames[1:Nfleets]
  fleetinfo2$input <- c("#_units_of_catch","#_se_log_catch")
  datlist$fleetinfo2 <- fleetinfo2
  ## if(verbose){
  ##   cat("fleetinfo2:\n")
  ##   print(t(fleetinfo2))
  ## }


  # more dimensions
  datlist$init_equil <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets

  # catch
  datlist$N_catch <- N_catch <- allnums[i]; i <- i+1
  if(verbose) cat("N_catch =",N_catch,"\n")
  Nvals <- N_catch*(Nfleets+2)
  catch <- data.frame(matrix(allnums[i:(i+Nvals-1)],
                             nrow=N_catch,ncol=(Nfleets+2),byrow=TRUE))
  names(catch) <- c(fleetnames[1:Nfleets],"year","seas")
  datlist$catch <- catch
  i <- i+Nvals
  if(echoall) print(catch)

  # CPUE
  datlist$N_cpue <- N_cpue <- allnums[i]; i <- i+1
  if(verbose) cat("N_cpue =",N_cpue,"\n")
  if(N_cpue > 0){
    CPUEinfo <- data.frame(matrix(allnums[i:(i+Nfleets*3-1)],
                                  nrow=Nfleets,ncol=3,byrow=TRUE))
    i <- i+Nfleets*3
    names(CPUEinfo) <- c("Fleet","Units","Errtype")
    CPUE <- data.frame(matrix(
                              allnums[i:(i+N_cpue*5-1)],nrow=N_cpue,ncol=5,byrow=TRUE))
    i <- i+N_cpue*5
    names(CPUE) <- c('year','seas','index','obs','se_log')
  }else{
    CPUEinfo <- NULL
    CPUE <- NULL
  }
  datlist$CPUEinfo <- CPUEinfo
  datlist$CPUE <- CPUE
  if(echoall){
    print(CPUEinfo)
    print(CPUE)
  }

  # discards
  # datlist$discard_units <- discard_units <- allnums[i]; i <- i+1
  datlist$N_discard_fleets <- N_discard_fleets <- allnums[i]; i <- i+1
  if(verbose) cat("N_discard_fleets =",N_discard_fleets,"\n")
  N_discard <- 0 # temporarily set to 0
  if(N_discard_fleets > 0){
    # fleet info
    Ncols <- 3
    discard_fleet_info <- data.frame(matrix(
                                            allnums[i:(i+N_discard_fleets*Ncols-1)],nrow=N_discard_fleets,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard_fleets*Ncols
    names(discard_fleet_info) <- c("Fleet","units","errtype")
  }else{
    discard_fleet_info <- NULL
  }
  datlist$N_discard <- N_discard <- allnums[i]; i <- i+1
  if(verbose) cat("N_discard =",N_discard,"\n")
  if(N_discard > 0){
    # discard data
    Ncols <- 5
    discard_data <- data.frame(matrix(
                                      allnums[i:(i+N_discard*Ncols-1)],nrow=N_discard,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard*Ncols
    names(discard_data) <- c('Yr','Seas','Flt','Discard','Std_in')
  }else{
    discard_data <- NULL
  }
  datlist$discard_fleet_info <- discard_fleet_info
  datlist$discard_data <- discard_data

  # meanbodywt
  datlist$N_meanbodywt <- N_meanbodywt <- allnums[i]; i <- i+1
  if(verbose) cat("N_meanbodywt =",N_meanbodywt,"\n")
  datlist$DF_for_meanbodywt <- allnums[i]
  i <- i+1
  if(echoall) cat("DF_for_meanbodywt =",datlist$DF_for_meanbodywt,"\n")

  if(N_meanbodywt > 0){
    Ncols <- 6
    meanbodywt <- data.frame(matrix(
                                    allnums[i:(i+N_meanbodywt*Ncols-1)],nrow=N_meanbodywt,ncol=Ncols,byrow=TRUE))
    i <- i+N_meanbodywt*Ncols
    names(meanbodywt) <- c('Year','Seas','Type','Partition','Value','CV')
  }else{
    meanbodywt <- NULL
  }
  datlist$meanbodywt <- meanbodywt
  if(echoall) print(meanbodywt)

  # length data
  datlist$lbin_method <- lbin_method <- allnums[i]; i <- i+1
  if(echoall) cat("lbin_method =",lbin_method,"\n")
  if(lbin_method==2){
    datlist$binwidth <- allnums[i]; i <- i+1
    datlist$minimum_size <- allnums[i]; i <- i+1
    datlist$maximum_size <- allnums[i]; i <- i+1
    if(echoall) cat("bin width, min, max =",datlist$binwidth,", ",datlist$minimum_size,", ",datlist$maximum_size,"\n")
  }else{
    datlist$binwidth <- NA
    datlist$minimum_size <- NA
    datlist$maximum_size <- NA
  }
  if(lbin_method==3){
    datlist$N_lbinspop <- N_lbinspop <- allnums[i]; i <- i+1
    datlist$lbin_vector_pop <- allnums[i:(i+N_lbinspop-1)]; i <- i+N_lbinspop
    if(echoall) cat("N_lbinspop =",N_lbinspop,"\nlbin_vector_pop:\n")
  }else{
    datlist$N_lbinspop <- NA
    datlist$lbin_vector_pop <- NA
  }

  datlist$comp_tail_compression <- allnums[i]; i <- i+1
  datlist$add_to_comp <- allnums[i]; i <- i+1
  datlist$max_combined_age <- allnums[i]; i <- i+1
  datlist$N_lbins <- N_lbins <- allnums[i]; i <- i+1
  datlist$lbin_vector <- lbin_vector <- allnums[i:(i+N_lbins-1)]; i <- i+N_lbins
  if(echoall) print(lbin_vector)

  datlist$N_lencomp <- N_lencomp <- allnums[i]; i <- i+1

  # if(verbose) cat(datlist[-15:0 + length(datlist)])
  if(verbose) cat("N_lencomp =",N_lencomp,"\n")

  if(N_lencomp > 0){
    Ncols <- N_lbins*Ngenders+6
    lencomp <- data.frame(matrix(
                                 allnums[i:(i+N_lencomp*Ncols-1)],nrow=N_lencomp,ncol=Ncols,byrow=TRUE))
    i <- i+N_lencomp*Ncols
    names(lencomp) <- c("Yr","Seas","FltSvy","Gender","Part","Nsamp",
                        if(Ngenders==1){paste("l",lbin_vector,sep="")}else{NULL},
                        if(Ngenders>1){ c(paste("f",lbin_vector,sep=""),paste("m",lbin_vector,sep="")) }else{ NULL } )
  }else{
    lencomp <- NULL
  }
  datlist$lencomp <- lencomp

  # age data
  datlist$N_agebins <- N_agebins <- allnums[i]; i <- i+1
  if(verbose) cat("N_agebins =",N_agebins,"\n")
  if(N_agebins > 0){
    agebin_vector <- allnums[i:(i+N_agebins-1)]; i <- i+N_agebins
  }else{
    agebin_vector <- NULL
  }
  datlist$agebin_vector <- agebin_vector
  if(echoall) print(agebin_vector)

  datlist$N_ageerror_definitions <- N_ageerror_definitions <- allnums[i]; i <- i+1
  if(N_ageerror_definitions > 0){
    Ncols <- Nages+1
    ageerror <- data.frame(matrix(
                                  allnums[i:(i+2*N_ageerror_definitions*Ncols-1)],
                                  nrow=2*N_ageerror_definitions,ncol=Ncols,byrow=TRUE))
    i <- i+2*N_ageerror_definitions*Ncols
    names(ageerror) <- paste("age",0:Nages,sep="")
  }else{
    ageerror <- NULL
  }
  datlist$ageerror <- ageerror

  datlist$N_agecomp <- N_agecomp <- allnums[i]; i <- i+1
  if(verbose) cat("N_agecomp =",N_agecomp,"\n")

  datlist$Lbin_method <- allnums[i]; i <- i+1
  datlist$max_combined_lbin <- allnums[i]; i <- i+1

  if(N_agecomp > 0){
    if(N_agebins==0) stop("N_agecomp =",N_agecomp," but N_agebins = 0")
    Ncols <- N_agebins*Ngenders+9
    agecomp <- data.frame(matrix(allnums[i:(i+N_agecomp*Ncols-1)],
                                 nrow=N_agecomp,ncol=Ncols,byrow=TRUE))
    i <- i+N_agecomp*Ncols
    names(agecomp) <- c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi","Nsamp",
                        if(Ngenders==1){paste("a",agebin_vector,sep="")}else{NULL},
                        if(Ngenders>1){ c(paste("f",agebin_vector,sep=""),paste("m",agebin_vector,sep="")) }else{ NULL } )
  }else{
    agecomp <- NULL
  }
  datlist$agecomp <- agecomp

  # MeanSize_at_Age
  datlist$N_MeanSize_at_Age_obs <- N_MeanSize_at_Age_obs <- allnums[i]; i <- i+1
  if(verbose) cat("N_MeanSize_at_Age_obs =",N_MeanSize_at_Age_obs,"\n")
  if(N_MeanSize_at_Age_obs > 0){
    Ncols <- 2*N_agebins*Ngenders + 7
    MeanSize_at_Age_obs <- data.frame(matrix(
                                             allnums[i:(i+N_MeanSize_at_Age_obs*Ncols-1)],nrow=N_MeanSize_at_Age_obs,ncol=Ncols,byrow=TRUE))
    i <- i+N_MeanSize_at_Age_obs*Ncols
    names(MeanSize_at_Age_obs) <- c('Yr','Seas','FltSvy','Gender','Part','AgeErr','Ignore',
                                    if(Ngenders==1){paste("a",agebin_vector,sep="")}else{NULL},
                                    if(Ngenders>1){ c(paste("f",agebin_vector,sep=""),paste("m",agebin_vector,sep="")) }else{ NULL },
                                    if(Ngenders==1){paste("N_a",agebin_vector,sep="")}else{NULL},
                                    if(Ngenders>1){ c(paste("N_f",agebin_vector,sep=""),paste("N_m",agebin_vector,sep="")) }else{ NULL } )
  }else{
    MeanSize_at_Age_obs <- NULL
  }
  datlist$MeanSize_at_Age_obs <- MeanSize_at_Age_obs

  # other stuff
  datlist$N_environ_variables <- N_environ_variables <- allnums[i]; i <- i+1
  datlist$N_environ_obs <- N_environ_obs <- allnums[i]; i <- i+1
  if(N_environ_obs > 0){
    Ncols <- 3
    envdat <- data.frame(matrix(
                                allnums[i:(i+Ncols*N_environ_obs-1)],nrow=N_environ_obs,ncol=Ncols,byrow=TRUE))
    i <- i+N_environ_obs*Ncols
    names(envdat) <- c("Yr","Variable","Value")
  }else{
    envdat <- NULL
  }
  datlist$envdat <- envdat

  datlist$N_sizefreq_methods <- N_sizefreq_methods <- allnums[i]; i <- i+1
  if(verbose) cat("N_sizefreq_methods =",N_sizefreq_methods,"\n")
  if(N_sizefreq_methods > 0){
    # get details of generalized size frequency methods
    datlist$nbins_per_method   <- nbins_per_method   <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$units_per_method   <- units_per_method   <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$scale_per_method   <- scale_per_method   <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$mincomp_per_method <- mincomp_per_method <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$Nobs_per_method    <- Nobs_per_method    <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    if(verbose){
      cat("details of generalized size frequency methods:\n")
      print(data.frame(method  = 1:N_sizefreq_methods,
                       nbins   = nbins_per_method,
                       units   = units_per_method,
                       scale   = scale_per_method,
                       mincomp = mincomp_per_method,
                       nobs    = Nobs_per_method))
    }
    # get list of bin vectors
    sizefreq_bins_list <- list()
    for(imethod in 1:N_sizefreq_methods){
      sizefreq_bins_list[[imethod]] <- allnums[i:(i+nbins_per_method[imethod]-1)]
      i <- i+nbins_per_method[imethod]
    }
    datlist$sizefreq_bins_list <- sizefreq_bins_list
    # read generalized size frequency data
    sizefreq_data_list <- list()
    for(imethod in 1:N_sizefreq_methods){
      Ncols <- 7+Ngenders*nbins_per_method[imethod]
      Nrows <- Nobs_per_method[imethod]
      sizefreq_data_tmp <- data.frame(matrix(allnums[i:(i+Nrows*Ncols-1)],
                                             nrow=Nrows,ncol=Ncols,byrow=TRUE))
      names(sizefreq_data_tmp) <-
        c("Method","Yr","Seas","FltSvy","Gender","Part","Nsamp",
          if(Ngenders==1){
            paste("a",sizefreq_bins_list[[imethod]],sep="")
          }else{NULL},
          if(Ngenders>1){
            c(paste("f",sizefreq_bins_list[[imethod]],sep=""),
              paste("m",sizefreq_bins_list[[imethod]],sep=""))
          }else{NULL})
      if(verbose){
        cat("Method =",imethod,"  (first two rows, ten columns):\n")
        print(sizefreq_data_tmp[1:min(Nrows,2),1:min(Ncols,10)])
      }
      if(any(sizefreq_data_tmp$Method!=imethod))
        stop("Problem with method in size frequency data:\n",
             "Expecting method: ",imethod,"\n",
             "Read method(s): ",paste(unique(sizefreq_data_tmp$Method),collapse=", "))
      sizefreq_data_list[[imethod]] <- sizefreq_data_tmp
      i <- i+Nrows*Ncols
    }
    datlist$sizefreq_data_list <- sizefreq_data_list
  }else{
    datlist$nbins_per_method   <- NULL
    datlist$units_per_method   <- NULL
    datlist$scale_per_method   <- NULL
    datlist$mincomp_per_method <- NULL
    datlist$Nobs_per_method    <- NULL
    datlist$sizefreq_bins_list <- NULL
    datlist$sizefreq_data_list <- NULL
  }

  datlist$do_tags <- do_tags <- allnums[i]; i <- i+1
  if(verbose) cat("do_tags =",do_tags,"\n")

  if(do_tags != 0){
    datlist$N_tag_groups <- N_tag_groups <- allnums[i]; i <- i+1
    datlist$N_recap_events <- N_recap_events <- allnums[i]; i <- i+1
    datlist$mixing_latency_period <- mixing_latency_period <- allnums[i]; i <- i+1
    datlist$max_periods <- max_periods <- allnums[i]; i <- i+1

    # read tag release data
    if(N_tag_groups > 0){
      Ncols <- 8
      tag_releases <- data.frame(matrix(allnums[i:(i+N_tag_groups*Ncols-1)],nrow=N_tag_groups,ncol=Ncols,byrow=TRUE))
      i <- i+N_tag_groups*Ncols
      names(tag_releases) <- c('TG', 'Area', 'Yr', 'Season', 'tfill', 'Gender', 'Age', 'Nrelease')
      if(verbose){
        cat("Head of tag release data:\n")
        print(head(tag_releases))
      }
    }else{
      tag_releases <- NULL
    }
    datlist$tag_releases <- tag_releases

    # read tag recapture data
    if(N_recap_events > 0){
      Ncols <- 5
      tag_recaps <- data.frame(matrix(allnums[i:(i+N_recap_events*Ncols-1)],nrow=N_recap_events,ncol=Ncols,byrow=TRUE))
      i <- i+N_recap_events*Ncols
      names(tag_recaps) <- c('TG', 'Yr', 'Season', 'Fleet', 'Nrecap')
      if(verbose){
        cat("Head of tag recapture data:\n")
        print(head(tag_recaps))
      }
    }else{
      tag_recaps <- NULL
    }
    datlist$tag_recaps <- tag_recaps
  }

  datlist$morphcomp_data <- do_morphcomps <- allnums[i]; i <- i+1
  if(verbose) cat("do_morphcomps =",do_morphcomps,"\n")

  if(allnums[i]==999){
    if(verbose) cat("read of data file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # return the result
  return(datlist)
}
