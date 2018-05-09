#' read data file from SS version 2.00
#' 
#' Read Stock Synthesis (version 2.00) data file into list object in R.
#' This function was formerly called SS_readdat. That name is now used
#' for a wrapper function that calls either SS_readdat_2.00 SS_readdat_3.00 
#' SS_readdat_3.24 or SS_readdat_3.30 (and potentially additional functions in the future).
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
#' will read input data, (equivalent to section=1). ## needs to be added
#' @author Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar, Neil L. Klaer
#' @export
#' @seealso \code{\link{SS_readdat}}, \code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}
SS_readdat_2.00 <- function(file,verbose=TRUE,echoall=FALSE,section=NULL){
  # function to read Stock Synthesis data files

  if(verbose) cat("running SS_readdat_2.00\n")
  dat <- readLines(file,warn=FALSE)

  # split apart any bootstrap or expected value sections in data.ss_new
  if(!is.null(section)){
    Nsections <- 1
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
  
  datlist$eof <- FALSE

  # specifications
  datlist$sourcefile <- file
  datlist$type <- "Stock_Synthesis_data_file"
  datlist$ReadVersion <- "2.00"
  if (verbose){
    message("SS_readdat_2.00 - read version = ", datlist$ReadVersion)
  }
  
  # return(datlist)

  # model dimensions
  datlist$styr  <- allnums[i]; i <- i+1
  datlist$endyr <- allnums[i]; i <- i+1
  datlist$nseas <- nseas <- allnums[i]; i <- i+1
  datlist$months_per_seas <- allnums[i:(i+nseas-1)]; i <- i+nseas
  datlist$spawn_seas <- allnums[i]; i <- i+1
  datlist$Nfleet <- Nfleet <- allnums[i]; i <- i+1
  datlist$Nsurveys <- Nsurveys <- allnums[i]; i <- i+1
  tt<-array(dim=Nfleet)
  tt[1:Nfleet]<-1
  datlist$units_of_catch <- tt
  Ntypes <- Nfleet+Nsurveys
  datlist$N_areas <- 1
  
  # an attempt at getting the fleet names based on occurance of %-sign
  fleetnames.good <- NULL
  if(Ntypes>1){
    percentlines <- grep('%',dat)
    for(iline in percentlines){
      fleetnames <- dat[iline]
      # message(fleetnames,Ntypes)
      
      if(length(grepRaw("%",fleetnames,all=TRUE))>=Ntypes-1) # this line has a lot of percent symbols
      {
          # strip off any label at the end
          if(length(grepRaw("#",fleetnames,all=TRUE))>0){
              fleetnames<-strsplit(as.character(fleetnames),"#")
              fleetnames<-fleetnames[[1]]
          }

          # message(fleetnames," stage 1")
          
          # browser()

          fleetnames <- strsplit(as.character(fleetnames),"%")     
          # strip any white space off the end of the fleetnames
          fleetnames[[1]][Ntypes] <- strsplit(as.character(fleetnames[[1]][Ntypes]),"[[:blank:]]+")[[1]][1]
          fleetnames<-fleetnames[[1]]

          # message(fleetnames," stage 2")

          if(length(fleetnames)==Ntypes) fleetnames.good <- fleetnames
          #browser()
        
      }
    }
    fleetnames <- fleetnames.good
    if(is.null(fleetnames))
      #  fleetnames <- c(paste("fishery",1:Nfleet),paste("survey",1:Nsurveys))
      fleetnames <- c(paste("fishsurv",1:Ntypes))
  }else{
    fleetnames <- "fleet1"
  }
  #if(verbose) cat("fleetnames:",fleetnames,'\n')
  datlist$fleetnames <- fleetnames
  datlist$surveytiming <- surveytiming <- allnums[i:(i+Ntypes-1)]; i <- i+Ntypes
  datlist$areas <- vector(length=Ntypes)
  datlist$areas <- 1
  areas <- datlist$areas
  if(verbose){
    cat("areas:",areas,'\n')
    cat("fleet info:\n")
    print(data.frame(fleet  = 1:Ntypes,
                     name   = fleetnames,
                     area   = areas,
                     timing = surveytiming,
                     type   = c(rep("FISHERY",Nfleet), rep("SURVEY",Nsurveys))))
  }

  # browser()
  # fleet info
  fleetinfo1 <- data.frame(rbind(surveytiming,areas))
  names(fleetinfo1) <- fleetnames
  fleetinfo1$input <- c("#_surveytiming","#_areas")
  datlist$fleetinfo1 <- fleetinfo1
  ## if(verbose){
  ##   cat("fleetinfo1:\n")
  ##   print(t(fleetinfo1))
  ## }
  
  # more dimensions
  datlist$Ngenders <- Ngenders <- allnums[i]; i <- i+1
  datlist$Nages <- Nages <- allnums[i]; i <- i+1

  
  datlist$init_equil <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet
  
  N_catch <- datlist$endyr-datlist$styr+1
  
  # catch
  if(verbose) cat("N_catch =",N_catch,"\n")
  Nvals <- N_catch*(Nfleet)
  catch <- data.frame(matrix(allnums[i:(i+Nvals-1)],
                             nrow=N_catch,ncol=(Nfleet),byrow=TRUE))
  yr<-datlist$styr:datlist$endyr
  se<-rep(1,N_catch)

  catch<-cbind(catch,yr,se)

  names(catch) <- c(fleetnames[1:Nfleet],"year","seas")
  datlist$catch <- catch
  i <- i+Nvals
  if(echoall) print(catch)
  

  # CPUE
  datlist$N_cpue <- N_cpue <- allnums[i]; i <- i+1
  if(verbose) cat("N_cpue =",N_cpue,"\n")
  if(N_cpue > 0){
    CPUEinfo <- data.frame(matrix(c(1:Ntypes,rep(1,Ntypes),rep(0,Ntypes)),
                                  nrow=Ntypes,ncol=3,byrow=FALSE))  # fill CPUEinfo with defaults
    names(CPUEinfo) <- c("Fleet","Units","Errtype")
    CPUE <- data.frame(matrix(
                              allnums[i:(i+N_cpue*5-1)],nrow=N_cpue,ncol=5,byrow=TRUE))
    i <- i+N_cpue*5
    names(CPUE) <- c('year','seas','index','obs','se_log')
    CPUE<-CPUE[which(CPUE$obs>=0),] # remove negative observations (commented?)
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
  Dis_type <- allnums[i]; i <- i+1
  datlist$N_discard <- N_discard <- allnums[i]; i <- i+1
  
  if(verbose) cat("N_discard =",N_discard,"\n")
  if(N_discard > 0){
    # discard data
    Ncols <- 5
    discard_data <- data.frame(matrix(
                                      allnums[i:(i+N_discard*Ncols-1)],nrow=N_discard,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard*Ncols
    names(discard_data) <- c('Yr','Seas','Flt','Discard','Std_in')
    datlist$discard_data <- discard_data
    
    datlist$N_discard_fleets <- N_discard_fleets <- length(unique(discard_data$Flt))
    
    
    datlist$discard_fleet_info <- data.frame(matrix(c(unique(discard_data$Flt),
                                  rep(Dis_type,N_discard_fleets),rep(0,N_discard_fleets)),
                                  nrow=N_discard_fleets,ncol=3,byrow=FALSE))  # fill discard fleet info with defaults
    names(datlist$discard_fleet_info) <- c("Fleet","units","errtype")
    
  }else{
    datlist$N_discard_fleets <- 0
    datlist$discard_data <- NULL
    datlist$discard_fleet_info <- NULL
  }
  

  # meanbodywt
  datlist$N_meanbodywt <- N_meanbodywt <- allnums[i]; i <- i+1
  if(verbose) cat("N_meanbodywt =",N_meanbodywt,"\n")
  
  
  if(N_meanbodywt > 0){
    Ncols <- 6
    meanbodywt <- data.frame(matrix(
                                    allnums[i:(i+N_meanbodywt*Ncols-1)],nrow=N_meanbodywt,ncol=Ncols,byrow=TRUE))
    i <- i+N_meanbodywt*Ncols
    names(meanbodywt) <- c('Year','Seas','Type','Partition','Value','CV')
    
    # don't know if this is needed for version 2.00
    #datlist$DF_for_meanbodywt <- allnums[i]
    #i <- i+1
    #if(echoall) cat("DF_for_meanbodywt =",datlist$DF_for_meanbodywt,"\n")
    datlist$DF_for_meanbodywt <- NULL
    
    
  }else{
    datlist$DF_for_meanbodywt <- NULL
    meanbodywt <- NULL
  }
  datlist$meanbodywt <- meanbodywt
  if(echoall) print(meanbodywt)
  
  datlist$comp_tail_compression <- allnums[i]; i <- i+1
  datlist$add_to_comp <- allnums[i]; i <- i+1
  datlist$max_combined_age <- NULL
  
  # length data
  datlist$N_lbinspop <- NULL
  datlist$lbin_vector_pop <- NULL
  
  datlist$lbin_method <- 3
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

  datlist$Lbin_method <- NULL
  datlist$max_combined_lbin <- NULL

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
  
  if(allnums[i]==999){
    if(verbose) cat("read of data file 2.00 complete (final value = 999)\n")
    datlist$eof <- TRUE
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
    datlist$eof <- FALSE
  }

  # return the result
  return(datlist)
}
