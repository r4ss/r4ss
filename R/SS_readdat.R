#' read Stock Synthesis data file
#'
#' Read Stock Synthesis data file into list object in R. This function is a
#' wrapper which calls SS_readdat_2.00, SS_readdat_3.00, SS_readdat_3.24, or SS_readdat_3.30
#' (and potentially additional functions in the future). This setup allows those
#' functions to be cleaner (if somewhat redundant) than a single function that
#' attempts to do everything. Returned datlist is mostly consistent across versions.
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param version SS version number.
#' Currently "2.00", "3.00", "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Allan C. Hicks, Neil L. Klaer
#' @export
#' @seealso \code{\link{SS_readdat_2.00}}, \code{\link{SS_readdat_3.00}},
#' \code{\link{SS_readdat_3.24}}, \code{\link{SS_readdat_3.30}},
#' \code{\link{SS_readctl}}, \code{\link{SS_readctl_3.24}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}

SS_readdat <- function(file, version="3.24", verbose=TRUE,echoall=FALSE,section=NULL){
  # wrapper function to call old or new version of SS_readdat

  # automatic testing of version number (not yet used by default)
  if(is.null(version)) {
    # look for 3.24 or 3.30 at the top of the chosen file
    version <- scan(file, what=character(), nlines=1)
    version <- substring(version,3,6)
    # if that fails, look for data.ss_new file in the same directory
    if(version %in% c("3.24", "3.30")){
      cat("assuming version", version, "based on first line of data file\n")
    }else{
      newfile <- file.path(dirname(file), "data.ss_new")
      if(file.exists(newfile)){
        version <- scan(newfile, what=character(), nlines=1)
        version <- substring(version,3,6)
        cat("assuming version", version, "based on first line of data.ss_new\n")
      }else{
        stop("input 'version' required due to missing value at top of", file)
      }
    }
  }

  nver=as.numeric(substring(version,1,4))

  if(verbose) cat("Char version is ", version, "\n")
  if(verbose) cat("Numeric version is ", nver, "\n")

  # call function for SS version 2.00
  if(nver<3){
    datlist <- SS_readdat_2.00(file=file, verbose=verbose,
                               echoall=echoall, section=section)
    # get fleet info
    finfo<-rbind(datlist$fleetinfo,c(rep(1,datlist$Nfleet),rep(3,datlist$Nsurveys)))
    finfo<-rbind(finfo,c(datlist$units_of_catch,rep(0,datlist$Nsurveys)))
    rownames(finfo)[3]<-"type"
    rownames(finfo)[4]<-"units"
    finfo<-finfo[,1:(length(finfo)-1)]
    finfo<-as.data.frame(t(finfo))
    datlist$fleetinfo<-finfo

    ##!!! need to add fixes to pop len bins? (see 3.24)

  }

  # call function for SS version 3.00
  if((nver>=3)&&(nver<3.2)){
    datlist <- SS_readdat_3.00(file=file, verbose=verbose,
                               echoall=echoall, section=section)

    # get fleet info
    finfo<-rbind(datlist$fleetinfo1,c(rep(1,datlist$Nfleet),rep(3,datlist$Nsurveys)))
    finfo<-rbind(finfo,c(datlist$units_of_catch,rep(0,datlist$Nsurveys)))
    rownames(finfo)[3]<-"type"
    rownames(finfo)[4]<-"units"
    finfo<-finfo[,1:(length(finfo)-1)]
    finfo<-as.data.frame(t(finfo))
    datlist$fleetinfo<-finfo

    ##!!! need to add fixes to pop len bins? (see 3.24)

  }

    # call function for SS version 3.24
  if((nver>=3.2)&&(nver<3.3)){
    datlist <- SS_readdat_3.24(file=file, verbose=verbose,
                               echoall=echoall, section=section)

    # get fleet info
    finfo<-rbind(datlist$fleetinfo1,c(rep(1,datlist$Nfleet),rep(3,datlist$Nsurveys)))
    finfo<-rbind(finfo,c(datlist$units_of_catch,rep(0,datlist$Nsurveys)))
    rownames(finfo)[3]<-"type"
    rownames(finfo)[4]<-"units"
    finfo<-finfo[,1:(length(finfo)-1)]
    finfo<-as.data.frame(t(finfo))
    datlist$fleetinfo<-finfo

    datlist$NCPUEObs<-array(data=0,dim=datlist$Nfleet+datlist$Nsurveys)

    for(j in 1:nrow(datlist$CPUE))
    {
      if(datlist$CPUE[j,]$index>0)datlist$NCPUEObs[datlist$CPUE[j,]$index]<-datlist$NCPUEObs[datlist$CPUE[j,]$index]+1
    }

    # fix some things
    if(!is.null(datlist$lbin_method))
    {
      if(datlist$lbin_method==1) # same as data bins
      {
        datlist$N_lbinspop<-datlist$N_lbins
        datlist$lbin_vector_pop<-datlist$lbin_vector
      }

      if(datlist$lbin_method==2) # defined wid, min, max
      {
        if(!is.null(datlist$binwidth)&&!is.null(datlist$minimum_size)&&!is.null(datlist$maximum_size))
        {
          datlist$N_lbinspop<-(datlist$maximum_size-datlist$minimum_size)/datlist$binwidth+1
          datlist$lbin_vector_pop<-vector()
          for(j in 0:datlist$N_lbinspop)
          {
            datlist$lbin_vector_pop<-c(datlist$lbin_vector_pop,datlist$minimum_size+(j*datlist$binwidth))
          }
        }

      }

      if(datlist$lbin_method==3) # vector
      {
        if(!is.null(datlist$lbin_vector_pop))
        {
          datlist$N_lbinspop<-length(datlist$lbin_vector_pop)
        }
      }
    }

  }

  # call function for SS version 3.30
  if(nver>=3.3){
    datlist <- SS_readdat_3.30(file=file, verbose=verbose,
                               echoall=echoall, section=section)

    datlist$spawn_seas <- datlist$spawn_month

    # compatibility: get the old number values
    datlist$Nfleet <- nrow(subset(datlist$fleetinfo,datlist$fleetinfo$type<=2))
    datlist$Nsurveys <- datlist$Nfleets-datlist$Nfleet
    totfleets<-datlist$Nfleet+datlist$Nsurveys
    datlist$N_areas <- datlist$Nareas
    datlist$Ngenders <- datlist$Nsexes
    datlist$N_cpue <- length(datlist$CPUE)

    # fleet details
    datlist$fleetinfo1<-t(datlist$fleetinfo)
    colnames(datlist$fleetinfo1)<-datlist$fleetinfo$fleetname
    datlist$fleetinfo1<-datlist$fleetinfo1[1:5,]
    datlist$fleetinfo2<-datlist$fleetinfo1[4:5,]
    datlist$fleetinfo1<-datlist$fleetinfo1[c(2:3,1),]
    rownames(datlist$fleetinfo1)<-c("surveytiming","areas","type")
    datlist$fleetinfo1<-data.frame(datlist$fleetinfo1)  # convert all to numeric
    datlist$fleetinfo2<-data.frame(datlist$fleetinfo2)  # convert all to numeric
    if(!is.null(datlist$discard_fleet_info))colnames(datlist$discard_fleet_info)<-c("Fleet","units","errtype")

    # compatibility: create the old format catch matrix
    datlist$newcatch<-datlist$catch<-data.frame(datlist$catch)
    ny<-datlist$endyr-datlist$styr+1

    catch<-matrix(0,nrow=ny,ncol=length(datlist$fleetinfo$fleetname)+2)
    colnames(catch)<-c(datlist$fleetinfo$fleetname,"year","seas")
    rownames(catch)<-as.character(1:ny)
    catch[,"year"]<-datlist$styr:datlist$endyr
    #catch<-as.data.frame(catch)

    datlist$init_equil<-array(0,dim=totfleets)
    datlist$se_log_catch<-array(0,dim=totfleets)

    for(i in 1:nrow(datlist$catch))
    {
      if(datlist$catch$V4[i]>=0)
      {
        if(datlist$catch$V1[i]==-999)  # this is an equilibrium catch
        {
           datlist$init_equil[as.numeric(datlist$catch$V2[i])]<-as.numeric(datlist$catch$V4[i])
        }

        if((datlist$catch$V1[i]>=datlist$styr)&&(datlist$catch$V1[i]<=datlist$endyr))  # this is a simple catch record
        {
          catch[as.numeric(which(catch[,"year"]==datlist$catch$V1[i])),as.numeric(datlist$catch$V2[i])]<-datlist$catch$V4[i]
          catch[as.numeric(which(catch[,"year"]==datlist$catch$V1[i])),"seas"]<-datlist$catch$V3[i]
          datlist$se_log_catch[as.numeric(datlist$catch$V2[i])]<-as.numeric(datlist$catch$V5[i])
        }
      }
    }

    catch<-as.data.frame(catch)
    for(i in 1:totfleets)
    {
      catch[,i]<-as.double(as.character(catch[,i]))
    }
    catch$year<-as.numeric(as.character(catch$year))
    catch$seas<-as.numeric(as.character(catch$seas))

    datlist$catch<-catch

    datlist$CPUEinfo<-as.data.frame(datlist$CPUEinfo)
    for(i in 1:ncol(datlist$CPUEinfo))
    {
      datlist$CPUEinfo[,i]<-as.numeric(as.character(datlist$CPUEinfo[,i]))
    }

    datlist$CPUE<-as.data.frame(datlist$CPUE)
    datlist$CPUE$year<-as.numeric(as.character(datlist$CPUE$year))
    datlist$CPUE$seas<-as.numeric(as.character(datlist$CPUE$seas))
    datlist$CPUE$index<-as.numeric(as.character(datlist$CPUE$index))
    datlist$CPUE$obs<-as.double(as.character(datlist$CPUE$obs))
    datlist$CPUE$se_log<-as.double(as.character(datlist$CPUE$se_log))

    # mean body weight
    if(datlist$use_meanbodywt==0)
    {
      datlist$N_meanbodywt<-0
    }

    # length info
    datlist$comp_tail_compression<-datlist$len_info$mintailcomp
    datlist$add_to_comp<-datlist$len_info$addtocomp
    datlist$max_combined_lbin<-datlist$len_info$combine_M_F

    if(is.null(datlist$lencomp))datlist$N_lencomp<-0

    # compatibility: values in data frames need to be numeric
    if(datlist$N_discard_fleets>0)
    {
      datlist$discard_data$Yr<-as.numeric(datlist$discard_data$Yr)
      datlist$discard_data$Seas<-as.numeric(datlist$discard_data$Seas)
      datlist$discard_data$Flt<-as.numeric(datlist$discard_data$Flt)
      datlist$discard_data$Discard<-as.double(datlist$discard_data$Discard)
      datlist$discard_data$Std_in<-as.double(datlist$discard_data$Std_in)
    }

    if(!is.null(datlist$lencomp))
    {
      datlist$lencomp$Yr<-as.numeric(datlist$lencomp$Yr)
      datlist$lencomp$Seas<-as.numeric(datlist$lencomp$Seas)
      datlist$lencomp$FltSvy<-as.numeric(datlist$lencomp$FltSvy)
      datlist$lencomp$Gender<-as.numeric(datlist$lencomp$Gender)
      datlist$lencomp$Part<-as.numeric(datlist$lencomp$Part)
      datlist$lencomp$Nsamp<-as.double(datlist$lencomp$Nsamp)
    }

    if(!is.null(datlist$agecomp))
    {
      datlist$agecomp$Yr<-as.numeric(datlist$agecomp$Yr)
      datlist$agecomp$Seas<-as.numeric(datlist$agecomp$Seas)
      datlist$agecomp$FltSvy<-as.numeric(datlist$agecomp$FltSvy)
      datlist$agecomp$Gender<-as.numeric(datlist$agecomp$Gender)
      datlist$agecomp$Part<-as.numeric(datlist$agecomp$Part)
      datlist$agecomp$Nsamp<-as.double(datlist$agecomp$Nsamp)
    }

    if(datlist$use_MeanSize_at_Age_obs==0)
    {
      datlist$N_MeanSize_at_Age_obs<-0
    }

    ##!!! need to add fixes to pop len bins? (see 3.24)

    datlist$NCPUEObs<-array(data=0,dim=datlist$Nfleets)

    for(j in 1:nrow(datlist$CPUE))
    {
      if(datlist$CPUE[j,]$index>0)datlist$NCPUEObs[datlist$CPUE[j,]$index]<-datlist$NCPUEObs[datlist$CPUE[j,]$index]+1
    }

    # fix some things
    if(!is.null(datlist$lbin_method))
    {
      if(datlist$lbin_method==1) # same as data bins
      {
        datlist$N_lbinspop<-datlist$N_lbins
        datlist$lbin_vector_pop<-datlist$lbin_vector
      }

      if(datlist$lbin_method==2) # defined wid, min, max
      {
        if(!is.null(datlist$binwidth)&&!is.null(datlist$minimum_size)&&!is.null(datlist$maximum_size))
        {
          datlist$N_lbinspop<-(datlist$maximum_size-datlist$minimum_size)/datlist$binwidth+1
          datlist$lbin_vector_pop<-vector()
          for(j in 0:datlist$N_lbinspop)
          {
            datlist$lbin_vector_pop<-c(datlist$lbin_vector_pop,datlist$minimum_size+(j*datlist$binwidth))
          }
        }

      }

      if(datlist$lbin_method==3) # vector
      {
        if(!is.null(datlist$lbin_vector_pop))
        {
          datlist$N_lbinspop<-length(datlist$lbin_vector_pop)
        }
      }
    }



  }

  # return the result
  return(datlist)
}
