#' write data file
#'
#' write Stock Synthesis data file from list object in R which was probably
#' created using \code{\link{SS_readdat}}
#'
#'
#' @param datlist List object created by \code{\link{SS_readdat}}.
#' @param outfile Filename for where to write new data file.
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor, Yukio Takeuchi
#' @export
#' @seealso \code{\link{SS_makedatlist}}, \code{\link{SS_readstarter}},
#' \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
#' \code{\link{SS_writedat}}
SS_writedat <- function(datlist,outfile,overwrite=FALSE,verbose=TRUE){
  # function to write Stock Synthesis data files
  if(verbose) cat("running SS_writedat\n")

  if(datlist$type!="Stock_Synthesis_data_file"){
    stop("input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'")
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }

  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name,comment=NULL){
    # simple function to clean up many repeated commands
    value = datlist[names(datlist)==name]
    if(is.null(comment)){
      writeLines(paste(value," #_",name,sep="",collapse="_"))
    }else{
      writeLines(paste(value,comment))
    }
  }

  #### this function isn't being used at the moment
  ## wl.vector <- function(name,comment=NULL){
  ##   # simple function to clean up many repeated commands
  ##   value = datlist[names(datlist)==name]
  ##   if(is.null(comment)){
  ##     writeLines(paste(paste(value,collapse=" ")," #_",name,sep=""))
  ##   }else{
  ##     writeLines(paste(paste(value,collapse=" "),comment))
  ##   }
  ## }

  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print.data.frame(dataframe, row.names=FALSE, strip.white=TRUE)
   # write.table(file=zz,x=dataframe,append=TRUE,sep=" ",quote=FALSE,row.names=FALSE)
  #  write_delim(path=zz,x=dataframe,append=TRUE,delim=" ",col_names=TRUE)
  }

  # write a header
  writeLines("#C data file created using the SS_writedat function in the R package r4ss")
  writeLines(paste("#C should work with SS version:",datlist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # write the contents
  wl("styr")
  wl("endyr")
  wl("nseas")
  writeLines(paste(paste(datlist$months_per_seas,collapse=" "),"#_months_per_seas"))
  wl("spawn_seas")
  wl("Nfleet")
  wl("Nsurveys")
  wl("N_areas")
  writeLines(paste(paste(datlist$fleetnames,collapse="%"),"#_fleetnames"))
  writeLines(paste(paste(datlist$surveytiming,collapse=" "),"#_surveytiming_in_season"))
  writeLines(paste(paste(datlist$areas,collapse=" "),"#_area_assignments_for_each_fishery_and_survey"))
  writeLines(paste(paste(datlist$units_of_catch,collapse=" "),"#_units of catch:  1=bio; 2=num"))
  writeLines(paste(paste(datlist$se_log_catch,collapse=" "),"#_se of log(catch) only used for init_eq_catch and for Fmethod 2 and 3"))
  wl("Ngenders")
  wl("Nages")
  writeLines(paste(paste(datlist$init_equil,collapse=" "),"#_init_equil_catch_for_each_fishery"))
  wl("N_catch",comment="#_N_lines_of_catch_to_read")
  if(!is.null(datlist$catch)) printdf(datlist$catch)

  # write index info
  wl("N_cpue")
  if(datlist$N_cpue>0){
    cat("#_Units:  0=numbers; 1=biomass; 2=F\n")
    cat("#_Errtype:  -1=normal; 0=lognormal; >0=T\n")
    cat("#_Fleet Units Errtype\n")
    printdf(datlist$CPUEinfo)
    printdf(datlist$CPUE)
  }
  # wl("discard_units")
  wl("N_discard_fleets")
  writeLines("#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)")
  writeLines("#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal")
  if(!is.null(datlist$discard_fleet_info)) printdf(datlist$discard_fleet_info)
  wl("N_discard")
  if(!is.null(datlist$discard_data)) printdf(datlist$discard_data)
  wl("N_meanbodywt")
  wl("DF_for_meanbodywt", comment="#_DF_for_meanbodywt_T-distribution_like")
  if(!is.null(datlist$meanbodywt)) printdf(datlist$meanbodywt)

  # write length and age comps
  # length data
  wl("lbin_method",comment="# length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector")
  if(datlist$lbin_method==2){
    wl("binwidth",comment="# binwidth for population size comp")
    wl("minimum_size",comment="# minimum size in the population (lower edge of first bin and size at age 0.00)")
    wl("maximum_size",comment="# maximum size in the population (lower edge of last bin)")
  }
  if(datlist$lbin_method==3){
    wl("N_lbinspop")
    writeLines("#_lbin_vector_pop")
    writeLines(paste(datlist$lbin_vector_pop,collapse=" "))
  }
  wl("comp_tail_compression")
  wl("add_to_comp")
  wl("max_combined_lbin",comment="#_combine males into females at or below this bin number")
  wl("N_lbins")
  writeLines("#_lbin_vector")
  writeLines(paste(datlist$lbin_vector,collapse=" "))
  wl("N_lencomp",comment="#_N_Length_comp_observations")
  if(!is.null(datlist$lencomp)) printdf(datlist$lencomp)
  wl("N_agebins")
  writeLines("#_agebin_vector")
  writeLines(paste(datlist$agebin_vector,collapse=" "))
  wl("N_ageerror_definitions")
  if(!is.null(datlist$ageerror)) printdf(datlist$ageerror)
  wl("N_agecomp")
  wl("Lbin_method", comment="#_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths")
  wl("max_combined_age", comment="#_combine males into females at or below this bin number")
  if(!is.null(datlist$agecomp)) printdf(datlist$agecomp)
  wl("N_MeanSize_at_Age_obs")
  #    datlist$MeanSize_at_Age_obs2 <- matrix(datlist$N_MeanSize_at_Age_obs)
  if(!is.null(datlist$MeanSize_at_Age)) printdf(datlist$MeanSize_at_Age_obs)
  wl("N_environ_variables")
  wl("N_environ_obs")
  if(!is.null(datlist$envdat)) printdf(datlist$envdat)

  # write generalized size frequency data
  if(is.null(datlist$N_sizefreq_methods))datlist$N_sizefreq_methods<-0
  wl("N_sizefreq_methods")

  writeLines(paste(paste(datlist$nbins_per_method,collapse=" "),"#_nbins_per_method"))

  writeLines(paste(paste(datlist$units_per_method,collapse=" "),"#_units_per_method"))

  writeLines(paste(paste(datlist$scale_per_method,collapse=" "),"#_scale_per_method"))

  writeLines(paste(paste(datlist$mincomp_per_method,collapse=" "),"#_mincomp_per_method"))

  writeLines(paste(paste(datlist$Nobs_per_method,collapse=" "),"#_Nobs_per_method"))
  writeLines("#_Sizefreq bins")
#  wl("size_freq_bins_list")
  writeLines("#_sizefreq_bins_list")
  lapply(datlist$sizefreq_bins_list,FUN=function(line){writeLines(paste(line,collapse=" "))})

#  writeLines("#_Year season Fleet Gender Partition SampleSize <data> ")

  # write tagging data
  lapply(datlist$sizefreq_data_list,printdf)
  wl("do_tags")
  if(datlist$do_tags != 0){
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    if(!is.null(datlist$tag_releases)) printdf(datlist$tag_releases)
    if(!is.null(datlist$tag_recaps)) printdf(datlist$tag_recaps)
  }
  if(is.null(datlist$morphcomp_data))datlist$morphcomp_data<-0
  wl("morphcomp_data")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth,max.print=oldmax.print)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
