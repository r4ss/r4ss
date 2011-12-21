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
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
    # simple function to clean up many repeated commands
    value = datlist[names(datlist)==name]
    writeLines(paste(value," #_",name,sep=""),con=zz,)
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
  writeLines(paste(paste(datlist$surveytiming,collapse=" "),"#_surveytiming"))
  writeLines(paste(paste(datlist$areas,collapse=" "),"#_areas"))
  writeLines(paste(paste(datlist$units_of_catch,collapse=" "),"#_units_of_catch"))
  writeLines(paste(paste(datlist$se_log_catch,collapse=" "),"#_se_log_catch"))
  wl("Ngenders")
  wl("Nages")
  writeLines(paste(paste(datlist$init_equil,collapse=" "),"#_init_equil_catch"))
  wl("N_catch")
  if(!is.null(datlist$catch)) printdf(datlist$catch)
  wl("N_cpue")
  if(datlist$N_cpue>0){
    printdf(datlist$CPUEinfo)
    printdf(datlist$CPUE)
  }
  # wl("discard_units")
  wl("N_discard_fleets")
  wl("N_discard")
  if(!is.null(datlist$discard_data)) printdf(datlist$discard_data)
  wl("N_meanbodywt")
  if(!is.null(datlist$meanbodywt)) printdf(datlist$meanbodywt)

  wl("DF_for_meanbodywt")
  
  # length data
  wl("lbin_method")
  if(datlist$lbin_method==2){
    wl("binwidth")
    wl("minimum_size")
    wl("maximum_size")
  }
  if(datlist$lbin_method==3){
    wl("N_lbinspop")
    writeLines("#_lbin_vector_pop")
    writeLines(paste(datlist$lbin_vector_pop,collapse=" "))
  }
  wl("comp_tail_compression")
  wl("add_to_comp")
  wl("max_combined_lbin")
  wl("N_lbins")
  writeLines("#_lbin_vector")
  writeLines(paste(datlist$lbin_vector,collapse=" "))
  wl("N_lencomp")
  if(!is.null(datlist$lencomp)) printdf(datlist$lencomp)
  wl("N_agebins")
  writeLines("#_agebin_vector")
  writeLines(paste(datlist$agebin_vector,collapse=" "))
  wl("N_ageerror_definitions")
  if(!is.null(datlist$ageerror)) printdf(datlist$ageerror)
  wl("N_agecomp")
  wl("Lbin_method")
  wl("max_combined_age")
  if(!is.null(datlist$agecomp)) printdf(datlist$agecomp)
  wl("N_MeanSize_at_Age_obs")
  #    datlist$MeanSize_at_Age_obs2 <- matrix(datlist$N_MeanSize_at_Age_obs)
  if(!is.null(datlist$MeanSize_at_Age)) printdf(datlist$MeanSize_at_Age_obs)
  wl("N_environ_variables")
  wl("N_environ_obs")
  if(!is.null(datlist$envdat)) printdf(datlist$envdat)
  wl("N_sizefreq_methods")
  wl("do_tags")
  if(datlist$do_tags != 0){
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    if(!is.null(datlist$tag_releases)) printdf(datlist$tag_releases)
    if(!is.null(datlist$tag_recaps)) printdf(datlist$tag_recaps)
  }
  wl("morphcomp_data")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth,max.print=oldmax.print)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
