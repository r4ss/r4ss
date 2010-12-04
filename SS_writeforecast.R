SS_writeforecast <-  function(mylist, dir=NULL, file="forecast.ss",
                              overwrite=F, verbose=T){
  # function to write Stock Synthesis forecast files
  if(verbose) cat("running SS_writeforecast",quote=F)

  if(mylist$type!="Stock_Synthesis_forecast_file"){
    cat("input 'mylist' should be a list with $type=='Stock_Synthesis_forecast_file'",quote=F)
    return()
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  outfile <- paste(dir,file,sep="/")
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=T to replace\n"))
    }else{
      cat("overwriting file:",outfile,"\n")
      file.remove(outfile)
    }
  }else{
    cat("writing new file:",outfile,"\n")
  }

  # preliminary setup
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
    # simple function to clean up many repeated commands
    value = mylist[names(mylist)==name]
    writeLines(paste(value," #_",name,sep=""),con=zz)
  }
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    cat(dataframe, row.names=F, strip.white=T)
  }

  writeLines("#C forecast file written by R function SS_writeforecast")
  writeLines("#C rerun model to get more complete formatting in forecast.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  wl("Forecast")
  wl("first_selex_yr")
  wl("last_selex_yr")
  wl("benchmarks")
  wl("MSY")
  wl("SPRtarget")
  wl("Btarget")
  wl("Nforecastyrs")
  wl("AdvancedOptions")
  if(mylist$AdvancedOptions==1){ # go through 10 advanced options
    wl("RebuilderOutput")
    wl("Ydecl")
    wl("Yinit")
    wl("ControlRuleMethod")
    wl("BforconstantF")
    wl("BfornoF")
    wl("Flimitfraction")
    wl("MaxCatchBasis")
    wl("ImplementError")
    wl("ErrorStdDev")
  }else{
    writeLines("# 10 advanced options go here")
  }# end advanced options

  wl("FleetAllocation")
  if(mylist$FleetAllocation==2) wl("FleetAllocationVec")
  wl("Ncatch")
  # forcast catch levels
  if(mylist$Ncatch>0){
    wl("InputBasis")
    printdf(mylist$ForeCatch)
  }
  wl("ForeCatch")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
