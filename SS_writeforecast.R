SS_writeforecast <-  function(mylist, dir=NULL, file="forecast.ss", nareas=1, nfleets=1,
                              overwrite=F, verbose=T){
  # function to write Stock Synthesis forecast files
  # updated for SSv3.20 test on 12/6/2010
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

  wl("benchmarks")
  wl("MSY")
  wl("SPRtarget")
  wl("Btarget")
  writeLines("#_Bmark_years: beg_bio end_bio beg_selex end_selex beg_alloc end_alloc")
  writeLines(" 0 0 0 0 0 0")
  wl("Forecast")
  writeLines("0.2 # F scalar (only used for Do_Forecast==5)")
  writeLines("#_Fcast_years:  beg_selex end_selex beg_alloc end_alloc")
  writeLines(paste(first_selex_yr,last_selex_yr,-10,0))
  ## wl("first_selex_yr")
  ## wl("last_selex_yr")
  ## wl("Nforecastyrs")
  ## wl("AdvancedOptions")
  ## if(mylist$AdvancedOptions==1){ # go through 10 advanced options
  wl("ControlRuleMethod")
  wl("BforconstantF")
  wl("BfornoF")
  wl("Flimitfraction")
  writeLines("3 #_First forecast loop with stochastic recruitment (fixed at 3 for now)")
  writeLines("-1 #_Forecast loop control #3 (reserved) ")
  writeLines("2004  #FirstYear for caps and allocations (should be after any fixed inputs) ")
  writeLines(paste(ErrorStdDev,"# stddev of log(realized catch/target catch) in forecast"))
  writeLines(paste(RebuilderOutput,"# Do West Coast gfish rebuilder output (0/1) "))
  writeLines(paste(Ydecl,"# Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)"))
  writeLines(paste(Yinit,"# Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)"))
  writeLines("1 # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below")
  writeLines("# Note that fleet allocation is used directly as average F if Do_Forecast=4 ")
  writeLines("2 # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)")

  ## wl("MaxCatchBasis")
  ## wl("ImplementError")
  ## wl("ErrorStdDev")

  writeLines("# max totalcatch by fleet (-1 to have no max)")
  writeLines(paste(rep(-1,nfleets)))
  writeLines("# max totalcatch by area (-1 to have no max)")
  writeLines(paste(rep(-1,nfleets)))
  writeLines("# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)")
  writeLines(paste(rep(0,nfleets)))
  writeLines("#_Conditional on >1 allocation group")
  writeLines("# allocation fraction for each of: 2 allocation groups")
  writeLines("# 0.7 0.3")
  writeLines("0 # Number of forecast catch levels to input (else calc catch from forecast F) ")
  writeLines("2 # basis for input Fcast catch:  2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)")
  writeLines("# Input fixed catch values")
  writeLines("#Year Seas Fleet Catch(or_F) ")
  writeLines("")
  writeLines("#")
  writeLines("999 # verify end of input ")

  ## wl("FleetAllocation")
  ## if(mylist$FleetAllocation==2) wl("FleetAllocationVec")
  ## wl("Ncatch")
  ## # forcast catch levels
  ## if(mylist$Ncatch>0){
  ##   wl("InputBasis")
  ##   printdf(mylist$ForeCatch)
  ## }
  ## wl("ForeCatch")
  ## writeLines("#")
  ## writeLines("999")
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
