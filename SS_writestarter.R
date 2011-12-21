SS_writestarter <- function(mylist, dir=NULL, file="starter.ss",
                            overwrite=F, verbose=T){
  if(verbose) cat("running SS_writestarter\n")
  if(mylist$type!="Stock_Synthesis_starter_file"){
    stop("input 'mylist' should be a list with $type=='Stock_Synthesis_starter_file'\n")
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

  writeLines("#C starter file written by R function SS_writestarter")
  writeLines("#C rerun model to get more complete formatting in starter.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # strings for control and data file names
  wl("datfile")
  wl("ctlfile")

  wl("init_values_src")
  wl("run_display_detail")
  wl("detailed_age_structure")
  wl("checkup")
  wl("parmtrace")
  wl("cumreport")
  wl("prior_like")
  wl("soft_bounds")
  wl("N_bootstraps")
  wl("last_estimation_phase")
  wl("MCMCburn")
  wl("MCMCthin")
  wl("jitter_fraction")
  wl("minyr_sdreport")
  wl("maxyr_sdreport")
  wl("N_STD_yrs")
  if(mylist$N_STD_yrs>0){
    wl("STD_yr_vec")
  }
  wl("converge_criterion")
  wl("retro_yr")
  wl("min_age_summary_bio")
  wl("depl_basis")
  wl("depl_denom_frac")
  wl("SPR_basis")
  wl("F_report_units")
  if(mylist$F_report_units==4){
    cat(mylist[["F_age_range"]],"#_F_age_range\n")
  }    
  wl("F_report_basis")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
