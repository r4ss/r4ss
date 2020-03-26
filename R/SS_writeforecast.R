#' write forecast file
#'
#' write Stock Synthesis forecast file from list object in R which was probably
#' created using \code{\link{SS_readforecast}}
#'
#'
#' @param mylist List object created by \code{\link{SS_readforecast}}.
#' @param dir Directory for new forecast file. Default=NULL (working
#' directory).
#' @param file Filename for new forecast file. Default="forecast.ss".
#' @param writeAll Should the function continue even if Forecast=0
#' (at which point SS stops reading, and remaining elements in list may not be
#' available, depending on settings used in SS_readforecast)
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readdat}},
#' \code{\link{SS_writestarter}}, \code{\link{SS_writedat}}
SS_writeforecast <-  function(mylist, dir=NULL, file="forecast.ss",
                              writeAll=FALSE, overwrite=FALSE, verbose=TRUE){
  # function to write Stock Synthesis forecast files
  if(verbose) cat("running SS_writeforecast\n")
  
  if(!is.list(mylist) || mylist$type!="Stock_Synthesis_forecast_file"){
    stop("input 'mylist' should be a list with $type=='Stock_Synthesis_forecast_file'")
  }
  
  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})
  
  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  outfile <- paste(dir,file,sep="/")
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=TRUE to replace\n"))
    }else{
      if(verbose) cat("overwriting file:",outfile,"\n")
      file.remove(outfile)
    }
  }else{
    if(verbose) cat("writing new file:",outfile,"\n")
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
    if(is.character(dataframe))dataframe<-mylist[names(mylist)==dataframe][[1]]
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print.data.frame(dataframe, row.names=FALSE, strip.white=TRUE)
  }
  
  SSversion <- mylist$SSversion
  
  writeLines("#C forecast file written by R function SS_writeforecast")
  writeLines("#C rerun model to get more complete formatting in forecast.ss_new")
  writeLines(paste("#C should work with SS version:",SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")
  
  wl("benchmarks")
  wl("MSY")
  wl("SPRtarget")
  wl("Btarget")
  if(SSversion==3.24){
    writeLines("#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_alloc, end_alloc")
  }else{
    writeLines("#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF,  beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)")
  }
  writeLines(paste(paste(mylist$Bmark_years,collapse=" ")))
  wl("Bmark_relF_Basis")
  wl("Forecast")
  

  if(mylist$Forecast > 0 | writeAll) {
    if(mylist[["Forecast"]] <= 0 & is.null(mylist[["eof"]])) {
      # only continue beyond this point if Forecast is not 0 or writeAll==TRUE,
      # so dont do other processing.
      warning("Even though writeAll == TRUE, cannot write past list element ", 
              "Forecast because inputs past this are not available. Output ", 
              "still be a usable SS forecast file.")

     } else {
      wl("Nforecastyrs")
      wl("F_scalar")
      if(SSversion==3.24){
        writeLines("#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF")
        writeLines(paste(paste(mylist$Fcast_years,collapse=" ")))
      }else{
        writeLines("#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF, beg_recruits, end_recruits (enter actual year, or values of 0 or -integer to be rel. endyr)")
        writeLines(paste(paste(mylist$Fcast_years,collapse=" ")))
        wl("Fcast_selex")
      }
      wl("ControlRuleMethod")
      wl("BforconstantF")
      wl("BfornoF")
      wl("Flimitfraction")
      if (mylist$Flimitfraction < 0) {
        printdf("Flimitfraction_m")
      }
      wl("N_forecast_loops")
    
      wl("First_forecast_loop_with_stochastic_recruitment")
      wl("Forecast_loop_control_3")
      wl("Forecast_loop_control_4")
      wl("Forecast_loop_control_5")
      wl("FirstYear_for_caps_and_allocations")
      wl("stddev_of_log_catch_ratio")
      wl("Do_West_Coast_gfish_rebuilder_output")
      wl("Ydecl")
      wl("Yinit")
      wl("fleet_relative_F")
      writeLines("# Note that fleet allocation is used directly as average F if Do_Forecast=4 ")
      wl("basis_for_fcast_catch_tuning")
      
      # fleet and area-specific inputs for version 3.24
      if(SSversion==3.24){
        
        # write relative F values by fleet
        if(mylist$fleet_relative_F==2){
          writeLines("#_vals_fleet_relative_f")
          writeLines(paste(mylist$vals_fleet_relative_f,collapse=" "))
        }
        
        writeLines("# max totalcatch by fleet (-1 to have no max)")
        writeLines(paste(mylist$max_totalcatch_by_fleet,collapse=" "))
        writeLines("# max totalcatch by area (-1 to have no max)")
        writeLines(paste(mylist$max_totalcatch_by_area,collapse=" "))
        writeLines("# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)")
        writeLines(paste(mylist$fleet_assignment_to_allocation_group,collapse=" "))
        if(any(mylist$fleet_assignment_to_allocation_group!=0)){
          writeLines(paste("# allocation fraction for each of:",mylist$N_allocation_groups," allocation groups"))
          writeLines(paste(mylist$allocation_among_groups,collapse=" "))
        }
        wl("Ncatch")
        wl("InputBasis")
        if(mylist$Ncatch>0){
          printdf(mylist$ForeCatch)
        }
      }
      # fleet and area-specific inputs for version 3.30
      if(SSversion=="3.30" | SSversion==3.3){
        
        # write relative F values by fleet
        if(mylist$fleet_relative_F==2){
          writeLines("#_vals_fleet_relative_f")
          printdf(mylist$vals_fleet_relative_f)
          writeLines("-9999 0 0")
        }
        
        writeLines("# enter list of fleet number and max for fleets with max annual catch; terminate with fleet=-9999")
        if(!is.null(mylist$max_totalcatch_by_fleet)){
          printdf(mylist$max_totalcatch_by_fleet)
        }
        writeLines("-9999 -1")
        writeLines("# enter list of area ID and max annual catch; terminate with area=-9999")
        if(!is.null(mylist$max_totalcatch_by_area)){
          printdf(mylist$max_totalcatch_by_area)
        }
        writeLines("-9999 -1")
        writeLines("# enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999")
        if(!is.null(mylist$fleet_assignment_to_allocation_group)){
          printdf(mylist$fleet_assignment_to_allocation_group)
        }
        writeLines("-9999 -1")
        if(mylist$N_allocation_groups>0){
          printdf(mylist$allocation_among_groups)
          writeLines(paste0(c("-9999",rep(-1,mylist$N_allocation_groups)),
                            collapse=" "))
        }
        wl("InputBasis")
        if(!is.null(mylist$ForeCatch) && nrow(mylist$ForeCatch > 0)){
          printdf(mylist$ForeCatch)
        }
        if(mylist$InputBasis==-1){
          writeLines("-9999 0 0 0 0")
        }else{
          writeLines("-9999 0 0 0")
        }
      }
    }
  }

  writeLines("#")
  writeLines("999 # verify end of input ")
  
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
