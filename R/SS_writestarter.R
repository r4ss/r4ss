#' write starter file
#'
#' write Stock Synthesis starter file from list object in R which was probably
#' created using \code{\link{SS_readstarter}}
#'
#'
#' @param mylist List object created by \code{\link{SS_readstarter}}.
#' @param dir Directory for new starter file. Default=NULL (working directory).
#' @param file Filename for new starter file. Default="starter.ss".
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param warn Print warning if overwriting file?
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_readctl}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},
#' \code{\link{SS_writectl}}
SS_writestarter <- function(mylist, dir=NULL, file="starter.ss",
                            overwrite=FALSE, verbose=TRUE, warn=TRUE){
  if(verbose) cat("running SS_writestarter\n")
  if(mylist$type!="Stock_Synthesis_starter_file"){
    stop("input 'mylist' should be a list with $type=='Stock_Synthesis_starter_file'\n")
  }
  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  if(grepl("/$", dir)) {
    outfile <- paste0(dir, file) # bc trailing backslash
  } else {
    outfile <- paste(dir,file,sep="/")
  }
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=TRUE to replace\n"))
    }else{
      if(warn) {cat("overwriting file:",outfile,"\n")}
      file.remove(outfile)
    }
  }else{
    if(verbose)cat("writing new file:",outfile,"\n")
  }

  # record current max characters per line and then expand in case of long lines
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)

  # simple function to clean up many repeated commands
  # writes the content of an R object, followed by the object name with "#_" in front
  wl <- function(name){
    value = mylist[names(mylist)==name]
    writeLines(paste0(value," #_",name),con=zz)
  }

  writeLines("#C starter file written by R function SS_writestarter")
  writeLines("#C rerun model to get more complete formatting in starter.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # strings for control and data file names
  wl("datfile")
  wl("ctlfile")

  # lots of single numerical values
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
  # only write ALK_tolerance if this is SSv3.30 (value didn't exist in 3.24)
  if(mylist$final==3.3){
    wl("ALK_tolerance")
  }
  writeLines("#")
  wl("final")

  # restore printing width to whatever the user had before
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
