#' read forecast file
#'
#' read Stock Synthesis forecast file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param Nfleets Number of fleets (not required in 3.30).
#' @param Nareas Number of areas (not required in 3.30).
#' @param nseas number of seasons (not required in 3.30).
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
#' @param readAll Should the function continue even if Forecast = 0 or -1
#' (at which point SS stops reading)?
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor + Nathan Vaughan
#' @export
#' @seealso \code{\link{SS_readstarter}}, \code{\link{SS_readdat}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}},

SS_readforecast <-  function(file='forecast.ss', Nfleets=NULL, Nareas=NULL, nseas=NULL,
                                  version="3.30", readAll=FALSE, verbose=TRUE){
  
  # function to read Stock Synthesis forecast files
  if(!(version=="3.24" | version=="3.30" | version==3.3)){
    # turns out 3.30 != "3.30" in R
    stop('version must be either 3.24 or 3.30')
  }
  
  if(version=="3.24"){
    if(is.null(Nfleets) | is.null(Nareas) | is.null(nseas)){
      stop('version 3.24 must include values for Nfleets, Nareas, and nseas. At least one of these is missing')
    }
  }
  
  if(verbose) cat("running SS_readforecast\n")
  dat <- readLines(file,warn=FALSE)
  
  nver=as.numeric(substring(version,1,4))
  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2]," ")[[1]][1]
  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in 1:length(dat)){
    # First split between input and comments
    mysplit <- strsplit(dat[i],split="#")[[1]]
    if(!is.na(mysplit[1]))
    {
      # split along blank spaces
      mysplit <- strsplit(mysplit[1],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      # convert to numeric
      nums <- suppressWarnings(as.numeric(mysplit))
      nums <- nums[!is.na(nums)]
      # append new values to allnums vector
      if(length(nums) > 0){
        allnums <- c(allnums, nums)
      }
    }
  }
  
  # internally used fun definitions ----
  # Function to add vector to forelist
  
  add_vec<-function(forelist,length,name,comments=NULL){
    i<-forelist$'.i'
    dat<-forelist$'.dat'
    forelist$temp<-dat[i+1:length-1]
    forelist$'.i'<-i+length
    if(is.null(comments)){
      names(forelist$temp)<-paste0(paste0("#_",name,"_",collapse=""),1:length)
    }else{
      names(forelist$temp)<-comments
    }
    if(!is.na(name))names(forelist)[names(forelist)=="temp"]<-name
    if(verbose){cat(name,",i=",forelist$'.i',"\n");print(forelist[name])}
    return(forelist)
  }
  
  find.index <- function(dat, ind, str){
    ## Find the first line at position ind or later that
    ## contains the string str and return the index of that
    ## line. If the end of the data is reached, an error
    ## will be shown.
    while(ind < length(dat) & !length(grep(str, dat[ind]))){
      ind <- ind + 1
    }
    if(ind == length(dat)){
      stop("SS_readctl_3.30-find.index: Error - ",
           "the value of ", str, " was not found. ",
           "Check the control file and make sure all ",
           "data frames are correctly formed.\n")
    }
    ind
  }
  
  # Function to add data as data.frame to forelist
  add_df<-function(forelist,nrows=NULL,ncol,col.names,name,comments=NULL){
    i<-forelist$'.i'
    dat<-forelist$'.dat'
    if(is.null(nrows))
    {
      end.ind <- find.index(dat, i, "-9999")
      nrow<-as.integer((end.ind-i)/ncol)
      if(nrow==0) # there isn't any data so just return
      {
        forelist$'.i'<-forelist$'.i'+ncol
        return(forelist)
      }
    }
    else nrow<-nrows
    
    k<-nrow*ncol
    
    df0<-as.data.frame(matrix(dat[i+1:k-1],nrow=nrow,ncol=ncol,byrow=TRUE))
    colnames(df0)<-col.names
    if(is.null(comments)){
      rownames(df0)<-paste0(paste0("#_",name,collapse=""),1:nrow)
    }else{
      rownames(df0)<-comments
    }
    i <- i+k
    
    if(is.null(nrows))i <- i+ncol
    
    forelist$temp<-df0
    forelist$'.i'<-i
    if(!is.na(name))names(forelist)[names(forelist)=="temp"]<-name
    if(verbose){
      cat(name,",i=",forelist$'.i',"\n")
      print(forelist[[which(names(forelist)==name)]])
    }
    return(forelist)
  }
  
  
  ## function to add an element to forelist
  add_elem<-function(forelist=NA,name){
    i<-forelist$'.i'
    dat<-forelist$'.dat'
    forelist$temp<-dat[i]
    forelist$'.i'<-i+1
    if(!is.na(name))names(forelist)[names(forelist)=="temp"]<-name
    if(verbose)cat(name,",i=",forelist$'.i'," ;",forelist[[which(names(forelist)==name)]],"\n")
    return(forelist)
  }
  
  ## function to add list  to forelist
  add_list<-function(forelist=NA,name,length,length_each){
    i<-forelist$'.i'
    dat<-forelist$'.dat'
    forelist$temp<-list()
    for(j in 1:length){
      forelist$temp[[j]]<-dat[i+1:length_each[j]-1]; i <- i+length_each[j]
    }
    forelist$'.i'<-i
    if(!is.null(name))names(forelist)[names(forelist)=="temp"]<-name
    if(verbose)cat(name,",i=",forelist$'.i',"\n")
    return(forelist)
  }
  
  # setup ----
  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  forelist <- list()
  forelist$'.i' <- i
  forelist$'.dat' <- allnums
  forelist$warnings <- ""
  if(!is.null(nseas)){
    forelist$nseas <- as.numeric(nseas)}
  if(!is.null(Nfleets)){
    forelist$Nfleets <- as.numeric(Nfleets)}
  if(!is.null(Nareas)){
    forelist$Nareas <- as.numeric(Nareas)}
  forelist$SSversion <- as.numeric(version)
  forelist$sourcefile <- file
  forelist$type <- "Stock_Synthesis_forecast_file"
  
  forelist<-add_elem(forelist,"benchmarks")
  forelist<-add_elem(forelist,"MSY")
  forelist<-add_elem(forelist,"SPRtarget")
  forelist<-add_elem(forelist,"Btarget")
  if(forelist$SSversion==3.24){
    forelist<-add_vec(forelist,length=6,name="Bmark_years")
  }else{
    forelist<-add_vec(forelist,length=10,name="Bmark_years")
  }
  if(verbose){
    cat("Benchmark years: ", forelist$Bmark_years, "\n")
  }
  forelist<-add_elem(forelist,"Bmark_relF_Basis")
  forelist<-add_elem(forelist,"Forecast")
  if(forelist$Forecast %in% c(0, -1) & !readAll) {
    if(verbose) {
      message("Forecast is ", forelist$Forecast, 
              " and input readAll=FALSE so skipping remainder of file")
    }
  } else if(forelist$Forecast %in% c(0, -1) & readAll & 
           ((is.na(forelist$.dat[forelist$.i]) |
             forelist$.dat[forelist$.i] == 999 ))) {
    # stop reading if forecast 0 or -1 used, and no other lines present 
    # (aside from 999), but readAll = TRUE.
    if(verbose){
      message("Forecast =", forelist$Forecast, "\n")
    }
    warning("readAll selected as TRUE, but lines beyond Forecast are not ", 
            "present in the forecasting file, so skipping remainder of ", 
            "file")
  }else{
    # continue reading forecast
    if(verbose){
      message("Forecast =", forelist$Forecast, "\n")
    }
    forelist<-add_elem(forelist,"Nforecastyrs")
    # check for compatible input with forecast option 1.
    if(forelist[["Forecast"]] == 0 & forelist[["Nforecastyrs"]] != 1) {
      warning("Forecast = 0 should always be used with 1 forecast year. ", 
              "Changing Nforecastyrs to 1. If you would prefer to use 0 years ",
              "of forecast, please use Forecast = -1; if you would like to ",
              " forecast for > 1 year, please select a value of Forecast > 0.")
      forelist[["Nforecastyrs"]] <- 1
    }
    forelist<-add_elem(forelist,"F_scalar")
    if(forelist$SSversion==3.24){
      forelist<-add_vec(forelist,length=4,name="Fcast_years")
    }else{
      forelist<-add_vec(forelist,length=6,name="Fcast_years")
    }
    if(verbose){
      cat("Forecast years: ", forelist$Fcast_years, "\n")
    }
    
    if(version=="3.30" | version==3.3){
      forelist<-add_elem(forelist,"Fcast_selex")
      if(verbose){
        cat("Forecast selectivity option: ", forelist$Fcast_selex, "\n")
      }
    }else{
      forelist$Fcast_selex <- NA
    }
    
    forelist<-add_elem(forelist,"ControlRuleMethod")
    forelist<-add_elem(forelist,"BforconstantF")
    forelist<-add_elem(forelist,"BfornoF")
    forelist<-add_elem(forelist,"Flimitfraction")
    
    if(forelist$Flimitfraction<0){
      forelist<-add_df(forelist,ncol=2,col.names=c("Year","Fraction"),name="Flimitfraction")
    }
    
    forelist<-add_elem(forelist,"N_forecast_loops")
    forelist<-add_elem(forelist,"First_forecast_loop_with_stochastic_recruitment")
    forelist<-add_elem(forelist,"Forecast_loop_control_3")
    forelist<-add_elem(forelist,"Forecast_loop_control_4")
    forelist<-add_elem(forelist,"Forecast_loop_control_5")
    forelist<-add_elem(forelist,"FirstYear_for_caps_and_allocations")
    forelist<-add_elem(forelist,"stddev_of_log_catch_ratio")
    forelist<-add_elem(forelist,"Do_West_Coast_gfish_rebuilder_output")
    forelist<-add_elem(forelist,"Ydecl")
    forelist<-add_elem(forelist,"Yinit")
    forelist<-add_elem(forelist,"fleet_relative_F")
    forelist<-add_elem(forelist,"basis_for_fcast_catch_tuning")
    
    if(version==3.24){
      if(forelist$fleet_relative_F==2){
        forelist<-add_df(forelist,nrows=forelist$nseas,ncol=forelist$Nfleets,col.names=paste0("Fleet ",1:forelist$Nfleets),name="vals_fleet_relative_f")
      }
      forelist<-add_vec(forelist,length=forelist$Nfleets,name="max_totalcatch_by_fleet")
      forelist<-add_vec(forelist,length=forelist$Nareas,name="max_totalcatch_by_area")
      forelist<-add_vec(forelist,length=forelist$Nfleets,name="fleet_assignment_to_allocation_group")
      forelist$N_allocation_groups<-max(forelist$fleet_assignment_to_allocation_group)
      if(forelist$N_allocation_groups>0){
        forelist<-add_vec(forelist,length=forelist$N_allocation_groups,name="allocation_among_groups")
      }else{
        forelist$allocation_among_groups<-NULL
      }
      
      forelist<-add_elem(forelist,"Ncatch")
      forelist<-add_elem(forelist,"InputBasis")
      if(forelist$Ncatch==0){
        forelist$ForeCatch<-NULL
      }else{
        if(forelist$InputBasis==-1){
          forelist<-add_df(forelist,nrows=forelist$Ncatch,ncol=5,col.names=c("Year","Seas","Fleet","Catch or F","Basis"),name="ForeCatch")
        }else{
          forelist<-add_df(forelist,nrows=forelist$Ncatch,ncol=4,col.names=c("Year","Seas","Fleet","Catch or F"),name="ForeCatch")
        }
      }
    }else{
      if(forelist$fleet_relative_F==2){
        forelist<-add_df(forelist,ncol=3,col.names=c("Season","Fleet","Relative F"),name="vals_fleet_relative_f")
      }
      forelist<-add_df(forelist,ncol=2,col.names=c("Fleet","Max Catch"),name="max_totalcatch_by_fleet")
      forelist<-add_df(forelist,ncol=2,col.names=c("Area","Max Catch"),name="max_totalcatch_by_area")
      forelist<-add_df(forelist,ncol=2,col.names=c("Fleet","Group"),name="fleet_assignment_to_allocation_group")
      if(!is.null(forelist$fleet_assignment_to_allocation_group)){
        forelist$N_allocation_groups<-max(forelist$fleet_assignment_to_allocation_group[,2])
        forelist<-add_df(forelist,ncol=(forelist$N_allocation_groups+1),col.names=c("Year",paste0("Group ",1:forelist$N_allocation_groups)),name="allocation_among_groups")
      }else{
        forelist$N_allocation_groups<-0
        forelist$allocation_among_groups<-NULL
      }
      forelist<-add_elem(forelist,"InputBasis")
      if(forelist$InputBasis==-1){
        forelist<-add_df(forelist,ncol=5,col.names=c("Year","Seas","Fleet","Catch or F","Basis"),name="ForeCatch")
      }else{
        forelist<-add_df(forelist,ncol=4,col.names=c("Year","Seas","Fleet","Catch or F"),name="ForeCatch")
      }
    }
    if(forelist$'.dat'[forelist$'.i']==999){
      if(verbose) message("read of forecast file complete (final value = 999)\n")
      forelist$eof <- TRUE
    }else{
      warning("Error: final value is ", forelist$'.dat'[forelist$'.i'], " but ",
              "should be 999\n")
      forelist$eof <- FALSE
    }
  }
  
  forelist$'.dat'<-NULL
  forelist$'.i'<-NULL
  return(forelist) 
}