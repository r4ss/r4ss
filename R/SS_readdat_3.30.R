#' read data file from SS version 3.30
#'
#' Read Stock Synthesis (version 3.30) data file into list object in R.
#' This function was formerly called SS_readdat. That name is now used
#' for a wrapper function that calls either SS_readdat_3.24 or SS_readdat_3.30
#' (and potentially additional functions in the future).
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
#' will read input data, (equivalent to section=1).
#' @author Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar, Chris J. Grandin
#' @export
#' @seealso \code{\link{SS_readdat}}, \code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}
SS_readdat_3.30 <-
  function(file, verbose = TRUE, echoall = FALSE, section = NULL){
  if (verbose){
    message("Running SS_readdat_3.30")
  }

  if (echoall){
    message("Echoing blocks of data as it's being read")
    if (!verbose){
      message("Changing input 'verbose' to TRUE")
      verbose <- TRUE
    }
  }
  dat <- readLines(file, warn = FALSE)

  ###############################################################################
  sec.end.inds <- grep("^999$", dat)
  Nsections <- length(sec.end.inds)
  if(!Nsections){
    stop("Error - There was no EOF marker (999) in the data file.")
  }
  if(is.null(section)){
    if(Nsections > 1){
      message("The supplied data file has ", Nsections,
              ifelse(Nsections == 1, " section. ", " sections. "),
              " Using section = 1.")
    }
    section <- 1
  }
  if(!section %in% 1:Nsections){
    if(Nsections == 1){
      stop("The 'section' input must be 1 for this data file.\n")
    }else{
      stop("The 'section' input must be between 1 and ", Nsections,
           " for this data file.\n")
    }
  }
  if(!is.null(section)){
    start <- 1
    end <- sec.end.inds[section]
    if(section > 1){
      start <- sec.end.inds[section - 1] + 1
    }
    dat <- dat[start:end]
  }

  ## Retrieve start time
  ## tmp <- strsplit(dat[grep("Start_time", dat)], " ")[[1]]
  ## start.time <- tmp[grep("^[0-9]{2}:[0-9]{2}:[0-9]+{2}$", tmp)]

  find.index <- function(dat, ind, str){
    ## Find the first line at position ind or later that
    ## contains the string str and return the index of that
    ## line. If the end of the data is reached, an error
    ## will be shown.
    while(ind < length(dat) & !length(grep(str, dat[ind]))){
      ind <- ind + 1
    }
    if(ind == length(dat)){
      stop("SS_readdat_3.30-find.index: Error - ",
           "the value of ", str, " was not found. ",
           "Check the data file and make sure all ",
           "data frames are correctly formed.\n")
    }
    ind
  }

  get.val <- function(dat, ind){
    ## Returns the next numeric value in the dat vector.
    ## Increments ind in the parent environment.
    assign("ind", ind + 1, parent.frame())
    as.numeric(dat[ind])
  }

  get.vec <- function(dat, ind){
    ## Returns the next vector of numbers in the dat vector.
    ## Increments ind in the parent environment.
    assign("ind", ind + 1, parent.frame())
    ## Split by whitespace and collapse (+).
    vec <- strsplit(dat[ind], "[[:blank:]]+")
    as.numeric(vec[[1]])
  }

  get.df <- function(dat, ind, nrow = NULL){
    ## Returns the next data frame in the dat vector.
    ## If nrow is NULL, the function will:
    ##  1. search for the next line starting with -9999.
    ##  2. ind will be incremented in the parent frame
    ##     to 1 past the end of the data frame.
    ##  3. if the ind line starts with -9999,
    ##     the function will return NULL
    ## If nrow is not NULL, the function will:
    ##  1. will return a data frame with rows from
    ##     ind to ind + nrow - 1.
    ##  2. ind will be incremented in the parent frame
    ##     to 1 past the end of the data frame.
    if(is.null(nrow)){
      end.ind <- find.index(dat, ind, "-9999")
      assign("ind", end.ind + 1, parent.frame())
      if(ind != end.ind){
        df <- dat[ind:(end.ind - 1)]
      }else{
        return(NULL)
      }
    }else{
      df <- dat[ind:(ind + nrow - 1)]
      assign("ind", ind + nrow, parent.frame())
    }
    df <- strsplit(df, "[[:blank:]]+") ## Split by whitespace and collapse (+)
    df <- as.list(df)                  ## Must be a list for the next operation
    df <- do.call("rbind", df)         ## Make it into a dataframe
    as.data.frame(df, stringsAsFactors = FALSE)
  }

  ###############################################################################
  ## Set up the data lines for parsing
  ## Remove any preceeding whitespace on all lines.
  dat <- gsub("^[[:blank:]]+", "", dat)
  ## Remove all comments.
  dat <- gsub("#.*", "", dat)
  ## Remove trailing whitespace on all lines
  dat <- gsub("[[:blank:]]+$", "", dat)
  ## Remove blank lines.
  dat <- dat[dat != ""]

  d <- list()
  d$sourcefile <- file
  d$type <- "Stock_Synthesis_data_file"
  d$ReadVersion <- "3.30"
  if (verbose){
    message("SS_readdat_3.30 - read version = ", d$ReadVersion)
  }

  ##############################################################################
  ## Get general model information
  ind <- 1
  d$styr <- get.val(dat, ind)
  d$endyr <- get.val(dat, ind)
  d$nseas <- get.val(dat, ind)
  d$months_per_seas <- get.vec(dat, ind)
  d$Nsubseasons <- get.val(dat, ind)
  d$spawn_month <- get.val(dat, ind)
  d$Nsexes <- get.val(dat, ind)
  d$Nages <- get.val(dat, ind)
  d$Nareas <- get.val(dat, ind)
  d$Nfleets <- get.val(dat, ind)

  ###############################################################################
  ## Fleet data
  d$fleetinfo <- get.df(dat, ind, d$Nfleets)
  colnames(d$fleetinfo) <- c("type",
                             "surveytiming",
                             "area",
                             "units",
                             "need_catch_mult",
                             "fleetname")
  if(echoall){
    message("Fleet information:")
    print(d$fleetinfo)
  }

  d$fleetnames <- d$fleetinfo$fleetname
  d$surveytiming <- as.numeric(d$fleetinfo$surveytiming)
  d$units_of_catch <- as.numeric(d$fleetinfo$units)
  d$areas <- as.numeric(d$fleetinfo$area)

  ## ## For backwards compatability add the fleetinfo1 data frame
  ## d$fleetinfo1 <- as.data.frame(do.call("rbind", list(d$fleetinfo$surveytiming,
  ##                                                     d$fleetinfo$areas)))
  ## d$fleetinfo1 <- cbind(d$fleetinfo1, c("#_surveytiming", "#_areas"))
  ## rownames(d$fleetinfo1) <- c("surveytiming", "areas")
  ## colnames(d$fleetinfo1) <- c(d$fleetinfo$fleetname, "input")

  ###############################################################################
  ## Catch data
  d$catch <- get.df(dat, ind)

  #### NOTE:
  #### code below was used to reformat the table of
  #### catch from the 3.30 format to 3.24 format
  #### for simplicity, keeping it in the 3.30 format
  #### will be easier going forward

  ## c.df <- get.df(dat, ind)
  ##
  ## ## Reform catch matrix so each fleet has its own column
  ## ## Make a list of data frames, one for each fleet
  ## c.list <- split(c.df, c.df[,3])
  ##
  ## ## Extract the SE in log(catch) and initial equilibrium values for each fleet
  ## se.and.initeq <-
  ##   lapply(c.list,
  ##          function(x){se.fleet <- as.numeric(x[1, 5])
  ##                      init.eq <- ifelse(as.numeric(x[1, 1]) == -999, 1, 0)
  ##                      fleet.num <- x[1,3]
  ##                      se <- data.frame(c(fleet.num, se.fleet, init.eq),
  ##                                       stringsAsFactors = FALSE)
  ##                      colnames(se) <- d$fleetnames[as.numeric(x[1,3])]
  ##                      se})
  ## se.and.initeq <- do.call("cbind", se.and.initeq)
  ## fleet.nums.with.catch <- as.numeric(se.and.initeq[1,])
  ## d$se_log_catch <- as.numeric(se.and.initeq[2,])
  ## d$init_equil <- as.numeric(se.and.initeq[3,])
  ##
  ## ## Remove all instances of -999 from all catch data frames
  ## c.list <- lapply(c.list, function(x){x[x[,1] != -999,]})
  ##
  ## ## Make the catch data frame
  ## ## Extract the years from the first dataframe in the list
  ## yrs <- as.numeric(c.list[[1]][,1])
  ## ## Apply fleet name to the catch column for each dataframe
  ## c.list <- lapply(c.list,
  ##                  function(x){colnames(x)[4] <- d$fleetnames[as.numeric(x[1,3])]
  ##                              x})
  ## ## Get rid of unwanted columns.
  ## c.list <- lapply(c.list,
  ##                  function(x){x[, -c(1,2,3,5), drop = FALSE]})
  ## ## Reform the dataframe
  ## c.list <- do.call("cbind", c.list)
  ## d$catch <- cbind(yrs, c.list)
  ## if(echoall){
  ##   message("Final years of input catch:")
  ##   print(tail(d$catch))
  ## }
  ##
  ## ## For backwards compatability add the fleetinfo2 data frame
  ## ## This comes after the catch matrix because the SE is read from there.
  ## d$fleetinfo2 <- do.call("rbind",
  ##                         list(d$units_of_catch[fleet.nums.with.catch],
  ##                              as.numeric(d$se_log_catch)))
  ## d$fleetinfo2 <- data.frame(d$fleetinfo2, c("#_units_of_catch", "#_se_log_catch"))
  ## rownames(d$fleetinfo2) <- c("units_of_catch", "se_log_catch")
  ## colnames(d$fleetinfo2) <- c(colnames(se.and.initeq), "input")

  ###############################################################################
  ## CPUE data
  d$CPUEinfo <- get.df(dat, ind, d$Nfleets)
  # note SD_Report column wasn't present in early 3.30 versions of SS
  CPUEinfo_names <- c("Fleet", "Units", "Errtype", "SD_Report")
  colnames(d$CPUEinfo) <- CPUEinfo_names[1:ncol(d$CPUEinfo)]
  rownames(d$CPUEinfo) <- d$fleetnames
  if(echoall){
    message("CPUE information:")
    print(d$CPUEinfo)
  }

  ## CPUE data matrix
  d$CPUE <- get.df(dat, ind)
  colnames(d$CPUE) <- c("year", "seas", "index", "obs", "se_log")
  if (echoall) {
    message("CPUE data:")
    print(d$CPUE)
  }

  ###############################################################################
  ## Discard data
  ## fleet.nums.with.catch is defined in the catch section above.
  d$N_discard_fleets <- get.val(dat, ind)
  if(d$N_discard_fleets){
    ## Discard info data
    d$discard_fleet_info <- get.df(dat, ind, d$N_discard_fleets)
    colnames(d$discard_fleet_info) <- c("Fleet", "Units", "Errtype")
    rownames(d$discard_fleet_info) <-
      d$fleetnames[as.numeric(d$discard_fleet_info$Fleet)]

    ## Discard data
    d$discard_data <- get.df(dat, ind)
    colnames(d$discard_data) <- c("Yr", "Seas", "Flt", "Discard", "Std_in")
  }else{
    d$discard_fleet_info <- NULL
    d$discard_data <- NULL
  }
  if (echoall & !is.null(d$discard_fleet_info)) {
    message("Discard fleet information:")
    print(d$discard_fleet_info)
  }
  if (echoall & !is.null(d$discard_data)) {
    message("Discard data:")
    print(d$discard_data)

  }


  ###############################################################################
  ## Mean body weight data
  d$use_meanbodywt <- get.val(dat, ind)
  if(d$use_meanbodywt){
    d$DF_for_meanbodywt <- get.val(dat, ind)
    d$meanbodywt <- get.df(dat, ind)
    colnames(d$meanbodywt) <- c("Year", "Seas", "Type",
                                "Partition", "Value", "CV")
  }else{
    d$DF_for_meanbodywt <- NULL
    d$meanbodywt <- NULL
  }
  if (verbose) {
    message("use_meanbodywt (0/1): ", d$use_meanbodywt)
  }
  if (echoall & !is.null(d$meanbodywt)) {
    message("meanbodywt:")
    print(d$meanbodywt)
  }


  ###############################################################################
  ## Population size structure - Length
  d$lbin_method <- get.val(dat, ind)
  if(d$lbin_method == 2){
    bin_info_tmp <- get.val(dat, ind)
    # if as.numeric doesn't match, probably has 3 values on one line
    if(is.na(bin_info_tmp)){
      ind <- ind - 1 # reset index to allow read of that value again
      bin_info_tmp <- get.vec(dat, ind)
      d$binwidth <- bin_info_tmp[1]
      d$minimum_size <- bin_info_tmp[2]
      d$maximum_size <- bin_info_tmp[3]
    }else{
      # otherwise, more likely separate lines
      d$binwidth <- bin_info_tmp
      d$minimum_size <- get.val(dat, ind)
      d$maximum_size <- get.val(dat, ind)
    }
  }else if(d$lbin_method == 3){
    d$N_lbinspop <- get.val(dat, ind)
    d$lbin_vector_pop <- get.vec(dat, ind)
  }else{
    d$binwidth <- NULL
    d$minimum_size <- NULL
    d$maximum_size <- NULL
    d$N_lbinspop <- NULL
    d$lbin_vector_pop <- NULL
  }
  if (verbose) {
    message("N_lbinspop: ", d$N_lbinspop)
  }

  ## Length Comp information matrix (new for 3.30)
  d$use_lencomp <- get.val(dat, ind)
  if (verbose) {
    message("use_lencomp (0/1): ", d$use_lencomp)
  }

  # only read all the stuff related to length comps if switch above = 1
  if(d$use_lencomp){
    # note: minsamplesize column not present in early 3.30 versions of SS
    d$len_info <- get.df(dat, ind, d$Nfleets)
    colnames(d$len_info) <- c("mintailcomp", "addtocomp", "combine_M_F",
                              "CompressBins", "CompError", "ParmSelect",
                              "minsamplesize")[1:ncol(d$len_info)]

    rownames(d$len_info) <- d$fleetnames

    if (echoall) {
      message("\nlen_info:")
      print(d$len_info)
    }

    ## Length comp data
    d$N_lbins <- get.val(dat, ind)
    if (verbose) {
      message("N_lbins: ", d$N_lbins)
    }
    d$lbin_vector <- get.vec(dat, ind)
    d$lencomp <- get.df(dat, ind)
    if(!is.null(d$lencomp)){
      colnames(d$lencomp) <-
        c("Yr", "Seas", "FltSvy", "Gender", "Part", "Nsamp",
          if(d$Nsexes == 1){paste0("l", d$lbin_vector)}else{NULL},
          if(d$Nsexes > 1){c(paste0("f", d$lbin_vector),
                             paste0("m", d$lbin_vector))}else{NULL})
    }

    # echo values
    if (echoall) {
      message("\nFirst 2 rows of lencomp:")
      print(head(d$lencomp, 2))
      message("\nLast 2 rows of lencomp:")
      print(tail(d$lencomp, 2))
      cat("\n")
    }
  }

  ###############################################################################
  ## Population size structure - Age
  d$N_agebins <- get.val(dat, ind)
  if (verbose) {
    message("N_agebins: ", d$N_agebins)
  }
  if (d$N_agebins){
    d$agebin_vector <- get.vec(dat, ind)
    if (echoall) {
      message("agebin_vector:")
      print(d$agebin_vector)
    }
  }else{
    d$agebin_vector <- NULL
  }

  ## Age error data
  if (d$N_agebins) {
    d$N_ageerror_definitions <- get.val(dat, ind)
    if(d$N_ageerror_definitions){
      d$ageerror <- get.df(dat, ind, d$N_ageerror_definitions * 2)
      colnames(d$ageerror) <- paste0("age", 0:d$Nages)
    }else{
      d$ageerror <- NULL
    }
    # echo values
    if (echoall) {
      message("\nN_ageerror_definitions:")
      print(d$N_ageerror_definitions)
      message("\nageerror:")
      print(d$ageerror)
    }
  }


  ###############################################################################
  ## Age Comp information matrix
  if(d$N_agebins){
    d$age_info <- get.df(dat, ind, d$Nfleets)
    # note: minsamplesize column not present in early 3.30 versions of SS
    colnames(d$age_info) <- c("mintailcomp",
                              "addtocomp",
                              "combine_M_F",
                              "CompressBins",
                              "CompError",
                              "ParmSelect",
                              "minsamplesize")[1:ncol(d$age_info)]

    rownames(d$age_info) <- d$fleetnames
    ## Length bin method
    d$Lbin_method <- get.val(dat, ind)
    if (echoall) {
      message("\nage_info:")
      print(d$age_info)
    }
  }

  ###############################################################################
  ## Age comp matrix
  if(d$N_agebins){
    d$agecomp <- get.df(dat, ind)
    if(!is.null(d$agecomp)){
      colnames(d$agecomp) <-
        c("Yr", "Seas", "FltSvy", "Gender",
          "Part", "Ageerr", "Lbin_lo", "Lbin_hi", "Nsamp",
          if(d$Nsexes == 1){paste0("a", d$agebin_vector)}else{NULL},
          if(d$Nsexes > 1){c(paste0("f", d$agebin_vector),
                               paste0("m", d$agebin_vector))}else{NULL})
    }

    # echo values
    if (echoall) {
      message("\nFirst 2 rows of agecomp:")
      print(head(d$agecomp, 2))
      message("\nLast 2 rows of agecomp:")
      print(tail(d$agecomp, 2))
      cat("\n")
    }
  }else{
    if(verbose){
      message("N_agebins = 0, skipping read remaining age-related stuff")
    }
  }

  ###############################################################################
  ## Mean size-at-age data
  d$use_MeanSize_at_Age_obs <- get.val(dat, ind)
  if (verbose) {
    message("use_MeanSize_at_Age_obs (0/1): ", d$use_MeanSize_at_Age_obs)
  }
  if(d$use_MeanSize_at_Age_obs){
    ind.tmp <- ind # save current position in case necessary to re-read
    d$MeanSize_at_Age_obs <- get.df(dat, ind)

    # extra code in case sample sizes are on a separate line from other inputs
    # first check if gender input is outside of normal range
    if(!all(as.numeric(d$MeanSize_at_Age_obs$V4) %in% 0:3)){
      if(verbose){
        message("Format of MeanSize_at_Age_obs appears to have sample sizes",
                "on separate lines than other inputs.")
      }
      ind <- ind.tmp # reset index to value prior to first attempt to read table
      N_MeanSize_at_Age_obs <- nrow(d$MeanSize_at_Age_obs)/2
      MeanSize_at_Age_obs <- NULL
      for(iobs in 1:N_MeanSize_at_Age_obs){
        # each observation is a combination of pairs of adjacent rows
        MeanSize_at_Age_obs <- rbind(MeanSize_at_Age_obs,
                                     c(get.vec(dat,ind), get.vec(dat,ind)))
      }
      # check terminator row
      test <- get.vec(dat,ind)
      if(test[1] != -9999){
        warning("Problem with read of MeanSize_at_Age, terminator value != -9999")
      }
      d$MeanSize_at_Age_obs <- as.data.frame(MeanSize_at_Age_obs)
    }


    colnames(d$MeanSize_at_Age_obs) <-
      c("Yr", "Seas", "FltSvy", "Gender", "Part", "AgeErr", "Ignore",
        if(d$Nsexes == 1){paste0("a", d$agebin_vector)}else{NULL},
        if(d$Nsexes > 1){c(paste0("f", d$agebin_vector),
                             paste0("m", d$agebin_vector))}else{NULL},
        if(d$Nsexes == 1){paste0("N_a", d$agebin_vector)}else{NULL},
        if(d$Nsexes > 1){c(paste0("N_f", d$agebin_vector),
                             paste0("N_m", d$agebin_vector))}else{NULL})
    # echo values
    if (echoall) {
      message("\nFirst 2 rows of MeanSize_at_Age_obs:")
      print(head(d$MeanSize_at_Age_obs, 2))
      message("\nLast 2 rows of MeanSize_at_Age_obs:")
      print(tail(d$MeanSize_at_Age_obs, 2))
      cat("\n")
    }
    # The formatting of the mean size at age in data.ss_new has sample sizes
    # on a separate line below the mean size values, and this applies to the
    # -9999 line as well. The lines below is an attempt to work around this
    test <- get.vec(dat, ind)
    # if only 1 value, then this isn't an issue and need to adjust ind
    if(length(test) == 1){
      ind <- ind - 1
    }
  }else{
    d$MeanSize_at_Age_obs <- NULL
  }



  ###############################################################################
  ## Environment variables
  d$N_environ_variables <- get.val(dat, ind)

  if (verbose) {
    message("N_environ_variables: ", d$N_environ_variables)
  }

  if(d$N_environ_variables){
    d$envdat <- get.df(dat, ind)
    colnames(d$envdat) <- c("Yr", "Variable", "Value")

    # echo values
    if (echoall) {
      message("\nFirst 2 rows of envdat:")
      print(head(d$envdat, 2))
      message("\nLast 2 rows of envdat:")
      print(tail(d$envdat, 2))
      cat("\n")
    }
  }else{
    d$envdat <- NULL
  }


  ###############################################################################
  ## Size frequency methods
  d$N_sizefreq_methods <- get.val(dat, ind)
  if(d$N_sizefreq_methods){
    ## Get details of generalized size frequency methods
    d$nbins_per_method <- get.vec(dat, ind)
    d$units_per_method <- get.vec(dat, ind)
    d$scale_per_method <- get.vec(dat, ind)
    d$mincomp_per_method <- get.vec(dat, ind)
    d$Nobs_per_method <- get.vec(dat, ind)
    if(echoall){
      message("Details of generalized size frequency methods:")
      print(data.frame(method  = 1:d$N_sizefreq_methods,
                       nbins   = d$nbins_per_method,
                       units   = d$units_per_method,
                       scale   = d$scale_per_method,
                       mincomp = d$mincomp_per_method,
                       nobs    = d$Nobs_per_method))
    }
    ## get list of bin vectors
    d$sizefreq_bins_list <- list()
    for(imethod in 1:d$N_sizefreq_methods){
      d$sizefreq_bins_list[[imethod]] <- get.vec(dat, ind)
    }
    ## Read generalized size frequency data
    d$sizefreq_data_list <- list()
    for(imethod in 1:d$N_sizefreq_methods){
      Ncols <- 7 + d$Nsexes * d$nbins_per_method[imethod]
      Nrows <- d$Nobs_per_method[imethod]
      d$sizefreq_data_list <- get.df(dat, ind, Nrows)
      colnames(d$sizefreq_data_list) <-
        c("Method", "Yr", "Seas", "FltSvy",
          "Gender", "Part", "Nsamp",
          if(d$Nsexes == 1){
            paste0("a", d$sizefreq_bins_list[[imethod]])
          }else{
            NULL
          },
          if(d$Nsexes > 1){
            c(paste0("f", d$sizefreq_bins_list[[imethod]]),
              paste0("m", d$sizefreq_bins_list[[imethod]]))
          }else{
            NULL
          })
      if(echoall){
        message("Method ", imethod, " (first two rows, ten columns):")
        print(d$sizefreq_data_list[1:min(Nrows,2), 1:min(Ncols, 10)])
      }
      if(any(d$sizefreq_data_list$Method!=imethod)){
        stop("Problem with method in size frequency data:\n",
             "Expecting method: ", imethod, "\n",
             "Read method(s): ",
             paste(unique(d$sizefreq_data_list$Method), collapse = ", "))
      }
    }
  }else{
    d$nbins_per_method   <- NULL
    d$units_per_method   <- NULL
    d$scale_per_method   <- NULL
    d$mincomp_per_method <- NULL
    d$Nobs_per_method    <- NULL
    d$sizefreq_bins_list <- NULL
    d$sizefreq_data_list <- NULL
  }

  ###############################################################################
  ## Tag data
  d$do_tags <- get.val(dat, ind)
  if(d$do_tags){
    d$N_tag_groups <- get.val(dat, ind)
    d$N_recap_events <- get.val(dat, ind)
    d$mixing_latency_period <- get.val(dat, ind)
    d$max_periods <- get.val(dat, ind)
    ## Read tag release data
    if(d$N_tag_groups > 0){
      Ncols <- 8
      d$tag_releases <- get.df(dat, ind, d$N_tag_groups)
      colnames(d$tag_releases) <- c("TG", "Area", "Yr", "Season",
                                    "tfill", "Gender", "Age", "Nrelease")
      if(echoall){
        message("Head of tag release data:")
        print(head(d$tag_releases))
      }
    }else{
      d$tag_releases <- NULL
    }
    ## Read tag recapture data
    if(d$N_recap_events > 0){
      Ncols <- 5
      d$tag_recaps <- get.df(dat, ind, d$N_recap_events)
      colnames(d$tag_recaps) <- c("TG", "Yr", "Season", "Fleet", "Nrecap")
      if(echoall){
        message("Head of tag recapture data:")
        print(head(d$tag_recaps))
      }
    }else{
      d$tag_recaps <- NULL
    }
  }

  ###############################################################################
  ## Morphometrics composition data
  d$morphcomp_data <- get.val(dat, ind)
  if(d$morphcomp_data){
    warning("Morph comp data not yet supported by SS_readdat_3.30\n",
            "  Please post issue to https://github.com/r4ss/r4ss/issues\n",
            "  or email ian.taylor@noaa.gov",
            "if you want this functionality added.")
  }
  ###############################################################################
  ## Selectivity priors
  d$use_selectivity_priors <- get.val(dat, ind)

  ###############################################################################
  eof <- get.val(dat, ind)
  if(verbose){
    if(Nsections == 1){
      message("Read of data file complete. Final value = ", eof)
    }else{
      message("Read of section ", section,
              " of data file complete. Final value = ", eof)
    }
  }

  d$eof <- FALSE
  if(eof==999)d$eof <- TRUE

  return(d)
}
