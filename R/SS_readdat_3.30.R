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
#' @author Ian G. Taylor, Yukio Takeuchi, Z. Teresa A'mar, Chris J. Grandin,
#' Kelli F. Johnson, Chantel R. Wetzel
#' @export
#' @importFrom utils type.convert
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
  if(length(dat) < 20){
    warning("Data file appears to be empty or incomplete.\n",
            "  If this is data.ss_new, change starter file to have\n",
            "  nonzero value for 'Number of datafiles to produce'")
    return()
  }

  ###############################################################################
  ## Divide data file into sections ----
  sec.end.inds <- grep("^999\\b", dat)
  ## Run some checks to ensure that only the section break 999's are being captured 
  ## and not things such as year 999 or a catch of 999mt etc
  incorr.secs <- NULL
  for(i in 1:length(sec.end.inds)){
    check_section <- dat[sec.end.inds[i]]
    check_section <- strsplit(check_section,"#")[[1]][1]
    check_section <- unlist(strsplit(unlist(strsplit(check_section,"\t"))," "))
    check_section <- check_section[check_section!=""]
    if(length(check_section)>1){
      incorr.secs <- c(incorr.secs,i)
    }
  }
  if(length(incorr.secs)>0){
    sec.end.inds <- sec.end.inds[-incorr.secs]
  }
  
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
  ###############################################################################
  ## Internally defined functions ----
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
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df <- utils::type.convert(df, as.is = TRUE)
    return(df)
  }

  ###############################################################################
  ## Set up the data lines for parsing ----
  ## Remove any preceeding whitespace on all lines.
  dat <- gsub("^[[:blank:]]+", "", dat)
  ## Remove all comments.
  dat <- gsub("#.*", "", dat)
  ## Remove trailing whitespace on all lines
  dat <- gsub("[[:blank:]]+$", "", dat)
  ## Remove blank lines.
  dat <- dat[dat != ""]
  datlist <- list()
  datlist$sourcefile <- file
  datlist$type <- "Stock_Synthesis_data_file"
  datlist$ReadVersion <- "3.30"
  if (verbose){
    message("SS_readdat_3.30 - read version = ", datlist$ReadVersion)
  }

  ##############################################################################
  ## Get general model information ----
  ind <- 1
  datlist$styr <- get.val(dat, ind)
  datlist$endyr <- get.val(dat, ind)
  datlist$nseas <- get.val(dat, ind)
  datlist$months_per_seas <- get.vec(dat, ind)
  datlist$Nsubseasons <- get.val(dat, ind)
  datlist$spawn_month <- get.val(dat, ind)
  datlist$Ngenders <- get.val(dat, ind)
  datlist$Nages <- get.val(dat, ind)
  datlist$N_areas <- get.val(dat, ind)
  datlist$Nfleets <- get.val(dat, ind)

  ###############################################################################
  ## Fleet data ----
  datlist$fleetinfo <- get.df(dat, ind, datlist$Nfleets)
  colnames(datlist$fleetinfo) <- c("type",
                             "surveytiming",
                             "area",
                             "units",
                             "need_catch_mult",
                             "fleetname")
  if(echoall){
    message("Fleet information:")
    print(datlist$fleetinfo)
  }

  datlist$fleetnames <- datlist$fleetinfo$fleetname
  datlist$surveytiming <- as.numeric(datlist$fleetinfo$surveytiming)
  datlist$units_of_catch <- as.numeric(datlist$fleetinfo$units)
  datlist$areas <- as.numeric(datlist$fleetinfo$area)
  
  ##############################################################################
  ### Bycatch Data (only added for fleets with type = 2) ----
  if(any(datlist$fleetinfo$type == 2)) {
    nbycatch <- length(datlist$fleetinfo$type[datlist$fleetinfo$type == 2])
    datlist$bycatch_fleet_info <- get.df(dat, ind, nbycatch)
  
    colnames(datlist$bycatch_fleet_info) <- c("fleetindex",
                                        "includeinMSY",
                                        "Fmult",
                                        "F_or_first_year",
                                        "F_or_last_year",
                                        "unused"
                                        )
  # add a fleetname column, as in fleet info.
    datlist$bycatch_fleet_info <-cbind(datlist$bycatch_fleet_info, 
                                 datlist$fleetinfo[datlist$fleetinfo$type == 2, "fleetname", drop = FALSE])
  }
  ###############################################################################
  ## Catch data ----
  datlist$catch <- get.df(dat, ind)
  colnames(datlist$catch) <- c("year", 
                         "seas", 
                         "fleet", 
                         "catch", 
                         "catch_se")

  ###############################################################################
  ## CPUE data  ----
  datlist$CPUEinfo <- get.df(dat, ind, datlist$Nfleets)
  # note SD_Report column wasn't present in early 3.30 versions of SS
  CPUEinfo_names <- c("Fleet", "Units", "Errtype", "SD_Report")
  colnames(datlist$CPUEinfo) <- CPUEinfo_names[1:ncol(datlist$CPUEinfo)]
  rownames(datlist$CPUEinfo) <- datlist$fleetnames
  if(echoall){
    message("CPUE information:")
    print(datlist$CPUEinfo)
  }

  ## CPUE data matrix
  CPUE <- get.df(dat, ind)
  if(!is.null(CPUE)){
    datlist$CPUE <- CPUE
    colnames(datlist$CPUE) <- c("year", "seas", "index", "obs", "se_log")
  }else{
    datlist$CPUE <- NULL
  }
  if (echoall) {
    message("CPUE data:")
    print(datlist[["CPUE"]])
  }

  ###############################################################################
  ## Discard data ----
  ## fleet.nums.with.catch is defined in the catch section above.
  datlist$N_discard_fleets <- get.val(dat, ind)
  if(datlist$N_discard_fleets){
    ## Discard info data
    datlist$discard_fleet_info <- get.df(dat, ind, datlist$N_discard_fleets)
    colnames(datlist$discard_fleet_info) <- c("Fleet", "Units", "Errtype")
    rownames(datlist$discard_fleet_info) <-
      datlist$fleetnames[as.numeric(datlist$discard_fleet_info$Fleet)]

    ## Discard data
    datlist$discard_data <- get.df(dat, ind)
    colnames(datlist$discard_data) <- c("Yr", "Seas", "Flt", "Discard", "Std_in")
  }else{
    datlist$discard_fleet_info <- NULL
    datlist$discard_data <- NULL
  }
  if (echoall & !is.null(datlist$discard_fleet_info)) {
    message("Discard fleet information:")
    print(datlist$discard_fleet_info)
  }
  if (echoall & !is.null(datlist$discard_data)) {
    message("Discard data:")
    print(datlist$discard_data)

  }

  ###############################################################################
  ## Mean body weight data ----
  datlist$use_meanbodywt <- get.val(dat, ind)
  if(datlist$use_meanbodywt){
    datlist$DF_for_meanbodywt <- get.val(dat, ind)
    datlist$meanbodywt <- get.df(dat, ind)
    colnames(datlist$meanbodywt) <- c("Year", "Seas", "Fleet", "Partition", "Type",
                                "Value", "Std_in")
  }else{
    datlist$DF_for_meanbodywt <- NULL
    datlist$meanbodywt <- NULL
  }
  if (verbose) {
    message("use_meanbodywt (0/1): ", datlist$use_meanbodywt)
  }
  if (echoall & !is.null(datlist$meanbodywt)) {
    message("meanbodywt:")
    print(datlist$meanbodywt)
  }

  ###############################################################################
  ## Population size structure - Length ----
  datlist$lbin_method <- get.val(dat, ind)
  if(datlist$lbin_method == 2){
    bin_info_tmp <- get.val(dat, ind)
    # if as.numeric doesn't match, probably has 3 values on one line
    if(is.na(bin_info_tmp)){
      ind <- ind - 1 # reset index to allow read of that value again
      bin_info_tmp <- get.vec(dat, ind)
      datlist$binwidth <- bin_info_tmp[1]
      datlist$minimum_size <- bin_info_tmp[2]
      datlist$maximum_size <- bin_info_tmp[3]
    }else{
      # otherwise, more likely separate lines
      datlist$binwidth <- bin_info_tmp
      datlist$minimum_size <- get.val(dat, ind)
      datlist$maximum_size <- get.val(dat, ind)
    }
  }else if(datlist$lbin_method == 3){
    datlist$N_lbinspop <- get.val(dat, ind)
    datlist$lbin_vector_pop <- get.vec(dat, ind)
  }else{
    datlist$binwidth <- NULL
    datlist$minimum_size <- NULL
    datlist$maximum_size <- NULL
    datlist$N_lbinspop <- NULL
    datlist$lbin_vector_pop <- NULL
  }
  if (verbose) {
    message("N_lbinspop: ", datlist$N_lbinspop)
  }
  ## Length Comp information matrix (new for 3.30)
  datlist$use_lencomp <- get.val(dat, ind)
  if (verbose) {
    message("use_lencomp (0/1): ", datlist$use_lencomp)
  }
  # only read all the stuff related to length comps if switch above = 1
  if(datlist$use_lencomp){
    # note: minsamplesize column not present in early 3.30 versions of SS
    datlist$len_info <- get.df(dat, ind, datlist$Nfleets)
    colnames(datlist$len_info) <- c("mintailcomp", "addtocomp", "combine_M_F",
                              "CompressBins", "CompError", "ParmSelect",
                              "minsamplesize")[1:ncol(datlist$len_info)]
    rownames(datlist$len_info) <- datlist$fleetnames
    if (echoall) {
      message("\nlen_info:")
      print(datlist$len_info)
    }
    ## Length comp data
    datlist$N_lbins <- get.val(dat, ind)
    if (verbose) {
      message("N_lbins: ", datlist$N_lbins)
    }
    datlist$lbin_vector <- get.vec(dat, ind)
    datlist$lencomp <- get.df(dat, ind)
    if(!is.null(datlist$lencomp)){
      colnames(datlist$lencomp) <-
        c("Yr", "Seas", "FltSvy", "Gender", "Part", "Nsamp",
          if(abs(datlist$Ngenders) == 1){paste0("l", datlist$lbin_vector)}else{NULL},
          if(datlist$Ngenders > 1){c(paste0("f", datlist$lbin_vector),
                             paste0("m", datlist$lbin_vector))}else{NULL})
    }
    # echo values
    if (echoall) {
      message("\nFirst 2 rows of lencomp:")
      print(head(datlist$lencomp, 2))
      message("\nLast 2 rows of lencomp:")
      print(tail(datlist$lencomp, 2))
      cat("\n")
    }
  }

  ###############################################################################
  ## Population size structure - Age ----
  datlist$N_agebins <- get.val(dat, ind)
  if (verbose) {
    message("N_agebins: ", datlist$N_agebins)
  }
  if (datlist$N_agebins){
    datlist$agebin_vector <- get.vec(dat, ind)
    if (echoall) {
      message("agebin_vector:")
      print(datlist$agebin_vector)
    }
  }else{
    datlist$agebin_vector <- NULL
  }

  ## Age error data
  if (datlist$N_agebins) {
    datlist$N_ageerror_definitions <- get.val(dat, ind)
    if(datlist$N_ageerror_definitions){
      datlist$ageerror <- get.df(dat, ind, datlist$N_ageerror_definitions * 2)
      colnames(datlist$ageerror) <- paste0("age", 0:datlist$Nages)
    }else{
      datlist$ageerror <- NULL
    }
    # echo values
    if (echoall) {
      message("\nN_ageerror_definitions:")
      print(datlist$N_ageerror_definitions)
      message("\nageerror:")
      print(datlist$ageerror)
    }
  }


  ###############################################################################
  ## Age Comp information matrix ----
  if(datlist$N_agebins){
    datlist$age_info <- get.df(dat, ind, datlist$Nfleets)
    # note: minsamplesize column not present in early 3.30 versions of SS
    colnames(datlist$age_info) <- c("mintailcomp",
                              "addtocomp",
                              "combine_M_F",
                              "CompressBins",
                              "CompError",
                              "ParmSelect",
                              "minsamplesize")[1:ncol(datlist$age_info)]
    rownames(datlist$age_info) <- datlist$fleetnames
    ## Length bin method for age data
    # note that Lbin_method below is related to the interpretation of
    # conditional age-at-length data and differs from lbin_method for the length
    # data read above
    datlist$Lbin_method <- get.val(dat, ind)
    if (echoall) {
      message("\nage_info:")
      print(datlist$age_info)
    }
  }

  ###############################################################################
  ## Age comp matrix ----
  if(datlist$N_agebins){
    datlist$agecomp <- get.df(dat, ind)
    if(!is.null(datlist$agecomp)){
      colnames(datlist$agecomp) <-
        c("Yr", "Seas", "FltSvy", "Gender",
          "Part", "Ageerr", "Lbin_lo", "Lbin_hi", "Nsamp",
          if(abs(datlist$Ngenders) == 1){paste0("a", datlist$agebin_vector)}else{NULL},
          if(datlist$Ngenders > 1){c(paste0("f", datlist$agebin_vector),
                               paste0("m", datlist$agebin_vector))}else{NULL})
    }
    # echo values
    if (echoall) {
      message("\nFirst 2 rows of agecomp:")
      print(head(datlist$agecomp, 2))
      message("\nLast 2 rows of agecomp:")
      print(tail(datlist$agecomp, 2))
      cat("\n")
    }
  }else{
    if(verbose){
      message("N_agebins = 0, skipping read remaining age-related stuff")
    }
  }

  ###############################################################################
  ## Mean size-at-age data ----
  datlist$use_MeanSize_at_Age_obs <- get.val(dat, ind)
  if (verbose) {
    message("use_MeanSize_at_Age_obs (0/1): ", datlist$use_MeanSize_at_Age_obs)
  }
  if(datlist$use_MeanSize_at_Age_obs){
    ind.tmp <- ind # save current position in case necessary to re-read
    endmwa <- ind - 2 + grep("-9999", dat[ind:length(dat)])[1]
    xx <- dat[ind:endmwa]
    if (length(unique(sapply(strsplit(xx, "\\s+"), length))) > 1) {
      if(verbose){
        message("Format of MeanSize_at_Age_obs appears to have sample sizes\n",
                "on separate lines than other inputs.")
      }
     xx <- paste(xx[1:length(xx) %% 2 == 1], xx[1:length(xx) %% 2 == 0])
    }
    datlist$MeanSize_at_Age_obs <- data.frame(do.call("rbind", strsplit(xx, "\\s+")),
      stringsAsFactors = FALSE)
    ind <- endmwa + 1
      # check terminator row
      test <- get.vec(dat,ind)
      if(test[1] != -9999){
        warning("Problem with read of MeanSize_at_Age, terminator value != -9999")
      }

    colnames(datlist$MeanSize_at_Age_obs) <-
      c("Yr", "Seas", "FltSvy", "Gender", "Part", "AgeErr", "Ignore",
        if(abs(datlist$Ngenders) == 1){paste0("a", datlist$agebin_vector)}else{NULL},
        if(datlist$Ngenders > 1){c(paste0("f", datlist$agebin_vector),
                             paste0("m", datlist$agebin_vector))}else{NULL},
        if(abs(datlist$Ngenders) == 1){paste0("N_a", datlist$agebin_vector)}else{NULL},
        if(datlist$Ngenders > 1){c(paste0("N_f", datlist$agebin_vector),
                             paste0("N_m", datlist$agebin_vector))}else{NULL})
    # echo values
    if (echoall) {
      message("\nFirst 2 rows of MeanSize_at_Age_obs:")
      print(head(datlist$MeanSize_at_Age_obs, 2))
      message("\nLast 2 rows of MeanSize_at_Age_obs:")
      print(tail(datlist$MeanSize_at_Age_obs, 2))
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
    datlist$MeanSize_at_Age_obs <- NULL
  }

  ###############################################################################
  ## Environment variables ----
  datlist$N_environ_variables <- get.val(dat, ind)

  if (verbose) {
    message("N_environ_variables: ", datlist$N_environ_variables)
  }

  if(datlist$N_environ_variables){
    datlist$envdat <- get.df(dat, ind)
    colnames(datlist$envdat) <- c("Yr", "Variable", "Value")

    # echo values
    if (echoall) {
      message("\nFirst 2 rows of envdat:")
      print(head(datlist$envdat, 2))
      message("\nLast 2 rows of envdat:")
      print(tail(datlist$envdat, 2))
      cat("\n")
    }
  }else{
    datlist$envdat <- NULL
  }

  ###############################################################################
  ## Size frequency methods ----
  datlist$N_sizefreq_methods <- get.val(dat, ind)
  if(datlist$N_sizefreq_methods){
    ## Get details of generalized size frequency methods
    datlist$nbins_per_method <- get.vec(dat, ind)
    datlist$units_per_method <- get.vec(dat, ind)
    datlist$scale_per_method <- get.vec(dat, ind)
    datlist$mincomp_per_method <- get.vec(dat, ind)
    datlist$Nobs_per_method <- get.vec(dat, ind)
    if(echoall){
      message("Details of generalized size frequency methods:")
      print(data.frame(method  = 1:datlist$N_sizefreq_methods,
                       nbins   = datlist$nbins_per_method,
                       units   = datlist$units_per_method,
                       scale   = datlist$scale_per_method,
                       mincomp = datlist$mincomp_per_method,
                       nobs    = datlist$Nobs_per_method))
    }
    ## get list of bin vectors
    datlist$sizefreq_bins_list <- list()
    for(imethod in seq_len(datlist$N_sizefreq_methods)) {
      datlist$sizefreq_bins_list[[imethod]] <- get.vec(dat, ind)
    }
    ## Read generalized size frequency data
    datlist$sizefreq_data_list <- list()
    for(imethod in seq_len(datlist$N_sizefreq_methods)) {
      Ncols <- 7 + abs(datlist$Ngenders) * datlist$nbins_per_method[imethod]
      Nrows <- datlist$Nobs_per_method[imethod]
      datlist$sizefreq_data_list[[imethod]] <- get.df(dat, ind, Nrows)
      colnames(datlist$sizefreq_data_list[[imethod]]) <-
        c("Method", "Yr", "Seas", "FltSvy",
          "Gender", "Part", "Nsamp",
          if(abs(datlist$Ngenders) == 1){
            paste0("a", datlist$sizefreq_bins_list[[imethod]])
          }else{
            NULL
          },
          if(datlist$Ngenders > 1){
            c(paste0("f", datlist$sizefreq_bins_list[[imethod]]),
              paste0("m", datlist$sizefreq_bins_list[[imethod]]))
          }else{
            NULL
          })
      if(echoall){
        message("Method ", imethod, " (first two rows, ten columns):")
        print(datlist$sizefreq_data_list[[imethod]][1:min(Nrows,2), 1:min(Ncols, 10)])
      }
      if(any(datlist$sizefreq_data_list[[imethod]][, "Method"] != imethod)) {
        stop("Problem with method in size frequency data:\n",
             "Expecting method: ", imethod, "\n",
             "Read method(s): ",
             paste(unique(datlist$sizefreq_data_list$Method), collapse = ", "))
      }
    }
  }else{
    datlist$nbins_per_method   <- NULL
    datlist$units_per_method   <- NULL
    datlist$scale_per_method   <- NULL
    datlist$mincomp_per_method <- NULL
    datlist$Nobs_per_method    <- NULL
    datlist$sizefreq_bins_list <- NULL
    datlist$sizefreq_data_list <- NULL
  }

  ###############################################################################
  ## Tag data ----
  datlist$do_tags <- get.val(dat, ind)
  if(datlist$do_tags){
    datlist$N_tag_groups <- get.val(dat, ind)
    datlist$N_recap_events <- get.val(dat, ind)
    datlist$mixing_latency_period <- get.val(dat, ind)
    datlist$max_periods <- get.val(dat, ind)
    ## Read tag release data
    if(datlist$N_tag_groups > 0){
      Ncols <- 8
      datlist$tag_releases <- get.df(dat, ind, datlist$N_tag_groups)
      colnames(datlist$tag_releases) <- c("TG", "Area", "Yr", "Season",
                                    "tfill", "Gender", "Age", "Nrelease")
      if(echoall){
        message("Head of tag release data:")
        print(head(datlist$tag_releases))
      }
    }else{
      datlist$tag_releases <- NULL
    }
    ## Read tag recapture data
    if(datlist$N_recap_events > 0){
      Ncols <- 5
      datlist$tag_recaps <- get.df(dat, ind, datlist$N_recap_events)
      colnames(datlist$tag_recaps) <- c("TG", "Yr", "Season", "Fleet", "Nrecap")
      if(echoall){
        message("Head of tag recapture data:")
        print(head(datlist$tag_recaps))
      }
    }else{
      datlist$tag_recaps <- NULL
    }
  }

  ###############################################################################
  ## Morphometrics composition data ----
  datlist$morphcomp_data <- get.val(dat, ind)
  if(datlist$morphcomp_data){
    warning("Morph comp data not yet supported by SS_readdat_3.30\n",
            "  Please post issue to https://github.com/r4ss/r4ss/issues\n",
            "  or email ian.taylor@noaa.gov",
            "if you want this functionality added.")
  }
  
  ###############################################################################
  ## Selectivity priors ----
  datlist$use_selectivity_priors <- get.val(dat, ind)

  ###############################################################################
  ## End of file ----
  eof <- get.val(dat, ind)
  if(verbose){
    if(Nsections == 1){
      message("Read of data file complete. Final value = ", eof)
    }else{
      message("Read of section ", section,
              " of data file complete. Final value = ", eof)
    }
  }
  datlist$eof <- FALSE
  if(eof==999)datlist$eof <- TRUE
  
  ###############################################################################
  ## Fixes pulled in from SS_readdat wrapper
  ##
  ## Note from IGT 27-March-2020:
  ## Many of the list elements created below are related to the format of SSv3.24
  ## data files and should likely be deprecated at some point in the future

  
  datlist$spawn_seas <- datlist$spawn_month
  # compatibility: get the old number values
  datlist$Nfleet <- nrow(subset(datlist$fleetinfo,datlist$fleetinfo$type<=2))
  datlist$Nsurveys <- datlist$Nfleets-datlist$Nfleet
  totfleets<-datlist$Nfleet+datlist$Nsurveys
  # Note that using NROW on datlist$CPUE that is NA will return 1, when in 
  # reality the number of years should be 0.
  datlist$N_cpue <- ifelse(is.null(datlist[["CPUE"]]) || 
                             (is.null(dim(datlist[["CPUE"]])) && is.na(datlist[["CPUE"]])), 
                           0,
                           NROW(datlist[["CPUE"]]))
  # fleet details
  if(nrow(datlist$fleetinfo) > 1){
    # if more than 1 fleet in the model, create legacy format tables associated with 3.24
    datlist$fleetinfo1 <- t(datlist$fleetinfo)
    colnames(datlist$fleetinfo1) <- datlist$fleetinfo$fleetname
    datlist$fleetinfo1 <- datlist$fleetinfo1[1:5,]
    datlist$fleetinfo2 <- datlist$fleetinfo1[4:5,]
    datlist$fleetinfo1 <- datlist$fleetinfo1[c(2:3,1),]
    rownames(datlist$fleetinfo1) <- c("surveytiming","areas","type")
    datlist$fleetinfo1 <- data.frame(datlist$fleetinfo1)  # convert all to numeric
    datlist$fleetinfo2 <- data.frame(datlist$fleetinfo2)  # convert all to numeric
  }else{
    # if only 1 fleet, skip those things rather than revise code to work with vectors
    # instead of transposed matrices of values
    datlist$fleetinfo1 <- NULL
    datlist$fleetinfo2 <- NULL
  }
  if(!is.null(datlist$discard_fleet_info)){
    colnames(datlist$discard_fleet_info) <- c("Fleet","units","errtype")
  }
  # compatibility: create the old format catch matrix
  datlist$catch <- datlist$catch[datlist$catch[, 1] >= -999, ]
  colnames(datlist$catch) = c("year", "seas", "fleet", "catch", "catch_se")
  # mean body weight
  if(datlist$use_meanbodywt==0)
  {
    datlist$N_meanbodywt <- 0
  }
  # length info
  datlist$comp_tail_compression <- datlist$len_info$mintailcomp
  datlist$add_to_comp <- datlist$len_info$addtocomp
  datlist$max_combined_lbin <- datlist$len_info$combine_M_F
  if(is.null(datlist$lencomp))datlist$N_lencomp <- 0
  if(datlist$use_MeanSize_at_Age_obs==0)
  {
    datlist$N_MeanSize_at_Age_obs <- 0
  }
  ##!!! need to add fixes to pop len bins? (see 3.24)
  # fix some things
  if(!is.null(datlist$lbin_method))
  {
    if(datlist$lbin_method==1) # same as data bins
    {
      datlist$N_lbinspop <- datlist$N_lbins
      datlist$lbin_vector_pop <- datlist$lbin_vector
    }
    
    if(datlist$lbin_method==2) # defined wid, min, max
    {
      if(!is.null(datlist$binwidth) &&
         !is.null(datlist$minimum_size) &&
         !is.null(datlist$maximum_size))
      {
        datlist$N_lbinspop <- (datlist$maximum_size-datlist$minimum_size)/datlist$binwidth+1
        datlist$lbin_vector_pop <- vector()
        for(j in 0:datlist$N_lbinspop)
        {
          datlist$lbin_vector_pop <- c(datlist$lbin_vector_pop,
                                       datlist$minimum_size + (j*datlist$binwidth))
        }
      }
    }
    if(datlist$lbin_method==3) # vector
    {
      if(!is.null(datlist$lbin_vector_pop))
      {
        datlist$N_lbinspop <- length(datlist$lbin_vector_pop)
      }
    }
  }
  
  return(datlist)
}
