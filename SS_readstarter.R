SS_readstarter <-  function(file='starter.ss', verbose=TRUE){
  if(verbose) cat("running SS_readstarter\n")
  starter <- readLines(file,warn=F)
  mylist <- list()

  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_starter_file"
  mylist$SSversion <- "SSv3.10b_or_later"

  # get strings for control and data file names
  starter2 <- NULL
  for(i in 1:length(starter)){
      # get only stuff before # marks
      mysplit <- strsplit(starter[i],split="#")[[1]][1]
      if(!is.na(mysplit) && length(mysplit) > 0) starter2 <- c(starter2,mysplit)
  }
  strings <- NULL
  for(i in 1:length(starter2)){
      mysplit <- strsplit(starter2[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      strings <- c(strings,mysplit)
  }
  strings <- strings[is.na(suppressWarnings(as.numeric(strings)))]
  if(length(strings)>2){
      cat("too many strings in starter file?\n")
      print(strings)
      return()
  }else{
      mylist$datfile <- strings[1]
      mylist$ctlfile <- strings[2]
  }

  # get numbers (could be better integrated with function above)
  allnums <- NULL
  for(i in 1:length(starter)){
      # split apart numbers from text in file
      mysplit <- strsplit(starter[i],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      nums <- suppressWarnings(as.numeric(mysplit))
      if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
      else maxcol <- length(nums)
      if(maxcol > 0){
          nums <- nums[1:maxcol]
          allnums <- c(allnums, nums)
      }
  }

  # go through numerical values and save as elements of a big list
  i <- 1

  mylist$init_values_src <- allnums[i]; i <- i+1
  mylist$run_display_detail <- allnums[i]; i <- i+1
  mylist$detailed_age_structure <- allnums[i]; i <- i+1
  mylist$checkup <- allnums[i]; i <- i+1
  mylist$parmtrace <- allnums[i]; i <- i+1
  mylist$cumreport <- allnums[i]; i <- i+1
  mylist$prior_like <- allnums[i]; i <- i+1
  mylist$soft_bounds <- allnums[i]; i <- i+1
  mylist$N_bootstraps <- allnums[i]; i <- i+1
  mylist$last_estimation_phase <- allnums[i]; i <- i+1
  mylist$MCMCburn <- allnums[i]; i <- i+1
  mylist$MCMCthin <- allnums[i]; i <- i+1
  mylist$jitter_fraction <- allnums[i]; i <- i+1
  mylist$minyr_sdreport <- allnums[i]; i <- i+1
  mylist$maxyr_sdreport <- allnums[i]; i <- i+1
  mylist$N_STD_yrs <- N_STD_yrs <- allnums[i]; i <- i+1
  if(N_STD_yrs>0){
      mylist$STD_yr_vec <- allnums[i:(i+N_STD_yrs-1)]; i <- i+N_STD_yrs
  }
  mylist$converge_criterion <- allnums[i]; i <- i+1
  mylist$retro_yr <- allnums[i]; i <- i+1
  mylist$min_age_summary_bio <- allnums[i]; i <- i+1
  mylist$depl_basis <- allnums[i]; i <- i+1
  mylist$depl_denom_frac <- allnums[i]; i <- i+1
  mylist$SPR_basis <- allnums[i]; i <- i+1
  mylist$F_report_units <- allnums[i]; i <- i+1
  mylist$F_age_range <- allnums[i]; i <- i+1
  mylist$F_age_range[2] <- allnums[i]; i <- i+1
  mylist$F_report_basis <- allnums[i]; i <- i+1

  # check final value
  if(allnums[i]==999){
    if(verbose) cat("read of starter file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # all done
  return(mylist)
}
