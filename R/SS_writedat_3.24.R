#' write data file for SS version 3.24
#'
#' Write Stock Synthesis data file from list object in R which was probably
#' created using \code{\link{SS_readdat}} (which would have called on
#' \code{\link{SS_readdat_3.24}}).
#'
#'
#' @param datlist List object created by \code{\link{SS_readdat}}.
#' @param outfile Filename for where to write new data file.
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param faster Speed up writing by writing length and age comps without aligning
#' the columns (by using write.table instead of print.data.frame)
#' @param verbose Should there be verbose output while running the file?
#' @author Ian G. Taylor, Yukio Takeuchi, Gwladys I. Lambert
#' @export
#' @seealso \code{\link{SS_writedat}}, \code{\link{SS_writedat_3.30}},
#' \code{\link{SS_readdat}}, \code{\link{SS_makedatlist}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_readforecast}}, \code{\link{SS_writeforecast}}
#'
SS_writedat_3.24 <- function(datlist,
                             outfile,
                             overwrite = FALSE,
                             faster = FALSE,
                             verbose = TRUE) {
  # function to write Stock Synthesis data files
  if (verbose){
    message("running SS_writedat_3.24")
  }

  # check datlist
  if (datlist$type != "Stock_Synthesis_data_file") {
    stop("input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'")
  }

  # check for existing file
  if (file.exists(outfile)) {
    if (!overwrite) {
      message("File exists and input 'overwrite'=FALSE: ", outfile)
      return()
    } else{
      file.remove(outfile)
    }
  }

  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width = 5000, max.print = 9999999)

  if (verbose){
    message("opening connection to ", outfile)
  }
  zz <- file(outfile, open = "at")
  # sink(zz)
  on.exit({
    close(zz)
    options(width = oldwidth, max.print = oldmax.print)
  })

  # simple function to write a single line
  wl <- function(name, comment = NULL) {
    value = datlist[names(datlist) == name]
    if (is.null(comment)) {
      writeLines(paste(value, " #_", name, sep = "", collapse = "_"), con = zz)
    }else{
      if (length(grep(comment, pattern = "^#")) != 0) {
        writeLines(paste(value, comment), con = zz)
      }else{
        writeLines(paste(value, " #_", comment, sep = "", collapse = "_"),
                   con = zz)
      }
    }
  }

  # function to write a vector
  wl.vector <- function(name,
                        comment = NULL,
                        collapse = NULL) {
    value = datlist[names(datlist) == name][[1]]
    if (is.null(collapse))
      collapse <- " "
    if (is.null(comment)) {
      writeLines(paste(paste(value, collapse = collapse), " #_", name, sep = ""), con =
                 zz)
    }else{
      writeLines(paste(paste(value, collapse = collapse), comment), con = zz)
    }
  }

  # function to write a list
  wl.list <- function(name,
                      comment = NULL,
                      header = NULL) {
    if (!is.null(header)) {
      writeLines(paste0("#_", header), con = zz)
    }
    value = datlist[names(datlist) == name][[1]]
    value1 <- sapply(value,
                     function(x) { paste(paste(x), collapse = " ") },
                     simplify = TRUE)
    writeLines(value1, con = zz)
  }

  # function to print data frame with hash mark before first column name
  printdf <- function(dataframe,
                      header = TRUE,
                      headerLine = NA) {
    if (is.character(dataframe)) {
      tmp <- datlist[names(datlist) == dataframe]
      if (length(tmp) > 0) {
        dataframe <- tmp[[1]]
      }else{
        dataframe <- NULL
      }
    }
    if (!is.null(dataframe)) {
      if (header) {
        names(dataframe)[1] <- paste("#_", names(dataframe)[1], sep = "")
        writeLines(paste(names(dataframe), collapse = "\t"), con = zz)
      }
      if (!is.na(headerLine))
        xxx <- 2
      if (!is.null(rownames(dataframe))) {
        rownames(dataframe) <-
          sapply(rownames(dataframe), function(z) {
            ifelse(length(grep(
                x = z, pattern = "^#"
            )) == 1, z, paste0("#_", z))
          })
        dataframe$comments <- rownames(dataframe)
      }
      if (faster) {
        write.table(
            dataframe,
            file = zz,
            append = TRUE,
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE
        )
      }else{
        gdata::write.fwf(
            file = zz,
            x = dataframe,
            append = TRUE,
            sep = "\t",
            quote = FALSE,
            rownames = FALSE,
            colnames = FALSE,
            digits = 6
        )
      }
    }
  }
  ## Function copied from SS_writectl3.24
  writeComment <- function(text, ...) {
    if (length(grep(x = text, pattern = "^#")) != length(text))
    text <- paste("#_", text, sep = "")
    writeLines(text = text, con = zz, ...)
  }

  # write a header
  writeComment(paste0("#V", datlist$SSversion))
  writeComment("#C data file created using the SS_writedat function in the R package r4ss")
  writeComment(paste("#C should work with SS version:", datlist$SSversion))
  writeComment(paste("#C file write time:", Sys.time()))
  writeComment("#")

  # write the contents
  wl("styr")
  wl("endyr")
  wl("nseas")
  wl.vector("months_per_seas", comment = "#_months_per_seas")
  wl("spawn_seas")
  wl("Nfleet")
  wl("Nsurveys")
  wl("N_areas")
  #writeLines(paste(paste(datlist$fleetnames,collapse="%"),"#_fleetnames"))
  wl.vector("fleetnames", collapse = "%", comment = "#_fleetnames")
  #writeLines(paste(paste(datlist$surveytiming,collapse=" "),"#_surveytiming_in_season"))
  wl.vector("surveytiming", comment = "#_surveytiming_in_season")
  # writelines(paste(paste(datlist$areas,collapse=" "),"#_area_assignments_for_each_fishery_and_survey"))
  wl.vector("areas", comment = "#_area_assignments_for_each_fishery_and_survey")
  # writeLines(paste(paste(datlist$units_of_catch,collapse=" "),"#_units of catch:  1=bio; 2=num"))
  wl.vector("units_of_catch", comment = "#_units of catch:  1=bio; 2=num")
  # writeLines(paste(paste(datlist$se_log_catch,collapse=" "),"#_se of log(catch) only used for init_eq_catch and for Fmethod 2 and 3"))
  wl.vector("se_log_catch", comment = "#_se of log(catch) only used for init_eq_catch and for Fmethod 2 and 3")
  wl("Ngenders")
  wl("Nages")
  # writeLines(paste(paste(datlist$init_equil,collapse=" "),"#_init_equil_catch_for_each_fishery"))
  wl.vector("init_equil", comment = "#_init_equil_catch_for_each_fishery")
  wl("N_catch", comment = "#_N_lines_of_catch_to_read")
  if (!is.null(datlist$catch))
    printdf(datlist$catch)

  # write index info
  wl("N_cpue")
  if (datlist$N_cpue > 0) {
    writeComment("#_Units:  0=numbers; 1=biomass; 2=F\n")
    writeComment("#_Errtype:  -1=normal; 0=lognormal; >0=T\n")
    writeComment("#_Fleet Units Errtype\n")
    printdf(datlist$CPUEinfo)
    printdf(datlist$CPUE)
  }
  # wl("discard_units")
  wl("N_discard_fleets")
  writeComment("#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)")
  writeComment(
      "#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal"
  )
  if (!is.null(datlist$discard_fleet_info))
    printdf(datlist$discard_fleet_info)
  wl("N_discard")
  if (!is.null(datlist$discard_data))
    printdf(datlist$discard_data)
  wl("N_meanbodywt")
  wl("DF_for_meanbodywt", comment = "#_DF_for_meanbodywt_T-distribution_like")
  if (!is.null(datlist$meanbodywt))
    printdf(datlist$meanbodywt)

  # write length and age comps
  # length data
  wl("lbin_method", comment = "# length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector")
  if (datlist$lbin_method == 2) {
    wl("binwidth", comment = "# binwidth for population size comp")
    wl("minimum_size", comment = "# minimum size in the population (lower edge of first bin and size at age 0.00)")
    wl("maximum_size", comment = "# maximum size in the population (lower edge of last bin)")
  }
  if (datlist$lbin_method == 3) {
    wl("N_lbinspop")
    writeComment("#_lbin_vector_pop")
    #   writeLines(paste(datlist$lbin_vector_pop,collapse=" "))
    wl.vector("lbin_vector_pop")
  }
  wl("comp_tail_compression")
  wl("add_to_comp")
  wl("max_combined_lbin", comment = "#_combine males into females at or below this bin number")
  wl("N_lbins")
  writeComment("#_lbin_vector")
  #writeLines(paste(datlist$lbin_vector,collapse=" "))
  wl.vector("lbin_vector")
  wl("N_lencomp", comment = "#_N_Length_comp_observations")
  if (!is.null(datlist$lencomp)) {
    printdf(datlist$lencomp)
  }
  wl("N_agebins")
  if (datlist$N_agebins > 0) {
    writeComment("#_agebin_vector")
    # writeLines(paste(datlist$agebin_vector,collapse=" "))
    #  cat("L232 in SS_writedat\n")
    wl.vector("agebin_vector")
  }
  wl("N_ageerror_definitions")
  if (!is.null(datlist$ageerror))
    printdf(datlist$ageerror)
  wl("N_agecomp")
  wl("Lbin_method", comment = "#_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths")
  wl("max_combined_age", comment = "#_combine males into females at or below this bin number")
  if (!is.null(datlist$agecomp)) {
    printdf(datlist$agecomp)
  }
  wl("N_MeanSize_at_Age_obs")
  #    datlist$MeanSize_at_Age_obs2 <- matrix(datlist$N_MeanSize_at_Age_obs)
  if (!is.null(datlist$MeanSize_at_Age))
    printdf(datlist$MeanSize_at_Age_obs)
  wl("N_environ_variables")
  wl("N_environ_obs")
  if (!is.null(datlist$envdat))
    printdf(datlist$envdat)

  # write generalized size frequency data
  if (is.null(datlist$N_sizefreq_methods))
    datlist$N_sizefreq_methods <- 0
  wl("N_sizefreq_methods")
  if (datlist$N_sizefreq_methods > 0) {
    #  writeLines(paste(paste(datlist$nbins_per_method,collapse=" "),"#_nbins_per_method"))
    wl.vector("nbins_per_method", comment = "#_nbins_per_method")
    #  writeLines(paste(paste(datlist$units_per_method,collapse=" "),"#_units_per_method"))
    wl.vector("units_per_method", comment = "#_units_per_method")
    #  writeLines(paste(paste(datlist$scale_per_method,collapse=" "),"#_scale_per_method"))
    wl.vector("scale_per_method", comment = "#_scale_per_method")
    #  writeLines(paste(paste(datlist$mincomp_per_method,collapse=" "),"#_mincomp_per_method"))
    wl.vector("mincomp_per_method", comment = "#_mincomp_per_method")
    #  writeLines(paste(paste(datlist$Nobs_per_method,collapse=" "),"#_Nobs_per_method"))
    wl.vector("Nobs_per_method", comment = "#_Nobs_per_method")
    writeComment("#_Sizefreq bins")
    #  wl("size_freq_bins_list")
    writeComment("#_sizefreq_bins_list")
    #  lapply(datlist$sizefreq_bins_list,FUN=function(line){writeLines(paste(line,collapse=" "))})
    wl.list("sizefreq_bins_list")
    #  writeLines("#_Year season Fleet Gender Partition SampleSize <data> ")
    lapply(datlist$sizefreq_data_list, printdf)
  }
  # write tagging data
  wl("do_tags")
  if (datlist$do_tags != 0) {
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    if (!is.null(datlist$tag_releases))
      printdf(datlist$tag_releases)
    if (!is.null(datlist$tag_recaps))
      printdf(datlist$tag_recaps)
  }
  if (is.null(datlist$morphcomp_data))
    datlist$morphcomp_data <- 0
  wl("morphcomp_data")
  writeComment("#")
  writeLines("999", con = zz)
  #  options(width=oldwidth,max.print=oldmax.print)
  #  sink()
  #  close(zz)
  if (verbose){
    message("file written to ", outfile)
  }
}
