#' write data file for SS version 3.30
#'
#' Write Stock Synthesis data file from list object in R which was probably
#' created using \code{\link{SS_readdat}} (which would have called on
#' \code{\link{SS_readdat_3.30}}).
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
#' @importFrom stats reshape
#' @seealso \code{\link{SS_writedat}}, \code{\link{SS_writedat_3.24}},
#' \code{\link{SS_readdat}}, \code{\link{SS_makedatlist}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_writestarter}},
#' \code{\link{SS_readforecast}}, \code{\link{SS_writeforecast}}
#'
SS_writedat_3.30 <- function(datlist,
                             outfile,
                             overwrite = FALSE,
                             faster = FALSE,
                             verbose = TRUE) {
  # function to write Stock Synthesis data files
  if (verbose){
    message("running SS_writedat_3.30")
  }

  # rename datlist to shorten the code
  d <- datlist

  # check datlist/d
  if (d$type != "Stock_Synthesis_data_file") {
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
    value = d[names(d) == name]
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
    value = d[names(d) == name][[1]]
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
    value = d[names(d) == name][[1]]
    value1 <- sapply(value,
                     function(x) { paste(paste(x), collapse = " ") },
                     simplify = TRUE)
    writeLines(value1, con = zz)
  }

  # function to print data frame with hash mark before first column name
  print.df <- function(dataframe,
                      header = TRUE,
                      headerLine = NA,
                      terminate = TRUE) {
    if (is.character(dataframe)) {
      tmp <- d[names(d) == dataframe]
      if (length(tmp) > 0) {
        dataframe <- tmp[[1]]
      }else{
        dataframe <- NULL
      }
    }
    if (!is.null(dataframe)) {
      if (terminate) {
        # add terminator line to end data frame
        newline <- c(-9999, rep(0, ncol(dataframe)-1))
        dataframe <- rbind(dataframe, newline)
        rownames(dataframe)[nrow(dataframe)] <- "terminator"
      }
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
    if (length(grep(x = text, pattern = "^#")) != length(text)){
      text <- paste("#_", text, sep = "")
    }
    writeLines(text = text, con = zz, ...)
  }

  # write a header
  writeComment(paste0("#V", d$ReadVersion))
  writeComment("#C data file created using the SS_writedat function in the R package r4ss")
  writeComment(paste("#C should work with SS version:", d$SSversion))
  writeComment(paste("#C file write time:", Sys.time()))
  writeComment("#")

  # write the contents
  wl("styr")
  wl("endyr")
  wl("nseas")
  wl.vector("months_per_seas", comment = "#_months_per_seas")
  wl("Nsubseasons")
  wl("spawn_month")
  wl("Nsexes")
  wl("Nages")
  wl("Nareas")
  wl("Nfleets")

  # write table of info on each fleet
  writeComment("#_fleetinfo")
  print.df(d$fleetinfo, terminate=FALSE)

  # write table of catch
  #year season  fleet catch catch_se
  catch.out <- d$catch
  #catch.out <- merge(stats::reshape(d$catch, direction = "long",
  #  idvar = c("year", "seas"),
  #  varying = colnames(d$catch)[(!colnames(d$catch) %in% c("year", "seas"))],
  #  timevar = "fleet",
  #  v.names = "catch",
  #  sep = ""),
  #  data.frame(
  #    "fleet" = 1:length(d$se_log_catch), 
  #    "catch_se" = d$se_log_catch),
  #  all.x = TRUE)
  catch.out <- catch.out[, c("year", "seas", "fleet", "catch", "catch_se")]
  colnames(catch.out) <- gsub("seas$", "season", colnames(catch.out))
  print.df(catch.out)

  # write index info
  writeComment("#_CPUE_and_surveyabundance_observations")
  writeComment("#_Units:  0=numbers; 1=biomass; 2=F; >=30 for special types")
  writeComment("#_Errtype:  -1=normal; 0=lognormal; >0=T")
  writeComment("#_SD_Report: 0=no sdreport; 1=enable sdreport")
  print.df(d$CPUEinfo, terminate=FALSE)

  writeComment("#\n#_CPUE_data")
  print.df(d$CPUE)

  # write discard info
  wl("N_discard_fleets")
  writeComment("#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)")
  writeComment(
      "#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal"
  )

  writeComment("#\n#_discard_fleet_info")
  print.df(d$discard_fleet_info, terminate=FALSE)

  writeComment("#\n#_discard_data")
  print.df(d$discard_data)

  writeComment("#\n#_meanbodywt")
  wl("use_meanbodywt")
  wl("DF_for_meanbodywt", comment = "#_DF_for_meanbodywt_T-distribution_like")
  print.df(d$meanbodywt)

  # write length and age comps
  # population length bins
  writeComment("#\n#_population_length_bins")
  wl("lbin_method", comment = "# length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector")
  if (d$lbin_method == 2) {
    wl("binwidth", comment = "# binwidth for population size comp")
    wl("minimum_size", comment = "# minimum size in the population (lower edge of first bin and size at age 0.00)")
    wl("maximum_size", comment = "# maximum size in the population (lower edge of last bin)")
  }
  if (d$lbin_method == 3) {
    wl("N_lbinspop")
    writeComment("#_lbin_vector_pop")
    wl.vector("lbin_vector_pop")
  }

  wl("use_lencomp")
  # only write further info on length data if used (even if zero rows)
  if(d$use_lencomp){
    # fleet-specific info on length comps
    writeComment("#\n#_len_info")
    print.df(d$len_info, terminate=FALSE)

    # data bins
    wl("N_lbins")
    writeComment("#_lbin_vector")
    wl.vector("lbin_vector")

    # length comps
    writeComment("#\n#_lencomp")
    if(is.null(d$lencomp) & d$use_lencomp==1){
      # empty data.frame with correct number of columns needed for terminator row
      d$lencomp <- data.frame(matrix(vector(), 0, 6 + d$N_lbins * d$Nsexes))
    }
    print.df(d$lencomp)
  }
  # age bins
  wl("N_agebins")
  if (d$N_agebins > 0) {
    writeComment("#\n#_agebin_vector")
    wl.vector("agebin_vector")
  }

  # ageing error
  writeComment("#\n#_ageing_error")
  wl("N_ageerror_definitions")
  print.df(d$ageerror, terminate=FALSE)

  # specification of age comps
  writeComment("#\n#_age_info")
  print.df("age_info", terminate=FALSE)

  wl("Lbin_method", comment = "#_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths")
  wl("max_combined_age", comment = "#_combine males into females at or below this bin number")

  # age comps
  if(is.null(d$agecomp)){
    # empty data.frame with correct number of columns needed for terminator row
    d$agecomp <- data.frame(matrix(vector(), 0, 9 + d$N_agebins * d$Nsexes))
  }
  print.df(d$agecomp)

  writeComment("#\n#_MeanSize_at_Age_obs")
  wl("use_MeanSize_at_Age_obs")
  print.df(d$MeanSize_at_Age_obs)

  wl("N_environ_variables")
  print.df(d$envdat)

  # write generalized size frequency data
  if (is.null(d$N_sizefreq_methods))
    d$N_sizefreq_methods <- 0
  wl("N_sizefreq_methods")
  if (d$N_sizefreq_methods > 0) {
    #  writeLines(paste(paste(d$nbins_per_method,collapse=" "),"#_nbins_per_method"))
    wl.vector("nbins_per_method", comment = "#_nbins_per_method")
    #  writeLines(paste(paste(d$units_per_method,collapse=" "),"#_units_per_method"))
    wl.vector("units_per_method", comment = "#_units_per_method")
    #  writeLines(paste(paste(d$scale_per_method,collapse=" "),"#_scale_per_method"))
    wl.vector("scale_per_method", comment = "#_scale_per_method")
    #  writeLines(paste(paste(d$mincomp_per_method,collapse=" "),"#_mincomp_per_method"))
    wl.vector("mincomp_per_method", comment = "#_mincomp_per_method")
    #  writeLines(paste(paste(d$Nobs_per_method,collapse=" "),"#_Nobs_per_method"))
    wl.vector("Nobs_per_method", comment = "#_Nobs_per_method")
    writeComment("#\n#_Sizefreq bins")
    #  wl("size_freq_bins_list")
    writeComment("#\n#_sizefreq_bins_list")
    #  lapply(d$sizefreq_bins_list,FUN=function(line){writeLines(paste(line,collapse=" "))})
    wl.list("sizefreq_bins_list")
    #  writeLines("#_Year season Fleet Gender Partition SampleSize <data> ")
    lapply(d$sizefreq_data_list, print.df)
  }
  # write tagging data
  wl("do_tags")
  if (d$do_tags != 0) {
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    print.df(d$tag_releases)
    print.df(d$tag_recaps)
  }

  # write morph composition data
  if (is.null(d$morphcomp_data))
    d$morphcomp_data <- 0
  wl("morphcomp_data")

  wl("use_selectivity_priors")

  writeComment("#")
  writeLines("999", con = zz)
  #  options(width=oldwidth,max.print=oldmax.print)
  #  sink()
  #  close(zz)
  if (verbose){
    message("file written to ", outfile)
  }
}
