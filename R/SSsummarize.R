#' Summarize the output from multiple Stock Synthesis models.
#'
#' Summarize various quantities from the model output collected by
#' [SSgetoutput()] and return them in a list of tables and vectors.
#' If the models have incompatible dimensions, some quantities can't be
#' compared via this function, such as selectivity.
#'
#' @param biglist A list of lists, one for each model. The individual lists can
#' be created by [SS_output()] or the list of lists can be
#' created by [SSgetoutput()] (which iteratively calls
#' [SS_output()]).
#' @param sizeselfactor A string or vector of strings indicating which elements
#' of the selectivity at length output to summarize. Default=c("Lsel").
#' @param ageselfactor A string or vector of strings indicating which elements
#' of the selectivity at age output to summarize. Default=c("Asel").
#' @param selfleet Vector of fleets for which selectivity will be summarized.
#' NULL=all fleets. Default=NULL.
#' @param selyr String or vector of years for which selectivity will be
#' summarized.  NOTE: NOT CURRENTLY WORKING.  Options: NULL=all years,
#' "startyr" = first year.
#' @param selgender Deprecated. Use selsex instead.
#' @param selsex Vector of sexes (1 and/or 2) for which selectivity will
#' be summarized. NULL=all sexes. Default=NULL.
#' @param SpawnOutputUnits Optional single value or vector of "biomass" or
#' "numbers" giving units of spawning for each model.
#' @param lowerCI Quantile for lower bound on calculated intervals. Default =
#' 0.025 for 95% intervals.
#' @param upperCI Quantile for upper bound on calculated intervals. Default =
#' 0.975 for 95% intervals.
#' @template verbose
#' @author Ian Taylor
#' @export
#' @seealso [SSgetoutput()]
SSsummarize <- function(
  biglist,
  sizeselfactor = "Lsel",
  ageselfactor = "Asel",
  selfleet = NULL,
  selyr = "startyr",
  selgender = lifecycle::deprecated(),
  selsex = 1,
  SpawnOutputUnits = NULL,
  lowerCI = 0.025,
  upperCI = 0.975,
  verbose = TRUE
) {
  if (lifecycle::is_present(selgender)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SSsummarize(selgender)",
      with = "SSsummarize(selsex)"
    )
    selsex <- selgender
  }
  # confirm that biglist is a list of lists, each created by SS_output()
  # this could be improved with the use of S3 classes in the future
  if (
    !is.list(biglist) | # if whole thing is not a list
      !is.list(biglist[[1]]) | # or if the first element isn't also a list
      !"parameters" %in% names(biglist[[1]])
  ) {
    # or if 1st list seems wrong
    stop(
      "Input 'biglist' needs to be a list of the lists returned by ",
      "SS_output(), either by grouping those lists within 'list()', or ",
      "running SSgetoutput() which calls SS_output() repeatedly ",
      "and returning a big list in the appropriate format."
    )
  }

  # loop over outputs to create list of parameters, derived quantities, and years
  parnames <- NULL
  dernames <- NULL
  likenames <- NULL
  allyears <- NULL

  # figure out how many models and give them names if they don't have them already
  n <- length(biglist)
  modelnames <- names(biglist)
  if (is.null(modelnames)) {
    modelnames <- paste0("model", 1:n)
  }
  # accumulator age for each model
  accuages <- rep(NA, n)
  summary_ages <- rep(NA, n)

  # do the loop
  for (imodel in 1:n) {
    stats <- biglist[[imodel]]
    parnames <- union(parnames, stats[["parameters"]][["Label"]])
    dernames <- union(dernames, stats[["derived_quants"]][["Label"]])
    allyears <- union(allyears, stats[["timeseries"]][["Yr"]])
    likenames <- union(likenames, rownames(stats[["likelihoods_used"]]))
    # accumulator age for each model
    accuages[imodel] <- stats[["accuage"]]
    # summary biomass age for each model
    summary_ages[imodel] <- stats[["summary_age"]]
  }
  allyears <- sort(allyears) # not actually getting any timeseries stuff yet

  # objects to store quantities
  pars <- parsSD <- parphases <- par_prior_likes <-
    as.data.frame(matrix(NA, nrow = length(parnames), ncol = n))
  quants <- quantsSD <- as.data.frame(matrix(
    NA,
    nrow = length(dernames),
    ncol = n
  ))
  maxgrad <- NULL
  nsexes <- NULL
  likelihoods <- likelambdas <- as.data.frame(matrix(
    NA,
    nrow = length(likenames),
    ncol = n
  ))
  likelihoods_by_fleet <- NULL
  likelihoods_by_tag_group <- NULL
  indices <- NULL
  sizesel <- NULL
  agesel <- NULL
  # check accumulator age
  accuage <- max(accuages)
  if (all(accuages == accuage)) {
    growth <- as.data.frame(matrix(NA, nrow = accuage + 1, ncol = n))
    names(growth) <- modelnames
  } else {
    warning(
      "problem summarizing growth due to different ",
      "accumulator ages among models"
    )
    growth <- NULL
  }
  # check summary biomass age
  if (length(unique(summary_ages)) > 1) {
    warning(
      "Age used in summary biomass calculations differs among models:",
      paste(summary_ages, collapse = " ")
    )
  }
  # notes about what runs were used
  sim <- NULL
  listnames <- NULL
  npars <- NULL
  startyrs <- NULL
  endyrs <- NULL
  BratioLabels <- NULL
  SPRratioLabels <- NULL
  FvalueLabels <- NULL
  SpawnOutputLabels <- NULL
  sprtargs <- NULL
  btargs <- NULL
  minbthreshs <- NULL
  FleetNames <- list()
  mcmc <- list()
  warn <- FALSE # flag for whether filter warning has been printed or not

  if (verbose) {
    message("Summarizing ", n, " models:")
  }

  # loop over models within biglist
  for (imodel in 1:n) {
    stats <- biglist[[imodel]]
    listname <- names(biglist)[imodel]
    if (verbose) {
      message("imodel=", imodel, "/", n)
    }

    # gradient
    maxgrad <- c(maxgrad, stats[["maximum_gradient_component"]])

    # nsexes
    nsexes <- c(nsexes, stats[["nsexes"]])

    # start and end years
    startyrs <- c(startyrs, stats[["startyr"]])
    endyrs <- c(endyrs, stats[["endyr"]])

    # size selectivity
    sizeseltemp <- stats[["sizeselex"]]
    # check for non-NULL selectivity table
    if (is.null(sizeseltemp)) {
      if (verbose) {
        message("  no selectivity-at-length output")
      }
    } else {
      # if factor(s) not input, get all unique values from table
      if (is.null(sizeselfactor) & !is.null(sizeseltemp)) {
        sizeselfactor <- unique(sizeseltemp[["Factor"]])
      }
      # loop over factor(s) input by user or taken from table
      for (iselfactor in seq_along(sizeselfactor)) {
        seltemp_i <- sizeseltemp[
          sizeseltemp[["Factor"]] == sizeselfactor[iselfactor],
        ]
        seltemp_i[["imodel"]] <- imodel
        seltemp_i[["name"]] <- modelnames[imodel]
        # if sizesel is not NULL, then check for whether columns of new addition
        # match existing file
        if (
          is.null(sizesel) ||
            (ncol(seltemp_i) == ncol(sizesel) &&
              all(names(seltemp_i) == names(sizesel)))
        ) {
          sizesel <- rbind(sizesel, seltemp_i)
        } else {
          warning(
            "problem summarizing size selectivity due to mismatched columns ",
            "(perhaps different bins)"
          )
        }
      }
      rownames(sizesel) <- 1:nrow(sizesel)
    }

    # age selectivity
    ageseltemp <- stats[["ageselex"]]
    # check for NULL selectivity table
    if (is.null(ageseltemp)) {
      if (verbose) {
        message("  no selectivity-at-age output")
      }
    } else {
      # if factor(s) not input, get all unique values from table
      if (is.null(ageselfactor)) {
        ageselfactor <- unique(ageseltemp[["Factor"]])
      }
      # loop over factor(s) input by user or taken from table
      for (iselfactor in seq_along(ageselfactor)) {
        seltemp_i <- ageseltemp[
          ageseltemp[["Factor"]] == ageselfactor[iselfactor],
        ]
        seltemp_i[["imodel"]] <- imodel
        seltemp_i[["name"]] <- modelnames[imodel]
        # if agesel is not NULL, then check for whether columns of new addition
        # match existing file
        if (
          is.null(agesel) ||
            (ncol(seltemp_i) == ncol(agesel) &&
              all(names(seltemp_i) == names(agesel)))
        ) {
          agesel <- rbind(agesel, seltemp_i)
        } else {
          warning(
            "problem summarizing age selectivity due to mismatched columns ",
            "(perhaps different bins)"
          )
        }
      }
      rownames(agesel) <- 1:nrow(agesel)
    }

    ## growth (females only)
    if (!is.null(growth)) {
      growthtemp <- stats[["growthseries"]]
      # check for non-NULL growth output
      if (!is.null(growthtemp)) {
        # subset for the female main morph
        imorphf <- stats[["morph_indexing"]][["Index"]][
          stats[["morph_indexing"]][["Sex"]] == 1 &
            stats[["morph_indexing"]][["Platoon"]] %in% stats[["mainmorphs"]]
        ]
        growthtemp <- growthtemp[growthtemp[["Morph"]] == imorphf, -(1:4)]
        # remove any rows with all zeros (not sure why these occur)
        growthtemp <- growthtemp[apply(growthtemp, 1, sum) > 0, ]
        # get last row and bind to values from previous models
        if (nrow(growthtemp) > 0) {
          growth[, imodel] <- as.numeric(growthtemp[nrow(growthtemp), ])
        } else {
          growth[, imodel] <- NA
        }
      }
    }

    ## likelihoods (total by component)
    liketemp <- stats[["likelihoods_used"]]
    for (irow in 1:nrow(liketemp)) {
      likelihoods[likenames == rownames(liketemp)[irow], imodel] <- liketemp[[
        "values"
      ]][irow]
      likelambdas[likenames == rownames(liketemp)[irow], imodel] <- liketemp[[
        "lambdas"
      ]][irow]
    }
    ## likelihoods by fleet
    # add initial column with model number to table from each model
    liketemp2 <- data.frame(model = imodel, stats[["likelihoods_by_fleet"]])
    # test for presence of existing table to append to with matching number of columns
    if (
      is.null(likelihoods_by_fleet) ||
        (ncol(likelihoods_by_fleet) == ncol(liketemp2) &&
          all(names(likelihoods_by_fleet) == names(liketemp2)))
    ) {
      likelihoods_by_fleet <- rbind(likelihoods_by_fleet, liketemp2)
    } else {
      likelihoods_by_fleet <- merge(likelihoods_by_fleet, liketemp2, all = TRUE)
    }

    ## likelihoods by tag group
    # add initial column with model number to table from each model
    if (!is.null(stats[["likelihoods_by_tag_group"]])) {
      liketemp3 <- data.frame(
        model = imodel,
        stats[["likelihoods_by_tag_group"]]
      )
      # test for presence of existing table to append to with matching number of columns
      if (
        is.null(likelihoods_by_tag_group) ||
          (ncol(likelihoods_by_tag_group) == ncol(liketemp3) &&
            all(names(likelihoods_by_tag_group) == names(liketemp3)))
      ) {
        likelihoods_by_tag_group <- rbind(likelihoods_by_tag_group, liketemp3)
      } else {
        warning(
          "problem summarizing likelihoods by fleet due to mismatched columns"
        )
      }
    }

    ## compile parameters
    parstemp <- stats[["parameters"]]
    for (ipar in 1:nrow(parstemp)) {
      pars[parnames == parstemp[["Label"]][ipar], imodel] <- parstemp[[
        "Value"
      ]][ipar]
      parsSD[parnames == parstemp[["Label"]][ipar], imodel] <- parstemp[[
        "Parm_StDev"
      ]][ipar]
      parphases[parnames == parstemp[["Label"]][ipar], imodel] <- parstemp[[
        "Phase"
      ]][ipar]
      par_prior_likes[
        parnames == parstemp[["Label"]][ipar],
        imodel
      ] <- parstemp[["Pr_Like"]][ipar]
    }
    if (verbose) {
      message("  N active pars = ", sum(!is.na(parstemp[["Active_Cnt"]])))
    }

    ## compile derived quantities
    quantstemp <- stats[["derived_quants"]]
    for (iquant in 1:nrow(quantstemp)) {
      quants[dernames == quantstemp[["Label"]][iquant], imodel] <- quantstemp[[
        "Value"
      ]][iquant]
      quantsSD[
        dernames == quantstemp[["Label"]][iquant],
        imodel
      ] <- quantstemp[["StdDev"]][iquant]
    }
    BratioLabels <- c(BratioLabels, stats[["BratioLabels"]])
    SPRratioLabels <- c(SPRratioLabels, stats[["SPRratioLabel"]])
    FvalueLabels <- c(FvalueLabels, stats[["F_std_basis"]])
    SpawnOutputLabels <- c(SpawnOutputLabels, stats[["SpawnOutputLabel"]])
    sprtargs <- c(sprtargs, stats[["sprtarg"]])
    btargs <- c(btargs, stats[["btarg"]])
    minbthreshs <- c(minbthreshs, stats[["minbthresh"]])
    FleetNames[[imodel]] <- stats[["FleetNames"]]

    ## indices
    indextemp <- stats[["cpue"]]
    if (is.null(indextemp) || is.na(indextemp[[1]][1])) {
      if (verbose) {
        message("  no index data")
      }
    } else {
      # temporarily remove columns added in SS version 3.30.13 (March 2019)
      indextemp <- indextemp[
        !names(indextemp) %in% c("Area", "Subseas", "Month")
      ]
      indextemp[["name"]] <- modelnames[imodel]
      indextemp[["imodel"]] <- imodel
      if (is.null(indices)) {
        # first pass through with nothing in combined data frame
        indices <- rbind(indices, indextemp)
      } else {
        # after indices contains output from at least one model
        # check that there are equal number of columns with matching names
        # Working here
        if (
          ncol(indextemp) == ncol(indices) &&
            all(names(indextemp) == names(indices))
        ) {
          indices <- rbind(indices, indextemp)
        } else {
          indices <- merge(indices, indextemp, all = TRUE)
        }
      }
    }

    # number of parameters
    npars <- c(npars, stats[["N_estimated_parameters"]])

    # 2nd fecundity parameter indicates whether spawning output is proportional to biomass
    if (!is.null(SpawnOutputUnits)) {
      # if 1 value is input, repeate n times
      if (length(SpawnOutputUnits) == 1) {
        SpawnOutputUnits <- rep(SpawnOutputUnits, n)
      }
      # if total doesn't currently equal n, stop everything
      if (length(SpawnOutputUnits) != n) {
        stop("'SpawnOutputUnits' should have length = 1 or", n)
      }
    } else {
      # if NULL, then make vector of NA values
      SpawnOutputUnits <- rep(NA, n)
    }
    # if NA value in vector for current model, replace with value from model
    if (
      is.na(SpawnOutputUnits[imodel]) & !is.null(stats[["SpawnOutputUnits"]])
    ) {
      SpawnOutputUnits[imodel] <- stats[["SpawnOutputUnits"]]
    }
    # get mcmc values if present
    if (!is.null(stats[["mcmc"]])) {
      mcmc[[imodel]] <- stats[["mcmc"]]
    }
  } # end loop over models

  ### format and process info from the models
  names(pars) <- names(parsSD) <- names(parphases) <- names(par_prior_likes) <-
    modelnames
  names(quants) <- names(quantsSD) <- modelnames
  names(likelihoods) <- names(likelambdas) <- modelnames

  pars[["Label"]] <- parsSD[["Label"]] <- parphases[["Label"]] <-
    par_prior_likes[["Label"]] <- parnames
  quants[["Label"]] <- quantsSD[["Label"]] <- dernames
  likelihoods[["Label"]] <- likelambdas[["Label"]] <- likenames
  # extract year values from labels for some parameters associated with years
  pars[["Yr"]] <- NA
  for (ipar in 1:nrow(pars)) {
    substrings <- strsplit(as.character(pars[["Label"]][ipar]), "_")[[1]]
    yr <- substrings[substrings %in% allyears][1]
    pars[["Yr"]][ipar] <- ifelse(is.null(yr), NA, as.numeric(yr))
  }

  quants[["Yr"]] <- quantsSD[["Yr"]] <- NA
  for (iquant in 1:nrow(quants)) {
    substrings <- strsplit(as.character(quants[["Label"]][iquant]), "_")[[1]]
    yr <- substrings[substrings %in% allyears][1]
    quants[["Yr"]][iquant] <- ifelse(is.null(yr), NA, as.numeric(yr))
    quantsSD[["Yr"]][iquant] <- ifelse(is.null(yr), NA, as.numeric(yr))
  }

  # rows numbers of derived quantities that start with "SSB_"
  SSBrows <- grep("SSB_", quants[["Label"]])
  # row numbers that start with "SSB_" but are not part of time series
  SSBexclude <- c(
    grep("SSB_unfished", quants[["Label"]], ignore.case = TRUE),
    grep("SSB_Btgt", quants[["Label"]], ignore.case = TRUE),
    grep("SSB_SPR", quants[["Label"]], ignore.case = TRUE),
    grep("SSB_MSY", quants[["Label"]], ignore.case = TRUE),
    grep("SSB_F01", quants[["Label"]], ignore.case = TRUE),
    grep("SSB_for_SRR_bench", quants[["Label"]], ignore.case = TRUE)
  )
  # filter rows to only include time series
  SSBrows <- setdiff(SSBrows, SSBexclude)
  # identify spawning biomass parameters
  SpawnBio <- quants[SSBrows, ]
  SpawnBioSD <- quantsSD[SSBrows, ]
  # add year values for Virgin and Initial years
  minyr <- min(SpawnBio[["Yr"]], na.rm = TRUE)
  SpawnBio[["Yr"]][grep("SSB_Virgin", SpawnBio[["Label"]])] <- minyr - 2
  SpawnBio[["Yr"]][grep("SSB_Initial", SpawnBio[["Label"]])] <- minyr - 1
  SpawnBioSD[["Yr"]] <- SpawnBio[["Yr"]]

  SpawnBio <- SpawnBio[order(SpawnBio[["Yr"]]), ]
  SpawnBioSD <- SpawnBioSD[order(SpawnBioSD[["Yr"]]), ]

  SpawnBioLower <- SpawnBioUpper <- SpawnBioSD
  SpawnBioLower[, 1:n] <- qnorm(
    p = lowerCI,
    mean = as.matrix(SpawnBio[, 1:n]),
    sd = as.matrix(SpawnBioSD[, 1:n])
  )
  SpawnBioUpper[, 1:n] <- qnorm(
    p = upperCI,
    mean = as.matrix(SpawnBio[, 1:n]),
    sd = as.matrix(SpawnBioSD[, 1:n])
  )

  # identify biomass ratio parameters
  Bratio <- quants[grep("^Bratio_", quants[["Label"]]), ]
  BratioSD <- quantsSD[grep("^Bratio_", quantsSD[["Label"]]), ]

  BratioLower <- BratioUpper <- BratioSD
  BratioLower[, 1:n] <- qnorm(
    p = lowerCI,
    mean = as.matrix(Bratio[, 1:n]),
    sd = as.matrix(BratioSD[, 1:n])
  )
  BratioUpper[, 1:n] <- qnorm(
    p = upperCI,
    mean = as.matrix(Bratio[, 1:n]),
    sd = as.matrix(BratioSD[, 1:n])
  )

  # identify SPR ratio derived quantities
  SPRratio <- quants[grep("^SPRratio_", quants[["Label"]]), ]
  SPRratioSD <- quantsSD[grep("^SPRratio_", quantsSD[["Label"]]), ]

  SPRratioLower <- SPRratioUpper <- SPRratioSD
  SPRratioLower[, 1:n] <- qnorm(
    p = lowerCI,
    mean = as.matrix(SPRratio[, 1:n]),
    sd = as.matrix(SPRratioSD[, 1:n])
  )
  SPRratioUpper[, 1:n] <- qnorm(
    p = upperCI,
    mean = as.matrix(SPRratio[, 1:n]),
    sd = as.matrix(SPRratioSD[, 1:n])
  )

  # identify F derived quantities
  Fvalue <- quants[grep("^F_", quants[["Label"]]), ]
  FvalueSD <- quantsSD[grep("^F_", quantsSD[["Label"]]), ]

  FvalueLower <- FvalueUpper <- FvalueSD
  FvalueLower[, 1:n] <- qnorm(
    p = lowerCI,
    mean = as.matrix(Fvalue[, 1:n]),
    sd = as.matrix(FvalueSD[, 1:n])
  )
  FvalueUpper[, 1:n] <- qnorm(
    p = upperCI,
    mean = as.matrix(Fvalue[, 1:n]),
    sd = as.matrix(FvalueSD[, 1:n])
  )

  # identify recruitment parameters and their uncertainty
  recruits <- quants[grep("^Recr_", quants[["Label"]]), ]
  recruitsSD <- quantsSD[grep("^Recr_", quantsSD[["Label"]]), ]
  # filter rows not associated with a year
  recr_exclude <- c("Recr_Unfished", "Recr_MSY_bmarkbio")
  recruits <- recruits[
    !tolower(recruits[["Label"]]) %in% tolower(recr_exclude),
  ]
  recruitsSD <- recruitsSD[
    !tolower(recruitsSD[["Label"]]) %in% tolower(recr_exclude),
  ]

  if (
    length(grep("Recr_Unfished", recruits[["Label"]], ignore.case = TRUE)) > 0
  ) {
    recruits <- recruits[
      -grep("Recr_Unfished", recruits[["Label"]], ignore.case = TRUE),
    ]
    recruitsSD <- recruitsSD[
      -grep("Recr_Unfished", recruitsSD[["Label"]], ignore.case = TRUE),
    ]
  }
  minyr <- min(recruits[["Yr"]], na.rm = TRUE)

  recruits[["Yr"]][grep("Recr_Virgin", recruits[["Label"]])] <- minyr - 2
  recruits[["Yr"]][grep("Recr_Initial", recruits[["Label"]])] <- minyr - 1
  recruitsSD[["Yr"]][grep("Recr_Virgin", recruitsSD[["Label"]])] <- minyr - 2
  recruitsSD[["Yr"]][grep("Recr_Initial", recruitsSD[["Label"]])] <- minyr - 1
  recruits <- recruits[order(recruits[["Yr"]]), ]
  recruitsSD <- recruitsSD[order(recruitsSD[["Yr"]]), ]

  recruitsLower <- recruitsUpper <- recruitsSD
  # assume recruitments have log-normal distribution
  # from first principals (multiplicative survival probabilities)
  # and from their basis as exponential of normal recdevs
  sdlog <- sqrt(log(
    1 +
      (as.matrix(recruitsSD[, 1:n]) /
        as.matrix(recruits[, 1:n]))^2
  ))
  recruitsLower[, 1:n] <- qlnorm(
    p = lowerCI,
    meanlog = log(as.matrix(recruits[, 1:n])),
    sdlog = sdlog
  )
  recruitsUpper[, 1:n] <- qlnorm(
    p = upperCI,
    meanlog = log(as.matrix(recruits[, 1:n])),
    sdlog = sdlog
  )

  # identify parameters that are recruitment deviations
  pars[["recdev"]] <- FALSE
  pars[["recdev"]][grep("RecrDev", pars[["Label"]])] <- TRUE
  pars[["recdev"]][grep("InitAge", pars[["Label"]])] <- TRUE
  pars[["recdev"]][grep("ForeRecr", pars[["Label"]])] <- TRUE

  # if there are any initial age parameters, figure out what year they're from
  InitAgeRows <- grep("InitAge", pars[["Label"]])
  if (length(InitAgeRows) > 0) {
    # separate out values from string
    temp <- unlist(strsplit(pars[["Label"]][InitAgeRows], "InitAge_"))
    # get odd entries in above separation
    InitAgeVals <- as.numeric(temp[seq(2, length(temp), 2)])
    # make empty matrix to store values
    InitAgeYrs <- matrix(NA, nrow = length(InitAgeRows), ncol = n)
    # loop over models
    for (imodel in 1:n) {
      # get parameters
      modelpars <- pars[, imodel]
      # get vector of years associated with recdevs
      devyears <- pars[["Yr"]][!is.na(modelpars) & pars[["recdev"]]]
      # figure out first year of recdevs that already have years figured out
      if (any(!is.na(devyears))) {
        minyr <- min(devyears, na.rm = TRUE)
      } else {
        minyr <- NA
      }
      # determine which parameter values are associated with InitAge and not NA
      good <- !is.na(modelpars[InitAgeRows])
      # if minyr was not NA, and is above 0 and there are good InitAge values,
      # then compute the associated year
      if (!is.na(minyr) & minyr > 0 & any(good)) {
        InitAgeYrs[good, imodel] <- minyr - InitAgeVals[good]
      }
    }
    # check for differences in assignment of initial ages
    if (
      any(
        apply(InitAgeYrs, 1, max, na.rm = TRUE) -
          apply(InitAgeYrs, 1, min, na.rm = TRUE) !=
          0
      )
    ) {
      warning(
        "years for InitAge parameters differ between models,",
        "use InitAgeYrs matrix"
      )
    } else {
      pars[["Yr"]][InitAgeRows] <- apply(InitAgeYrs, 1, max, na.rm = TRUE)
    }
  } else {
    # no parameters seem to be associated with initial age structure
    InitAgeYrs <- NA
  }
  if (any(pars[["recdev"]])) {
    recdevs <- pars[pars[["recdev"]], ]
    recdevsSD <- parsSD[pars[["recdev"]], ]
    myorder <- order(recdevs[["Yr"]]) # save order for use in both values and SDs
    recdevs <- recdevs[myorder, 1:(n + 2)]
    recdevsSD <- recdevsSD[myorder, 1:(n + 1)]
    recdevsSD[["Yr"]] <- recdevs[["Yr"]]
    recdevsLower <- recdevsUpper <- recdevsSD
    recdevsLower[, 1:n] <- qnorm(
      p = lowerCI,
      mean = as.matrix(recdevs[, 1:n]),
      sd = as.matrix(recdevsSD[, 1:n])
    )
    recdevsUpper[, 1:n] <- qnorm(
      p = upperCI,
      mean = as.matrix(recdevs[, 1:n]),
      sd = as.matrix(recdevsSD[, 1:n])
    )
  } else {
    recdevs <- recdevsSD <- recdevsLower <- recdevsUpper <- NULL
  }

  # Summary biomass (range of ages defined in starter file and
  # present in derived quantities depends on "read specs for more stddev
  # reporting" at the bottom of the control file)

  # rows numbers of derived quantities that start with "SmryBio_"
  SmryBiorows <- grep("SmryBio_", quants[["Label"]])
  # exclude "unfished" value which is reported separately from the timeseries--
  # though may be useful in the future
  SmryBiorows <- setdiff(
    SmryBiorows,
    grep("SmryBio_unfished", quants[["Label"]])
  )
  SmryBiorows <- setdiff(
    SmryBiorows,
    grep("SmryBio_Unfished", quants[["Label"]])
  )

  # identify summary biomass values in derived quantities
  SmryBio <- quants[SmryBiorows, ]
  SmryBioSD <- quantsSD[SmryBiorows, ]

  # create dataframe full of NAs based on SpawnBio to fill in any missing years
  # SpawnBio always spans full range of years included in any model
  SmryBio_extras <- SpawnBio[-(1:2), ] # exclude VIRG and INIT years as these should be added from one of the models
  SmryBio_extras[, 1:n] <- NA # remove SpawnBio values
  SmryBio_extras[["Label"]] <- NA # remove SpawnBio labels
  # only those years that aren't yet in the main dataframe
  SmryBio_extras <- SmryBio_extras |> dplyr::filter(!Yr %in% SmryBio[["Yr"]])
  SmryBioSD_extras <- SmryBio_extras
  # add to main dataframe
  SmryBio <- rbind(SmryBio, SmryBio_extras)
  SmryBioSD <- rbind(SmryBioSD, SmryBio_extras)
  # add year values for Virgin and Initial years (if present)
  minyr <- min(SpawnBio[["Yr"]], na.rm = TRUE) + 2 # startyr of model is 2 larger than minyr in table
  SmryBio[["Yr"]][grep("SmryBio_Virg", SmryBio[["Label"]])] <- minyr - 2
  SmryBio[["Yr"]][grep("SmryBio_InitEq", SmryBio[["Label"]])] <- minyr - 1
  SmryBioSD[["Yr"]] <- SmryBio[["Yr"]]
  # sort by year
  SmryBio <- SmryBio[order(SmryBio[["Yr"]]), ]
  SmryBioSD <- SmryBioSD[order(SmryBioSD[["Yr"]]), ]

  # add any missing values using timeseries
  # TODO: need to get VIRG and INIT values from timeseries
  for (imodel in 1:n) {
    # renumber years of timeseries so that models with different starting years
    # have VIRG and INIT values associated with the same numerical year
    ts <- biglist[[imodel]][["timeseries"]]
    ts[ts[["Era"]] == "VIRG", "Yr"] <- minyr - 2
    ts[ts[["Era"]] == "INIT", "Yr"] <- minyr - 1
    # get all years within modified timeseries for this model
    # excluding virgin summary biomass
    yrs <- ts |>
      dplyr::pull(Yr) |>
      unique()

    # years within range above that have NA in table
    # (may be all years in model if not included in derived quantities)
    NA_yrs <- intersect(yrs, SmryBio[["Yr"]][is.na(SmryBio[, imodel])])
    if (length(NA_yrs) > 0) {
      # filter years to only include those with NA values
      SmryBio[SmryBio[["Yr"]] %in% NA_yrs, imodel] <- ts |>
        dplyr::filter(Yr %in% NA_yrs) |>
        dplyr::group_by(Yr) |>
        dplyr::summarize(Bio_smry = sum(Bio_smry)) |>
        dplyr::pull(Bio_smry)
    }
  }

  SmryBioLower <- SmryBioUpper <- SmryBioSD
  SmryBioLower[, 1:n] <- qnorm(
    p = lowerCI,
    mean = as.matrix(SmryBio[, 1:n]),
    sd = as.matrix(SmryBioSD[, 1:n])
  )
  SmryBioUpper[, 1:n] <- qnorm(
    p = upperCI,
    mean = as.matrix(SmryBio[, 1:n]),
    sd = as.matrix(SmryBioSD[, 1:n])
  )

  # function to merge duplicate rows caused by different parameter labels
  # that are associated with the same year, such as the recdev for 2016
  # being called "ForeRecr_2016", "Late_RecrDev_2016", or "Main_RecrDev_2016",
  # in 3 different models depending on the ending year of each model and the
  # choice of recdev vector breaks
  merge.duplicates <- function(x) {
    if (!is.null(x)) {
      if (length(unique(x[["Yr"]])) < length(x[["Yr"]])) {
        # n should be number of models
        n <- sum(!names(x) %in% c("Label", "Yr"))
        x2 <- NULL # alternative data.frame
        for (Yr in unique(x[["Yr"]])) {
          x.Yr <- x[which(x[["Yr"]] == Yr), ]
          if (nrow(x.Yr) == 1) {
            # if only 1 row associated with this year add to new data.frame
            x2 <- rbind(x2, x.Yr)
          } else {
            # more than 1 row associated with this year
            # create empty row with matching names
            newrow <- data.frame(
              t(rep(NA, n)),
              Label = paste0("Multiple_labels_", Yr),
              Yr = Yr
            )
            names(newrow) <- names(x)
            # loop over models to pick the (hopefully) unique value among rows
            for (icol in 1:n) {
              good <- !is.na(x.Yr[, icol])
              if (sum(good) > 1) {
                # warn if more than 1 value
                warning("multiple recdevs values associated with year =", Yr)
              }
              if (sum(good) == 1) {
                # put good value into new row
                newrow[, icol] <- x.Yr[good, icol]
              }
              # if there are no good values, this model likely ends prior to Yr
            }
            # add new row to new data.frame
            x2 <- rbind(x2, newrow)
          } # end test for duplicates for particular year
        } # end loop over years
      } else {
        # end test for duplicates in general
        # if no duplicates, just return data.frame
        x2 <- x
      }
    } else {
      # test for is.null(x)
      return(x)
    }
    return(x2)
  }

  # helper fxn b/c name of DM parameter changed
  # todo: delete when these models no longer need to be maintained
  copy.dm <- function(data, oldgrep = "EffN", newgrep = "theta") {
    oldrows <- grep(oldgrep, data[, "Label"])
    if (length(oldrows) == 0) {
      return(data)
    }
    newrows <- grep(newgrep, data[, "Label"])
    fix <- which(apply(
      X = data[newrows, grep("model", colnames(data))],
      MARGIN = 2,
      function(vector) all(is.na(vector))
    ))
    if (length(oldrows) != length(newrows) | length(fix) == 0) {
      return(data)
    }
    if (
      get("verbose", envir = parent.frame()) &
        deparse(substitute(data)) == "pars"
    ) {
      message(
        "For model(s) ",
        paste(fix, collapse = ", "),
        ", values in 'pars', 'parsSD', 'parphases', and 'par_prior_likes' for\n",
        paste(
          data[oldrows, "Label"],
          data[newrows, "Label"],
          sep = " -> ",
          collapse = ", "
        ),
        "\nwere copied from x -> y."
      )
    }
    data[newrows, fix] <- data[oldrows, fix]
    return(data)
  }

  # function to sort by year
  sort.fn <- function(x) {
    if (!is.null(x)) {
      return(x[order(x[["Yr"]]), ])
    } else {
      return()
    }
  }

  mylist <- list()
  mylist[["n"]] <- n
  mylist[["npars"]] <- npars
  mylist[["modelnames"]] <- modelnames
  mylist[["maxgrad"]] <- maxgrad
  mylist[["nsexes"]] <- nsexes
  mylist[["startyrs"]] <- startyrs
  mylist[["endyrs"]] <- endyrs
  mylist[["pars"]] <- copy.dm(pars)
  mylist[["parsSD"]] <- copy.dm(parsSD)
  mylist[["parphases"]] <- copy.dm(parphases)
  mylist[["par_prior_likes"]] <- copy.dm(par_prior_likes)
  mylist[["quants"]] <- quants
  mylist[["quantsSD"]] <- quantsSD
  mylist[["likelihoods"]] <- likelihoods
  mylist[["likelambdas"]] <- likelambdas
  mylist[["likelihoods_by_fleet"]] <- likelihoods_by_fleet
  mylist[["likelihoods_by_tag_group"]] <- likelihoods_by_tag_group
  mylist[["SpawnBio"]] <- sort.fn(SpawnBio)
  mylist[["SpawnBioSD"]] <- sort.fn(SpawnBioSD)
  mylist[["SpawnBioLower"]] <- sort.fn(SpawnBioLower)
  mylist[["SpawnBioUpper"]] <- sort.fn(SpawnBioUpper)
  mylist[["Bratio"]] <- sort.fn(Bratio)
  mylist[["BratioSD"]] <- sort.fn(BratioSD)
  mylist[["BratioLower"]] <- sort.fn(BratioLower)
  mylist[["BratioUpper"]] <- sort.fn(BratioUpper)
  mylist[["BratioLabels"]] <- BratioLabels
  mylist[["SPRratio"]] <- sort.fn(SPRratio)
  mylist[["SPRratioSD"]] <- sort.fn(SPRratioSD)
  mylist[["SPRratioLower"]] <- sort.fn(SPRratioLower)
  mylist[["SPRratioUpper"]] <- sort.fn(SPRratioUpper)
  mylist[["SPRratioLabels"]] <- SPRratioLabels
  mylist[["Fvalue"]] <- sort.fn(Fvalue)
  mylist[["FvalueSD"]] <- sort.fn(FvalueSD)
  mylist[["FvalueLower"]] <- sort.fn(FvalueLower)
  mylist[["FvalueUpper"]] <- sort.fn(FvalueUpper)
  mylist[["FvalueLabels"]] <- FvalueLabels
  mylist[["sprtargs"]] <- sprtargs
  mylist[["btargs"]] <- btargs
  mylist[["minbthreshs"]] <- minbthreshs
  mylist[["recruits"]] <- sort.fn(recruits)
  mylist[["recruitsSD"]] <- sort.fn(recruitsSD)
  mylist[["recruitsLower"]] <- sort.fn(recruitsLower)
  mylist[["recruitsUpper"]] <- sort.fn(recruitsUpper)
  mylist[["recdevs"]] <- merge.duplicates(sort.fn(recdevs))
  mylist[["recdevsSD"]] <- merge.duplicates(sort.fn(recdevsSD))
  mylist[["recdevsLower"]] <- merge.duplicates(sort.fn(recdevsLower))
  mylist[["recdevsUpper"]] <- merge.duplicates(sort.fn(recdevsUpper))
  mylist[["SmryBio"]] <- sort.fn(SmryBio)
  mylist[["SmryBioSD"]] <- sort.fn(SmryBioSD)
  mylist[["SmryBioLower"]] <- sort.fn(SmryBioLower)
  mylist[["SmryBioUpper"]] <- sort.fn(SmryBioUpper)
  mylist[["growth"]] <- growth
  mylist[["sizesel"]] <- sizesel
  mylist[["agesel"]] <- agesel
  mylist[["indices"]] <- indices
  mylist[["InitAgeYrs"]] <- InitAgeYrs
  mylist[["lowerCI"]] <- lowerCI
  mylist[["upperCI"]] <- upperCI
  mylist[["SpawnOutputUnits"]] <- SpawnOutputUnits
  mylist[["SpawnOutputLabels"]] <- SpawnOutputLabels
  mylist[["FleetNames"]] <- FleetNames
  mylist[["mcmc"]] <- mcmc
  mylist[["accuages"]] <- accuages
  mylist[["summary_ages"]] <- summary_ages
  # mylist[["lbinspop"]]   <- as.numeric(names(stats[["sizeselex"]])[-(1:5)])

  if (verbose) {
    message(
      "Summary finished. ",
      "To avoid printing details above, use 'verbose = FALSE'."
    )
  }

  return(invisible(mylist))
} # end function
