#' Calculate Mohn's rho values for select quantities
#'
#' Function calculates:
#' 1. a rho value for the ending year for each retrospective relative to the
#'    reference model as in Mohn (1999);
#' 1. a ``Wood's Hole Mohn's rho'', which is a rho value averaged across all
#'    years for each retrospective relative to the reference model; and
#' 1. an Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's
#'    rho, which is the average rho per retrospective ``peel''.
#'
#' @param summaryoutput List created by [SSsummarize()]. The expected order for
#'   the models are the full reference model, the retro -1, retro -2, and so
#'   forth. Order matters for the calculations.
#' @param endyrvec Integer vector of years that should be used as the final year
#'   for each model in `summaryoutput`. The default, which happens if `endyrvec`
#'   is missing, is based on information in `summaryoutput`, i.e.,
#'   `summaryoutput[["endyrs"]][summaryoutput[["n"]]]:
#'   (summaryoutput[["endyrs"]][summaryoutput[["n"]]] - summaryoutput[["n"]] +
#'   1)`. This parameter will be used to extract estimates of fishing mortality
#'   for each year in `endyrvec` and estimates of biomass-based quantities for
#'   each year in `endyrvec + 1` because Stock Synthesis reports beginning of
#'   the year biomass, which we use here as a proxy for end of the year biomass.
#' @param startyr Single year used to calculate the start year for the
#'   calculation of the Wood's Hole Mohn's rho value, which is computed across
#'   the range of years in the model. If this parameter is missing, the default
#'   is to use the `startyr` of the reference model.
#' @template verbose
#'
#' @author Chantel R. Wetzel, Carey R. McGilliard, and Kelli F. Johnson
#' @references
#' * Hurtado-Ferro et al. 2015. Looking in the rear-view mirror: bias and
#'   retrospective patterns in integrated, age-structured stock assessment
#'   models. ICES J. Mar. Sci. 72(1), 99--110.
#'   https://doi.org/10.1093/icesjms/fsu198.
#' * Mohn, R. 1999. The retrospective problem in sequential population analysis:
#'   an investigation using cod fishery and simulated data. ICES J. Mar. Sci.
#'   56, 473--488. https://doi.org/10.1006/jmsc.1999.0481.
#' @return
#' A list with the following 12 entries:
#' * `"SSB"`
#' * `"Rec"`
#' * `"Bratio"`
#' * `"F"`
#' * `"WoodHole_SSB.all"`
#' * `"WoodHole_Rec.all"`
#' * `"WoodHole_Bratio.all"`
#' * `"WoodHole_F.all"`
#' * `"AFSC_Hurtado_SSB"`
#' * `"AFSC_Hurtado_Rec"`
#' * `"AFSC_Hurtado_F"`
#' * `"AFSC_Hurtado_Bratio"`
#' @export
SSmohnsrho <- function(summaryoutput,
                       endyrvec,
                       startyr,
                       verbose = TRUE) {
  if (verbose) {
    message(
      "The expected order of models in the summary output are the\n",
      "reference model followed by retro -1, retro -2, and so forth."
    )
  }

  N <- summaryoutput[["n"]]
  if (missing(endyrvec)) {
    endyrvec <- rev(
      (summaryoutput[["endyrs"]][N] - N + 1):summaryoutput[["endyrs"]][N]
    )
  } else {
    stopifnot(length(endyrvec) == summaryoutput[["n"]])
  }

  if (missing(startyr)) {
    startyr <- summaryoutput[["startyrs"]][1]
  }


  mohnSSB <- mohnRec <- mohnBratio <- mohnF <- numeric()
  mohnSSB.all <- mohnRec.all <- mohnBratio.all <- mohnF.all <- numeric()

  # Mohn's rho Calculation for the terminal year for each of
  # the retrospectives relative to the reference model
  # rho <- sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
  for (i in 1:(N - 1)) {
    ind <- which(summaryoutput[["SpawnBio"]][["Yr"]] == endyrvec[i + 1] + 1)
    mohnSSB[i] <- (summaryoutput[["SpawnBio"]][ind, i + 1] -
      summaryoutput[["SpawnBio"]][ind, 1]) /
      summaryoutput[["SpawnBio"]][ind, 1]

    ind <- which(summaryoutput[["recruits"]][["Yr"]] == endyrvec[i + 1] + 1)
    mohnRec[i] <- (summaryoutput[["recruits"]][ind, i + 1] -
      summaryoutput[["recruits"]][ind, 1]) /
      summaryoutput[["recruits"]][ind, 1]

    ind <- which(summaryoutput[["Bratio"]][["Yr"]] == endyrvec[i + 1] + 1)
    mohnBratio[i] <- (summaryoutput[["Bratio"]][ind, i + 1] -
      summaryoutput[["Bratio"]][ind, 1]) /
      summaryoutput[["Bratio"]][ind, 1]

    ind <- which(summaryoutput[["Fvalue"]][["Yr"]] == endyrvec[i + 1])
    mohnF[i] <- (summaryoutput[["Fvalue"]][ind, i + 1] -
      summaryoutput[["Fvalue"]][ind, 1]) /
      summaryoutput[["Fvalue"]][ind, 1]
  }

  # Wood's Hole Mohn's rho Calculation for all years for each of the
  # retrospectives relative to the reference model
  # rho <- sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
  # This rho value is then scaled according to the number of model years
  # for comparison between the one year and all year calculation
  # rho <- rho / Number of Years
  for (i in 1:(N - 1)) {
    ind <- which(summaryoutput[["SpawnBio"]][["Yr"]] == startyr + 1):
      which(summaryoutput[["SpawnBio"]][["Yr"]] == endyrvec[i + 1] + 1)
    mohnSSB.all[i] <- sum(
      (summaryoutput[["SpawnBio"]][ind, i + 1] -
       summaryoutput[["SpawnBio"]][ind, 1]
      ) /
      summaryoutput[["SpawnBio"]][ind, 1]
    ) / length(ind)
    ind <- which(summaryoutput[["recruits"]][["Yr"]] == startyr + 1):
      which(summaryoutput[["recruits"]][["Yr"]] == endyrvec[i + 1] + 1)
    mohnRec.all[i] <- sum(
      (summaryoutput[["recruits"]][ind, i + 1] -
       summaryoutput[["recruits"]][ind, 1]
      ) /
      summaryoutput[["recruits"]][ind, 1]
    ) / length(ind)
    if (length(which(summaryoutput[["Bratio"]][["Yr"]] == startyr + 1)) != 0) {
      ind <- which(summaryoutput[["Bratio"]][["Yr"]] == startyr + 1):
        which(summaryoutput[["Bratio"]][["Yr"]] == endyrvec[i + 1] + 1)
      mohnBratio.all[i] <- sum(
        (summaryoutput[["Bratio"]][ind, i + 1] -
         summaryoutput[["Bratio"]][ind, 1]
        ) /
        summaryoutput[["Bratio"]][ind, 1]
      ) / length(ind)
    } else {
      warning(
        "Skipping Wood's Hole Mohn's rho on Bratio, ",
        "as Bratio is not available for year after the first model year."
      )
      mohnBratio.all[i] <- NA
    }
    if (length(which(summaryoutput[["Fvalue"]][["Yr"]] == startyr)) != 0) {
      ind <- which(summaryoutput[["Fvalue"]][["Yr"]] == startyr):
        which(summaryoutput[["Fvalue"]][["Yr"]] == endyrvec[i + 1])
      mohnF.all[i] <- sum(
        (summaryoutput[["Fvalue"]][ind, i + 1] -
         summaryoutput[["Fvalue"]][ind, 1]
        ) /
        summaryoutput[["Fvalue"]][ind, 1]
      ) / length(ind)
    } else {
      warning(
        "Skipping Wood's Hole Mohn's rho on Fvalue, ",
        "because Fvalue is not available for first model year."
      )
      mohnF.all[i] <- NA
    }
  }

  mohn.out <- list()
  mohn.out[["SSB"]] <- sum(mohnSSB)
  mohn.out[["Rec"]] <- sum(mohnRec)
  mohn.out[["Bratio"]] <- sum(mohnBratio)
  mohn.out[["F"]] <- sum(mohnF)

  mohn.out[["WoodHole_SSB.all"]] <- sum(mohnSSB.all)
  mohn.out[["WoodHole_Rec.all"]] <- sum(mohnRec.all)
  mohn.out[["WoodHole_Bratio.all"]] <- sum(mohnBratio.all)
  mohn.out[["WoodHole_F.all"]] <- sum(mohnF.all)

  # Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho
  # https://www.afsc.noaa.gov/REFM/stocks/Plan_Team/2013/Sept/Retrospectives_2013_final3.pdf
  # Equation 1:  rho <- (sum over p [ (X_y-p,p -X_y-p,0) / X_y-p,0]) / P
  mohn.out[["AFSC_Hurtado_SSB"]] <- sum(mohnSSB) / length(mohnSSB)
  mohn.out[["AFSC_Hurtado_Rec"]] <- sum(mohnRec) / length(mohnRec)
  mohn.out[["AFSC_Hurtado_F"]] <- sum(mohnF) / length(mohnF)
  mohn.out[["AFSC_Hurtado_Bratio"]] <- sum(mohnBratio) / length(mohnBratio)

  return(mohn.out)
}
