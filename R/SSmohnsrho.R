##' Calculate Mohn's Rho values for select quantities
##'
##' Function calculates:
##' (1) a rho value for the ending year for each retrospective relative to the reference model
##' as in Mohn (1999),
##' (2) a "Wood's Hole Mohn's Rho", which is a rho value averaged across all years for each
##' retrospective relative to the reference model, and
##' (3) an "Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho,
##' which is the average rho per retrospective "peel".
##'
##'
##' @param summaryoutput List created by `SSsummarize`. The expected order for the
##' models are the full reference model, the retro -1, retro -2, and so forth.
##' @param endyrvec Single year or vector of years representing the
##' final year of values to show for each model.
##' @param startyr Single year used to calculate the start of the Wood's Hole
##' Mohn's Rho value across all years. Defaults to startyr of reference model.
##' @param verbose Print messages when running the function?
##'
##' @author Chantel R. Wetzel and Carey McGilliard
##' @references Hurtado-Ferro et al. 2015. Looking in the rear-view mirror: bias
##' and retrospective patterns in integrated, age-structured stock assessment
##' models. ICES J. Mar. Sci Volume 72, Issue 1, 1 January 2015,
##' Pages 99-110, https://doi.org/10.1093/icesjms/fsu198
##' Mohn, R. 1999. The retrospective problem in sequential population analysis:
##' An investigation using cod fishery and simulated data. ICES J. Mar. Sci
##' Volume 56, Pages 473-488
##'
##' @export

SSmohnsrho <-
  function(summaryoutput,
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
      endyrvec <- rev((summaryoutput[["endyrs"]][N] - N + 1):summaryoutput[["endyrs"]][N])
    }

    if (missing(startyr)) {
      startyr <- summaryoutput[["startyrs"]][1]
    }


    mohnSSB <- mohnRec <- mohnBratio <- mohnF <- numeric()
    mohnSSB.all <- mohnRec.all <- mohnBratio.all <- mohnF.all <- numeric()

    # Mohn's Rho Calculation for the terminal year for each of
    # the retrospectives relative to the reference model
    # Rho <- sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
    for (i in 1:(N - 1)) {
      ind <- which(summaryoutput[["SpawnBio"]][["Yr"]] == endyrvec[i + 1])
      mohnSSB[i] <- (summaryoutput[["SpawnBio"]][ind, i + 1] -
        summaryoutput[["SpawnBio"]][ind, 1]) /
        summaryoutput[["SpawnBio"]][ind, 1]

      ind <- which(summaryoutput[["recruits"]][["Yr"]] == endyrvec[i + 1])
      mohnRec[i] <- (summaryoutput[["recruits"]][ind, i + 1] -
        summaryoutput[["recruits"]][ind, 1]) /
        summaryoutput[["recruits"]][ind, 1]

      ind <- which(summaryoutput[["Bratio"]][["Yr"]] == endyrvec[i + 1])
      mohnBratio[i] <- (summaryoutput[["Bratio"]][ind, i + 1] -
        summaryoutput[["Bratio"]][ind, 1]) /
        summaryoutput[["Bratio"]][ind, 1]

      ind <- which(summaryoutput[["Fvalue"]][["Yr"]] == endyrvec[i + 1])
      mohnF[i] <- (summaryoutput[["Fvalue"]][ind, i + 1] -
        summaryoutput[["Fvalue"]][ind, 1]) /
        summaryoutput[["Fvalue"]][ind, 1]
    }

    # Wood's Hole Mohn's Rho Calculation for all years for each of the
    # retrospectives relative to the reference model
    # Rho <- sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
    # This rho value is then scaled according to the number of model years
    # for comparison between the one year and all year calculation
    # Rho <- Rho / Number of Years
    for (i in 1:(N - 1)) {
      ind <- which(summaryoutput[["SpawnBio"]][["Yr"]] == startyr):which(summaryoutput[["SpawnBio"]][["Yr"]] == endyrvec[i + 1])
      mohnSSB.all[i] <-
        sum((summaryoutput[["SpawnBio"]][ind, i + 1] - summaryoutput[["SpawnBio"]][ind, 1]) /
          summaryoutput[["SpawnBio"]][ind, 1]) / length(ind)
      ind <- which(summaryoutput[["recruits"]][["Yr"]] == startyr):which(summaryoutput[["recruits"]][["Yr"]] == endyrvec[i + 1])
      mohnRec.all[i] <-
        sum((summaryoutput[["recruits"]][ind, i + 1] - summaryoutput[["recruits"]][ind, 1]) /
          summaryoutput[["recruits"]][ind, 1]) / length(ind)
      if (length(which(summaryoutput[["Bratio"]][["Yr"]] == startyr + 1)) != 0) {
        ind <- which(summaryoutput[["Bratio"]][["Yr"]] == startyr + 1):which(summaryoutput[["Bratio"]][["Yr"]] == endyrvec[i + 1])
        mohnBratio.all[i] <-
          sum((summaryoutput[["Bratio"]][ind, i + 1] - summaryoutput[["Bratio"]][ind, 1]) /
            summaryoutput[["Bratio"]][ind, 1]) / length(ind)
      } else {
        warning("Skipping Wood's Hole Mohns Rho on Bratio, as Bratio is not available for year after the first model year.")
        mohnBratio.all[i] <- NA
      }
      if (length(which(summaryoutput[["Fvalue"]][["Yr"]] == startyr)) != 0) {
        ind <- which(summaryoutput[["Fvalue"]][["Yr"]] == startyr):which(summaryoutput[["Fvalue"]][["Yr"]] == endyrvec[i + 1])
        mohnF.all[i] <-
          sum((summaryoutput[["Fvalue"]][ind, i + 1] - summaryoutput[["Fvalue"]][ind, 1]) /
            summaryoutput[["Fvalue"]][ind, 1]) / length(ind)
      } else {
        warning("Skipping Wood's Hole Mohn's Rho on Fvalue, ecause Fvalue is not available for first model year.")
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
    # Equation 1:  Rho <- (sum over p [ (X_y-p,p -X_y-p,0) / X_y-p,0]) / P
    mohn.out[["AFSC_Hurtado_SSB"]] <- sum(mohnSSB) / length(mohnSSB)
    mohn.out[["AFSC_Hurtado_Rec"]] <- sum(mohnRec) / length(mohnRec)
    mohn.out[["AFSC_Hurtado_F"]] <- sum(mohnF) / length(mohnF)
    mohn.out[["AFSC_Hurtado_Bratio"]] <- sum(mohnBratio) / length(mohnBratio)
    return(mohn.out)
  }
