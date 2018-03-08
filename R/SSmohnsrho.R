##' Calculate Mohn's Rho values for select quantities
##'
##' Function calculates:
##' (1) a rho value for the ending year for each retrospective relative to the reference model,
##' (2) a "Wood's Hole Mohn's Rho", and
##' (3) an "Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho"
##' (2) and (3) are based on all years between the reference and the retrospective run.
##' Reference: Hurtado-Ferro et al. 2015. Looking in the rear-view mirror: bias and
##' retrospective patterns in integrated, age-structured stock assessment models. ICES J. Mar. Sci
##' 'Volume 72, Issue 1, 1 January 2015, Pages 99-110, https://doi.org/10.1093/icesjms/fsu198
##'
## Grab all the results
##' @param summaryoutput List created by \code{SSsummarize}. The expected order for the
##' models are the full reference model, the retro -1, retro -2, and so forth.
##' @param endyrvec Single year or vector of years representing the
##' final year of values to show for each model.
##' @param startyr Single year used to calculate the start of the Wood's Hole
##' Mohn's Rho value across all years.
##' @param verbose Print messages when running the function?
##'
##' @author Chantel R. Wetzel and Carey McGilliard
##'
##' @export

SSmohnsrho <-
  function(summaryoutput,
           endyrvec = NULL,
           startyr = NULL,
           verbose = TRUE) {
    if(verbose){
      message ("The expected order of models in the summary output are the\n",
               "reference model followed by retro -1, retro -2, and so forth.")
    }

    N <- summaryoutput$n
    if (length(endyrvec) == 0) {
      endyrvec <- rev((summaryoutput$endyrs[N] - N + 1):summaryoutput$endyrs[N])
    }

    if (length(startyr) == 0) {
      startyr <- summaryoutput$startyr[1]
    }


    mohnSSB <- mohnRec <- mohnBratio <- mohnF <- numeric()
    mohnSSB.all <- mohnRec.all <- mohnBratio.all <- mohnF.all <- numeric()
    HmohnSSB <- HmohnRec <- HmohnBratio <- HmohnF <- numeric()
    HmohnSSB.all <- HmohnRec.all <- HmohnBratio.all <- HmohnF.all <- numeric()

    # Mohn's Rho Calculation for the terminal year for each of
    # the retrospectives relative to the reference model
    # Rho <- sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
    for (i in 1:(N - 1)) {
      ind <- which(summaryoutput$SpawnBio$Yr == endyrvec[i])
      mohnSSB[i] <- (summaryoutput$SpawnBio[ind, i + 1] -
                       summaryoutput$SpawnBio[ind, 1]) /
                         summaryoutput$SpawnBio[ind, 1]

      ind <- which(summaryoutput$recruits$Yr == endyrvec[i])
      mohnRec[i] <- (summaryoutput$recruits[ind, i + 1] -
                       summaryoutput$recruits[ind, 1]) /
                         summaryoutput$recruits[ind, 1]

      ind <- which(summaryoutput$Bratio$Yr == endyrvec[i])
      mohnBratio[i] <- (summaryoutput$Bratio[ind, i + 1] -
                          summaryoutput$Bratio[ind, 1]) /
                            summaryoutput$Bratio[ind, 1]

      ind <- which(summaryoutput$Fvalue$Yr == endyrvec[i])
      mohnF[i] <- (summaryoutput$Fvalue[ind, i + 1] -
                     summaryoutput$Fvalue[ind, 1]) /
                       summaryoutput$Fvalue[ind, 1]
    }

    # Alaska Fisheries Science Center and Hurtado-Ferro et al. (2015) Mohn's rho
    # https://www.afsc.noaa.gov/REFM/stocks/Plan_Team/2013/Sept/Retrospectives_2013_final3.pdf
    # Equation 1:  Rho <- (sum over p [ (X_y-p,p -X_y-p,0) / X_y-p,0]) / P
    for (i in 1:(N - 1)) {
      Hind <- which(summaryoutput$SpawnBio$Yr == endyrvec[i + 1])
      HmohnSSB[i] <- (summaryoutput$SpawnBio[Hind, i + 1] -
                        summaryoutput$SpawnBio[Hind, 1]) /
                          summaryoutput$SpawnBio[Hind, 1]

      Hind <- which(summaryoutput$recruits$Yr == endyrvec[i + 1])
      HmohnRec[i] <- (summaryoutput$recruits[Hind, i + 1] -
                        summaryoutput$recruits[Hind, 1]) /
                          summaryoutput$recruits[Hind, 1]

      Hind <- which(summaryoutput$Fvalue$Yr == endyrvec[i + 1])
      HmohnF[i] <- (summaryoutput$Fvalue[Hind, i + 1] -
                      summaryoutput$Fvalue[Hind, 1]) /
                        summaryoutput$Fvalue[Hind, 1]

      Hind <- which(summaryoutput$Bratio$Yr == endyrvec[i + 1])
      HmohnBratio[i] <- (summaryoutput$Bratio[Hind, i + 1] -
                           summaryoutput$Bratio[Hind, 1]) /
                             summaryoutput$Bratio[Hind, 1]
    }

    # Wood's Hole Mohn's Rho Calculation for all years for each of the
    # retrospectives relative to the reference model
    # Rho <- sum over y [ (X_y,retro - X_y,ref) / X_y,ref ]
    # This rho value is then scaled acording to the number of model year
    # for comparison between the one year and all year calculation
    # Rho <- Rho / Number of Years
    for (i in 1:(N - 1)) {
      ind <- which(summaryoutput$SpawnBio$Yr == startyr):which(summaryoutput$SpawnBio$Yr == endyrvec[i])
      mohnSSB.all[i] <-
        sum((summaryoutput$SpawnBio[ind, i + 1] - summaryoutput$SpawnBio[ind, 1]) /
              summaryoutput$SpawnBio[ind, 1]) / length(ind)

      ind <- which(summaryoutput$recruits$Yr == startyr):which(summaryoutput$recruits$Yr == endyrvec[i])
      mohnRec.all[i] <-
        sum((summaryoutput$recruits[ind, i + 1] - summaryoutput$recruits[ind, 1]) /
              summaryoutput$recruits[ind, 1]) / length(ind)

      ind <- which(summaryoutput$Bratio$Yr == startyr + 1):which(summaryoutput$Bratio$Yr == endyrvec[i])
      mohnBratio.all[i] <-
        sum((summaryoutput$Bratio[ind, i + 1] - summaryoutput$Bratio[ind, 1]) /
              summaryoutput$Bratio[ind, 1]) / length(ind)

      ind <- which(summaryoutput$Fvalue$Yr == startyr):which(summaryoutput$Fvalue$Yr == endyrvec[i])
      mohnF.all[i] <-
        sum((summaryoutput$Fvalue[ind, i + 1] - summaryoutput$Fvalue[ind, 1]) /
              summaryoutput$Fvalue[ind, 1]) / length(ind)
    }

    mohn.out <- list()
    mohn.out$SSB <- sum(mohnSSB)
    mohn.out$Rec <- sum(mohnRec)
    mohn.out$Bratio <- sum(mohnBratio)
    mohn.out$F <- sum(mohnF)

    mohn.out$WoodHole_SSB.all <- sum(mohnSSB.all)
    mohn.out$WoodHole_Rec.all <- sum(mohnRec.all)
    mohn.out$WoodHole_Bratio.all <- sum(mohnBratio.all)
    mohn.out$WoodHole_F.all <- sum(mohnF.all)

    mohn.out$AFSC_Hurtado_SSB <- sum(HmohnSSB) / length(HmohnSSB)
    mohn.out$AFSC_Hurtado_Rec <- sum(HmohnRec) / length(HmohnRec)
    mohn.out$AFSC_Hurtado_F <- sum(HmohnF) / length(HmohnF)
    mohn.out$AFSC_Hurtado_Bratio <- sum(HmohnBratio) / length(HmohnBratio)
    return(mohn.out)
  }
