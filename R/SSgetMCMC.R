#' Read MCMC output.
#' 
#' Reads the MCMC output (in the posteriors.sso and derived_posteriors.sso
#' files) from a model.
#' 
#' 
#' @param dir Directory containing MCMC output.
#' @param verbose TRUE/FALSE switch to get more or less information about the
#' progress of the function.
#' @param writecsv Write key parameters and certainty nuisance quantities to a
#' CSV file.
#' @param postname Name of file with parameter posteriors (default matches
#' "posteriors.sso" used by SS, but the user could change the name)
#' @param derpostname Name of file with parameter posteriors (default matches
#' "derived_posteriors.sso" used by SS, but the user could change the name)
#' @param csv1 First CSV file for key parameters.
#' @param csv2 Second CSV file for nuisance quantities.
#' @param keystrings Vector of strings that partially match parameter names to
#' write to the file csv1. This file intended to feed into
#' \code{\link{mcmc.out}}.
#' @param nuisancestrings Vector of strings that partially match derived
#' quantity names to write to the file csv2. This file intended to feed into
#' \code{\link{mcmc.nuisance}}.
#' @param burnin Optional burn-in value to apply on top of the option in the
#' starter file.
#' @param thin Optional thinning value to apply on top of the option in the
#' starter file and in the \code{-mcsave} runtime command.
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{mcmc.out}}, \code{\link{mcmc.nuisance}},
#' \code{\link{SSplotPars}}
SSgetMCMC <-
  function(dir=NULL,
           verbose=TRUE,
           writecsv=FALSE,
           postname="posteriors.sso",
           derpostname="derived_posteriors.sso",
           csv1="keyposteriors.csv",
           csv2="nuisanceposteriors.csv",
           keystrings=c(       # values that get written to csv1
               "NatM",
               "R0",
               "steep",
               "RecrDev_2008",
               "Q_extraSD"),
           nuisancestrings=c(  # values that get written to csv2
               "Objective_function",
               "SSB_",
               "InitAge",
               "RecrDev"),
           burnin = 0,            #the number of values to discard for burnin
           thin = 1               #the thinning interval
           )
{
  
  # get MCMC output
  if(verbose){
    message("reading MCMC output in\n", dir)
  }
  post <- read.table(file.path(dir, postname), header=TRUE)
  names(post)[names(post)=="SR_LN.R0."] <- "SR_LN(R0)"
  
  derpost <- read.table(file.path(dir, derpostname), header=TRUE)
  # remove redundant values
  derpost <- derpost[, !(names(derpost) %in% c("Iter", "Objective_function"))]

  # combine two dataframes
  allpost <- cbind(post, derpost)

  #apply burnin and thinning
  allpost <- allpost[seq((burnin+1), nrow(allpost), thin), ]

  # separate "key posteriors" from "nuisance posteriors"
  keylabels <- NULL
  nuisancelabels <- NULL
  for(istring in 1:length(keystrings)){
    keylabels <- c(keylabels,
                   names(allpost)[grep(keystrings[istring], names(allpost))])
  }
  for(istring in 1:length(nuisancestrings)){
    nuisancelabels <- c(nuisancelabels,
                        names(allpost)[grep(nuisancestrings[istring], names(allpost))])
  }
  keypost <- allpost[, names(allpost) %in% keylabels]
  nuisancepost <- allpost[, names(allpost) %in% nuisancelabels]

  # if requested, write CSV files for key and nuisance posteriors
  if(writecsv){
    file1 <- file.path(dir, csv1)
    file2 <- file.path(dir, csv2)
    if(verbose){
      message("writing subset of posteriors to files:\n  ",
              file1, "\n  ", file2)
    }
    write.csv(keypost, file1, row.names=FALSE)
    write.csv(nuisancepost, file2, row.names=FALSE)
  }

  # return data.frame containing posteriors
  return(invisible(allpost))
}
