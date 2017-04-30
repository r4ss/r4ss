#' read control file from SS
#' 
#' Read Stock Synthesis control file into list object in R. This function is a
#' wrapper which calls either SS_readctl_3.24 or SS_readctl_3.30 (not yet written).
#' This setup allows those functions to be cleaner (if somewhat redundant)
#' than a single function that attempts to do everything.
#' 
#' 
#' @param file Filename either with full path or relative to working directory.
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param nseas number of season in the model. This information is not
#'  explicitly available in control file
#' @param N_areas number of spatial areas in the model. This information is also not
#'  explicitly available in control file
#' @param Nages oldest age in the model. This information is also not
#'  explicitly available in control file
#' @param Ngenders number of genders in the model. This information is also not
#'  explicitly available in control file
#' @param Npopbins number of population bins in the model. This information is also not
#'  explicitly available in control file and this information is only required if length based
#'  maturity vector is directly supplied (Maturity option of 6), and not yet tested
#' @param Nfish number of fisheries in the model. This information is also not
#'  explicitly available in control file
#' @param Nsurv number of survey fleets in the model. This information is also not
#'  explicitly available in control file
#' @param TG_Nrelgrp number of tag release groups in the model. This information is also not
#'  explicitly available in control file
#' @author Ian G. Taylor, Yukio Takeuchi
#' @export
#' @seealso \code{\link{SS_readctl_3.24}}, \code{\link{SS_readdat}},
#' \code{\link{SS_readdat_3.24}}

SS_readctl <- function(file, version="3.24", verbose=TRUE,echoall=FALSE,
                       ## Parameters that are not defined in control file
                       nseas=4,
                       N_areas=1,
                       Nages=20,
                       Ngenders=1,
                       Npopbins=NA,
                       Nfish=2,
                       Nsurv=2,
                       TG_Nrelgrp=NA){

  # wrapper function to call old or new version of SS_readctl

  # automatic testing of version number could be added here in the future
  # see SS_readdat for example attempt
  
  # call function for SS version 3.24
  if(version=="3.24"){ # should work whether "version" is character or numeric
    ctllist <- SS_readctl_3.24(file=file, verbose=verbose,
                               echoall=echoall,
                               nseas      = nseas,
                               N_areas    = N_areas,
                               Nages      = Nages,
                               Ngenders   = Ngenders,
                               Npopbins   = Npopbins,
                               Nfish      = Nfish,
                               Nsurv      = Nsurv,
                               TG_Nrelgrp = TG_Nrelgrp)
  }

  # call function for SS version 3.30
  if(version=="3.30" | version==3.3){ # turns out 3.30 != "3.30" in R
    stop("Function SS_readctl_3.30 has not been written yet")
  }

  # return the result
  return(ctllist)
}
