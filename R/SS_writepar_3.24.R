#' write ss.par file from SS version 3.24
#'
#' Write Stock Synthesis (version 3.24) parameter file from list object in R to file.
#'
#'
#' @param parlist  List object created by \code{\link{SS_readpar_3.24}}.
#' @param outfile Filename for where to write new parameter file.
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @param verbose Should there be verbose output while running the file?
#' @author Nathan R. Vaughan
#' @export
#' @seealso \code{\link{SS_readctl}}, \code{\link{SS_readdat}}
#' \code{\link{SS_readdat_3.24}},\code{\link{SS_readdat_3.24}}
#' \code{\link{SS_readctl_3.24}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}
SS_writepar_3.24 <- function(parlist, outfile, overwrite=TRUE, verbose=FALSE){
  
  # function to write Stock Synthesis parameter files
  if(verbose) cat("running SS_writepar_3.24\n")
  
  if(file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }
  
  if(verbose) message("Opening connection to ", outfile, "\n")
  zz <- file(outfile, open = "at") #open = "at" means open for appending in text mode.
  on.exit(close(zz)) # Needed in case the function exits early.

  if(!is.null(parlist$headerlines)){
    writeLines(paste0(parlist$headerlines[1]), con = zz)
    writeLines(paste0(parlist$headerlines[2]), con = zz)
    writeLines(paste0(parlist$headerlines[3]), con = zz)
  }
  
  if(!is.null(parlist$MG_parms)){
    for(i in 1:length(parlist$MG_parms[,1])){
      writeLines(paste0("# MGparm[",i,"]:"), con = zz)
      writeLines(paste0(parlist$MG_parms[i,2]), con = zz)
    }
  }

  if(!is.null(parlist$MG_parm_devs)){
    writeLines(paste0("# MGparm_dev:"), con = zz)
    for(i in 1:length(parlist$MG_parm_devs)){
      writeLines(paste0(parlist$MG_parm_devs[[i]][,2],collapse=" "), con = zz)
    }
  }
  
  if(!is.null(parlist$SR_parms)){
    for(i in 1:length(parlist$SR_parms[,1])){
      writeLines(paste0("# SR_parm[",i,"]:"), con = zz)
      writeLines(paste0(parlist$SR_parms[i,2]), con = zz)
    }
  }
  
  if(!is.null(parlist$recdev_cycle_parm)){
    for(i in 1:length(parlist$recdev_cycle_parm[,1])){
      writeLines(paste0("# recdev_cycle_parm[",i,"]:"), con = zz)
      writeLines(paste0(parlist$recdev_cycle_parm[i,2]), con = zz)
    }
  }

  if(!is.null(parlist$recdev_early)){
    writeLines(paste0("# recdev_early:"), con = zz)
    writeLines(paste0(parlist$recdev_early[,2],collapse=" "), con = zz)
  }

  if(!is.null(parlist$recdev1)){
    writeLines(paste0("# recdev1:"), con = zz)
    writeLines(paste0(parlist$recdev1[,2],collapse=" "), con = zz)
  }

  if(!is.null(parlist$recdev2)){
    writeLines(paste0("# recdev2:"), con = zz)
    writeLines(paste0(parlist$recdev2[,2],collapse=" "), con = zz)
  }

  if(!is.null(parlist$recdev_forecast)){
    writeLines(paste0("# Fcast_recruitments:"), con = zz)
    writeLines(paste0(parlist$recdev_forecast[,2],collapse=" "), con = zz)
  }

  if(!is.null(parlist$Fcast_impl_error)){
    writeLines(paste0("# Fcast_impl_error:"), con = zz)
    writeLines(paste0(parlist$Fcast_impl_error[,2],collapse=" "), con = zz)
  }

  if(!is.null(parlist$init_F)){
    for(i in 1:length(parlist$init_F)){
      writeLines(paste0("# init_F[",i,"]:"), con = zz)
      writeLines(paste0(parlist$init_F[i]), con = zz)
    }
  }

  if(!is.null(parlist$F_rate)){
    for(i in 1:length(parlist$F_rate[,1])){
      writeLines(paste0("# F_rate[",i,"]:"), con = zz)
      writeLines(paste0(parlist$F_rate[i,4]), con = zz)
    }
  }

  if(!is.null(parlist$Q_parms)){
    for(i in 1:length(parlist$Q_parms[,1])){
      writeLines(paste0("# Q_parm[",i,"]:"), con = zz)
      writeLines(paste0(parlist$Q_parms[i,2]), con = zz)
    }
  }

  if(!is.null(parlist$S_parms)){
    for(i in 1:length(parlist$S_parms[,1])){
      writeLines(paste0("# selparm[",i,"]:"), con = zz)
      writeLines(paste0(parlist$S_parms[i,2]), con = zz)
    }
  }
  
  if(!is.null(parlist$sel_parm_devs)){
    writeLines(paste0("# selparm_dev:"), con = zz)
    for(i in 1:length(parlist$sel_parm_devs)){
      writeLines(paste0(parlist$sel_parm_devs[[i]][,2],collapse=" "), con = zz)
    }
  }
 
  if(!is.null(parlist$TG_parms)){
    for(i in 1:length(parlist$TG_parms[,1])){
      writeLines(paste0("# TG_parm[",i,"]:"), con = zz)
      writeLines(paste0(parlist$TG_parms[i,2]), con = zz)
    }
  }
  
  
  
  return(parlist)
}


