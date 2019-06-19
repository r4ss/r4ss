#' Run multiple Stock Synthesis models
#'
#' Loops over a vector of directories and iteratively runs SS in each one
#'
#' @param dirvec List of directories containing the model files
#' @param model Executable name
#' @param extras Additional commands to use when running SS. Default = "-nox"
#' will reduce the amount of command-line output.
#' @param systemcmd Should R call SS using "system" function instead of "shell".
#' This may be required when running R in Emacs. Default = FALSE.
#' @param skipfinished Skip any folders that already contain a Report.sso file.
#' This can be helpful if the function is interrupted.
#' @param intern Show output in the R console or save to a file?
#' @param verbose Return updates of function progress to the R console?
#' @return Returns table showing which directories had model run and which
#' had errors like missing executable or Report.sso already present
#' @author Ian Taylor
#' @export
#' @seealso \code{\link{copy_SS_inputs}},
#' \code{\link{populate_multiple_folders}}
#' @examples
#'
#'   \dontrun{
#' dirvec <- dir("c:/SS/models_to_run", full.names=TRUE)
#' run_SS_models(dirvec=dirvec)
#' 
#'   }
#'

run_SS_models <- function(dirvec = NULL,
                          model = "ss",
                          extras = "-nox",
                          systemcmd = FALSE,
                          skipfinished = TRUE,
                          intern = FALSE,
                          verbose = TRUE){
  # check to make sure the first input is in the correct format
  if(!is.character(dirvec)){
    stop("Input 'dirvec' should be a character vector")
  }

  # vector of NA values to store results
  results <- rep(NA, length(dirvec))
  
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"

  # figure out name of executable based on 'model' input which may contain .exe
  if(length(grep(".exe",tolower(model))) == 1){
    # if input 'model' includes .exe then assume it's Windows and just use the name
    exe <- model
  }else{
    # if 'model' doesn't include .exe then append it (for Windows computers only)
    exe <- paste(model, ifelse(OS=="Windows",".exe",""),sep="")
  }

  # loop over directories
  for(idir in 1:length(dirvec)){
    # directory where stuff will happen
    dir <- dirvec[idir]
    
    # confirm that dir exists
    if(!dir.exists(dir)){
      warning("not a directory:", dir)
      results[idir] <- "not a directory"
    }else{
      # check whether exe is in directory
      if(all(file.info(dir(dir, full.names=TRUE))$exe=="no")){
        warning("Executable ",exe," not found in ",dir)
        results[idir] <- "no executable"
      }
      if(skipfinished & "Report.sso" %in% dir(dir)){
        # skip directories that have results in them
        message("Skipping ", dir, " since it contains a Report.sso file and skipfinished=TRUE")
        results[idir] <- "contained Report.sso"
      }else{
        # run model
        message("changing working directory to ",dir)
        setwd(dir) # change working directory

        command <- paste(model, extras)
        if(OS!="Windows"){
          command <- paste0("./", command)
        }
        message("Running model in directory: ",getwd())
        message("Using the command: ", command)
        if(OS=="Windows" & !systemcmd){
          console.output <- shell(cmd=command, intern=intern)
        }else{
          console.output <- system(command, intern=intern)
        }
        if(intern){
          writeLines(c("###",
                       "console output",
                       as.character(Sys.time()),
                       "###",
                       " ",
                       console.output),
                     con = 'console.output.txt')
          message("console output written to console.output.txt")
        }
        results[idir] <- "ran model"
      } # end model run
    } # end code for exe present
  } # end loop over directories

  # return table of results
  return(data.frame(dir=dirvec, results=results))
}
