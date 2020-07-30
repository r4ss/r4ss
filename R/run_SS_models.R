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
#' @param exe_in_path logical. If TRUE, will look for exe in the PATH. If FALSE,
#' will look for exe in the model folders. Default = FALSE.
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
                          verbose = TRUE, 
                          exe_in_path = FALSE){
  # check to make sure the first input is in the correct format
  if(!is.character(dirvec)){
    stop("Input 'dirvec' should be a character vector")
  }
  wd_orig <- getwd()
  on.exit(setwd(wd_orig), add = TRUE)
  
  # vector of NA values to store results
  results <- rep(NA, length(dirvec))

  # this should always be "windows" or "unix" (includes Mac and Linux)
  OS <- .Platform$OS.type

  # figure out name of executable based on 'model' input which may contain .exe
  if(length(grep(".exe",tolower(model))) == 1){
    # if input 'model' includes .exe then assume it's Windows and just use the name
    exe <- model
  }else{
    # if 'model' doesn't include .exe then append it (for Windows computers only)
    exe <- paste(model, ifelse(OS=="windows",".exe",""),sep="")
  }
  
  if(exe_in_path == TRUE) {
    tmp_exe <- Sys.which(exe)[[1]] # get 1st ss exe with name exe that is in your path
    if(tmp_exe == ""){
      stop("Exe named ", exe, " was not found in your PATH.")
    } else {
      exe <- tmp_exe
    }
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
      # check whether exe is in directory (if not using exe in path)
      if(all(file.info(dir(dir, full.names=TRUE))$exe=="no")){
        if(exe_in_path == FALSE){
          warning("Executable ",exe," not found in ",dir)
          results[idir] <- "no executable"
        }
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
        if(OS!="windows"){
          command <- paste0("./", command)
        }
        message("Running model in directory: ",getwd())
        message("Using the command: ", command)
        if(OS=="windows" & !systemcmd){
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
        setwd(wd_orig) # needed when using relative paths
      } # end model run
    } # end code for exe present
  } # end loop over directories

  # return table of results
  return(data.frame(dir=dirvec, results=results))
}
