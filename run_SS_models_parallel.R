#' Run multiple Stock Synthesis models in parallel using future and furrr
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
#'   will look for exe in the model folders. Default = FALSE.
#' @param parallel logical. If TRUE, will run in parallel, if FALSE, will not. 
#'   Default = TRUE.
#' @return Returns table showing which directories had model run and which
#' had errors like missing executable or Report.sso already present
#' @author Ian Taylor, Kathryn Doering
#' @importFrom furrr future_map
#' @import future
#' @export
#' @seealso \code{\link{copy_SS_inputs}},
#' \code{\link{populate_multiple_folders}}
#' @examples
#'
#'   \dontrun{
#' dirvec <- dir("c:/SS/models_to_run", full.names=TRUE)
#' run_SS_models_parallel(dirvec=dirvec)
#' 
#'   }
#'

run_SS_models_parallel <- function(dirvec = NULL,
                                   model = "ss",
                                   extras = "-nox",
                                   systemcmd = FALSE,
                                   skipfinished = TRUE,
                                   intern = FALSE,
                                   verbose = TRUE, 
                                   exe_in_path = FALSE, 
                                   parallel = TRUE) {
  # check to make sure the first input is in the correct format
  if(!is.character(dirvec)) {
    stop("Input 'dirvec' should be a character vector")
  }
  wd_orig <- getwd()
  on.exit(setwd(wd_orig), add = TRUE)
  
  OS <- "Mac" # don't know the version$os info for Mac
  if(length(grep("linux",version$os)) > 0) OS <- "Linux"
  if(length(grep("mingw",version$os)) > 0) OS <- "Windows"
  
  # figure out name of executable based on 'model' input which may contain .exe
  if(length(grep(".exe",tolower(model))) == 1) {
    # if input 'model' includes .exe then assume it's Windows and just use the name
    exe <- model
  } else {
    # if 'model' doesn't include .exe then append it (for Windows computers only)
    exe <- paste(model, ifelse(OS=="Windows",".exe",""),sep="")
  }
  
  if(exe_in_path == TRUE) {
    tmp_exe <- Sys.which(exe)[[1]] # get 1st ss exe with name exe that is in your path
    if(tmp_exe == ""){
      stop("Exe named ", exe, " was not found in your PATH.")
    } else {
      exe <- tmp_exe
    }
  }
  # set up model runs to run in parallel or not,
  if(parallel) {
    plan(multiprocess)
    prog_bar <- TRUE
  } else {
    plan(sequential)
    prog_bar <- FALSE
  }
  output <- future_map(dirvec ,
                       ~run_models_parallel_base(dir = .x,
                                                 exe = exe, 
                                                 exe_in_path = exe_in_path,
                                                 model = model,
                                                 skipfinished = skipfinished, 
                                                 extras = extras,
                                                 OS = OS,
                                                 systemcmd = systemcmd,
                                                 intern = intern), 
                       .progress = prog_bar)
  if(intern) {
    for(i in seq_along(output)) {
      writeLines(c("###",
                   "console output",
                   as.character(Sys.time()),
                   "###",
                   " ",
                   output[[i]]$console.output),
                 con = 'console.output.txt')
      message("console output written to console.output.txt")
    }
  }
  # return table of results
  all_results <- vapply(output, function(x) x$results, "string")
  return(data.frame(dir = dirvec, results = all_results))
}


#' Base function to run a Stock Synthesis model.
#'
#' Runs SS in a directory
#'
#' @param dir A director List of directories containing the model files
#' @param exe The path to the exe
#' @param exe_in_path logical. If TRUE, will look for exe in the PATH. If FALSE,
#'   will look for exe in the model folders. Default = FALSE.
#' @param skipfinished Skip any folders that already contain a Report.sso file.
#' This can be helpful if the function is interrupted.
#' @param model Executable name
#' @param extras Additional commands to use when running SS. Default = "-nox"
#' will reduce the amount of command-line output.
#' @param systemcmd Should R call SS using "system" function instead of "shell".
#' This may be required when running R in Emacs. Default = FALSE.
#' @param intern Show output in the R console or save to a file?
#'   Default = TRUE.
#' @return Returns table showing which directories had model run and which
#' had errors like missing executable or Report.sso already present
#' @export
#' @author Ian Taylor, Kathryn Doering
#' @seealso \code{\link{run_SS_models_parallel}}

run_models_parallel_base <- function(dir, exe, exe_in_path, skipfinished, 
                                     model, extras, OS, systemcmd, intern) {
  wd <- getwd()
  on.exit(setwd(wd))
  console.output <- NULL #initialize to avoid errors
  # confirm that dir exists
  if(!dir.exists(dir)) {
    warning("not a directory:", dir)
    results <- "not a directory"
  } else {
    # check whether exe is in directory (if not using exe in path)
    if(all(file.info(dir(dir, full.names=TRUE))$exe=="no")) {
      if(exe_in_path == FALSE) {
        warning("Executable ", exe, " not found in ", dir)
        results <- "no executable"
      }
    }
    if(skipfinished & "Report.sso" %in% dir(dir)) {
      # skip directories that have results in them
      message("Skipping ", dir, " since it contains a Report.sso file and ",
              "skipfinished = TRUE")
      results <- "contained Report.sso"
    }else {
      # run model
      message("changing working directory to ", dir)
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
      
      results <- "ran model"
    } # end model run
  } # end code for exe present
  
  return <- list(results = results, console.output = console.output)
}

