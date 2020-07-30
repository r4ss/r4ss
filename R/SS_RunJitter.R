#' Iteratively apply the jitter option in SS
#'
#' Iteratively run a Stock Synthesis model with different jittered starting
#' parameter values based on the jitter fraction. Output files are renamed
#' in the format Report1.sso, Report2.sso, etc.
#'
#' @param mydir Directory where model files are located.
#' @template model
#' @param extras Additional command line arguments passed to the executable.
#'   The default, \code{"-nohess"}, runs each jittered model without the hessian.
#' @param Njitter Number of jitters, or a vector of jitter iterations.
#'   If \code{length(Njitter) > 1} only the iterations specified will be ran,
#'   else \code{1:Njitter} will be executed.
#' @param Intern Show command line info in R console or keep hidden. The default,
#'   \code{TRUE}, keeps the executable hidden.
#' @param systemcmd Option to switch between 'shell' and 'system'. The default,
#'   \code{FALSE}, facilitates using the shell command on Windows.
#' @param printlikes A logical value specifying if the likelihood values should
#'   be printed to the console.
#' @template verbose
#' @param jitter_fraction The value, typically 0.1, used to define a uniform
#'   distribution in cumulative normal space to generate new initial parameter values.
#'   The default of \code{NULL} forces the user to specify the jitter_fraction
#'   in the starter file, and this value must be greater than zero and
#'   will not be overwritten.
#' @param init_values_src Either zero or one, specifying if the initial values to
#'   jitter should be read from the control file or from the par file, respectively.
#'   The default is \code{NULL}, which will leave the starter file unchanged.
#' @author James T. Thorson, Kelli F. Johnson, Ian G. Taylor
#' @return A vector of likelihoods for each jitter iteration.
#' @export
#' @examples
#' \dontrun{
#'   #### Run jitter from par file with arbitrary, but common, choice of 0.1
#'   modeldir <- tail(dir(system.file("extdata", package="r4ss"), full.names=TRUE),1)
#'   numjitter <- 25
#'   jit.likes <- SS_RunJitter(mydir=modeldir, Njitter=numjitter,
#'    jitter_fraction=0.1, init_value_src=1)
#'
#'   #### Read in results using other r4ss functions
#'   # (note that un-jittered model can be read using keyvec=0:numjitter)
#'   profilemodels <- SSgetoutput(dirvec=modeldir, keyvec=1:numjitter, getcovar=FALSE)
#'   # summarize output
#'   profilesummary <- SSsummarize(profilemodels)
#'   # Likelihoods
#'   profilesummary$likelihoods[1,]
#'   # Parameters
#'   profilesummary$pars
#' }

SS_RunJitter <- function(mydir,
                         model = "ss",
                         extras = "-nohess",
                         Njitter,
                         Intern = TRUE,
                         systemcmd = FALSE,
                         printlikes = TRUE,
                         verbose = FALSE,
                         jitter_fraction = NULL,
                         init_values_src = NULL) {
  # Determine working directory on start and return upon exit
  startdir <- getwd()
  on.exit(setwd(startdir))
  setwd(mydir)
  model <- check_model(model = model, mydir = getwd())

  if (verbose) {
    message("Temporarily changing working directory to:\n", mydir)
    if (!file.exists("Report.sso")) {
      message("Copy output files from a converged run into\n",
        mydir, "\nprior to running SS_RunJitter to enable easier comparisons.")
    }
    message("Checking starter file")
  }
  # read starter file to test for non-zero jitter value
  starter <- SS_readstarter(verbose=verbose)
  starter$parmtrace <- ifelse(starter$parmtrace == 0, 1, starter$parmtrace)
  if (starter$jitter_fraction == 0 & is.null(jitter_fraction)) {
    stop("Change the jitter value in the starter file to be > 0\n",
      "or change the jitter_fraction argument to be > 0.", call. = FALSE)
  }
  if (!is.null(jitter_fraction)) {
    starter$jitter_fraction <- jitter_fraction
  }
  if (!is.null(init_values_src)) {
    starter$init_values_src <- init_values_src
  }
  r4ss::SS_writestarter(starter, overwrite = TRUE, verbose = FALSE, warn = FALSE)
  file_increment(0, verbose = verbose)

  # create empty ss.dat file to avoid the ADMB message
  # "Error trying to open data input file ss.dat"
  if (!file.exists(paste0(model,".dat"))) {
    file.create(paste0(model,".dat"))
  }

  # check length of Njitter input
  if (length(Njitter) == 1){
    Njitter <- 1:Njitter
  }
  likesaved <- rep(NA, length(Njitter))
  for(i in Njitter){
    if (verbose) message("Jitter=", i, ", ", date())
    # check for use of .par file and replace original if needed
    if(starter$init_values_src == 1){
      if (verbose) message("Replacing .par file with original")
      file.copy(from=paste0(model,".par_0.sso"), to=paste0(model,".par"), overwrite=TRUE)
    }
    # run model
    command <- paste(model, extras)
    if (.Platform$OS.type!="windows") {
      command <- paste0("./", command)
    }

    if (i == 1 & verbose){
      message("Running SS jitter in directory: ", getwd(),
        "\nUsing the command: ", command)
    }
    if (.Platform$OS.type == "windows" & !systemcmd) {
      shell(cmd = command, intern = Intern)
    }else{
      system(command, intern = Intern, show.output.on.console = !Intern)
    }
    # Only save stuff if it converged
    if ("Report.sso" %in% list.files()) {
      rep <- SS_read_summary()
      if (is.null(rep)) {
        report <- SS_output(dir = getwd(), forecast = FALSE,
          covar = FALSE, checkcor = FALSE, NoCompOK = TRUE,
          verbose = verbose, warn = verbose, hidewarn = !verbose, printstats = verbose)
        like <- report$likelihoods_used[row.names(report$likelihoods_used) == "TOTAL", "values"]
      } else {
        like <- rep$likelihoods[grep("TOTAL", row.names(rep$likelihoods)), 1]
      }
      likesaved[i] <- like
      if (printlikes){
        message("Likelihood for jitter ", i, " = ", like)
      }
      # rename output files
      file_increment(i = i)
    } else{
      if (verbose) warning("No Report.sso file found from run ", i)
    }
  }
  # Move original files back
  pattern0 <- dir(pattern = "[a-z_]0\\.sso")
  file.copy(from = pattern0,
    to = gsub("([a-zA-Z])0|_0\\.sso", "\\1", pattern0),
    overwrite = TRUE)

  if (printlikes){
    message("Table of likelihood values:")
    print(table(likesaved))
  }
  return(invisible(likesaved))
}
