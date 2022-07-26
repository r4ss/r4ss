#' Iteratively apply the jitter option in SS
#'
#' Iteratively run a Stock Synthesis model with different jittered starting
#' parameter values based on the jitter fraction. Output files are renamed
#' in the format Report1.sso, Report2.sso, etc.
#'
#' @param dir Directory where model files are located.
#' @param mydir Deprecated. Use `dir` instead.
#' @param Intern Deprecated. Use `show_in_console` instead.
#' @param Njitter Number of jitters, or a vector of jitter iterations.
#'   If `length(Njitter) > 1` only the iterations specified will be ran,
#'   else `1:Njitter` will be executed.
#' @param printlikes A logical value specifying if the likelihood values should
#'   be printed to the console.
#' @param jitter_fraction The value, typically 0.1, used to define a uniform
#'   distribution in cumulative normal space to generate new initial parameter values.
#'   The default of `NULL` forces the user to specify the jitter_fraction
#'   in the starter file, and this value must be greater than zero and
#'   will not be overwritten.
#' @param init_values_src Either zero or one, specifying if the initial values to
#'   jitter should be read from the control file or from the par file, respectively.
#'   The default is `NULL`, which will leave the starter file unchanged.
#' @template exe
#' @template verbose
#' @param ... Additional arguments passed to [r4ss::run()], such as
#' `extras`, `show_in_console`, and `skipfinished`.
#'
#' @author James T. Thorson, Kelli F. Johnson, Ian G. Taylor
#'
#' @return A vector of likelihoods for each jitter iteration.
#' @export
#' @examples
#' \dontrun{
#' #### Run jitter from par file with arbitrary, but common, choice of 0.1
#' modeldir <- tail(dir(system.file("extdata", package = "r4ss"), full.names = TRUE), 1)
#' numjitter <- 25
#' jit.likes <- SS_RunJitter(
#'   dir = modeldir, Njitter = numjitter,
#'   jitter_fraction = 0.1, init_value_src = 1
#' )
#'
#' #### Read in results using other r4ss functions
#' # (note that un-jittered model can be read using keyvec=0:numjitter)
#' profilemodels <- SSgetoutput(dirvec = modeldir, keyvec = 1:numjitter, getcovar = FALSE)
#' # summarize output
#' profilesummary <- SSsummarize(profilemodels)
#' # Likelihoods
#' profilesummary[["likelihoods"]][1, ]
#' # Parameters
#' profilesummary[["pars"]]
#' }
#'
SS_RunJitter <- function(dir = getwd(),
                         mydir = lifecycle::deprecated(),
                         Intern = lifecycle::deprecated(),
                         Njitter,
                         printlikes = TRUE,
                         jitter_fraction = NULL,
                         init_values_src = NULL,
                         exe = "ss",
                         verbose = FALSE,
                         ...) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(Intern)) {
    lifecycle::deprecate_warn(
      when = "1.45.1",
      what = "SS_RunJitter(Intern)",
      details = "Please use 'show_in_console' instead"
    )
  }
  if (lifecycle::is_present(mydir)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "SS_RunJitter(mydir)",
      details = "Please use 'dir' instead"
    )
    dir <- mydir
  }

  # check for executable and keep cleaned name of executable file
  exe <- check_exe(exe = exe, dir = dir, verbose = verbose)[["exe"]]

  # Determine working directory on start and return upon exit
  startdir <- getwd()
  on.exit(setwd(startdir))
  setwd(dir)

  if (verbose) {
    message("Temporarily changing working directory to:\n", dir)
    if (!file.exists("Report.sso")) {
      message(
        "Copy output files from a converged run into\n",
        dir, "\nprior to running SS_RunJitter to enable easier comparisons."
      )
    }
    message("Checking starter file")
  }
  # read starter file to test for non-zero jitter value
  starter <- SS_readstarter(verbose = verbose)
  starter[["parmtrace"]] <- ifelse(starter[["parmtrace"]] == 0, 1, starter[["parmtrace"]])
  if (starter[["jitter_fraction"]] == 0 & is.null(jitter_fraction)) {
    stop("Change the jitter value in the starter file to be > 0\n",
      "or change the 'jitter_fraction' argument to be > 0.",
      call. = FALSE
    )
  }
  if (!is.null(jitter_fraction)) {
    starter[["jitter_fraction"]] <- jitter_fraction
  }
  if (!is.null(init_values_src)) {
    starter[["init_values_src"]] <- init_values_src
  }
  r4ss::SS_writestarter(starter, overwrite = TRUE, verbose = FALSE)
  file_increment(0, verbose = verbose)

  # check length of Njitter input
  if (length(Njitter) == 1) {
    Njitter <- 1:Njitter
  }
  likesaved <- rep(NA, length(Njitter))
  for (i in Njitter) {
    if (verbose) {
      message("Jitter=", i, ", ", date())
    }
    # check for use of .par file and replace original if needed
    if (starter[["init_values_src"]] == 1) {
      if (verbose) message("Replacing .par file with original")
      file.copy(from = "ss.par_0.sso", to = "ss.par", overwrite = TRUE)
    }
    # run model
    run(dir = dir, exe = exe, verbose = verbose, ...)

    # Only save stuff if it converged
    if ("Report.sso" %in% list.files()) {
      rep <- SS_read_summary()
      if (is.null(rep)) {
        report <- SS_output(
          dir = getwd(), forecast = FALSE,
          covar = FALSE, checkcor = FALSE, NoCompOK = TRUE,
          verbose = verbose, warn = verbose, hidewarn = !verbose, printstats = verbose
        )
        like <- report[["likelihoods_used"]][row.names(report[["likelihoods_used"]]) == "TOTAL", "values"]
      } else {
        like <- rep[["likelihoods"]][grep("TOTAL", row.names(rep[["likelihoods"]])), 1]
      }
      likesaved[i] <- like
      if (printlikes) {
        message("Likelihood for jitter ", i, " = ", like)
      }
      # rename output files
      file_increment(i = i)
    } else {
      if (verbose) warning("No Report.sso file found from run ", i)
    }
  }
  # Move original files back
  pattern0 <- dir(pattern = "[a-z_]0\\.sso")
  file.copy(
    from = pattern0,
    to = gsub("([a-zA-Z])0|_0\\.sso", "\\1", pattern0),
    overwrite = TRUE
  )

  if (printlikes) {
    message("Table of likelihood values:")
    print(table(likesaved))
  }
  return(invisible(likesaved))
}
