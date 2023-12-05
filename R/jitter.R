#' Deprecated function to run jitters, renamed to jitter()
#'
#' @template deprecated_dots
#' @description
#' `r lifecycle::badge("deprecated")`
#' SS_RunJitter() has been renamed as [jitter()]. See
#' https://github.com/r4ss/r4ss/issues/723 for more details.
#'
#' @author Ian G. Taylor
#' @export
#' @seealso [jitter()]
SS_RunJitter <-
  function(...) {
    lifecycle::deprecate_stop(
      when = "1.46.1",
      what = "SS_RunJitter()",
      with = "jitter()"
    )
  }

#' Iteratively run Stock Synthesis with jittered starting values
#'
#' Iteratively run a Stock Synthesis model with different jittered starting
#' parameter values based on the jitter fraction. Output files are renamed
#' in the format Report1.sso, Report2.sso, etc.
#'
#' @param dir Directory where model files are located.
#' @param mydir Deprecated. Use `dir` instead.
#' @param Intern Deprecated. Use `show_in_console` instead.
#' @param Njitter Number of jitters, or a vector of jitter iterations.
#'   If `length(Njitter) > 1` only the iterations specified will be run,
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
#' @author James T. Thorson, Kelli F. Johnson, Ian G. Taylor,
#' Kathryn L. Doering, Kiva L. Oken
#'
#' @return A vector of likelihoods for each jitter iteration.
#' @export
#' @importFrom furrr future_map_dbl
#' @family run functions
#' @details This function will loop through models using the default strategy set by the
#' `future` package in the current working environment. In general, this means models
#' will run sequentially. To run multiple models simultaneously using parallel
#' computing, see [future::plan()]
#'
#' Note that random number generation occurs outside of R directly in stock synthesis.
#' When running jitters in parallel (i.e. `future` strategy is not `sequential`), no
#' steps are taken to ensure independence of random numbers generated across
#' cores. While the likelihood of the cores using the exact same seed is infinitesimal,
#' random numbers may not technically be considered statistically independent. If
#' jitter results are only used  as a general heuristic for model convergence, this
#' mild lack of independence should not matter much.
#'
#' When running models in parallel, the transfer of large files leads to expensive
#' overheads and parallel processing may not be faster. Covariance files are
#' especially expensive to transfer, so the option `extras = '-nohess'` is
#' recommended when using parallel processing.
#' @examples
#' \dontrun{
#' #### Run jitter from par file with arbitrary, but common, choice of 0.1
#' modeldir <- tail(dir(system.file("extdata", package = "r4ss"), full.names = TRUE), 1)
#' numjitter <- 25
#' jit.likes <- jitter(
#'   dir = modeldir, Njitter = numjitter,
#'   jitter_fraction = 0.1, init_value_src = 1
#' )
#'
#' #### Run same jitter in parallel
#' ncores <- parallelly::availableCores() - 1
#' future::plan(future::multisession, workers = ncores)
#' jit.likes <- jitter(
#'   dir = modeldir, Njitter = numjitter,
#'   jitter_fraction = 0.1, init_value_src = 1
#' )
#' future::plan(future::sequential)
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
jitter <- function(dir = getwd(),
                   mydir = lifecycle::deprecated(),
                   Intern = lifecycle::deprecated(),
                   Njitter,
                   printlikes = TRUE,
                   jitter_fraction = NULL,
                   init_values_src = NULL,
                   exe = "ss3",
                   verbose = FALSE,
                   ...) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(Intern)) {
    lifecycle::deprecate_warn(
      when = "1.45.1",
      what = "jitter(Intern)",
      with = "jitter(show_in_console)"
    )
  }
  if (lifecycle::is_present(mydir)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "jitter(mydir)",
      with = "jitter(dir)"
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
        dir, "\nprior to running jitter to enable easier comparisons."
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

  # I'm not sure if this is necessary anymore
  file_increment(0, verbose = verbose)

  # check length of Njitter input
  if (length(Njitter) == 1) {
    Njitter <- 1:Njitter
  }

  likesaved <- furrr::future_map_dbl(Njitter, function(.x) {
    iterate_jitter(
      i = .x,
      dir = dir,
      printlikes = printlikes,
      exe = exe,
      verbose = verbose,
      init_values_src = starter[["init_values_src"]],
      ...
    )
  })

  # rename output files and move them to base model directory
  to_copy <- purrr::map(Njitter, ~ list.files(
    path = file.path(dir, paste0("jitter", .x)),
    pattern = "^[CcPRw][a-zA-Z]+\\.sso|summary\\.sso|\\.par$"
  ))

  new_name <- purrr::imap(to_copy, ~ gsub(
    pattern = "par",
    replacement = "par_",
    x = gsub(
      pattern = "\\.sso|(\\.par)",
      replacement = paste0("\\1", .y, ".sso"),
      x = .x
    )
  ))

  purrr::pwalk(
    list(Njitter, to_copy, new_name),
    function(.i, .x, .y) {
      file.copy(
        from = file.path(paste0("jitter", .i), .x),
        to = .y,
        overwrite = TRUE
      )
    }
  )

  # delete jitter model directory
  purrr::walk(Njitter, ~ unlink(paste0("jitter", .x), recursive = TRUE))

  # only necessary if the file_increment line is maintained.
  pattern0 <- list.files(pattern = "[a-z_]0\\.sso")
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

#' Execute a single jittered model run
#'
#' @param i Index of the jitter iteration.
#' @param dir Directory of the base model to be jittered
#' @param  printlikes A logical value specifying if the likelihood values should
#'   be printed to the console.
#' @template  exe
#' @template  verbose
#' @param init_values_src Either zero or one, specifying if the initial values to
#'   jitter should be read from the control file or from the par file, respectively.
#'   Cannot be `NULL`. Defaults to zero (initial values read from control file).
#' @param ... Additional arguments passed to [r4ss::run()]
#' @author James T. Thorson, Kelli F. Johnson, Ian G. Taylor,
#' Kathryn L. Doering, Kiva L. Oken
#'
#' @return Negative log-likelihood of one jittered model
#'
iterate_jitter <- function(i,
                           dir = getwd(),
                           printlikes = TRUE,
                           exe = "ss",
                           verbose = FALSE,
                           init_values_src = 0,
                           ...) {
  jitter_dir <- file.path(dir, paste0("jitter", i))
  copy_SS_inputs(
    dir.old = dir, dir.new = jitter_dir, overwrite = TRUE,
    verbose = verbose, copy_exe = TRUE,
    copy_par = as.logical(init_values_src)
  )
  # run model
  r4ss::run(dir = jitter_dir, exe = exe, verbose = verbose, ...)
  # Only save stuff if it converged
  if ("Report.sso" %in% list.files(path = jitter_dir)) {
    rep <- SS_read_summary(file.path(jitter_dir, "ss_summary.sso"))
    if (is.null(rep)) {
      report <- SS_output(
        dir = jitter_dir, forecast = FALSE,
        covar = FALSE, NoCompOK = TRUE,
        verbose = verbose, warn = verbose, hidewarn = !verbose, printstats = verbose
      )
      like <- report[["likelihoods_used"]][row.names(report[["likelihoods_used"]]) == "TOTAL", "values"]
    } else {
      like <- rep[["likelihoods"]][grep("TOTAL", row.names(rep[["likelihoods"]])), 1]
    }
    if (printlikes) {
      message("Likelihood for jitter ", i, " = ", like)
    }
    return(like)
  } else {
    unlink(jitter_dir, recursive = TRUE)
    if (verbose) warning("No Report.sso file found from run ", i)
  }
}
