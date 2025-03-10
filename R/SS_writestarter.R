#' write starter file
#'
#' write Stock Synthesis starter file from list object in R which was probably
#' created using [SS_readstarter()]
#'
#'
#' @param mylist List object created by [SS_readstarter()].
#' @template dir
#' @param file Filename for new starter file. Default="starter.ss".
#' @template overwrite
#' @template verbose
#' @param warn Deprecated.
#' @author Ian G. Taylor, Kelli F. Johnson, Kathryn R. Doering
#' @export
#' @family read/write functions

SS_writestarter <- function(mylist, dir = NULL, file = "starter.ss",
                            overwrite = FALSE, verbose = TRUE,
                            warn = lifecycle::deprecated()) {
  if (lifecycle::is_present(warn)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SS_writestarter(warn)"
    )
  }
  if (verbose) message("running SS_writestarter")
  if (mylist[["type"]] != "Stock_Synthesis_starter_file") {
    stop("input 'mylist' should be a list with $type=='Stock_Synthesis_starter_file'\n")
  }
  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({
    if (sink.number() > 0) sink()
  })

  if (is.null(dir)) dir <- getwd() # set to working directory if no input provided
  if (grepl("/$", dir)) {
    outfile <- paste0(dir, file) # bc trailing backslash
  } else {
    outfile <- paste(dir, file, sep = "/")
  }
  if (file.exists(outfile)) {
    if (!overwrite) {
      stop(paste("file exists:", outfile, "\n  set overwrite=TRUE to replace\n"))
    } else {
      file.remove(outfile)
    }
  } else {
    if (verbose) message("writing new file: ", outfile)
  }

  # record current max characters per line and then expand in case of long lines
  oldwidth <- options()[["width"]]
  options(width = 1000)

  if (verbose) message("opening connection to ", outfile)
  zz <- file(outfile, open = "at")
  sink(zz)

  # simple function to clean up many repeated commands
  # writes the content of an R object, followed by the object name with "#_" in front
  wl <- function(name) {
    value <- mylist[names(mylist) == name]
    writeLines(paste0(value, " #_", name), con = zz)
  }

  # function to write a vector
  wl.vector <- function(name,
                        comment = NULL,
                        collapse = NULL) {
    value <- mylist[names(mylist) == name][[1]]
    if (is.null(collapse)) {
      collapse <- " "
    }
    if (is.null(comment)) {
      writeLines(paste(paste(value, collapse = collapse), " #_", name, sep = ""),
        con =
          zz
      )
    } else {
      writeLines(paste(paste(value, collapse = collapse), comment), con = zz)
    }
  }

  # write a header
  add_file_header(mylist, con = zz)

  # strings for control and data file names
  wl("datfile")
  wl("ctlfile")

  # lots of single numerical values
  wl("init_values_src")
  wl("run_display_detail")
  wl("detailed_age_structure")
  if (mylist[["detailed_age_structure"]] == 3) {
    writeLines(paste0(
      "# custom report options: -100 to start with minimal; ",
      "-101 to start with all; -number to remove, +number to add, -999 to end"
    ))
    wl("custom_start")
    if (!is.null(mylist[["custom_add_rm"]])) wl.vector("custom_add_rm")
    writeLines("-999")
  }
  wl("checkup")
  wl("parmtrace")
  wl("cumreport")
  wl("prior_like")
  wl("soft_bounds")
  wl("N_bootstraps")
  wl("last_estimation_phase")
  wl("MCMCburn")
  wl("MCMCthin")
  wl("jitter_fraction")
  wl("minyr_sdreport")
  wl("maxyr_sdreport")
  wl("N_STD_yrs")
  if (mylist[["N_STD_yrs"]] > 0) {
    wl("STD_yr_vec")
  }
  wl("converge_criterion")
  wl("retro_yr")
  wl("min_age_summary_bio")
  wl("depl_basis")
  wl("depl_denom_frac")
  wl("SPR_basis")
  wl("F_std_units")
  if (mylist[["F_std_units"]] %in% 4:5) {
    cat(mylist[["F_age_range"]], "#_F_age_range\n")
  }
  wl("F_std_basis")
  # only write ALK_tolerance if this is SSv3.30 (value didn't exist in 3.24)
  if (mylist[["final"]] == 3.3) {
    wl("MCMC_output_detail")
    wl("ALK_tolerance")
  }
  writeLines("#")
  if (!is.null(mylist[["seed"]])) { # seed option added in 3.30.15
    wl("seed")
  }
  wl("final")

  # restore printing width to whatever the user had before
  options(width = oldwidth)
  sink()
  close(zz)
  if (verbose) {
    message("file written to ", outfile)
  }
}
