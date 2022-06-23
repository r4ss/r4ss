#' Read control file from SS
#'
#' Read control file from Stock Synthesis (SS) into R as a list object.
#' This function acts as a wrapper for version-specific SS_readctl_ functions.
#' For example, if the control file was written using SS 3.24,
#' then `SS_readctl` will call [SS_readctl_3.24]. Input arguments that do not
#' pertain to the version of your control file can be left at their
#' default values.
#'
#' @template file
#' @template verbose
#' @template readctl_vars
#' @template version
#' @param N_CPUE_obs Number of CPUE observations. Used only in control file 3.24
#'  syntax if `use_datlist = FALSE`.
#' @param catch_mult_fleets Integer vector of fleets using the catch multiplier
#'   option. Defaults to NULL and should be left as such if 1) the catch
#'   multiplier option is not used for any fleet or 2) `use_datlist = TRUE` and
#'   datlist is specified. Used only in control file 3.30 syntax if
#'   `use_datlist = FALSE`.
#' @param predM_fleets integer vector of fleets with predator mortality included.
#'  Predator mortality fleets are only available in v3.30.18 and
#'  higher. Defaults to NULL and should be left as such if 1) predation mortality
#'  is not used for any fleets; 2) `use_datlist = TRUE` and `datlist` is specified;
#'  or 3) if comments in the control file should be used instead to determine
#'  the the predM_fleets. Used only in control file 3.30 syntax if
#'  `use_datlist = FALSE`.
#' @param Ntag_fleets The number of catch fleets in the model (fleets of )
#'  type 1 or 2; not surveys). Used to set the number of survey parameters.
#'  Only used in control file 3.30 reading if tagging data is in the model and
#'  `use_datlist = FALSE`.
#' @param N_rows_equil_catch Integer value of the number of parameter lines to
#'  read for equilibrium catch. Defaults to NULL, which means the function will
#'  attempt to figure out how many lines of equilibrium catch to read from the
#'  control file comments. Used only in control file 3.30 syntax if
#'  `use_datlist = FALSE`.
#' @param Nfleets Number of fishing fleets and surveys, for 3.30 models.
#' @param Nfleet Number of fishing fleets, for 3.24 and lower version models.
#' @param Nsurveys Number of surveys, for 3.24 and lower version models.
#' @param N_dirichlet_parms Integer value of the number of Dirichlet-Multinomial
#'  parameters. Defaults to 0. Used only in control file 3.30 syntax if
#'  `use_datlist = FALSE`.
#' @param ptype Deprecated.
#' @author Ian G. Taylor, Yukio Takeuchi, Neil L. Klaer, Kelli F.
#' Johnson, Kathryn L. Doering, Nathan R. Vaughan
#' @export
#' @md
#' @return
#' A list structure where each element is a section of the control file.
#' @seealso
#' See the following for version-specific SS_readctl functions:
#' `r ls("package:r4ss", pattern = "SS_readctl_")`.
#' The returned list structure can be written back to the disk using
#' [r4ss::SS_writectl].\cr
#' See the following for other `SS_read` functions:
#' `r ls("package:r4ss", pattern = "SS_read[a-z]+$")`.\cr
#' @examples
#' # Read in the 'simple' example SS model stored in r4ss
#' # Find the directory
#' dirsimple <- system.file("extdata", "simple_3.30.13", package = "r4ss")
#' # Read in the dat file to define the structure of the control file so that
#' # you don't have to specify things in the function call such as 'Nfleet'
#' datfilename <- dir(dirsimple, pattern = "data\\.ss", full.names = TRUE)
#' dat <- r4ss::SS_readdat(file = datfilename, verbose = FALSE)
#' # Read in the control file using a list object for datlist
#' ctl <- r4ss::SS_readctl(
#'   file = dir(dirsimple, pattern = "control\\.ss", full.names = TRUE),
#'   verbose = FALSE,
#'   datlist = dat, use_datlist = TRUE
#' )
#' # Read in the control file using a file name for datlist
#' ctl <- r4ss::SS_readctl(
#'   file = dir(dirsimple, pattern = "control\\.ss", full.names = TRUE),
#'   verbose = FALSE,
#'   datlist = datfilename, use_datlist = TRUE
#' )
SS_readctl <- function(file,
                       version = "3.30",
                       verbose = FALSE,
                       use_datlist = TRUE,
                       datlist = "data.ss_new",
                       ## Parameters that are not defined in control file
                       nseas = NULL,
                       N_areas = NULL,
                       Nages = NULL,
                       Nsexes = NULL,
                       Npopbins = NA,
                       Nfleets = NULL,
                       Nfleet = NULL,
                       Do_AgeKey = NULL,
                       Nsurveys = NULL,
                       N_tag_groups = NULL,
                       N_CPUE_obs = NULL,
                       catch_mult_fleets = NULL,
                       predM_fleets = NULL,
                       Ntag_fleets = NULL,
                       N_rows_equil_catch = NULL,
                       N_dirichlet_parms = NULL,
                       ptype = lifecycle::deprecated()) {

  # warn about soft deprecated arguments ----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(ptype)) {
    lifecycle::deprecate_warn(
      when = "1.45.0",
      what = "SS_readctl(ptype)"
    )
  }

  nver <- as.numeric(substring(version, 1, 4))

  # call function for SS version 2.00
  if (nver < 3) {
    stop("Function SS_readctl_2.00 has not been written yet")
  }

  # call function for SS version 3.00
  if ((nver >= 3) && (nver < 3.2)) {
    stop("Function SS_readctl_3.00 has not been written yet")
  }

  # call function for SS version 3.24
  if ((nver >= 3.2) && (nver < 3.3)) {
    if (isTRUE(!is.null(Nfleets))) {
      stop("SS v3.24 uses Nfleet and Nsurveys but a value has been input for Nfleets instead")
    }
    ctllist <- SS_readctl_3.24(
      file = file,
      verbose = verbose,
      nseas = nseas,
      N_areas = N_areas,
      Nages = Nages,
      Nsexes = Nsexes,
      Npopbins = Npopbins,
      Nfleet = Nfleet,
      Nsurveys = Nsurveys,
      Do_AgeKey = Do_AgeKey,
      N_tag_groups = N_tag_groups,
      N_CPUE_obs = N_CPUE_obs,
      use_datlist = use_datlist,
      datlist = datlist
    )
  }

  # call function for SS version 3.30
  if (nver >= 3.3) {
    if (isTRUE(!is.null(Nfleet) | !is.null(Nsurveys))) {
      stop("SS v3.30 uses Nfleets but values have been input for Nfleet and/or Nsurveys")
    }
    ctllist <- SS_readctl_3.30(
      file = file,
      verbose = verbose,
      nseas = nseas,
      N_areas = N_areas,
      Nages = Nages,
      Nsexes = Nsexes,
      Npopbins = Npopbins,
      Nfleets = Nfleets,
      Do_AgeKey = Do_AgeKey,
      N_tag_groups = N_tag_groups,
      catch_mult_fleets = catch_mult_fleets,
      predM_fleets = predM_fleets,
      Ntag_fleets = Ntag_fleets,
      N_rows_equil_catch = N_rows_equil_catch,
      N_dirichlet_parms = N_dirichlet_parms,
      use_datlist = use_datlist,
      datlist = datlist
    )
  }

  # return the result
  return(ctllist)
}
