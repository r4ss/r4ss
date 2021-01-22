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
#' @template version
#' @template verbose
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is read. The default is `FALSE`, which turns the
#' functionality off.
#' @param nseas Number of seasons in the model. This information is not
#'  explicitly available in control file.
#' @param N_areas Number of spatial areas in the model. This information is not
#'  explicitly available in control file.
#' @param Nages Oldest age in the model, also known as the accumulator age.
#'  SS always starts age bins in the model with age-zero fish,
#'  so `Nages+1` will be the number of ages represented.
#'  This information is not explicitly available in control file.
#' @param Ngenders Number of genders in the model. This information is not
#'  explicitly available in control file.
#' @param Npopbins Number of population bins in the model. This information is
#'  not explicitly available in control file and is only
#'  required if length-based maturity vector is directly supplied
#'  in the control file (i.e., maturity option of 6).
#'  Note that the functionality of this option is not yet fully tested.
#'  Please contact the package authors if you are willing to test it out.
#' @param Nfleets Number of Fishery + Survey fleets in the model. This
#'  information is not explicitly available in control file 3.30 syntax.
#' @param Nfleet Number of Fishery fleets in the model. This information is not
#'  explicitly available in control file 3.24 syntax.
#'  Only passed to `SS_readctl_3.24`.
#' @param Nsurveys Number of Survey fleets in the model. This information is
#'  not explicitly available in control file 3.24 syntax.
#'  Only passed to `SS_readctl_3.24`.
#' @param N_tag_groups Number of tag release groups in the model.
#' This information is also not explicitly available in control file.
#' @param N_CPUE_obs Number of CPUE observations.
#'  Only passed to SS_readctl_3.24.
#' @param use_datlist LOGICAL if `TRUE`, use datlist to derive parameters which can not be
#'  determined from the control file. See `datlist` argument for options for
#'  passing the data file.
#' @param catch_mult_fleets Integer vector of fleets using the catch multiplier
#'   option. Defaults to NULL and should be left as such if 1) the catch
#'   multiplier option is not used for any fleet or 2) `use_datlist = TRUE` and
#'   datlist is specified. Used only in control file 3.30 syntax.
#' @param N_rows_equil_catch Integer value of the number of parameter lines to
#'  read for equilibrium catch. Defaults to NULL, which means the function will
#'  attempt to figure out how many lines of equilibrium catch to read from the
#'  control file comments. Used only in control file 3.30 syntax.
#' @param N_dirichlet_parms Integer value of the number of Dirichlet-Multinomial
#' parameters. Defaults to 0. Not used in control file 3.24 syntax.
#' @param datlist List or character. If list, a list returned from
#'  [r4ss::SS_writedat].
#'  If character, a file name for a dat file to be read in.
#' @param ptype LOGICAL if `TRUE`, which is the default,
#'  a column will be included in the output indicating parameter type.
#'  Using `TRUE` can be useful, but causes problems for [SS_writectl],
#'  and therefore is not recommended if you intend to write the list
#'  back out into a file.
#'  Used only in control file 3.30 syntax.
#' @author Ian G. Taylor, Yukio Takeuchi, Neil L. Klaer
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
#' dat <- r4ss::SS_readdat( file = datfilename, verbose = FALSE)
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

SS_readctl <- function(file, version = NULL, verbose = TRUE, echoall = FALSE,
                       ## Parameters that are not defined in control file
                       nseas = 4,
                       N_areas = 1,
                       Nages = 20,
                       Ngenders = 1,
                       Npopbins = NA,
                       Nfleets = 4,
                       Nfleet = 2,
                       Nsurveys = 2,
                       N_tag_groups = NA,
                       N_CPUE_obs = NA,
                       catch_mult_fleets = NULL,
                       N_rows_equil_catch = NULL,
                       N_dirichlet_parms = 0,
                       use_datlist = FALSE,
                       datlist = NULL,
                       ptype = TRUE) {

  # wrapper function to call old or new version of SS_readctl

  # automatic testing of version number
  if (is.null(version)) {
    # look for 3.24 or 3.30 at the top of the chosen file
    version <- scan(file, what = character(), nlines = 1, quiet = !verbose)
    version <- substring(version, 3, 6)[1]
    # if that fails, look for data.ss_new file in the same directory
    if (version %in% c("3.24", "3.30")) {
      if (verbose) cat("assuming version", version, "based on first line of control file\n")
    } else {
      newfile <- file.path(dirname(file), "control.ss_new")
      if (file.exists(newfile)) {
        version <- scan(newfile, what = character(), nlines = 1, quiet = !verbose)
        version <- substring(version, 3, 6)
        if (verbose) cat("assuming version", version, "based on first line of control.ss_new\n")
      } else {
        stop("input 'version' required due to missing value at top of", file)
      }
    }
  }

  nver <- as.numeric(substring(version, 1, 4))

  if (verbose) cat("Char version is ", version, "\n")
  if (verbose) cat("Numeric version is ", nver, "\n")

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
    if (Nfleets != 4) {
      if (Nfleets != (Nfleet + Nsurveys)) {
        if (Nfleet == 2 & Nsurveys == 2) {
          stop("SS v3.24 uses Nfleet and Nsurveys but you have input a value for Nfleets instead")
        } else {
          stop("SS v3.30 uses Nfleet and Nsurveys but you have input a value for Nfleets as well that doesn't match with your Nfleet and Nsurveys inputs")
        }
      }
    }

    ctllist <- SS_readctl_3.24(
      file = file,
      version = version,
      verbose = verbose,
      echoall = echoall,
      nseas = nseas,
      N_areas = N_areas,
      Nages = Nages,
      Ngenders = Ngenders,
      Npopbins = Npopbins,
      Nfleet = Nfleet,
      Nsurveys = Nsurveys,
      N_tag_groups = N_tag_groups,
      N_CPUE_obs = N_CPUE_obs,
      use_datlist = use_datlist,
      datlist = datlist,
      ptype = ptype
    )
  }

  # call function for SS version 3.30
  if (nver >= 3.3) {
    if (Nfleet != 2 | Nsurveys != 2) {
      if (Nfleets != (Nfleet + Nsurveys)) {
        if (Nfleets == 4) {
          stop("SS v3.30 uses Nfleets but you have input values for Nfleet and Nsurveys")
        } else {
          stop("SS v3.30 uses Nfleets but you have input values for Nfleet and Nsurveys as well that don't match with your Nfleets input")
        }
      }
    }

    ctllist <- SS_readctl_3.30(
      file = file,
      version = version,
      verbose = verbose,
      echoall = echoall,
      nseas = nseas,
      N_areas = N_areas,
      Nages = Nages,
      Ngenders = Ngenders,
      Npopbins = Npopbins,
      Nfleets = Nfleets,
      N_tag_groups = N_tag_groups,
      catch_mult_fleets = catch_mult_fleets,
      N_rows_equil_catch = N_rows_equil_catch,
      N_dirichlet_parms = N_dirichlet_parms,
      use_datlist = use_datlist,
      datlist = datlist
    )
  }

  # return the result
  return(ctllist)
}
