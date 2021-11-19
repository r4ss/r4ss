#' Extract total catch, spawning output, and fraction unfished from forecast years
#'
#' Values of total catch, spawning output, and fraction unfished are extracted
#' from the forecast years of a time series table for inclusion in a decision table.
#'
#' @template replist
#' @param yrs Range of years from which to extract values
#' @param digits Vector of number of digits to round to in table for
#' \itemize{
#'   \item 1 catch
#'   \item 2 spawning output
#'   \item 3 fraction unfished (column is called "depl")
#' }
#' @seealso [SS_ForeCatch()]
#' @author Ian G. Taylor
#' @export

SS_decision_table_stuff <- function(replist, yrs = 2021:2032, digits = c(0, 0, 3)) {
  # needs to be able to aggregate across areas for spatial models
  if (replist[["nareas"]] > 1) {
    warning("You probably need to aggregate function output across areas")
  }
  # subset timeseries
  ts <- replist[["timeseries"]][replist[["timeseries"]][["Yr"]] %in% yrs, ]
  # note that new $dead_B_sum quantity can be used in future versions
  catch <- round(
    apply(ts[, grep("dead(B)", names(ts), fixed = TRUE)],
      MARGIN = 1, FUN = sum
    ),
    digits[1]
  )
  yr <- ts[["Yr"]]
  # get spawning biomass
  SpawnBio <- round(ts[["SpawnBio"]], digits[2])
  # get depletion (this calc is independent of Bratio definition)
  SpawnBioVirg <-
    replist[["timeseries"]][["SpawnBio"]][replist[["timeseries"]][["Era"]] == "VIRG"]
  dep <- round(SpawnBio / SpawnBioVirg, digits[3])
  # get summary biomass (not currently reported)
  Bio_smry <- ts[["Bio_smry"]]
  # combine stuff
  # stuff <- data.frame(yr=yr[ts[["Area"]]==1], catch, dep, SpawnBio, Bio_smry)
  stuff <- data.frame(yr, catch, SpawnBio, dep)
  return(stuff)
}
