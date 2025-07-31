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
#' @param OFL Logical indicating whether to include the overfishing limit (OFL) 
#' instead of spawning output in the table. Defaults to `FALSE`. 
#' @export
#' @return A tibble with columns for year, total catch (dead biomass),
#' spawning output, and fraction unfished.
#' @seealso [SS_ForeCatch()]
#' @author Ian G. Taylor
#' @export

SS_decision_table_stuff <- function(
  replist,
  yrs = 2025:2036,
  digits = c(0, 0, 3),
  OFL = FALSE
) {
  unfished <- replist[["derived_quants"]]["SSB_Virgin", "Value"]
  tab <- replist[["timeseries"]] |>
    dplyr::filter(Yr %in% yrs) |>
    dplyr::group_by(Yr) |>
    dplyr::summarise(
      catch = sum(rowSums(
        dplyr::across(dplyr::starts_with("dead(B)")),
        na.rm = TRUE
      )),
      spawn_bio = sum(SpawnBio, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # calculation of fraction_unfished is independent of Bratio definition
      # which could have a different denominator
      fraction_unfished = spawn_bio / unfished
    ) |>
    dplyr::mutate(
      catch = round(catch, digits[1]),
      spawn_bio = round(spawn_bio, digits[2]),
      fraction_unfished = round(fraction_unfished, digits[3])
    ) |>
    dplyr::rename(
      # matching names in earlier version of the function for backwards compatibility
      yr = Yr,
      SpawnBio = spawn_bio,
      dep = fraction_unfished
    )
  # replace the SpawnBio column with OFL
  if (OFL) {
    OFL <- replist[["derived_quants"]]$Value[
      replist[["derived_quants"]]$Label %in% paste0("OFLCatch_", tab$yr)
    ] |> round(digits[2])
    tab <- tab |> 
    dplyr::select(-SpawnBio) |>
    dplyr::mutate(OFL = OFL) |>
    dplyr::relocate(OFL, .after = catch)
  }
  return(tab)
}
