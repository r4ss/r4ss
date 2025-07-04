#' Create table of fixed forecast catches
#'
#' Processing values of dead or retained biomass from timeseries output
#' to fit the format required at the bottom of the forecast file.
#' This can be used to map the catches resulting from forecasting with a particular
#' harvest control rule into a model representing a different state of nature.
#' This is a common task for US west coast groundfish but might be useful elsewhere.
#'
#' @template replist
#' @param yrs Range of years in which to fill in forecast catches from timeseries
#' @param average Use average catch over a range of years for forecast
#' (as opposed to using forecast based on control rule)
#' @param avg.yrs Range of years to average over
#' @param total Either single value or vector of annual total forecast catch
#' used to scale values (especially if values are from average catches).
#' For west coast groundfish, total might be ACL for next 2 forecast years
#' @param digits Number of digits to round to in table
#' @param dead TRUE/FALSE switch to choose dead catch instead of retained catch.
#' @param zeros Include entries with zero catch (TRUE/FALSE)
#'
#' @seealso [SS_readforecast()], [SS_readforecast()]
#' @author Ian G. Taylor
#' @examples
#' \dontrun{
#' # create table based on average over past 5 years
#' SS_ForeCatch(base, # object created by SS_output
#'   yrs = 2021:2022, # years with fixed catch
#'   average = TRUE, # catch by fleet from average catch
#'   avg.yrs = 2014:2018
#' ) # use average of catches over past 5 years
#'
#' # create table with pre-defined totals where the first 2 years
#' # are based on current harvest specifications and the next 10 are set to some
#' # new value (with ratio among fleets based on average over past 5 years)
#' SS_ForeCatch(base, # object created by SS_output
#'   yrs = 2021:2022, # years with fixed catch
#'   average = TRUE, # catch by fleet from average catch
#'   avg.yrs = 2016:2020, # use average of catches over past 5 years
#'   total = c(rep(241.3, 2), rep(300, 10))
#' ) # total
#'
#' # create table based on harvest control rule projection in SS
#' # that can be mapped into an alternative state of nature
#' SS_ForeCatch(low_state, # object created by SS_output for low state
#'   yrs = 2021:2032, # forecast period after fixed ACL years
#'   average = FALSE
#' ) # use values forecast in SS, not historic catch
#' }
#'
#' @export

SS_ForeCatch <- function(
  replist,
  yrs = 2025:2036,
  average = FALSE,
  avg.yrs = 2020:2024,
  total = NULL,
  digits = 2,
  dead = TRUE,
  zeros = FALSE
) {
  # check catch units
  catch_units <- replist[["catch_units"]][replist[["fleet_type"]] == 1]
  mixed_units <- FALSE
  if (length(unique(catch_units)) > 1) {
    mixed_units <- TRUE
    cli::cli_alert_info(
      "Catch units are not the same for all fleets, so units will match the settings for each fleet."
    )
  }
  timeseries <- replist[["timeseries"]]

  # create new empty object to store stuff
  forecast_catches <- NULL

  if (!all(yrs %in% timeseries[["Yr"]])) {
    cli::cli_alert_warning(
      "Not all requested years are present in timeseries output."
    )
    yrs <- yrs[yrs %in% timeseries[["Yr"]]]
  }
  # if only one value for total is input, repeat for all years
  if (!is.null(total) & length(total) == 1) {
    total <- rep(total, length(yrs))
  }
  # loop over seasons and areas (only 1 for most models) to subset timeseries
  for (iyr in seq_along(yrs)) {
    y <- yrs[iyr]
    forecast_catches_y <- NULL
    for (iseas in 1:replist[["nseasons"]]) {
      for (iarea in 1:replist[["nareas"]]) {
        if (replist[["SS_versionNumeric"]] < 3.30) {
          fleets_with_catch <- 1:replist[["nfishfleets"]]
        } else {
          fleets_with_catch <- which(replist[["fleet_type"]] == 1)
        }
        for (ifleet in fleets_with_catch) {
          # figure out column name
          if (replist[["catch_units"]][ifleet] == 1) {
            string <- ifelse(dead, "dead(B)", "retain(B):_")
          }
          if (replist[["catch_units"]][ifleet] == 2) {
            string <- ifelse(dead, "dead(N)", "retain(N):_")
          }
          colname <- paste0(string, ":_", ifleet)
          # extract catch
          if (average) {
            catches <- timeseries[
              timeseries[["Area"]] == iarea &
                timeseries[["Seas"]] == iseas &
                timeseries[["Yr"]] %in% avg.yrs,
              colname
            ]
            catch <- mean(catches)
          } else {
            catch <- timeseries[
              timeseries[["Area"]] == iarea &
                timeseries[["Seas"]] == iseas &
                timeseries[["Yr"]] == y,
              colname
            ]
          }
          # create new row for table
          newrow <- data.frame(
            Year = y,
            Seas = iseas,
            Fleet = ifleet,
            Catch = catch
          )
          # add new row to previous rows
          forecast_catches_y <- rbind(forecast_catches_y, newrow)
        } # end loop over fleets
      } # end loop over areas
    } # end loop over seasons

    # if requested, scale catches to sum to input total (such as ACL)
    if (!is.null(total)) {
      forecast_catches_y[["Catch"]] <- total[iyr] *
        forecast_catches_y[["Catch"]] /
        sum(forecast_catches_y[["Catch"]])
    }

    # round values
    forecast_catches_y[["Catch"]] <- round(
      forecast_catches_y[["Catch"]],
      digits
    )

    # add comment on right-hand-side
    forecast_catches_y[["comment"]] <- ""
    forecast_catches_y[["comment"]][1] <- paste0(
      "#sum_for_",
      y,
      ": ",
      sum(forecast_catches_y[["Catch"]]),
      ifelse(mixed_units, " (mixed units)", "")
    )
    # add block for this year to other blocks
    forecast_catches <- rbind(forecast_catches, forecast_catches_y)
  } # end loop over years

  # fix up column names
  names(forecast_catches)[1] <- "#Year"
  names(forecast_catches)[4] <- string

  # remove rows with zero catch
  if (!zeros & any(forecast_catches[[string]] == 0)) {
    forecast_catches <- forecast_catches[!forecast_catches[[string]] == 0, ]
  }

  return(forecast_catches)
}
