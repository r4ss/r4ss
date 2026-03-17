#' Convert SS3 data into format required by FIMS
#'
#' Uses output from `r4ss::SS_read()` and does
#' filtering, simplifying, and reformatting.
#'
#' @param ss3_dir Directory containing the SS3 input and output model
#' files. Alternatively the SS3 input and output lists can be input
#' directly using `ss3_inputs` and `ss3_output`
#' @param ss_new Logical indicating whether to read in the .ss_new files
#' instead of the original input files. Beneficial for dealining with
#' negative years in the wtatage file which can't yet be processed by
#' this function.
#' @param ss3_inputs A list containing `dat` and `wtatage` such as that
#' created by `r4ss::SS_read()`. Only required if `ss3_dir` is not provided.
#' @param ss3_output A list created by `r4ss::SS_output()`. Only required if
#' `ss3_dir` is not provided.
#' @param fleets Which fleets to include in the processed output.
#' Note that the only start year population weight_at_age is read from the
#' `wtatage` element (fleet = 0). NULL will default to including all fleets
#' from the SS3 model.
#' @param ages Vector of ages to index. NULL will default to using
#' all age data bins from the SS3 model.
#' @param lengths Vector of lengths to index. NULL will default to using
#' all length data bins from the SS3 model.
#' @return A data frame that can be passed to `FIMS::FIMSFrame()`
#' @author Ian G. Taylor, Megumi Oshima, Kelli F. Johnson
#' @export

ss3_data_to_fims <- function(
  ss3_dir = NULL,
  ss_new = TRUE,
  ss3_inputs = NULL,
  ss3_output = NULL,
  fleets = NULL,
  ages = NULL,
  lengths = NULL
) {
  # check if input is a string and if so, try to read in the data using SS_read()
  if (!is.null(ss3_dir)) {
    if (!is.character(ss3_dir)) {
      cli::cli_abort("{.code ss3_dir} should be a character string")
    }
    cli::cli_alert_info(
      "Reading in data from {.code {ss3_dir}} using {.code r4ss::SS_read()}"
    )
    ss3_inputs <- r4ss::SS_read(
      dir = ss3_dir,
      ss_new = ss_new,
      read_wtatage = TRUE
    )
    cli::cli_alert_info(
      "Reading in output from {.code {ss3_dir}} using {.code r4ss::SS_output()}"
    )
    ss3_output <- r4ss::SS_output(
      dir = ss3_dir,
      verbose = FALSE,
      printstats = FALSE,
    )
  }
  # check inputs for necessary elements
  if (!is.list(ss3_inputs) || !"dat" %in% names(ss3_inputs)) {
    cli::cli_abort(
      "`ss3_inputs` should be a list containing both 'dat' and 'wtatage'"
    )
  }
  if (!"wtatage" %in% names(ss3_inputs)) {
    cli::cli_abort(
      "'ss3_inputs' is missing element 'wtatage'. You may have to add it by running 'r4ss::SS_readwtatage()'"
    )
  }
  if (any(ss3_inputs[["wtatage"]][["year"]] < 0)) {
    cli::cli_abort(
      "The 'wtatage' element includes negative years which can't yet be processed by this function, please use the wtatage.ss_new file."
    )
  }

  # pull out dat element from the list to simplify code
  dat <- ss3_inputs$dat

  # fill in any missing inputs
  if (is.null(fleets)) {
    fleets <- seq_along(dat$fleetnames)
  }
  if (is.null(ages)) {
    ages <- dat$agebin_vector
  }
  if (is.null(lengths)) {
    lengths <- dat$lbin_vector
  }

  # create empty data frame
  res <- data.frame(
    type = character(),
    name = character(),
    age = integer(),
    length = integer(),
    timing = integer(),
    value = double(),
    unit = character(),
    uncertainty = double()
  )

  # get landings data and filter by fleet and year
  n_catch_before <- nrow(dat$catch)
  catch_filtered <- dat$catch |>
    dplyr::filter(year != -999) # year = -999 in SS3 designates initial equilibrium catch
  n_catch_before_fleet <- nrow(catch_filtered)
  catch_by_year_fleet <- catch_filtered |>
    dplyr::filter(fleet %in% fleets)
  n_catch_after <- nrow(catch_by_year_fleet)
  cli::cli_alert_info(
    "catch rows before fleet filter: {n_catch_before_fleet}; after: {n_catch_after}"
  )

  # convert landings to FIMSFrame format
  landings <- data.frame(
    type = "landings",
    name = dat$fleetnames[catch_by_year_fleet$fleet],
    age = NA,
    length = NA,
    timing = catch_by_year_fleet$year,
    value = catch_by_year_fleet$catch,
    unit = "mt",
    uncertainty = catch_by_year_fleet$catch_se
  )

  # check for any gaps in landings time series
  years <- min(catch_by_year_fleet$year):max(catch_by_year_fleet$year)
  if (!all(years %in% catch_by_year_fleet$year)) {
    cli::cli_abort("missing years in landings")
  }

  if (!is.null(dat$CPUE)) {
    # convert indices to FIMSFrame format
    n_cpue_before <- nrow(dat$CPUE)
    cpue_filtered <- dat$CPUE |>
      dplyr::filter(index %in% fleets)
    n_cpue_after <- nrow(cpue_filtered)
    cli::cli_alert_info(
      "CPUE rows before fleet filter: {n_cpue_before}; after: {n_cpue_after}"
    )
    index_info <- cpue_filtered |>
      dplyr::select(year, index, obs, se_log) |>
      dplyr::arrange(index, year)

    indices <- data.frame(
      type = "index",
      name = dat$fleetnames[index_info$index],
      age = NA,
      length = NA,
      timing = index_info$year,
      value = index_info$obs,
      unit = "",
      uncertainty = index_info$se_log
    )
  } else {
    indices <- NULL
  }

  if (!is.null(dat$agecomp)) {
    # partially convert age comps (filter, make into long table)

    # first rescale females to sum to 1.0
    # (data processing step had females + males sum to 100 for no good reason)
    dat$agecomp$sum_fem <-
      dat$agecomp |>
      dplyr::select(dplyr::starts_with(c("f", "a"), ignore.case = FALSE)) |> # get female comps (or comps if single-sex)
      rowSums()
    # couldn't figure out dplyr approach to rescaling the subset of columns
    # with female proportions to sum to 1.0
    fcols <- dat$agecomp |>
      dplyr::select(dplyr::starts_with("f", ignore.case = FALSE))
    if (length(fcols) > 0) {
      dat$agecomp[, names(dat$agecomp) %in% paste0("f", ages)] <-
        dat$agecomp[, names(dat$agecomp) %in% paste0("f", ages)] /
        dat$agecomp$sum_fem
    } else {
      dat$agecomp[, names(dat$agecomp) %in% paste0("a", ages)] <-
        dat$agecomp[, names(dat$agecomp) %in% paste0("a", ages)] /
        dat$agecomp$sum_fem
    }

    # further processing
    n_agecomp_before <- nrow(dat$agecomp)
    agecomp_filtered <- dat$agecomp |>
      dplyr::filter(fleet %in% fleets)
    n_agecomp_after <- nrow(agecomp_filtered)
    cli::cli_alert_info(
      "agecomp rows before fleet filter: {n_agecomp_before}; after: {n_agecomp_after}"
    )
    age_info <-
      agecomp_filtered |>
      dplyr::mutate(fleet = abs(fleet)) |> # convert any negative fleet to positive
      dplyr::select(!dplyr::matches("^m[0-9]")) |> # exclude male comps
      tidyr::pivot_longer(
        # convert columns f1...f17 to values in a new "age" colum of a longer table
        cols = dplyr::matches("^f[0-9]") | dplyr::matches("^a[0-9]"), # 2-sex model uses f1, f2, ...; 1-sex model uses a1, a2, ...
        names_to = "age",
        values_to = "value"
      ) |>
      dplyr::mutate(age = as.numeric(substring(age, first = 2))) |> # convert "f17" to 17
      dplyr::select(year, fleet, Nsamp, age, value) |>
      # Find missing age in composition data and fill in for each fleet
      dplyr::group_by(fleet, year) |>
      tidyr::complete(age = ages) |>
      tidyr::fill(Nsamp, .direction = "updown") |>
      dplyr::mutate(value = ifelse(is.na(value), 0, value)) |>
      dplyr::ungroup() |>
      dplyr::arrange(fleet, year, age)

    # finish converting age comps to FIMSFrame format
    agecomps <- data.frame(
      type = "age_comp",
      name = dat$fleetnames[abs(age_info$fleet)], # abs to include negative fleet numbers which may contain marginal ages
      age = age_info$age,
      length = NA,
      timing = age_info$year,
      value = age_info$value + 0.001, # add constant to avoid 0 values
      unit = "",
      # Q: should uncertainty here be the total sample size across bins, or the samples within the bin?
      # uncertainty = round(age_info$Nsamp * age_info$value)
      uncertainty = round(age_info$Nsamp)
    )
  } else {
    # if no age comps present
    agecomps <- NULL
  }

  # Length composition data
  if (!is.null(dat[["lencomp"]])) {
    # leaving out the re-scaling part for females to 1
    n_lencomp_before <- nrow(dat$lencomp)
    lencomp_filtered <- dat$lencomp |>
      dplyr::filter(fleet %in% fleets) # filter by requested fleets
    n_lencomp_after <- nrow(lencomp_filtered)
    cli::cli_alert_info(
      "lencomp rows before fleet filter: {n_lencomp_before}; after: {n_lencomp_after}"
    )
    len_info <-
      lencomp_filtered |>
      dplyr::mutate(fleet = abs(fleet)) |> # convert any negative fleet to positive
      dplyr::select(!dplyr::matches("^m[0-9]")) |> # exclude male comps
      tidyr::pivot_longer(
        # convert columns f1...f17 to values in a new "length" colum of a longer table
        cols = dplyr::matches("^f[0-9]") | dplyr::matches("^l[0-9]"), # 2-sex model uses f1, f2, ...; 1-sex model uses l1, l2, ...
        names_to = "length",
        values_to = "value"
      ) |>
      dplyr::mutate(length = as.numeric(substring(length, first = 2))) |> # convert "l17" to 17
      dplyr::select(year, fleet, Nsamp, length, value) |>
      dplyr::arrange(fleet, year, length)

    # finish converting age comps to FIMSFrame format
    lencomps <- data.frame(
      type = "length_comp", # will likely need to change name
      name = dat$fleetnames[abs(len_info$fleet)], # abs to include fleet == -4
      age = NA,
      length = len_info$length,
      timing = len_info$year,
      value = len_info$value + 0.001, # add constant to avoid 0 values
      unit = "",
      # Q: should uncertainty here be the total sample size across bins, or the samples within the bin?
      # uncertainty = round(len_info$Nsamp * len_info$value)
      uncertainty = round(len_info$Nsamp)
    )
  } else {
    # if no length comps present
    lencomps <- NULL # not sure if we need this but wanting to avoid an error if missing age or length comps
  }

  # weight_at_age data
  wtatage <- ss3_inputs$wtatage |>
    dplyr::filter(fleet == 0 & sex == 1 & seas == 1 & birthseas == 1) |> # TODO make this more flexible
    dplyr::select("year", dplyr::matches("[0-9]+")) |>
    tidyr::pivot_longer(cols = -year, names_to = "age") |>
    dplyr::mutate(age = as.numeric(age)) |>
    dplyr::filter(age <= max(ages)) |>
    dplyr::mutate(
      type = "weight_at_age",
      name = dat$fleetnames[1], # weight-at-age is only needed for one fleet, so arbitrarily assigning to fleet 1
      length = NA,
      timing = year,
      value = value / 1000, # covert to metric tons (SS3)
      unit = "mt",
      uncertainty = NA
    ) |>
    dplyr::select(-year)

  # get age-to-length conversion matrix
  # TODO: is it correct to make this conditional on length comps existing?
  if (!is.null(lencomps)) {
    timing_length_combinations <- lencomps |>
      dplyr::select(timing, name) |>
      dplyr::distinct()

    # initially always take the matrix for females in the middle of season 1
    ALK <- ss3_output$ALK[,, "Seas: 1 Sub_Seas: 2 Morph: 1"]
    ages <- as.integer(colnames(ALK))
    lengths <- as.integer(rownames(ALK))

    # function to provide length at age from SS3 output
    length_at_age_lookup <- function(age, length) {
      if (age %in% ages & length %in% lengths) {
        return(ALK[as.character(length), as.character(age)])
      } else {
        return(NA)
      }
    }

    # create empty data frame to store the age to length conversions
    age_to_length <- data.frame(
      type = character(),
      name = character(),
      age = integer(),
      length = integer(),
      timing = character()
    )
    # for every combination of timing and name (fleet) that has length comps
    # create a data frame with age, length to which values will be added
    for (irow in 1:nrow(timing_length_combinations)) {
      age_to_length <- rbind(
        age_to_length,
        expand.grid(
          name = timing_length_combinations$name[irow],
          timing = timing_length_combinations$timing[irow],
          age = ages,
          length = lengths
        )
      )
    }
    # now fill in the value column with the proportion of each age
    # within each length bin
    age_to_length <- age_to_length |>
      rowwise() |>
      mutate(value = length_at_age_lookup(age, length))

    # now add additional columns
    age_to_length <- age_to_length |>
      mutate(
        type = "age_to_length_conversion",
        unit = "proportion",
        uncertainty = NA
      ) |>
      select(names(res))
  } else {
    # if no length comps, then we don't need the age to length conversion matrix
    age_to_length <- NULL
  }

  # combine all data sources
  res <- rbind(
    res,
    landings,
    indices,
    agecomps,
    lencomps,
    wtatage,
    age_to_length
  )

  # remove any projection/forecast years
  res <- res |>
    dplyr::filter_out(timing > dat$endyr + 1) |> 
    dplyr::filter_out(type != "weight_at_age" & timing > dat$endyr)

  return(res)
}
