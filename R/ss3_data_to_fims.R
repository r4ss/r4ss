#' Convert SS3 data into format required by FIMS
#'
#' Uses output from `r4ss::SS_read()` and does
#' filtering, simplifying, and reformatting.
#' Discard age- and length-composition rows (`part = 1`) are excluded because
#' FIMS does not support them.
#' After fleet filtering, only the observation with the largest `Nsamp` is
#' retained for each fleet-year, separately for length and age compositions.
#' Age-composition selection is across marginal and conditional age-at-length
#' samples. Ties retain the first input row.
#'
#' @param ss3_dir Directory containing the SS3 input and output model
#' files. Alternatively the SS3 input and output lists can be input
#' directly using `ss3_inputs` and `ss3_output`
#' @param ss_new Logical indicating whether to read in the .ss_new files
#' instead of the original input files. Beneficial for dealing with
#' negative years in the wtatage file which can't yet be processed by
#' this function.
#' @param ss3_inputs A list containing `dat` such as that created by
#' `r4ss::SS_read()`. When `EWAA` is `TRUE`, the list must also contain
#' `wtatage`. Only required if `ss3_dir` is not provided.
#' @param ss3_output A list created by `r4ss::SS_output()`. Only required for
#' age-to-length conversion if `ss3_dir` is not provided.
#' @param EWAA Logical indicating whether to use empirical weight-at-age
#' and age_to_length conversion tables from the SS3 model or to rely on
#' the FIMS growth model. If NULL then it will follow the setting in the
#' SS3 control file (`ctl[['EmpiricalWAA']]`)
#' @param fleets Which fleets to include in the processed output.
#' Note that the only start year population weight-at-age is read from the
#' `wtatage` element (fleet = 0). NULL will default to including all fleets
#' from the SS3 model.
#' @param maxage Max age to include in the processed output.
#' NULL will default to using age 0 up to the maximum age data bin in
#' the SS3 model.
#' @param lengths Vector of lengths to include in the processed output.
#' Needs to be a subset of the length bins in the SS3 model.
#' NULL will default to using all length data bins from the SS3 model.
#' @param ageing_error_matrices Optional vector of SS3 age-error definition IDs
#' to convert. If NULL, all positive IDs referenced by retained age-composition
#' data are converted. Definitions omitted from this vector are not applied.
#' @return A data frame that can be passed to `FIMS::FIMSFrame()`
#' @author Ian G. Taylor, Megumi Oshima, Kelli F. Johnson
#' @export
#' @seealso [SS_write()] can be used to write the input files using the list
ss3_data_to_fims <- function(
  ss3_dir = NULL,
  ss_new = TRUE,
  ss3_inputs = NULL,
  ss3_output = NULL,
  EWAA = NULL,
  fleets = NULL,
  maxage = NULL,
  lengths = NULL,
  ageing_error_matrices = NULL
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
  }
  # check inputs for necessary elements
  if (!is.list(ss3_inputs) || !"dat" %in% names(ss3_inputs)) {
    cli::cli_abort(
      "`ss3_inputs` should be a list containing 'dat'"
    )
  }
  # An explicit EWAA setting overrides SS3; otherwise follow its control file.
  # Empirical mode supplies fixed weight and age-to-length tables to FIMS.
  if (is.null(EWAA)) {
    if (is.null(ss3_inputs[["ctl"]][["EmpiricalWAA"]])) {
      cli::cli_abort(
        "{.code EWAA} is {.code NULL}, but {.code ss3_inputs[['ctl']][['EmpiricalWAA']]} is unavailable."
      )
    }
    EWAA <- as.logical(ss3_inputs[["ctl"]][["EmpiricalWAA"]])
  }
  if (!is.logical(EWAA) || length(EWAA) != 1 || is.na(EWAA)) {
    cli::cli_abort(
      "{.code EWAA} must be {.code TRUE}, {.code FALSE}, or {.code NULL}."
    )
  }
  if (
    EWAA &&
      (is.null(ss3_inputs[["wtatage"]]) || !"wtatage" %in% names(ss3_inputs))
  ) {
    cli::cli_abort(
      "'ss3_inputs' is missing element 'wtatage'. You may have to add it by running 'r4ss::SS_readwtatage()'. If the wtatage.ss_new file is empty, change the starter file setting `inputs[['start']][['detailed_age_structure']]` to 1."
    )
  }
  if (EWAA && any(ss3_inputs[["wtatage"]][["year"]] < 0)) {
    cli::cli_abort(
      "The 'wtatage' element includes negative years which can't yet be processed by this function, please use the wtatage.ss_new file"
    )
  }

  # pull out dat element from the list to simplify code
  dat <- ss3_inputs[["dat"]]

  # fill in any missing inputs
  if (is.null(fleets)) {
    fleets <- seq_along(dat[["fleetnames"]])
  }
  # if maxage not provided, use values from the model
  if (is.null(maxage)) {
    # first look to the agebin vector in the data file
    if (!is.null(dat[["agebin_vector"]])) {
      maxage <- max(dat[["agebin_vector"]])
    } else {
      # if not present, just use maximum age in the population
      maxage <- ss3_inputs[["dat"]][["Nages"]]
    }
  }
  # Validate maxage before constructing its sequence of age bins.
  if (
    !is.numeric(maxage) ||
      length(maxage) != 1 ||
      is.na(maxage) ||
      !is.finite(maxage) ||
      maxage < 0 ||
      maxage != floor(maxage)
  ) {
    cli::cli_abort("{.code maxage} must be a single non-negative whole number.")
  }
  ages <- 0:maxage

  cli::cli_alert_info(
    "Using age bins: {paste(ages, collapse = ', ')}"
  )
  if (is.null(lengths)) {
    lengths <- dat[["lbin_vector"]]
  }
  # Ensure requested length bins are unique values in the SS3 data bins.
  if (
    !is.numeric(lengths) ||
      length(lengths) == 0 ||
      anyNA(lengths) ||
      any(!is.finite(lengths)) ||
      anyDuplicated(lengths)
  ) {
    cli::cli_abort(
      "{.code lengths} must be a non-empty vector of unique finite numbers."
    )
  }
  if (!all(lengths %in% dat[["lbin_vector"]])) {
    cli::cli_abort(
      "Every value in {.code lengths} must be a length bin in the SS3 data."
    )
  }
  cli::cli_alert_info(
    "Using length bins: {paste(lengths, collapse = ', ')}"
  )

  # Define the common FIMS schema used by all data types below. Coordinates
  # that do not apply to a data type are filled with NA when its rows are built.
  res <- data.frame(
    type = character(),
    fleet = character(),
    age = integer(),
    length = integer(),
    timing = integer(),
    observed = double(),
    unit = character(),
    uncertainty = character()
  )

  # Filter observations to selected fleets and report any rows removed.
  # data contains the fleet_column used to select rows; label names the data
  # type in the row-count message. Return the filtered data frame.
  filter_and_report_fleets <- function(data, fleet_column, label) {
    n_before <- nrow(data)
    filtered_data <- data |>
      dplyr::filter(.data[[fleet_column]] %in% fleets)
    n_after <- nrow(filtered_data)

    if (n_after < n_before) {
      cli::cli_alert_info(
        "{label} rows before fleet filter: {n_before}; after: {n_after}"
      )
    }

    filtered_data
  }

  # Normalize SS3 composition bins and reshape them into one row per bin.
  # bin_pattern identifies the age or length columns; extra_columns preserves
  # sample coordinates in the long-format output alongside fleet, year, and Nsamp.
  process_composition <- function(
    data,
    bin_pattern,
    extra_columns = character()
  ) {
    bin_columns <- grep(bin_pattern, names(data), value = TRUE)
    if (length(bin_columns) == 0) {
      cli::cli_abort("No composition-bin columns were found in the SS3 data.")
    }

    # Normalize each sample across its retained bins before converting wide
    # columns to long rows. Nsamp remains the sample size, not a bin count.
    # Preserve extra coordinates such as conditioning length for CAAL samples.
    data |>
      dplyr::mutate(fleet = abs(fleet)) |> # convert any negative fleet to positive
      dplyr::select(!dplyr::matches("^m[0-9]")) |> # exclude male comps
      dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(bin_columns),
          ~ .x / sum(dplyr::c_across(dplyr::all_of(bin_columns)), na.rm = TRUE)
        )
      ) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(bin_columns),
        names_to = "bin",
        values_to = "observed"
      ) |>
      dplyr::mutate(bin = as.numeric(substring(bin, first = 2))) |>
      dplyr::select(
        year,
        fleet,
        Nsamp,
        bin,
        observed,
        dplyr::all_of(extra_columns)
      ) |>
      dplyr::arrange(fleet, year, bin)
  }

  # Reject conflicting duplicate keys and remove exact repeated observations.
  # Input rows must all have the same data type. Return at most one row per key.
  filter_repeated_rows <- function(data) {
    n_rows_before <- nrow(data)
    data_type <- unique(data[["type"]])
    identifying_columns <- c("type", "fleet", "age", "length", "timing")
    # For ageing_error, uncertainty stores true age, while age is observed age.
    # Both are needed to identify a matrix cell rather than a whole age row.
    if (identical(data_type, "ageing_error")) {
      identifying_columns <- c(identifying_columns, "uncertainty")
    }
    value_columns <- setdiff(names(data), identifying_columns)
    # Do not silently choose between different values for the same FIMS key.
    # Only exact repetitions can be safely removed by distinct() below.
    conflicting_rows <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(identifying_columns))) |>
      dplyr::filter(dplyr::if_any(
        dplyr::all_of(value_columns),
        ~ dplyr::n_distinct(.x, na.rm = FALSE) > 1
      )) |>
      dplyr::ungroup()

    if (nrow(conflicting_rows) > 0) {
      cli::cli_abort(
        "Repeated rows for type {.val {data_type}} have conflicting values."
      )
    }

    data <- data |>
      dplyr::distinct(
        dplyr::across(dplyr::all_of(identifying_columns)),
        .keep_all = TRUE
      )
    n_rows_removed <- n_rows_before - nrow(data)

    if (n_rows_removed > 0) {
      percent_removed <- round(100 * n_rows_removed / n_rows_before, digits = 1)
      cli::cli_alert_warning(
        "Removed {n_rows_removed} repeated rows ({percent_removed}%) out of {n_rows_before} total rows for type {.val {data_type}}."
      )
    }

    data
  }

  # get catch data and filter by fleet and year
  # remove initial equilibrium catch rows (year = -999)
  catch_filtered <- dat[["catch"]] |>
    dplyr::filter(year != -999)
  catch_by_year_fleet <- filter_and_report_fleets(
    catch_filtered,
    fleet_column = "fleet",
    label = "catch"
  )
  # A non-empty catch series is required to check that years are continuous.
  if (nrow(catch_by_year_fleet) == 0) {
    cli::cli_abort(
      "No catch data remain after removing equilibrium rows and filtering fleets."
    )
  }
  # SS3 can report catch by season, but FIMS has only fleet and year keys.
  # First reject conflicting repeats of the same seasonal observation; exact
  # repeated rows are safe to remove before combining seasons.
  catch_keys <- c("fleet", "year", "seas")
  n_catch_before_dedup <- nrow(catch_by_year_fleet)
  repeated_catch <- catch_by_year_fleet |>
    dplyr::group_by(dplyr::across(dplyr::all_of(catch_keys))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()
  conflicting_catch <- repeated_catch |>
    dplyr::group_by(dplyr::across(dplyr::all_of(catch_keys))) |>
    dplyr::filter(dplyr::if_any(
      dplyr::all_of(c("catch", "catch_se")),
      ~ dplyr::n_distinct(.x, na.rm = FALSE) > 1
    )) |>
    dplyr::ungroup()
  if (nrow(conflicting_catch) > 0) {
    cli::cli_abort('Repeated rows for type "catch" have conflicting values.')
  }
  catch_by_year_fleet <- catch_by_year_fleet |>
    dplyr::distinct(dplyr::across(dplyr::all_of(catch_keys)), .keep_all = TRUE)
  n_catch_duplicates <- n_catch_before_dedup - nrow(catch_by_year_fleet)
  if (n_catch_duplicates > 0) {
    percent_removed <- round(
      100 * n_catch_duplicates / n_catch_before_dedup,
      digits = 1
    )
    cli::cli_alert_warning(
      "Removed {n_catch_duplicates} repeated rows ({percent_removed}%) out of {n_catch_before_dedup} total rows for type {.val catch}."
    )
  }

  # Sum seasonal catches to the annual fleet-year values FIMS can represent.
  # For independent lognormal seasonal errors, combine variances on the catch
  # scale and convert the resulting coefficient of variation back to sdlog.
  catch_by_year_fleet <- catch_by_year_fleet |>
    dplyr::group_by(.data[["fleet"]], .data[["year"]]) |>
    dplyr::summarize(
      annual_catch = sum(.data[["catch"]]),
      catch_se = {
        total_catch <- sum(.data[["catch"]])
        if (total_catch > 0) {
          variance <- sum(
            .data[["catch"]]^2 * expm1(.data[["catch_se"]]^2)
          )
          sqrt(log1p(variance / total_catch^2))
        } else {
          max(.data[["catch_se"]])
        }
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(catch = .data[["annual_catch"]]) |>
    dplyr::select(-"annual_catch")
  # Replace zero catches with a small positive value for the lognormal
  # likelihood; leave the remaining observations and their uncertainty intact.
  n_catch_zeros <- sum(catch_by_year_fleet[["catch"]] == 0)
  if (n_catch_zeros > 0) {
    cli::cli_alert_info(
      "adding 0.1 to {n_catch_zeros} rows with zero catch"
    )
    catch_by_year_fleet <- catch_by_year_fleet |>
      dplyr::mutate(catch = ifelse(catch == 0, 0.1, catch))
  }

  # convert catch to FIMSFrame format
  catch <- data.frame(
    type = "catch",
    fleet = dat[["fleetnames"]][catch_by_year_fleet[["fleet"]]],
    age = NA,
    length = NA,
    timing = catch_by_year_fleet[["year"]],
    observed = catch_by_year_fleet[["catch"]],
    unit = "mt",
    uncertainty = glue::glue(
      "~dlnorm(meanlog = log_catch_expected, sdlog = {catch_by_year_fleet[['catch_se']]})"
    )
  )

  # check for any gaps in catch time series
  years <- min(catch_by_year_fleet[["year"]]):max(catch_by_year_fleet[["year"]])
  if (!all(years %in% catch_by_year_fleet[["year"]])) {
    cli::cli_abort("missing years in catch")
  }
  # weight_at_age needs additional year
  years_wtatage <- c(years, max(years) + 1)

  if (!is.null(dat[["CPUE"]])) {
    # convert indices to FIMSFrame format
    cpue_filtered <- filter_and_report_fleets(
      dat[["CPUE"]],
      fleet_column = "index",
      label = "CPUE"
    )
    # Observation rows identify the survey by index; CPUEinfo identifies it
    # by fleet. Join these IDs to choose the likelihood for each observation.
    index_info <- cpue_filtered |>
      dplyr::select(year, index, obs, se_log) |>
      dplyr::arrange(index, year) |>
      dplyr::left_join(
        y = dat[["CPUEinfo"]][, c("fleet", "errtype")],
        by = c("index" = "fleet")
      ) |>
      dplyr::mutate(
        distribution = dplyr::case_when(
          errtype == -1 ~ "dnorm(mean = index_expected, sd =",
          errtype == 0 ~ "dlnorm(meanlog = log_index_expected, sdlog =",
          # Send unsupported SS3 error types to the validation below.
          TRUE ~ "unknown"
        ),
        uncertainty = glue::glue("~{distribution} {se_log})")
      )
    # FIMS currently supports only normal and lognormal index likelihoods.
    if (any(index_info[["distribution"]] == "unknown", na.rm = TRUE)) {
      cli::cli_abort(c(
        x = "FIMS cannot accommodate Student's t-distributions in index data.",
        i = "Check the distribution in the CPUEinfo section of your dat file
        for errtype greater than zero and change to lognormal or normal."
      ))
    }
    indices <- data.frame(
      type = "index",
      fleet = dat[["fleetnames"]][index_info[["index"]]],
      age = NA,
      length = NA,
      timing = index_info[["year"]],
      observed = index_info[["obs"]],
      unit = "mt",
      uncertainty = index_info[["uncertainty"]]
    )
  } else {
    indices <- NULL
  }

  ageing_error <- NULL
  if (!is.null(dat[["agecomp"]])) {
    agecomp_filtered <- filter_and_report_fleets(
      dat[["agecomp"]],
      fleet_column = "fleet",
      label = "agecomp"
    )
    # Exclude unsupported discard samples before ranking Nsamp, so a larger
    # discard sample cannot replace a usable sample or supply its age-error ID.
    n_discard_agecomp <- sum(agecomp_filtered[["part"]] == 1, na.rm = TRUE)
    if (n_discard_agecomp > 0) {
      agecomp_filtered <- agecomp_filtered |>
        dplyr::filter(is.na(.data[["part"]]) | .data[["part"]] != 1)
      cli::cli_alert_info(
        "Excluded {n_discard_agecomp} discard age-composition rows (part = 1), which FIMS does not support."
      )
    }
  }
  # Filtering can remove every sample. Skip reshaping and bin completion in
  # that case, while still allowing catch and other data types to be converted.
  if (!is.null(dat[["agecomp"]]) && nrow(agecomp_filtered) > 0) {
    # Filter signed fleet IDs first, so negative fleets require explicit selection.
    # Then map retained positive/negative IDs to the same FIMS fleet for grouping.
    n_agecomp <- nrow(agecomp_filtered)
    # FIMS uses one age-error matrix per fleet-year. Keep only the largest-Nsamp
    # row across marginal ages and all CAAL length strata, not one per stratum.
    # Do this before reshaping so compositions and age-error mappings use the
    # same retained observations. Equal sample sizes retain the first input row.
    agecomp_filtered <- agecomp_filtered |>
      dplyr::mutate(fleet = abs(.data[["fleet"]])) |>
      dplyr::group_by(.data[["fleet"]], .data[["year"]]) |>
      dplyr::slice_max(.data[["Nsamp"]], n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    if (nrow(agecomp_filtered) < n_agecomp) {
      cli::cli_alert_info(
        "Retained the largest-sample age composition per fleet-year: {n_agecomp} rows reduced to {nrow(agecomp_filtered)}."
      )
    }

    # if no age-0 observations, then insert columns for age-0 up to first age bin with 0 values
    # to avoid issues with missing age bins in the FIMSFrame
    if (!0 %in% dat[["agebin_vector"]]) {
      first_age_bin <- min(dat[["agebin_vector"]])
      # should be OK if resulting names are a0, f1, f2, etc.
      new_column_names <- paste0("a", 0:(first_age_bin - 1))
      # matrix of zeros
      new_columns <- data.frame(matrix(
        0,
        nrow = nrow(agecomp_filtered),
        ncol = length(new_column_names)
      ))
      names(new_columns) <- new_column_names
      cli::cli_alert_info(
        "Adding columns for missing age bins: {paste(new_column_names, collapse = ', ')}"
      )
      # add to data
      Nsamp_column <- match("Nsamp", names(agecomp_filtered))
      columns_before <- seq_len(Nsamp_column)
      columns_after <- setdiff(seq_len(ncol(agecomp_filtered)), columns_before)
      # Keep the original columns in order, even when Nsamp is the last column.
      agecomp_filtered <- dplyr::bind_cols(
        agecomp_filtered[, columns_before, drop = FALSE],
        new_columns,
        agecomp_filtered[, columns_after, drop = FALSE]
      )
    }

    # Resolve age-composition bounds to population-bin indices. Depending on
    # Lbin_method, SS3 interprets the input as population-bin indexes, data-bin
    # indexes, or actual lengths. Zero and -1 select the corresponding endpoint.
    lbin_method <- dat[["Lbin_method"]]
    pop_length_bins <- dat[["lbin_vector_pop"]]
    # The vector includes the final upper edge, which SS3 can use as a bound.
    n_pop_length_bins <- length(pop_length_bins)
    if (
      length(lbin_method) != 1 ||
        is.na(lbin_method) ||
        !lbin_method %in% 1:3 ||
        is.null(pop_length_bins) ||
        n_pop_length_bins < 1
    ) {
      cli::cli_abort(
        "Age-composition length bounds require a valid {.code Lbin_method} and population length-bin vector."
      )
    }
    pop_length_bins <- pop_length_bins[seq_len(n_pop_length_bins)]
    resolve_pop_bin <- function(value, lower) {
      if (is.na(value)) {
        cli::cli_abort("Age-composition length bounds cannot be missing.")
      }
      if (value %in% c(-1, 0)) {
        return(if (lower) 1L else as.integer(n_pop_length_bins))
      }

      if (lbin_method == 1) {
        # Method 1 uses population-bin indexes; SS3 truncates fractional input.
        index <- as.integer(value)
      } else if (lbin_method == 2) {
        # Method 2 uses data-bin indexes, but SS3 may already have expanded
        # sentinel bounds in parsed data to population-bin indexes.
        index <- as.integer(value)
        if (index > length(dat[["lbin_vector"]]) && index <= n_pop_length_bins) {
          return(index)
        }
        if (index < 1 || index > length(dat[["lbin_vector"]])) {
          cli::cli_abort(
            "Age-composition length bound is outside the data-bin indexes."
          )
        }
        index <- match(dat[["lbin_vector"]][index], pop_length_bins)
      } else {
        # Method 3 uses actual lengths and requires an exact population-bin match.
        index <- match(value, pop_length_bins)
      }

      if (is.na(index) || index < 1 || index > n_pop_length_bins) {
        cli::cli_abort(
          "Age-composition length bound does not identify a population length bin."
        )
      }
      index
    }
    # In parsed SS3 data, the full-range pair can be stored as its expanded
    # population-bin endpoints instead of the original 1/-1 input pair.
    full_pop_range <- agecomp_filtered[["Lbin_lo"]] == 1 &
      agecomp_filtered[["Lbin_hi"]] == n_pop_length_bins
    lower_bin <- vapply(
      agecomp_filtered[["Lbin_lo"]],
      resolve_pop_bin,
      integer(1),
      lower = TRUE
    )
    upper_bin <- vapply(
      agecomp_filtered[["Lbin_hi"]],
      resolve_pop_bin,
      integer(1),
      lower = FALSE
    )
    lower_bin[full_pop_range] <- 1L
    upper_bin[full_pop_range] <- as.integer(n_pop_length_bins)
    # Full-range bounds are marginal ages. Restricted ranges are CAAL; store
    # their lower bound as an actual length, regardless of the input method.
    agecomp_filtered <- agecomp_filtered |>
      dplyr::mutate(
        length = ifelse(
          lower_bin == 1 & upper_bin == n_pop_length_bins,
          NA_real_,
          pop_length_bins[lower_bin]
        )
      )
    age_info <- process_composition(
      agecomp_filtered,
      "^(f|a)[0-9]+$",
      extra_columns = "length"
    ) |>
      dplyr::rename(age = bin) |>
      # Find missing age in composition data and fill in for each fleet
      dplyr::group_by(fleet, year, length) |>
      tidyr::complete(age = ages) |>
      tidyr::fill(Nsamp, .direction = "updown") |>
      dplyr::mutate(observed = ifelse(is.na(observed), 0, observed)) |>
      dplyr::ungroup() |>
      dplyr::arrange(fleet, year, age, length)
    # Change from proportion to number
    #dplyr::mutate(observed = (observed * Nsamp) |> round(0))

    # finish converting age comps to FIMSFrame format
    agecomps <- data.frame(
      type = ifelse(is.na(age_info[["length"]]), "age_comp", "caal_comp"),
      fleet = dat[["fleetnames"]][abs(age_info[["fleet"]])], # abs to include negative fleet numbers which may contain marginal ages
      age = age_info[["age"]],
      length = age_info[["length"]],
      timing = age_info[["year"]],
      observed = age_info[["observed"]], # + 0.001, # add constant to avoid 0 values
      unit = "proportion",
      # Q: should uncertainty here be the total sample size across bins, or the samples within the bin?
      uncertainty = glue::glue(
        "~dmultinom(prob = {ifelse(is.na(age_info[['length']]), 'agecomp_proportion', 'caalcomp_proportion')}, size = {round(age_info[['Nsamp']])})"
      )
    )
    if (any(dat[["age_info"]][["CompError"]] != 0)) {
      bad_age_fleet <- dplyr::filter(
        dat[["age_info"]],
        .data[["CompError"]] != 0
      )
      cli::cli_alert_info(
        "FIMS can currently only accommodate the multinomial distribution
        for age-composition data."
      )
      cli::cli_alert_warning(
        "The following fleets use the Dirichlet-multinomial distribution and
        will be changed to use the multinomial: {row.names(bad_age_fleet)}."
      )
    }
  } else {
    # if no retained age comps present
    agecomps <- NULL
  }

  # Convert SS3's vector definitions to one matrix for each referenced fleet-year.
  if (!is.null(dat[["agecomp"]]) && nrow(agecomp_filtered) > 0) {
    age_error_column <- names(agecomp_filtered)[
      tolower(names(agecomp_filtered)) %in% c("ageerr", "ageerror")
    ]
    if (length(age_error_column) != 1) {
      cli::cli_abort(
        "Age-composition data must contain one age-error definition column."
      )
    }

    age_error_ids <- agecomp_filtered[[age_error_column]]
    if (
      anyNA(age_error_ids) ||
        !is.numeric(age_error_ids) ||
        any(!is.finite(age_error_ids)) ||
        any(age_error_ids < 0) ||
        any(age_error_ids != floor(age_error_ids))
    ) {
      cli::cli_abort(
        "Age-error definition IDs in age-composition data must be non-negative whole numbers."
      )
    }

    # Definition k occupies rows 2*k-1 and 2*k: mean observed age and SD,
    # respectively, with one column for each true age.
    ss3_ageerror <- dat[["ageerror"]]
    n_ageerror_definitions <- if (is.null(ss3_ageerror)) {
      0L
    } else {
      nrow(ss3_ageerror) / 2
    }
    if (n_ageerror_definitions != floor(n_ageerror_definitions)) {
      cli::cli_abort(
        "SS3 age-error definitions must have paired mean and SD rows."
      )
    }
    if (any(age_error_ids > n_ageerror_definitions)) {
      cli::cli_abort(
        "An age-composition row references an unavailable SS3 age-error definition."
      )
    }

    # Convert only definitions referenced by retained samples. ID zero needs
    # no error matrix; user-omitted definitions also use identity below.
    referenced_ids <- unique(age_error_ids[age_error_ids > 0])
    if (!is.null(ageing_error_matrices)) {
      if (
        !is.numeric(ageing_error_matrices) ||
          anyNA(ageing_error_matrices) ||
          any(!is.finite(ageing_error_matrices)) ||
          any(ageing_error_matrices < 1) ||
          any(ageing_error_matrices != floor(ageing_error_matrices))
      ) {
        cli::cli_abort(
          "{.code ageing_error_matrices} must contain positive whole-number SS3 definition IDs."
        )
      }
      if (any(ageing_error_matrices > n_ageerror_definitions)) {
        cli::cli_abort(
          "{.code ageing_error_matrices} includes an unavailable SS3 definition ID."
        )
      }
      selected_ids <- intersect(unique(ageing_error_matrices), referenced_ids)
    } else {
      selected_ids <- referenced_ids
    }

    if (length(selected_ids) > 0) {
      age_columns <- match(paste0("age", ages), names(ss3_ageerror))
      if (anyNA(age_columns)) {
        cli::cli_abort(
          "SS3 age-error vectors do not include every requested true age."
        )
      }

      # Calculate probabilities from the mean/SD pair identified by definition_id.
      # Return a matrix with true ages in rows and observed ages in columns.
      make_ageing_error_matrix <- function(definition_id) {
        mean_observed_age <- as.numeric(
          ss3_ageerror[(2 * definition_id) - 1, age_columns, drop = TRUE]
        )
        sd_observed_age <- as.numeric(
          ss3_ageerror[2 * definition_id, age_columns, drop = TRUE]
        )

        # SS3 treats a mean of -1 or less as an unbiased reader.
        unbiased_mean <- which(
          !is.na(mean_observed_age) & mean_observed_age <= -1
        )
        mean_observed_age[unbiased_mean] <- ages[unbiased_mean] + 0.5
        if (sd_observed_age[1] < 0) {
          cli::cli_abort(
            "Parameter-based SS3 age-error definitions cannot be converted."
          )
        }
        if (
          any(!is.finite(mean_observed_age)) ||
            any(!is.finite(sd_observed_age)) ||
            any(sd_observed_age <= 0)
        ) {
          cli::cli_abort(
            "SS3 age-error means must be finite and SDs must be finite and positive for definition {.val {definition_id}}."
          )
        }

        n_ages <- length(ages)
        probabilities <- matrix(0, nrow = n_ages, ncol = n_ages)
        if (n_ages == 1) {
          probabilities[1, 1] <- 1
        } else {
          # Each bin spans [age, age + 1); the end bins collect both tails.
          boundaries <- ages[-1]
          for (true_age_index in seq_along(ages)) {
            cumulative_probability <- stats::pnorm(
              boundaries,
              mean = mean_observed_age[true_age_index],
              sd = sd_observed_age[true_age_index]
            )
            probabilities[true_age_index, 1] <- cumulative_probability[1]
            if (n_ages > 2) {
              probabilities[true_age_index, 2:(n_ages - 1)] <-
                diff(cumulative_probability)
            }
            probabilities[true_age_index, n_ages] <-
              1 - cumulative_probability[n_ages - 1]
          }
        }
        probabilities
      }

      # Map retained samples to FIMS fleet names and years. Other SS3 sample
      # coordinates are not part of this age-error matrix assignment.
      all_ageing_error_mappings <- agecomp_filtered |>
        dplyr::transmute(
          fleet = dat[["fleetnames"]][abs(.data[["fleet"]])],
          timing = .data[["year"]],
          definition_id = .data[[age_error_column]]
        ) |>
        dplyr::distinct()

      # Check all definitions in any fleet-year that needs a selected matrix,
      # including omitted IDs, to avoid assigning that matrix to other samples.
      selected_fleet_years <- all_ageing_error_mappings |>
        dplyr::filter(definition_id %in% selected_ids) |>
        dplyr::distinct(fleet, timing)
      conflicting_mappings <- all_ageing_error_mappings |>
        dplyr::semi_join(selected_fleet_years, by = c("fleet", "timing")) |>
        dplyr::count(fleet, timing) |>
        dplyr::filter(n > 1)
      if (nrow(conflicting_mappings) > 0) {
        cli::cli_abort(
          paste(
            "A fleet-year references multiple SS3 age-error definitions,",
            "which cannot be represented by one FIMS ageing_error matrix."
          )
        )
      }

      # Use matrix values as keys so identical SS3 definitions can be coalesced.
      matrix_key <- function(probabilities) {
        paste(sprintf("%.17g", as.vector(probabilities)), collapse = ",")
      }
      identity_matrix <- diag(length(ages))
      identity_key <- matrix_key(identity_matrix)
      # Cache each distinct matrix once and map definition IDs to its key.
      # Identity represents no misclassification: observed age equals true age.
      matrix_catalog <- list()
      definition_keys <- stats::setNames(
        vapply(
          selected_ids,
          function(definition_id) {
            probabilities <- make_ageing_error_matrix(definition_id)
            key <- matrix_key(probabilities)
            matrix_catalog[[key]] <<- probabilities
            key
          },
          character(1)
        ),
        as.character(selected_ids)
      )
      matrix_catalog[[identity_key]] <- identity_matrix

      ageing_error_mapping <- all_ageing_error_mappings |>
        dplyr::mutate(
          matrix_key = ifelse(
            definition_id %in% selected_ids,
            definition_keys[as.character(definition_id)],
            identity_key
          )
        ) |>
        dplyr::distinct(fleet, timing, matrix_key)

      # Candidate defaults let FIMS apply one matrix broadly and override only
      # fleet-, year-, or fleet-year-specific exceptions.
      candidate_defaults <- unique(c(
        identity_key,
        ageing_error_mapping[["matrix_key"]]
      ))
      best_rule_count <- Inf
      best_rules <- NULL
      for (default_key in candidate_defaults) {
        # A fleet or year can share one override only if all its mapped samples
        # use the same matrix. The chosen default needs no such override.
        fleet_rules <- ageing_error_mapping |>
          dplyr::group_by(fleet) |>
          dplyr::summarize(
            is_uniform = dplyr::n_distinct(matrix_key) == 1,
            matrix_key = dplyr::first(matrix_key),
            .groups = "drop"
          ) |>
          dplyr::filter(is_uniform, matrix_key != default_key)
        year_rules <- ageing_error_mapping |>
          dplyr::group_by(timing) |>
          dplyr::summarize(
            is_uniform = dplyr::n_distinct(matrix_key) == 1,
            matrix_key = dplyr::first(matrix_key),
            .groups = "drop"
          ) |>
          dplyr::filter(is_uniform, matrix_key != default_key)

        # Find exceptions left after broader rules. Fleet overrides take
        # precedence over year overrides here; otherwise use the default.
        specific_rules <- ageing_error_mapping |>
          dplyr::rowwise() |>
          dplyr::mutate(
            applied_key = {
              fleet_key <- fleet_rules[["matrix_key"]][match(
                fleet,
                fleet_rules[["fleet"]]
              )]
              year_key <- year_rules[["matrix_key"]][match(
                timing,
                year_rules[["timing"]]
              )]
              if (!is.na(fleet_key)) {
                fleet_key
              } else if (!is.na(year_key)) {
                year_key
              } else {
                default_key
              }
            }
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(matrix_key != applied_key) |>
          dplyr::select(fleet, timing, matrix_key)

        # Choose the representation with the fewest complete matrix tables.
        # An identity default is implicit, so it adds no output table.
        rule_count <- as.integer(default_key != identity_key) +
          nrow(fleet_rules) +
          nrow(year_rules) +
          nrow(specific_rules)
        if (rule_count < best_rule_count) {
          best_rule_count <- rule_count
          best_rules <- list(
            default_key = default_key,
            fleet = fleet_rules,
            year = year_rules,
            specific = specific_rules
          )
        }
      }

      # Flatten each matrix into FIMS rows: age identifies observed age and
      # uncertainty identifies true age. NA fleet/timing gives a broader scope.
      matrix_rows <- function(key, fleet, timing) {
        probabilities <- matrix_catalog[[key]]
        data.frame(
          type = "ageing_error",
          fleet = fleet,
          age = rep(ages, times = length(ages)),
          length = NA_real_,
          timing = timing,
          observed = as.vector(t(probabilities)),
          unit = "proportion",
          uncertainty = as.character(rep(ages, each = length(ages)))
        )
      }

      # Add the general age-error matrix, then any different matrices needed
      # for particular fleets, years, or fleet-year combinations. Use the same
      # matrix values wherever the same matrix is needed.
      ageing_error_rows <- list()
      if (best_rules[["default_key"]] != identity_key) {
        ageing_error_rows[[length(ageing_error_rows) + 1]] <- matrix_rows(
          best_rules[["default_key"]],
          fleet = NA_character_,
          timing = NA_real_
        )
      }
      for (rule_index in seq_len(nrow(best_rules[["fleet"]]))) {
        rule <- best_rules[["fleet"]][rule_index, ]
        ageing_error_rows[[length(ageing_error_rows) + 1]] <- matrix_rows(
          rule[["matrix_key"]],
          fleet = rule[["fleet"]],
          timing = NA_real_
        )
      }
      for (rule_index in seq_len(nrow(best_rules[["year"]]))) {
        rule <- best_rules[["year"]][rule_index, ]
        ageing_error_rows[[length(ageing_error_rows) + 1]] <- matrix_rows(
          rule[["matrix_key"]],
          fleet = NA_character_,
          timing = rule[["timing"]]
        )
      }
      for (rule_index in seq_len(nrow(best_rules[["specific"]]))) {
        rule <- best_rules[["specific"]][rule_index, ]
        ageing_error_rows[[length(ageing_error_rows) + 1]] <- matrix_rows(
          rule[["matrix_key"]],
          fleet = rule[["fleet"]],
          timing = rule[["timing"]]
        )
      }
      ageing_error <- dplyr::bind_rows(ageing_error_rows)
    }
  }

  # Length composition data
  if (!is.null(dat[["lencomp"]])) {
    lencomp_filtered <- filter_and_report_fleets(
      dat[["lencomp"]],
      fleet_column = "fleet",
      label = "lencomp"
    )
    # Remove discard samples before duplicate selection, just as for ages.
    # Missing partition values are retained rather than assumed to be discards.
    n_discard_lencomp <- sum(lencomp_filtered[["part"]] == 1, na.rm = TRUE)
    if (n_discard_lencomp > 0) {
      lencomp_filtered <- lencomp_filtered |>
        dplyr::filter(is.na(.data[["part"]]) | .data[["part"]] != 1)
      cli::cli_alert_info(
        "Excluded {n_discard_lencomp} discard length-composition rows (part = 1), which FIMS does not support."
      )
    }
  }
  if (!is.null(dat[["lencomp"]]) && nrow(lencomp_filtered) > 0) {
    # As for ages, select one sample per FIMS fleet-year before reshaping.
    # Signed fleet filtering has already occurred; ties keep the first input row.
    n_lencomp <- nrow(lencomp_filtered)
    lencomp_filtered <- lencomp_filtered |>
      dplyr::mutate(fleet = abs(.data[["fleet"]])) |>
      dplyr::group_by(.data[["fleet"]], .data[["year"]]) |>
      dplyr::slice_max(.data[["Nsamp"]], n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    if (nrow(lencomp_filtered) < n_lencomp) {
      cli::cli_alert_info(
        "Retained the largest-sample length composition per fleet-year: {n_lencomp} rows reduced to {nrow(lencomp_filtered)}."
      )
    }

    len_info <- process_composition(lencomp_filtered, "^(f|l)[0-9]+$") |>
      dplyr::rename(length = bin)
    # Change from proportion to number
    #dplyr::mutate(observed = (observed * Nsamp) |> round(0))

    # finish converting age comps to FIMSFrame format
    lencomps <- data.frame(
      type = "length_comp", # will likely need to change name
      fleet = dat[["fleetnames"]][abs(len_info[["fleet"]])], # abs to include fleet == -4
      age = NA,
      length = len_info[["length"]],
      timing = len_info[["year"]],
      observed = len_info[["observed"]], # + 0.001, # add constant to avoid 0 values
      unit = "proportion",
      # Q: should uncertainty here be the total sample size across bins, or the samples within the bin?
      uncertainty = glue::glue(
        "~dmultinom(prob = lengthcomp_proportion, size = {round(len_info[['Nsamp']])})"
      )
    )
    if (any(dat[["len_info"]][["CompError"]] != 0)) {
      bad_length_fleet <- dplyr::filter(
        dat[["len_info"]],
        .data[["CompError"]] != 0
      )
      cli::cli_alert_info(
        "FIMS can currently only accommodate the multinomial distribution
        for length-composition data."
      )
      cli::cli_alert_warning(
        "The following fleets use the Dirichlet-multinomial distribution and
        will be changed to use the multinomial: {row.names(bad_length_fleet)}."
      )
    }
  } else {
    # if no retained length comps present
    lencomps <- NULL # not sure if we need this but wanting to avoid an error if missing age or length comps
  }

  if (EWAA) {
    # Use SS3's population (fleet 0) female weights in season/birthseason 1.
    # This is a fixed subset, not a fleet-specific or sex-averaged weight table.
    wtatage <- ss3_inputs[["wtatage"]] |>
      dplyr::filter(fleet == 0 & sex == 1 & seas == 1 & birthseas == 1) |> # TODO make this more flexible
      dplyr::select("year", dplyr::matches("[0-9]+")) |>
      tidyr::pivot_longer(cols = -year, names_to = "age") |>
      dplyr::mutate(age = as.numeric(age)) |>
      dplyr::filter(age <= max(ages)) |>
      dplyr::mutate(
        type = "weight_at_age",
        fleet = dat[["fleetnames"]][1], # weight-at-age is only needed for one fleet, so arbitrarily assigning to fleet 1
        length = NA,
        timing = year,
        observed = value / 1000, # convert to metric tons (SS3)
        unit = "mt",
        uncertainty = NA_character_
      ) |>
      dplyr::select(-year, -value)

    # if end year + 1 is not present in weight-at-age matrix,
    # copy ending year rows and add 1 to timing for the copy
    if (!max(years_wtatage) %in% wtatage[["timing"]]) {
      cli::cli_alert_warning(
        "The weight-at-age data does not include end year + 1. Adding rows for year {max(years_wtatage)} by copying rows from year {max(wtatage[['timing']])}."
      )
      wtatage <- rbind(
        wtatage,
        wtatage |>
          dplyr::filter(timing == max(timing)) |>
          dplyr::mutate(
            timing = max(timing) + 1
          )
      )
    }
  } else {
    wtatage <- NULL
  }

  # In empirical mode, length and CAAL likelihoods need age-to-length
  # probabilities; marginal age compositions alone do not trigger this table.
  # TODO: is it correct to make this conditional on length comps existing?
  if (EWAA && (!is.null(lencomps) || any(agecomps[["type"]] == "caal_comp"))) {
    if (!is.null(ss3_dir)) {
      cli::cli_alert_info(
        "Reading in output from {.code {ss3_dir}} using {.code r4ss::SS_output()}"
      )
      ss3_output <- r4ss::SS_output(
        dir = ss3_dir,
        verbose = FALSE,
        printstats = FALSE
      )
    }
    if (!is.list(ss3_output) || is.null(ss3_output[["ALK"]])) {
      cli::cli_abort(
        c(
          "Age-to-length conversion requires `ss3_output` with an `ALK` component.",
          "i" = "Provide `ss3_output` containing `ALK`, or supply `ss3_dir` so SS3 output can be read when length or conditional age-at-length compositions are present."
        )
      )
    }
    # Use the female ALK at the middle of season 1. Rows are population length
    # bins and columns are ages; no seasonal or sex averaging is performed.
    ALK <- ss3_output[["ALK"]][,, "Seas: 1 Sub_Seas: 2 Morph: 1"]

    # check for bin widths different between data and population length bins
    data_bin_widths <- sort(unique(diff(dat[["lbin_vector"]])))
    pop_bin_widths <- sort(unique(diff(dat[["lbin_vector_pop"]])))
    if (!identical(data_bin_widths, pop_bin_widths)) {
      cli::cli_alert_warning(
        "Age-to-length conversion matrix from SS3 is based on population length
         bins which have different minimum bin widths than the data length bins. 
         The function will not aggregate all the proportions, and the results may not 
         make sense. Consider modifying the SS3 model to have matching population 
         and data bins."
      )
    }
    # check for values in dat[["lbin_vector_pop"]] that are not in dat[["lbin_vector"]]
    # and filter them from the age_to_length data frame if they exist
    if (!identical(dat[["lbin_vector_pop"]], dat[["lbin_vector"]])) {
      cli::cli_alert_info(
        "Aggregating low and high length bins in the `age_to_length_conversion` to match first and last data bins"
      )
      # population bins below the first data bin
      low_bins <- dat[["lbin_vector_pop"]][
        dat[["lbin_vector_pop"]] < min(dat[["lbin_vector"]])
      ]
      # population bins above the last data bin
      high_bins <- dat[["lbin_vector_pop"]][
        dat[["lbin_vector_pop"]] > max(dat[["lbin_vector"]])
      ]
      # remove last bin (really just an upper bound)
      high_bins <- high_bins[high_bins != max(dat[["lbin_vector_pop"]])]
    } else {
      low_bins <- NULL
      high_bins <- NULL
    }

    # Look up the SS3 proportion for one age and length bin, aggregating tails.
    # Return NA when either coordinate is outside the requested bins.
    length_at_age_lookup <- function(age, length) {
      if (age %in% ages && length %in% lengths) {
        # add the low or high population bins to the first and last data bin
        if (length == min(lengths)) {
          length_vec <- c(low_bins, length)
        } else if (length == max(lengths)) {
          length_vec <- c(length, high_bins)
        } else {
          length_vec <- length
        }
        if (!all(as.character(length_vec) %in% dimnames(ALK)[[1]])) {
          cli::cli_abort(
            "Length bins in the age-to-length conversion matrix do not match the length bins in the data. Check that the population length bins in the SS3 model are consistent with the data length bins."
          )
        }
        value <- ALK[as.character(length_vec), as.character(age)] |> sum()
        return(value)
      } else {
        return(NA)
      }
    }

    # create empty data frame to store the age to length conversions
    age_to_length <- data.frame(
      type = character(),
      fleet = character(),
      age = integer(),
      length = integer(),
      timing = character()
    )
    # Build every requested age-length pair, including zero probabilities.
    # NA fleet and timing mark this conversion table as shared across the model.
    age_to_length <-
      expand.grid(
        age = ages,
        length = lengths
      ) |>
      rowwise() |>
      mutate(observed = length_at_age_lookup(age, length)) |>
      mutate(
        fleet = NA,
        timing = NA,
        type = "age_to_length_conversion",
        unit = "proportion",
        uncertainty = NA_character_
      ) |>
      dplyr::select(dplyr::all_of(names(res)))
  } else {
    # if no length comps, then we don't need the age to length conversion matrix
    age_to_length <- NULL
  }

  # combine all data sources
  res <- rbind(
    res,
    catch,
    indices,
    agecomps,
    ageing_error,
    lencomps,
    wtatage,
    age_to_length
  )

  # Keep observations through endyr and weights through endyr + 1. filter_out()
  # preserves rows with NA timing, such as model-wide conversion matrices.
  res <- res |>
    dplyr::filter_out(type == "weight_at_age" & timing > dat[["endyr"]] + 1) |>
    dplyr::filter_out(type != "weight_at_age" & timing > dat[["endyr"]])

  # remove negative year values
  if (any(res[["timing"]] < 0, na.rm = TRUE)) {
    res_filtered_out <- res |>
      dplyr::filter(timing < 0)
    cli::cli_alert_warning(
      "Negative year values found and removed (count by type):"
    )
    print(res_filtered_out |> dplyr::count(type))
    res <- res |>
      dplyr::filter(is.na(timing) | timing >= 0)
  }

  if (nrow(res) > 0) {
    # Validate final keys after all transformations. Check each type separately
    # because ageing_error uses an additional true-age coordinate in its key.
    res <- res |>
      split(factor(res[["type"]], levels = unique(res[["type"]]))) |>
      lapply(filter_repeated_rows) |>
      dplyr::bind_rows()
  }

  if (nrow(res) > 0) {
    cli::cli_alert_success(
      "Data processing completed successfully. N rows by type (count by type):"
    )
    print(res |> dplyr::count(type))
  } else {
    cli::cli_alert_warning("No data rows found after processing.")
  }

  return(tibble::as_tibble(res))
}
