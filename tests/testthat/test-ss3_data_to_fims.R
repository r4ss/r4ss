context("Convert SS3 data to FIMS format")

test_that("ss3_data_to_fims() runs on simple_small", {
  path <- system.file("extdata", "simple_small", package = "r4ss")

  expect_no_error(
    fims_data <- ss3_data_to_fims(
      ss3_dir = path,
      ss_new = FALSE,
      EWAA = TRUE
    )
  )

  expect_named(
    fims_data,
    c(
      "type",
      "fleet",
      "age",
      "length",
      "timing",
      "observed",
      "unit",
      "uncertainty"
    )
  )

  expect_true(all(
    c(
      "catch",
      "index",
      "age_comp",
      "ageing_error",
      "length_comp",
      "weight_at_age",
      "age_to_length_conversion"
    ) %in%
      fims_data$type
  ))

  simple_small_input <- SS_read(dir = path)

  years <- simple_small_input[["dat"]][["styr"]]:simple_small_input[["dat"]][[
    "endyr"
  ]]
  length_bins <- simple_small_input[["dat"]][["lbin_vector"]]
  age_bins <- 0:max(simple_small_input[["dat"]][["agebin_vector"]])

  age_to_length <- subset(fims_data, type == "age_to_length_conversion")

  expect_equal(nrow(age_to_length), length(length_bins) * length(age_bins))

  expect_setequal(
    object = unique(age_to_length$length),
    expected = length_bins
  )
  expect_setequal(
    object = unique(age_to_length$age),
    expected = age_bins
  )

  identifying_columns <- c("type", "fleet", "age", "length", "timing")
  expect_equal(
    nrow(fims_data),
    nrow(
      fims_data |>
        dplyr::mutate(
          true_age = ifelse(type == "ageing_error", uncertainty, NA_character_)
        ) |>
        dplyr::distinct(
          dplyr::across(dplyr::all_of(c(identifying_columns, "true_age")))
        )
    )
  )

  # tests for converted weight-at-age data
  weight_at_age <- subset(fims_data, type == "weight_at_age")
  expect_equal(nrow(weight_at_age), length(age_bins) * (1 + length(years)))
})

test_that("ss3_data_to_fims() converts SS3 ageing-error vectors", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  maxage <- max(ss3_inputs[["dat"]][["agebin_vector"]])
  ages <- 0:maxage
  age_columns <- paste0("age", ages)
  mean_row <- 3
  sd_row <- 4
  ss3_inputs[["dat"]][["ageerror"]][mean_row, age_columns] <- ages + 0.5
  ss3_inputs[["dat"]][["ageerror"]][mean_row, "age0"] <- -1
  ss3_inputs[["dat"]][["ageerror"]][sd_row, age_columns] <- 0.5

  fims_data <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ageing_error <- dplyr::filter(fims_data, type == "ageing_error")
  first_matrix <- ageing_error |>
    dplyr::filter(is.na(timing))

  expect_equal(
    nrow(first_matrix),
    length(ages)^2
  )
  expect_true(all(is.na(ageing_error[["fleet"]])))
  expect_true(all(is.na(ageing_error[["timing"]])))
  expect_equal(
    sum(dplyr::filter(
      first_matrix,
      age == 0,
      uncertainty == "0"
    )[["observed"]]),
    stats::pnorm(1, mean = 0.5, sd = 0.5)
  )
  expect_equal(
    sum(dplyr::filter(
      first_matrix,
      age == maxage,
      uncertainty == as.character(maxage)
    )[["observed"]]),
    1 - stats::pnorm(maxage, mean = maxage + 0.5, sd = 0.5)
  )

  probability_sums <- ageing_error |>
    dplyr::group_by(fleet, timing, uncertainty) |>
    dplyr::summarize(probability_sum = sum(observed), .groups = "drop")
  expect_equal(probability_sums[["probability_sum"]], rep(1, nrow(probability_sums)))
})

test_that("ss3_data_to_fims() ageing error matches SS3 AAK matrix 2", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_output <- SS_output(path, verbose = FALSE, printstats = FALSE)
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  converted <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ages <- 0:15
  ageing_error <- dplyr::filter(
    converted,
    type == "ageing_error",
    is.na(fleet),
    is.na(timing)
  )
  converter_matrix <- matrix(
    NA_real_,
    nrow = length(ages),
    ncol = length(ages),
    dimnames = list(as.character(ages), as.character(ages))
  )
  for (row in seq_len(nrow(ageing_error))) {
    converter_matrix[
      as.character(ageing_error[["age"]][row]),
      ageing_error[["uncertainty"]][row]
    ] <- ageing_error[["observed"]][row]
  }

  # SS3 observed-age bin 1 combines the converter's age-0 and age-1 bins.
  ss3_observed_bins <- rbind(
    converter_matrix["0", , drop = FALSE] +
      converter_matrix["1", , drop = FALSE],
    converter_matrix[as.character(2:15), , drop = FALSE]
  )
  rownames(ss3_observed_bins) <- as.character(1:15)
  ss3_observed_bins <- ss3_observed_bins[as.character(15:1), , drop = FALSE]
  ss3_ageing_error <- ss3_output[["AAK"]][2, 1:15, 1:16]

  expect_equal(
    unname(ss3_observed_bins),
    unname(ss3_ageing_error),
    tolerance = 1e-6
  )
})

test_that("ss3_data_to_fims() emits year-specific ageing-error overrides", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  ageerror <- ss3_inputs[["dat"]][["ageerror"]]
  maxage <- ss3_inputs[["dat"]][["Nages"]]
  ages <- 0:maxage
  age_columns <- paste0("age", ages)
  ageerror[1, age_columns] <- ages + 0.5
  ageerror[2, age_columns] <- 0.5
  ageerror[3, age_columns] <- ages + 1
  ageerror[4, age_columns] <- 0.5
  ss3_inputs[["dat"]][["ageerror"]] <- ageerror

  years <- sort(unique(ss3_inputs[["dat"]][["agecomp"]][["year"]]))
  year_definition <- stats::setNames(
    rep(c(1, 2), length.out = length(years)),
    as.character(years)
  )
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]] <- unname(
    year_definition[as.character(ss3_inputs[["dat"]][["agecomp"]][["year"]])]
  )

  fims_data <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ageing_error <- dplyr::filter(fims_data, type == "ageing_error")

  expect_true(all(is.na(ageing_error[["fleet"]])))
  expect_true(all(is.na(ageing_error[["timing"]]) |
    ageing_error[["timing"]] %in% years))
  expect_equal(
    length(unique(ageing_error[["timing"]][!is.na(ageing_error[["timing"]])])),
    sum(year_definition != year_definition[1])
  )

  for (year in years) {
    year_matrix <- ageing_error |>
      dplyr::filter(is.na(timing) | timing == year) |>
      dplyr::group_by(timing) |>
      dplyr::group_split() |>
      lapply(function(rows) {
        list(
          timing = rows[["timing"]][1],
          value = sum(rows[["observed"]][rows[["age"]] == 0 & rows[["uncertainty"]] == "0"])
        )
      })
    default_index <- which(vapply(year_matrix, function(x) is.na(x$timing), logical(1)))
    specific_index <- which(vapply(year_matrix, function(x) !is.na(x$timing), logical(1)))
    applied_matrix <- if (length(specific_index) > 0) {
      year_matrix[[specific_index[1]]]
    } else {
      year_matrix[[default_index[1]]]
    }
    definition_id <- year_definition[as.character(year)]
    expected_probability <- if (definition_id == 1) {
      stats::pnorm(1, mean = 0.5, sd = 0.5)
    } else {
      stats::pnorm(1, mean = 1, sd = 0.5)
    }
    expect_equal(applied_matrix$value, expected_probability)
  }
})

test_that("ss3_data_to_fims() leaves fleets without rows at identity", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  fleet_without_ageing_error <- abs(ss3_inputs[["dat"]][["agecomp"]][["fleet"]][1])
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]] <- 2
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]][
    abs(ss3_inputs[["dat"]][["agecomp"]][["fleet"]]) == fleet_without_ageing_error
  ] <- 0

  fims_data <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ageing_error <- dplyr::filter(fims_data, type == "ageing_error")
  matrix_rows <- dplyr::filter(
    ageing_error,
    fleet == ss3_inputs[["dat"]][["fleetnames"]][2],
    is.na(timing)
  )

  expect_equal(nrow(matrix_rows), length(0:max(ss3_inputs[["dat"]][["agebin_vector"]]))^2)
  expect_false(any(ageing_error[["fleet"]] ==
    ss3_inputs[["dat"]][["fleetnames"]][fleet_without_ageing_error]))
  expect_true(all(is.na(ageing_error[["timing"]])))
})

test_that("ss3_data_to_fims() does not convert parameter-based age error", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]] <- 0

  fims_data <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)

  expect_false(any(fims_data[["type"]] == "ageing_error"))
})

test_that("ss3_data_to_fims() rejects mixed age-error keys within a fleet-year", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  agecomp_row <- ss3_inputs[["dat"]][["agecomp"]] |>
    dplyr::slice(1)
  agecomp_row[["ageerr"]] <- 0
  ss3_inputs[["dat"]][["agecomp"]] <- dplyr::bind_rows(
    ss3_inputs[["dat"]][["agecomp"]],
    agecomp_row
  )

  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE),
    "fleet-year references multiple SS3 age-error definitions"
  )
})

test_that("ss3_data_to_fims() skips empirical growth data when EWAA is false", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  input_only_dir <- tempfile("ss3_inputs_")
  dir.create(input_only_dir)
  on.exit(unlink(input_only_dir, recursive = TRUE), add = TRUE)
  file.copy(
    file.path(path, c("starter.ss", "control.ss", "data.ss", "forecast.ss")),
    input_only_dir
  )

  expect_no_error(
    fims_data <- ss3_data_to_fims(
      ss3_dir = input_only_dir,
      ss_new = FALSE,
      EWAA = FALSE
    )
  )

  expect_false(any(
    c(
      "weight_at_age",
      "age_to_length_conversion"
    ) %in%
      fims_data[["type"]]
  ))
})

test_that("ss3_data_to_fims() inherits EWAA from the control file", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)

  expect_identical(ss3_inputs[["ctl"]][["EmpiricalWAA"]], 0)
  fims_data <- ss3_data_to_fims(
    ss3_inputs = ss3_inputs,
    ss3_output = NULL,
    EWAA = NULL
  )

  expect_false(any(
    c(
      "weight_at_age",
      "age_to_length_conversion"
    ) %in%
      fims_data[["type"]]
  ))
})

test_that("ss3_data_to_fims() rejects unsupported CPUE distributions", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  ss3_inputs[["dat"]][["CPUEinfo"]][["errtype"]] <- 1

  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs),
    "FIMS cannot accommodate Student's t-distributions in index data"
  )
})

test_that("ss3_data_to_fims() validates age and length bins", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)

  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE, maxage = -1),
    "maxage.*non-negative whole number"
  )
  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE, lengths = 999),
    "lengths.*length bin in the SS3 data"
  )
})

test_that("ss3_data_to_fims() rejects empty filtered catch data", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)

  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE, fleets = 999),
    "No catch data remain after removing equilibrium rows"
  )
})

test_that("ss3_data_to_fims() rejects conflicting repeated rows", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  catch_row <- ss3_inputs[["dat"]][["catch"]] |>
    dplyr::filter(year != -999) |>
    dplyr::slice(1)
  conflicting_row <- catch_row
  conflicting_row[["catch"]] <- conflicting_row[["catch"]] + 1
  ss3_inputs[["dat"]][["catch"]] <- dplyr::bind_rows(
    ss3_inputs[["dat"]][["catch"]],
    conflicting_row
  )

  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE),
    "have conflicting values"
  )
})

test_that("ss3_data_to_fims() filters and reports repeated rows by type", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(
    dir = path,
    ss_new = FALSE,
    read_wtatage = TRUE
  )
  ss3_output <- SS_output(
    dir = path,
    verbose = FALSE,
    printstats = FALSE
  )
  catch_to_repeat <- ss3_inputs[["dat"]][["catch"]] |>
    dplyr::filter(year != -999) |>
    dplyr::slice(1)
  ss3_inputs[["dat"]][["catch"]] <- rbind(
    ss3_inputs[["dat"]][["catch"]],
    catch_to_repeat
  )

  expect_message(
    fims_data <- ss3_data_to_fims(
      ss3_inputs = ss3_inputs,
      ss3_output = ss3_output
    ),
    "Removed 1 repeated rows \\(7.7%\\) out of 13 total rows for type.*catch"
  )

  catch <- dplyr::filter(fims_data, type == "catch")
  expect_equal(
    nrow(catch),
    nrow(dplyr::distinct(catch, type, fleet, age, length, timing))
  )
})
