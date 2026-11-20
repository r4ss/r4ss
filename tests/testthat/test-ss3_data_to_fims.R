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

  # Confirm the FIMS schema and the data types represented by this model.
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
  expect_false("caal_comp" %in% fims_data$type)

  simple_small_input <- SS_read(dir = path)

  years <- simple_small_input[["dat"]][["styr"]]:simple_small_input[["dat"]][[
    "endyr"
  ]]
  length_bins <- simple_small_input[["dat"]][["lbin_vector"]]
  age_bins <- 0:max(simple_small_input[["dat"]][["agebin_vector"]])

  # The conversion table should contain every age-length combination.
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

  # Weight-at-age includes every age for each model year plus the terminal year.
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
  # Use known age-specific normal parameters to check probabilities and row sums.
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
  expect_equal(
    probability_sums[["probability_sum"]],
    rep(1, nrow(probability_sums))
  )
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
  # Rebuild the converter's long rows as a matrix before aligning SS3 age bins.
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
  # Alternate two definitions across years to require year-specific overrides.
  year_definition <- stats::setNames(
    rep(c(1, 2), length.out = length(years)),
    as.character(years)
  )
  # Apply the assigned definition IDs to the composition observations by year.
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]] <- unname(
    year_definition[as.character(ss3_inputs[["dat"]][["agecomp"]][["year"]])]
  )

  fims_data <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ageing_error <- dplyr::filter(fims_data, type == "ageing_error")

  # Check each year against its override, or the default matrix when none exists.
  expect_true(all(is.na(ageing_error[["fleet"]])))
  expect_true(all(
    is.na(ageing_error[["timing"]]) |
      ageing_error[["timing"]] %in% years
  ))
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
          value = sum(rows[["observed"]][
            rows[["age"]] == 0 & rows[["uncertainty"]] == "0"
          ])
        )
      })
    default_index <- which(vapply(
      year_matrix,
      function(x) is.na(x$timing),
      logical(1)
    ))
    specific_index <- which(vapply(
      year_matrix,
      function(x) !is.na(x$timing),
      logical(1)
    ))
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

  # Selecting either available definition should change output; invalid IDs fail.
  first_definition <- ss3_data_to_fims(
    ss3_inputs = ss3_inputs,
    EWAA = FALSE,
    ageing_error_matrices = 1
  ) |>
    dplyr::filter(type == "ageing_error")
  second_definition <- ss3_data_to_fims(
    ss3_inputs = ss3_inputs,
    EWAA = FALSE,
    ageing_error_matrices = 2
  ) |>
    dplyr::filter(type == "ageing_error")
  expect_false(isTRUE(all.equal(first_definition, second_definition)))
  expect_error(
    ss3_data_to_fims(
      ss3_inputs = ss3_inputs,
      EWAA = FALSE,
      ageing_error_matrices = 0
    ),
    "must contain positive whole-number"
  )
  expect_error(
    ss3_data_to_fims(
      ss3_inputs = ss3_inputs,
      EWAA = FALSE,
      ageing_error_matrices = 3
    ),
    "unavailable SS3 definition ID"
  )
})

test_that("ss3_data_to_fims() leaves fleets without rows at identity", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  fleet_without_ageing_error <- abs(ss3_inputs[["dat"]][["agecomp"]][["fleet"]][
    1
  ])
  # Code 0 leaves this fleet at the identity matrix, so it needs no output rows.
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]] <- 2
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]][
    abs(ss3_inputs[["dat"]][["agecomp"]][["fleet"]]) ==
      fleet_without_ageing_error
  ] <- 0

  fims_data <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ageing_error <- dplyr::filter(fims_data, type == "ageing_error")
  matrix_rows <- dplyr::filter(
    ageing_error,
    fleet == ss3_inputs[["dat"]][["fleetnames"]][2],
    is.na(timing)
  )

  expect_equal(
    nrow(matrix_rows),
    length(0:max(ss3_inputs[["dat"]][["agebin_vector"]]))^2
  )
  expect_false(any(
    ageing_error[["fleet"]] ==
      ss3_inputs[["dat"]][["fleetnames"]][fleet_without_ageing_error]
  ))
  expect_true(all(is.na(ageing_error[["timing"]])))
})

test_that("ss3_data_to_fims() does not convert parameter-based age error", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  age_columns <- grep(
    "^age[0-9]+$",
    names(ss3_inputs[["dat"]][["ageerror"]]),
    value = TRUE
  )
  # A negative mean marks a parameter-based definition, which is unsupported.
  ss3_inputs[["dat"]][["ageerror"]][2, age_columns[1]] <- -1
  ss3_inputs[["dat"]][["agecomp"]][["ageerr"]] <- 1

  expect_error(
    ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE),
    "Parameter-based SS3 age-error definitions cannot be converted"
  )
})

test_that("ss3_data_to_fims() retains the largest sample with mixed age-error keys", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  agecomp_row <- ss3_inputs[["dat"]][["agecomp"]] |>
    dplyr::slice(1)
  agecomp_row[["ageerr"]] <- 1
  agecomp_row[["Nsamp"]] <- 10
  larger_row <- agecomp_row
  larger_row[["ageerr"]] <- 0
  larger_row[["Nsamp"]] <- 20

  ss3_inputs[["dat"]][["agecomp"]] <- larger_row
  expected <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  # The smaller sample's age-error definition must not affect the conversion.
  ss3_inputs[["dat"]][["agecomp"]] <- dplyr::bind_rows(
    agecomp_row,
    larger_row
  )
  converted <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  expect_equal(converted, expected)
  expect_false(any(converted[["type"]] == "ageing_error"))
})

test_that("ss3_data_to_fims() skips empirical growth data when EWAA is false", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  input_only_dir <- tempfile("ss3_inputs_")
  dir.create(input_only_dir)
  on.exit(unlink(input_only_dir, recursive = TRUE), add = TRUE)
  # Omit wtatage and other growth inputs to verify they are not needed with EWAA off.
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

  # The fixture sets EmpiricalWAA to 0, so a NULL argument should omit growth data.
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

test_that("ss3_data_to_fims() interprets CAAL bounds using Lbin_method", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  inputs <- SS_read(dir = path, ss_new = FALSE)
  inputs[["dat"]][["agecomp"]] <- inputs[["dat"]][["agecomp"]][
    1,
    ,
    drop = FALSE
  ]

  # Simple_small uses population-bin indexes; 1 through -1 spans all bins.
  marginal <- ss3_data_to_fims(ss3_inputs = inputs, EWAA = FALSE)
  expect_true("age_comp" %in% marginal[["type"]])
  expect_false("caal_comp" %in% marginal[["type"]])

  # SS3 may expand the method-2 sentinel pair to population-bin endpoints.
  expanded_inputs <- inputs
  expanded_inputs[["dat"]][["Lbin_method"]] <- 2
  expanded_inputs[["dat"]][["agecomp"]][["Lbin_lo"]] <- 1
  expanded_inputs[["dat"]][["agecomp"]][["Lbin_hi"]] <- length(
    expanded_inputs[["dat"]][["lbin_vector_pop"]]
  )
  expanded <- ss3_data_to_fims(ss3_inputs = expanded_inputs, EWAA = FALSE)
  expect_true("age_comp" %in% expanded[["type"]])
  expect_false("caal_comp" %in% expanded[["type"]])

  # The same population length (28) is expressed differently by each method:
  # population-bin index, matching data-bin index, or actual length.
  method_cases <- list(
    list(method = 1, lower = 10, upper = 10),
    list(method = 2, lower = 2, upper = 2),
    list(method = 3, lower = 28, upper = 28)
  )
  for (case in method_cases) {
    conditional_inputs <- inputs
    conditional_inputs[["dat"]][["Lbin_method"]] <- case$method
    conditional_inputs[["dat"]][["agecomp"]][["Lbin_lo"]] <- case$lower
    conditional_inputs[["dat"]][["agecomp"]][["Lbin_hi"]] <- case$upper

    conditional <- ss3_data_to_fims(
      ss3_inputs = conditional_inputs,
      EWAA = FALSE
    )
    caal <- dplyr::filter(conditional, type == "caal_comp")
    expect_gt(nrow(caal), 0)
    expect_true(all(caal[["length"]] == 28))
    expect_false("age_comp" %in% conditional[["type"]])
  }
})

test_that("ss3_data_to_fims() combines seasonal catches by fleet and year", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  inputs <- SS_read(dir = path, ss_new = FALSE)
  first_season <- inputs[["dat"]][["catch"]] |>
    dplyr::filter(year == 2013, fleet == 1, seas == 1) |>
    dplyr::slice(1)
  second_season <- first_season
  second_season[["seas"]] <- 2
  second_season[["catch"]] <- 50
  second_season[["catch_se"]] <- 0.2
  inputs[["dat"]][["catch"]] <- dplyr::bind_rows(
    inputs[["dat"]][["catch"]],
    second_season
  )

  converted <- ss3_data_to_fims(ss3_inputs = inputs, EWAA = FALSE)
  annual_catch <- dplyr::filter(
    converted,
    type == "catch",
    timing == 2013,
    fleet == inputs[["dat"]][["fleetnames"]][1]
  )
  total_catch <- first_season[["catch"]] + second_season[["catch"]]
  variance <- first_season[["catch"]]^2 *
    expm1(first_season[["catch_se"]]^2) +
    second_season[["catch"]]^2 * expm1(second_season[["catch_se"]]^2)
  expected_sdlog <- sqrt(log1p(variance / total_catch^2))

  expect_equal(nrow(annual_catch), 1)
  expect_equal(annual_catch[["observed"]], total_catch)
  expect_equal(
    annual_catch[["uncertainty"]],
    glue::glue(
      "~dlnorm(meanlog = log_catch_expected, sdlog = {expected_sdlog})"
    )
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

test_that("ss3_data_to_fims() excludes discard compositions before sample selection", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  inputs <- SS_read(dir = path, ss_new = FALSE)
  for (datatype in c("agecomp", "lencomp")) {
    inputs[["dat"]][[datatype]] <- inputs[["dat"]][[datatype]][
      1,
      ,
      drop = FALSE
    ]
    inputs[["dat"]][[datatype]][["part"]] <- 2
  }

  for (length_bin in c(-1, 10)) {
    inputs[["dat"]][["agecomp"]][["Lbin_lo"]] <- length_bin
    expected <- ss3_data_to_fims(ss3_inputs = inputs, EWAA = FALSE)
    with_discards <- inputs
    for (datatype in c("agecomp", "lencomp")) {
      discard <- inputs[["dat"]][[datatype]]
      discard[["part"]] <- 1
      discard[["Nsamp"]] <- discard[["Nsamp"]] + 1000
      if (datatype == "agecomp") {
        discard[["ageerr"]] <- 999
      }
      with_discards[["dat"]][[datatype]] <- rbind(
        discard,
        inputs[["dat"]][[datatype]]
      )
    }
    messages <- character()
    converted <- withCallingHandlers(
      ss3_data_to_fims(ss3_inputs = with_discards, EWAA = FALSE),
      message = function(message) {
        messages <<- c(messages, conditionMessage(message))
      }
    )
    expect_equal(converted, expected)
    expect_true(any(grepl("Excluded 1 discard age-composition rows", messages)))
    expect_true(any(grepl(
      "Excluded 1 discard length-composition rows",
      messages
    )))
  }

  for (datatype in c("agecomp", "lencomp")) {
    inputs[["dat"]][[datatype]][["part"]] <- 1
  }
  discarded <- ss3_data_to_fims(ss3_inputs = inputs, EWAA = FALSE)
  expect_false(any(
    discarded[["type"]] %in%
      c("age_comp", "caal_comp", "length_comp", "ageing_error")
  ))
})

test_that("ss3_data_to_fims() retains the largest length sample after fleet filtering", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  ss3_inputs[["dat"]][["agecomp"]] <- NULL
  small <- ss3_inputs[["dat"]][["lencomp"]][1, , drop = FALSE]
  bin_columns <- grep("^(f|l)[0-9]+$", names(small), value = TRUE)
  small[, bin_columns] <- 0
  small[[bin_columns[1]]] <- 1
  small[["Nsamp"]] <- 10
  large <- small
  large[, bin_columns] <- 0
  large[[tail(bin_columns, 1)]] <- 1
  large[["Nsamp"]] <- 20
  tied <- small
  tied[["Nsamp"]] <- large[["Nsamp"]]
  negative <- small
  negative[["fleet"]] <- -small[["fleet"]]
  negative[["Nsamp"]] <- 100

  ss3_inputs[["dat"]][["lencomp"]] <- large
  expected <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  ss3_inputs[["dat"]][["lencomp"]] <- dplyr::bind_rows(
    small,
    large,
    tied,
    negative
  )
  converted <- ss3_data_to_fims(ss3_inputs = ss3_inputs, EWAA = FALSE)
  expect_equal(converted, expected)

  fleets <- c(
    seq_along(ss3_inputs[["dat"]][["fleetnames"]]),
    negative[["fleet"]]
  )
  selected <- ss3_data_to_fims(
    ss3_inputs = ss3_inputs,
    EWAA = FALSE,
    fleets = fleets
  )
  ss3_inputs[["dat"]][["lencomp"]] <- negative
  expected_negative <- ss3_data_to_fims(
    ss3_inputs = ss3_inputs,
    EWAA = FALSE,
    fleets = fleets
  )
  expect_equal(selected, expected_negative)
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
  # Add a duplicate catch observation to check that it is reported and removed.
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
