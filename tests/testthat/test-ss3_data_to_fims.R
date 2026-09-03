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
    nrow(dplyr::distinct(
      fims_data,
      dplyr::across(dplyr::all_of(identifying_columns))
    ))
  )

  # tests for converted weight-at-age data
  weight_at_age <- subset(fims_data, type == "weight_at_age")
  expect_equal(nrow(weight_at_age), length(age_bins) * (1 + length(years)))
})

test_that("ss3_data_to_fims() skips empirical growth data when EWAA is false", {
  path <- system.file("extdata", "simple_small", package = "r4ss")
  ss3_inputs <- SS_read(dir = path, ss_new = FALSE)
  ss3_inputs[["wtatage"]] <- NULL

  fims_data <- ss3_data_to_fims(
    ss3_inputs = ss3_inputs,
    ss3_output = NULL,
    EWAA = FALSE
  )

  expect_false(any(c(
    "weight_at_age",
    "age_to_length_conversion"
  ) %in% fims_data[["type"]]))
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

  expect_false(any(c(
    "weight_at_age",
    "age_to_length_conversion"
  ) %in% fims_data[["type"]]))
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
