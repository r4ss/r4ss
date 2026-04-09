context("Convert SS3 data to FIMS format")

test_that("ss3_data_to_fims() handles irregular population length bins", {
  path <- system.file("extdata", "simple_small", package = "r4ss")

  expect_no_error(
    fims_data <- ss3_data_to_fims(ss3_dir = path)
  )

  expect_named(
    fims_data,
    c(
      "type",
      "name",
      "age",
      "length",
      "timing",
      "value",
      "unit",
      "uncertainty"
    )
  )

  expect_true(all(c(
    "landings",
    "index",
    "age_comp",
    "length_comp",
    "weight_at_age",
    "age_to_length_conversion"
  ) %in% fims_data$type))

  age_to_length <- subset(fims_data, type == "age_to_length_conversion")
  expect_equal(nrow(age_to_length), 25 * 16)
  expect_setequal(unique(age_to_length$length), c(
    26, 28, 30, 32, 34, 36, 38, 40, 42, 44,
    46, 48, 50, 52, 54, 56, 58, 60, 62, 64,
    68, 72, 76, 80, 90
  ))
  expect_setequal(unique(age_to_length$age), 0:15)
})