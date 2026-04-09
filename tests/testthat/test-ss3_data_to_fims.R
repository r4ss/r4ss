context("Convert SS3 data to FIMS format")

test_that("ss3_data_to_fims() runs on simple_small", {
  path <- system.file("extdata", "simple_small", package = "r4ss")

    expect_no_error(
        fims_data <- ss3_data_to_fims(ss3_dir = path, ss_new = FALSE)
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

    expect_true(all(
        c(
            "landings",
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

    # tests for converted weight-at-age data
    weight_at_age <- subset(fims_data, type == "weight_at_age")
    expect_equal(nrow(weight_at_age), length(age_bins) * (1 + length(years)))
})
