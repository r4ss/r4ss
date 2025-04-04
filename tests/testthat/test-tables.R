context("Run table functions on model output")

example_path <- system.file("extdata", package = "r4ss")
# create a temporary directory location to write files to.
# to view the temp_path location, you can add a browser() statement after
# creating the temp_path directory and see what temp_path is. R should write
# to the same location for the same R session (if you restart R, temp_path will)
# change.
temp_path <- file.path(tempdir(), "test_basics")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

# get model output (already tested in test-basics.R)
simple_small <- SS_output(file.path(example_path, "simple_small"),
  verbose = FALSE, printstats = FALSE
)

# run table_exec_summary
table_exec_summary(simple_small, dir = temp_path)

test_that("table_exec_summary() runs on simple_small model", {
  expect_true("time_series.rda" %in% dir(file.path(temp_path, "tables")))
})

# run SSexecutivesummary (older version of table_exec_summary)
SSexecutivesummary(simple_small, plotfolder = temp_path)

test_that("SSexecutivesummary() runs on simple_small model", {
  expect_true("table_labels.csv" %in% dir(file.path(temp_path, "tables")))
})

# run table_pars
table_pars_output <- table_pars(simple_small, dir = temp_path)

test_that("table_pars() runs on simple_small model", {
  expect_true("table_pars.rda" %in% dir(file.path(temp_path, "tables")))
  expect_true(is.data.frame(table_pars_output$table))
  expect_true(nrow(table_pars_output$table) == 59)
  expect_true(table_pars_output$table$Bounds[1] == "(0.05, 0.15)")
  expect_true(is.character(table_pars_output$cap))
})

# run table_parcounts
table_parcounts_output <- table_parcounts(simple_small, dir = temp_path)

test_that("table_parcounts() runs on simple_small model", {
  expect_true("table_parcounts.rda" %in% dir(file.path(temp_path, "tables")))
  expect_true(is.data.frame(table_parcounts_output$table))
  expect_true(table_parcounts_output$table$Count[table_parcounts_output$table$Type == "Size selectivity"] == 4)
})

# run table_parcounts with inputs
simple_small_inputs <- SS_read(simple_small$inputs$dir, verbose = FALSE)
file.remove(file.path(temp_path, "tables", "table_parcounts.rda"))
table_parcounts_output <- table_parcounts(simple_small, inputs = simple_small_inputs, dir = temp_path)
test_that("table_parcounts() runs with 'inputs' argument", {
  expect_true("table_parcounts.rda" %in% dir(file.path(temp_path, "tables")))
  expect_true(is.data.frame(table_parcounts_output$table))
  expect_true(table_parcounts_output$table$Count[table_parcounts_output$table$Type == "Size selectivity"] == 4)
})

# run table_ts
table_ts_output <- table_ts(simple_small, dir = temp_path)

test_that("table_ts() runs on simple_small model", {
  expect_true("table_ts.rda" %in% dir(file.path(temp_path, "tables")))
  expect_true(is.data.frame(table_ts_output$table))
  expect_true(nrow(table_ts_output$table) == 12)
})

# run table_compweight
table_compweight_output <- table_compweight(simple_small, dir = temp_path)

test_that("table_compweight() runs on simple_small model", {
  expect_true("table_compweight.rda" %in% dir(file.path(temp_path, "tables")))
  expect_true(is.data.frame(table_compweight_output$table))
  expect_true(
    table_compweight_output$table |>
      dplyr::filter(Type == "Age", Fleet == "SURVEY1") |>
      dplyr::pull("Sum N adj.") ==
      100
  )
})


# run table_outputconfig
table_outputconfig_output <- table_outputconfig(simple_small, dir = temp_path)

test_that("table_outputconfig() runs on simple_small model", {
  expect_true("table_outputconfig.rda" %in% dir(file.path(temp_path, "tables")))
  expect_true(is.data.frame(table_outputconfig_output$table))
  expect_true(
    table_outputconfig_output$table |>
      dplyr::filter(Section == "Data age bins") |>
      dplyr::pull(Configuration) ==
      "1-15 by 1 year"
  )
})
