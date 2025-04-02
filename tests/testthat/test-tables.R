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
table_pars_output <- table_pars(simple_small)

test_that("table_pars() runs on simple_small model", {
  expect_true(is.data.frame(table_pars_output))
  expect_true(table_pars_output$Bounds[1] == "(0.05, 0.15)")
})

# run table_parcounts
table_parcounts_output <- table_parcounts(simple_small)

test_that("table_parcounts() runs on simple_small model", {
  expect_true(is.data.frame(table_parcounts_output))
  expect_true(table_parcounts_output$Count[table_parcounts_output$Type == "Size selectivity"] == 4)
})


# run table_parcounts
table_parcounts_output <- table_parcounts(simple_small)

test_that("table_parcounts() runs on simple_small model", {
  expect_true(is.data.frame(table_parcounts_output))
  expect_true(table_parcounts_output$Count[table_parcounts_output$Type == "Size selectivity"] == 4)
})
