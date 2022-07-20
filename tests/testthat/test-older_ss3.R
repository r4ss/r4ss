###############################################################################
### tests of SS_output() and SS_plots() in SS3 version 3.24
### this file should not be included in the CRAN version
### because they depend on model files that are too large
###############################################################################

context("Basic r4ss functions")

example_path <- system.file("extdata", package = "r4ss")
# create a temporary directory location to write files to.
# to view the temp_path location, you can add a browser() statement after
# creating the temp_path directory and see what temp_path is. R should write
# to the same location for the same R session (if you restart R, temp_path will)
# change.
temp_path <- file.path(tempdir(), "test_older_ss3")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

###############################################################################
# testing SS_output
###############################################################################
simple3.24 <- SS_output(file.path(example_path, "simple_3.24"),
  verbose = FALSE,
  printstats = FALSE
)
test_that("SS_output() runs on simple_3.24 model", {
  expect_equal(tail(names(simple3.24), 1), "inputs")
})

###############################################################################
# specific tests for elements in the list created by SS_output
###############################################################################
test_that("SS_output() list: Kobe looks right", {
  expect_true(all(simple3.24$Kobe$Year %in% 1950:2050))
})

###############################################################################
# testing SS_plots with models loaded above
###############################################################################

test_that("SS_plots runs on simple_3.24 model", {
  plots3.24 <- SS_plots(simple3.24,
    dir = temp_path, printfolder = "plots_3.24",
    verbose = FALSE
  )
  expect_equal(tail(plots3.24$file, 1), "parameterchecks.html")
})

###############################################################################
# testing SSsummarize, SSplotComparisons, and SStableComparisons
###############################################################################

test_that("SSsummarize and SSplotComparisons work to compare 3.24 to 3.30", {
  # first read current model (different configuration)
  simple_small <- SS_output(file.path(example_path, "simple_small"),
    verbose = FALSE,
    printstats = FALSE
  )

  # summarize results
  simple_summary <- SSsummarize(list(simple3.24, simple_small))

  # plot comparisons of results
  comparison_plots <- SSplotComparisons(simple_summary,
    png = TRUE,
    plotdir = temp_path, verbose = FALSE,
    indexUncertainty = TRUE
  )

  # confirm that function finished
  expect_equal(length(comparison_plots), 17)

  # make table of comparisons
  simple_table <- SStableComparisons(simple_summary, verbose = FALSE)
  # confirm that output produces a data.frame
  # with 3 variables (label, model1, model2)
  expect_output(str(simple_table), "variables")
})

# clean up
unlink(temp_path, recursive = TRUE)
