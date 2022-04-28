example_path <- system.file("extdata", package = "r4ss")
# create a temporary directory location to write files to.
# to view the temp_path location, you can add a browser() statement after
# creating the temp_path directory and see what temp_path is. R should write
# to the same location for the same R session (if you restart R, temp_path will)
# change.
temp_path <- file.path(tempdir(), "test_simple")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

out <- SS_output(file.path(example_path, "simple_3.30.13"),
  verbose = FALSE, printstats = FALSE, hidewarn = TRUE
)

test_that("TSCplot function runs", {
  # Note: could look into using snapshot test to verify output
  spawning_df <- TSCplot(out, makePNG = file.path(temp_path, "tscplot.png"))
  expect_equivalent(ncol(spawning_df), 4)
})
