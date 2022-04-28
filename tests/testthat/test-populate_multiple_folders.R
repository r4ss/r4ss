example_path <- system.file("extdata", package = "r4ss")
# create a temporary directory location to write files to.
# to view the temp_path location, you can add a browser() statement after
# creating the temp_path directory and see what temp_path is. R should write
# to the same location for the same R session (if you restart R, temp_path will)
# change.
temp_path <- file.path(tempdir(), "test_pop_mult_folders")
dir.create(temp_path, showWarnings = FALSE)
# remove all artifacts created from testing. (developers: simply comment out
# the line below if you want to keep artifacts for troubleshooting purposes)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

test_that("populate_multiple_folders runs", {
  copy_info <- populate_multiple_folders(outerdir.old = example_path,
                            outerdir.new = temp_path,
                            exe.file = NULL,
                            verbose = FALSE
                            )
  expect_true(all(copy_info[["results.files"]]))
  lapply(copy_info[["dir"]], function(d) {
    expect_true(file.exists(file.path(temp_path, d)))
    expect_length(list.files(file.path(temp_path,d)), 4)
  })
})
