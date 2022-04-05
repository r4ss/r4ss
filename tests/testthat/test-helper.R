test_that("version_check() returns '' when file is not found ", {
  testthat::expect_equal("", version_check("c:/thisfiledoesnotexist.ss"))
})

test_that("version_search() returns appropriate range of values ", {
  # Version number for each SS3 example with control file
  ctlexamplepaths <- dir(
  path = system.file("extdata", package = "r4ss"),
  pattern = "simple",
  full.names = TRUE
  ) %>%
  dir(pattern = "control", full.names = TRUE)
  versions <- lapply(ctlexamplepaths, version_search)
  versionsnumeric <- type.convert(versions, as.is = TRUE)
  # All values should be between 3.3 and 4.0 because SS3 will never have a v.4
  testthat::expect_true(all(versionsnumeric >= 3.3))
  testthat::expect_true(all(versionsnumeric < 4))
  testthat::expect_false(any(versionsnumeric) > 3.3,
    label = glue::glue("
      SS3 has incremented the minor version number and
      there are some down-stream functions that need updated.
      `version_search(allow = )` should be updated to include the
      newest version number.
      "
    )
  )
})

test_that("version_search() with manually created file ", {
  filelocation <- tempfile(fileext = ".txt")
  writeLines("#V3.99", filelocation)
  testthat::expect_equal(
    object = version_search(filelocation, allow = c("3.99", "4.0")),
    "3.99",
    label = "xx" 
  )
  file.remove(filelocation)
})

test_that("Create numeric version number ", {
  testthat::expect_equal(version_numeric("3.40"), 3.4)
})
