context("Basic r4ss functions")

example_path <- system.file("extdata", package="r4ss")

# testing SS_output with models run in 3.24U and 3.30.01.12
test_that("SS_output runs on simple_3.24 model", {
  simple3.24 <- SS_output(file.path(example_path,"simple_3.24"))
  #expect_equal(is.list(simple3.24), "TRUE")
  expect_equal(tail(names(simple3.24),1), "inputs")
})

test_that("SS_output runs on simple_3.30 model", {
  simple3.30 <- SS_output(file.path(example_path,"simple_3.30"))
  #expect_equal(is.list(simple3.30), "TRUE")
  expect_equal(tail(names(simple3.30),1), "inputs")
})

# testing SS_plots with models loaded above
test_that("SS_plots runs on simple_3.24 model", {
  simple3.24 <- SS_output(file.path(example_path,"simple_3.24"),
                          verbose=FALSE, printstats=FALSE)
  plots3.24 <- SS_plots(simple3.24)
  expect_equal(tail(plots3.24$file,1), "data_plot2.png")
})

# testing SS_plots with models loaded above
test_that("SS_plots runs on simple_3.30 model", {
  simple3.30 <- SS_output(file.path(example_path,"simple_3.30"),
                          verbose=FALSE, printstats=FALSE)
  plots3.30 <- SS_plots(simple3.30)
  expect_equal(tail(plots3.30$file,1), "data_plot2.png")
})

# testing SSsummarize, SSplotComparisons, and SStableComparisons
test_that("SSsummarize and SSplotComparisons both work", {
  # read output from two models
  simple3.24 <- SS_output(file.path(example_path,"simple_3.24"),
                          verbose=FALSE, printstats=FALSE)
  simple3.30 <- SS_output(file.path(example_path,"simple_3.30"),
                          verbose=FALSE, printstats=FALSE)
  # run summarize function
  simple_summary <- SSsummarize(list(simple3.24, simple3.30))

  # plot comparisons of results
  comparison_plots <- SSplotComparisons(simple_summary, png=TRUE,
                                        plotdir=example_path)
  # confirm that function finished
  expect_equal(comparison_plots, "finished comparison plots")

  # make table of comparisons
  simple_table <- SStableComparisons(simple_summary)
  # confirm that output produces a data.frame
  # with 3 variables (label, model1, model2)
  expect_output(str(SStableComparisons(simple_summary)), "3 variables")
  
})

