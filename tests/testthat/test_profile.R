###############################################################################
### automated tests of r4ss package
###############################################################################

context("Profiling and changing parameter values")

example_path <- system.file("extdata", package="r4ss")

###############################################################################
# global variables
###############################################################################
ctlfile <- file.path(example_path, "simple_3.30.13", "simple_control.ss")
ss3file <- file.path(dirname(ctlfile), "ss.exe")
pars <- SS_parlines(ctlfile = ctlfile)

###############################################################################
# test r4ss functions used by SS_profile
###############################################################################
test_that("blocks_per_pattern not in read pars",
  expect_true(!"blocks_per_pattern" %in% pars[, "Label"]))

test_that("Error when repeat isn't true and string matches > 1 par", 
  expect_error(SS_changepars(dir = dirname(ctlfile), ctlfile = basename(ctlfile),
  newctlfile = "control_change.ss",
  strings = "NatM", newvals = 2, verbose = FALSE)))
test_that("Error when repeat is true and strings aren't unique", 
  expect_error(SS_changepars(dir = dirname(ctlfile), ctlfile = basename(ctlfile),
  newctlfile = "control_change.ss", repeat.vals = TRUE,
  strings = c("CV", "Mal"), newvals = c(2, 3), verbose = FALSE)))
test_that("Error when repeat is false and string matches > 1 par", 
  expect_error(SS_changepars(dir = dirname(ctlfile), ctlfile = basename(ctlfile),
  newctlfile = "control_change.ss", repeat.vals = FALSE,
  strings = c("CV", "Mal"), newvals = c(2, 3), verbose = FALSE)))
test_that("Should change the lower bound and INIT of 10 pars", {
  outs <- SS_changepars(dir = dirname(ctlfile), ctlfile = basename(ctlfile),
    newctlfile = "control_change.ss", repeat.vals = FALSE,
    strings = c("CV", "Mal"), newvals = rep(0.025, 10), 
    newlos = rep(0, 10), verbose = FALSE)
  expect_equal(NROW(outs), 10)
  expect_equal(outs[, "newvals"], rep(0.025, 10))
  expect_equal(outs[, "newlos"], rep(0.0, 10))
  })
test_that("Error when repeat is true and vals have length > 1", 
  expect_error(SS_changepars(dir = dirname(ctlfile), ctlfile = basename(ctlfile),
  newctlfile = "control_change.ss", repeat.vals = TRUE,
  strings = c("NatM", "CV"), newvals = 2:3, verbose = FALSE)))
unlink(file.path(dirname(ctlfile), "control_change.ss"))

# writeLines("dummy", con = ss3file)
# SS_profile(dir = dirname(ctlfile),
#   masterctlfile = basename(ctlfile), 
#   newctlfile = "test_ctl.ss", string = "NatM")
