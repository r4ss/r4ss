### automated tests of r4ss package
context("r4ss functions that require executables to run")

example_path <- system.file("extdata", package="r4ss")
# SSutils package available via devtools::install_github("r4ss/SSutils")
# require(SSutils) 

## # testing SS_doRetro
## test_that("SS_doRetro runs on simple_3.24 model", {
##   path_3.24 <- file.path(example_path, "simple_3.24")
##   skip_if(all(file.info(dir(path_3.24, full.names=TRUE))$exe=="no"),
##           message = "skipping test that requires SS executable")
##   SS_doRetro(masterdir=file.path(example_path,"simple_3.24"),
##              oldsubdir="", newsubdir="retrospectives", years=0:-2)
## })

## test_that("SS_doRetro runs on simple_3.30.01 model", {
##   path_3.30.01 <- file.path(example_path, "simple_3.30.01")
##   skip_if(all(file.info(dir(path_3.30.01, full.names=TRUE))$exe=="no"),
##           message = "skipping test that requires SS executable")
##   SS_doRetro(masterdir=file.path(example_path,"simple_3.30.01"),
##              oldsubdir="", newsubdir="retrospectives", years=0:-2)

##   # read model output from the retrospectives
##   retroModels <- SSgetoutput(
##       dirvec=file.path(example_path,"simple_3.30.01/retrospectives",
##           paste0("retro",0:-2)))
##   # summarize the model output
##   retroSummary <- SSsummarize(retroModels)
##   # set the fector of ending years
##   endyrvec <- retroSummary$endyrs + 0:-2
##   SSplotComparisons(retroSummary, endyrvec=endyrvec,
##                     legendlabels=paste("Data",0:-2,"years"))
## })

## test_that("SS_doRetro runs on simple_3.30.12 model", {
##   path_3.30.12 <- file.path(example_path, "simple_3.30.12")
##   skip_if(all(file.info(dir(path_3.30.12, full.names=TRUE))$exe=="no"),
##           message = "skipping test that requires SS executable")
##   SS_doRetro(masterdir=file.path(example_path,"simple_3.30.12"),
##              oldsubdir="", newsubdir="retrospectives", years=0:-2)

##   # read model output from the retrospectives
##   retroModels <- SSgetoutput(
##       dirvec=file.path(example_path,"simple_3.30.12/retrospectives",
##           paste0("retro",0:-2)))
##   # summarize the model output
##   retroSummary <- SSsummarize(retroModels)
##   # set the fector of ending years
##   endyrvec <- retroSummary$endyrs + 0:-2
##   SSplotComparisons(retroSummary, endyrvec=endyrvec,
##                     legendlabels=paste("Data",0:-2,"years"))
## })

## test_that("SS_RunJitter runs on simple_3.30.12 model", {
##   path_3.30.12 <- file.path(example_path, "simple_3.30.12")
##   skip_if(all(file.info(dir(path_3.30.12, full.names=TRUE))$exe=="no"),
##           message = "skipping test that requires SS executable")
##   dir.jit <- file.path(example_path,"simple_3.30.12/jitter")
##   SSutils::copy_SS_inputs(dir.old=file.path(example_path,"simple_3.30.12"),
##                           dir.new=dir.jit,
##                           create.dir=TRUE,
##                           overwrite=TRUE,
##                           copy_exe=TRUE,
##                           copy_par=TRUE,
##                           verbose=TRUE)
##   # read starter file
##   starter <- SS_readstarter(file.path(dir.jit, 'starter.ss'))
##   # Make use the par file as a starting point
##   starter$init_values_src = 1
##   # Change jitter (0.1 is an arbitrary, but common choice for jitter amount)
##   starter$jitter_fraction = 0.1
##   # write modified starter file
##   SS_writestarter(starter, dir=dir.jit, overwrite=TRUE)
##   # run jitters
##   likesaved <- SS_RunJitter(mydir=file.path(example_path,"simple_3.30.12/jitter"),
##                             Njitter=2)
##   expect_equal(is.vector(likesaved) & length(likesaved)==2, TRUE)
## })

## ###############################################################################

## test_that("SS_profile runs on simple_3.30.12 model", {
##   path_3.30.12 <- file.path(example_path, "simple_3.30.12")
##   skip_if(all(file.info(dir(path_3.30.12, full.names=TRUE))$exe=="no"),
##           message = "skipping test that requires SS executable")
##   dir.prof <- file.path(example_path,"simple_3.30.12/profile")
##   SSutils::copy_SS_inputs(dir.old=file.path(example_path,"simple_3.30.12"),
##                           dir.new=dir.prof,
##                           create.dir=TRUE,
##                           overwrite=TRUE,
##                           copy_exe=TRUE,
##                           copy_par=TRUE,
##                           verbose=TRUE)
##   starter <- SS_readstarter(file.path(dir.prof, 'starter.ss'))
##   # Make use the par file as a starting point
##   starter$ctlfile <- "control_modified.ss"
##   # write modified starter file
##   SS_writestarter(starter, dir=dir.prof, overwrite=TRUE)
##   # run profile
##   prof.table <- SS_profile(dir=dir.prof,
##                            masterctlfile="simple_control.ss",
##                            string="R0", profilevec=c(8.5, 9))
##   prof.out <- SSgetoutput(dirvec=dir.prof, keyvec=1:2)
##   plotprofile.out <- SSplotProfile(summaryoutput=SSsummarize(prof.out))
##   # check that the minimum of the total likelihood is 0
##   # (also serves to indicate that there are no NA values in this column)
##   expect_equal(min(plotprofile.out$TOTAL), 0)
## })
