## ### automated tests of r4ss package
## context("r4ss functions that require model to run")

## example_path <- system.file("extdata", package="r4ss")

## # testing SS_doRetro
## test_that("SS_doRetro runs on simple_3.24 model", {
##     SS_doRetro(masterdir=file.path(example_path,"simple_3.24"),
##              oldsubdir="", newsubdir="retrospectives", years=0:-2)
  
##   #expect_equal(is.list(simple3.24), "TRUE")
##   #expect_equal(tail(names(simple3.24),1), "inputs")
## })

## test_that("SS_doRetro runs on simple_3.30.01 model", {
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
