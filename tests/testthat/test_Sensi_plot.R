###############################################################################
### automated tests of r4ss package
###############################################################################

context("SS_Sensi_plot")

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

test_that("SS_Sensi_plot runs", {
# read model output
simple3.30.13 <- SS_output(file.path(example_path,"simple_3.30.13"),
                           verbose = FALSE, printstats = FALSE)

# create a fake summary of 19 models (all the same in this case)
# to match the example in SS_Sensi_plot()
zz <- list()
for (i in 1:19) {
  zz[[i]] <- simple3.30.13
}

#Use the summarize function in r4ss to get model summaries
model.summaries <- SSsummarize(zz)

#Define the names of each model. This will be used to label runs in the table and in the figures.
mod.names<-c(
  "Reference",
  "M: Fix to 2009",
  "M: Fix to prior",
  "M: Fix to Hamel",
  "M: Fix to VBGF",
  "M: Fix to OR",
  "VBGF 2009",
  "VBGF Grebel",
  "OR maturity",
  "Est. h",
  "All rec devs",
  "No rec devs",
  "High bias adj.",
  "Harmonic mean",
  "Dirichlet",
  "Wts = 1",
  "No blocks",
  "First blocks in 2000",
  "Alt rec catches"
)

#Run the sensitivity plot function
SS_Sensi_plot(model.summaries=model.summaries,
              dir = temp_path,
              current.year = 2001,
              mod.names = mod.names, #List the names of the sensitivity runs
              likelihood.out = c(1,1,0),
              Sensi.RE.out = "Sensi_RE_out.DMP", #Saved file of relative errors
              CI = 0.95, #Confidence interval box based on the reference model
              TRP.in = 0.4, #Target relative abundance value
              LRP.in = 0.25, #Limit relative abundance value
              sensi_xlab = "Sensitivity scenarios", #X-axis label
              ylims.in = c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1), #Y-axis label
              plot.figs = c(1,1,1,1,1,1), #Which plots to make/save? 
              sensi.type.breaks = c(6.5,9.5,13.5,16.5), #vertical breaks that can separate out types of sensitivities
              anno.x = c(3.75,8,11.5,15,18), # Vertical positioning of the sensitivity types labels
              anno.y = c(1,1,1,1,1), # Horizontal positioning of the sensitivity types labels
              anno.lab = c("Natural mortality","VBGF/Mat.","Recruitment","Data Wts.","Other") #Sensitivity types labels
              )
  expect_true(file.exists(file.path(temp_path, "Sensi_logREplot_FMSY.png")))
})
