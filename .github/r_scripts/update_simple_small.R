# Script to update simple_small model with the latest ss3 executable.

devtools::load_all()

get_ss3_exe(dir = "inst/extdata/simple_small")
dir <- "inst/extdata/simple_small"
run(dir = dir, verbose = TRUE, skipfinished = FALSE)

files_to_keep <- c("CompReport.sso","covar.sso","data_boot_001.ss","data_expval.ss",
                   "Forecast-report.sso","Report.sso","ss_summary.sso","ss3.par",
                   "starter.ss_new","control.ss_new","data_echo.ss_new","forecast.ss_new",
                   "warning.sso")
files_to_delete <- setdiff(list.files(dir, full.names = FALSE), files_to_keep)

purrr::map(files_to_delete, function(x){
    file_name <- file.path(dir,x)
    file.remove(file_name)
})

file.rename(file.path(dir,"control.ss_new"), file.path(dir,"control.ss"))
file.rename(file.path(dir,"starter.ss_new"), file.path(dir,"starter.ss"))
file.rename(file.path(dir,"forecast.ss_new"), file.path(dir,"forecast.ss"))
file.rename(file.path(dir,"data_echo.ss_new"), file.path(dir,"data.ss"))
