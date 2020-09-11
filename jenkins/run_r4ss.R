#.libPaths() # see the library paths
install.packages("remotes", lib = "/usr/local/lib/R/site-library")
remotes::install_local(".", upgrade = "always", 
                       lib = "/usr/local/lib/R/site-library")
						
# get model folder names
mod_names <- list.dirs("run_R/models", full.names = FALSE, recursive = FALSE)
print(mod_names)

out <- lapply(mod_names, function(i) {
  tryCatch(r4ss::SS_output(file.path("run_R", "models", i), 
                           verbose = FALSE, hidewarn = TRUE, printstats = FALSE), 
           error = function(e) {
             print(e)
           }
   )
 })

plots <- lapply(out, function(x) {
  tryCatch(r4ss::SS_plots(x, verbose = FALSE),
			   error = function(e) {
			 print(e)
			   })
  })

# determine if job fails or not
out_issues <- mod_names[unlist(lapply(out, function(x) "error" %in% class(x)))]
plotting_issues<- mod_names[unlist(lapply(plots, function(x) "error" %in% class(x)))]

if(length(out_issues) == 0 & length(plotting_issues) == 0) {
  message("all r4ss functions completed successfully")
} else {
  message("There were some errors. SS_output failed to run for models ", 
          paste0(out_issues, collapse = ", "), "; SS_plots failed to run for ",
          "models ", paste0(plotting_issues, collapse = " ,"), ". More info ", 
          "below.")
  message("Problems with SS_output:")
  for(i in out_issues) {
    message(i)
    tmp_loc <- which(mod_names == i)
    print(out[[tmp_loc]])
  }
  for(p in plotting_issues) {
    message(p)
    tmp_loc <- which(mod_names == p)
    print(plots[[tmp_loc]])
  }
  q(status = 1)
}
