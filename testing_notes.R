if(FALSE){

  devtools::install_github('r4ss/SStesting')
  ## source('c:/github/SStesting/R/populate_multiple_folders.R')
  ## source('c:/github/SStesting/R/copy_SS_inputs.R')
  ## source('c:/github/SStesting/R/run_SS_models.R')

  populate_multiple_folders(outerdir.old = 'c:/SS/old_models',
                            outerdir.new = 'c:/SS/new_models',
                            exe.dir = 'c:/SS/SSv3.30.12.00')

  mods.dir13 <- dir('C:/ss/modeltesting/Version_3.30.13-beta_Oct15', full.names=TRUE)
  mods13 <- SSgetoutput(dirvec=mods.dir13)

  for(imod in 1:length(mods13)){
    print(imod)
    print(mods.dir13[imod])
    graphics.off()
    SS_plots(mods13[[imod]])
  }


  for(imod in 1:length(mods.dir13)){
    print(imod)
    print(mods.dir13[imod])
    print(file.info(file.path(mods.dir13[imod], "plots/SS_output.html"))$size)
  }


}
