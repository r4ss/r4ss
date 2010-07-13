SSmakesims <-
function(copymasters=TRUE,makecases=FALSE,olddir=NULL,newdir=NULL,
         exe="ss3.exe",steep=FALSE,M=FALSE,CV=FALSE,trend=F)
{
  if(copymasters){
    # new directory
    dir.create(newdir)
    dir.create(paste(newdir,"sims",sep="/"))
    dir.create(paste(newdir,"fits",sep="/"))
    copyfile <- function(name,overwrite=F){
        file.copy(paste(olddir,name,sep="/"),paste(newdir,name,sep="/"),
                  overwrite=overwrite)
    }
    copyfile('starter.ss',overwrite=T)
    copyfile('forecast.ss',overwrite=T)
    copyfile('dat_master.ss',overwrite=T)
    copyfile('ctl_master.ss',overwrite=T)

    copyfile(paste('sims',exe,sep='/'),overwrite=T)
    copyfile(paste('fits',exe,sep='/'),overwrite=T)
  }
  if(makecases){
    setwd(newdir)
    # create a new set of control files
    source('c:/SS/morphs/R/make_ctl_files.R')
    wd <- getwd()
    # simulation models
    makectl(newmaxbias=1,indir=wd,outdir='same',outname='ctl_simA.ss',selex='d',Nmorphs=5,overwrite=T,fixed=T,note='simulation model CaseA')
    makectl(newmaxbias=1,indir=wd,outdir='same',outname='ctl_simB.ss',selex='d',Nmorphs=1,overwrite=T,fixed=T,note='simulation model CaseB')
    makectl(newmaxbias=1,indir=wd,outdir='same',outname='ctl_simC.ss',selex='a',Nmorphs=5,overwrite=T,fixed=T,note='simulation model CaseC')
    makectl(newmaxbias=1,indir=wd,outdir='same',outname='ctl_simD.ss',selex='a',Nmorphs=1,overwrite=T,fixed=T,note='simulation model CaseD')

    estimated=c('R0','steep','VonBert','L_at_A','NatM_p_1_Fem','CV_young_Fem_GP_1')
    if(!steep) estimated <- estimated[estimated!="steep"]
    if(!M) estimated <- estimated[estimated!="NatM_p_1_Fem"]
    if(!CV) estimated <- estimated[estimated!="CV_young_Fem_GP_1"]

    # estimation models
    makectl(indir=wd,outdir='same',outname='ctl_fitA.ss',selex='d',Nmorphs=5,overwrite=T,estimated=estimated,fixed=F,note='estimation model CaseA')
    makectl(indir=wd,outdir='same',outname='ctl_fitB.ss',selex='d',Nmorphs=1,overwrite=T,estimated=estimated,fixed=F,note='estimation model CaseB')
    makectl(indir=wd,outdir='same',outname='ctl_fitC.ss',selex='a',Nmorphs=5,overwrite=T,estimated=estimated,fixed=F,note='estimation model CaseC')
    makectl(indir=wd,outdir='same',outname='ctl_fitD.ss',selex='a',Nmorphs=1,overwrite=T,estimated=estimated,fixed=F,note='estimation model CaseD')

    # estimation models with trend in selectivity
    if(trend){
      makectl(newmaxbias=maxb,newramp=newr,indir=wd,outdir='same',outname='ctl_fitA_trend.ss',selex='d',Nmorphs=5,overwrite=T,estimated=estimated,fixed=F,seltrend=T,note='CaseA with trend in selectivity')
      makectl(newmaxbias=maxb,newramp=newr,indir=wd,outdir='same',outname='ctl_fitB_trend.ss',selex='d',Nmorphs=1,overwrite=T,estimated=estimated,fixed=F,seltrend=T,note='CaseB with trend in selectivity')
      makectl(newmaxbias=maxb,newramp=newr,indir=wd,outdir='same',outname='ctl_fitC_trend.ss',selex='a',Nmorphs=5,overwrite=T,estimated=estimated,fixed=F,seltrend=T,note='CaseC with trend in selectivity')
      makectl(newmaxbias=maxb,newramp=newr,indir=wd,outdir='same',outname='ctl_fitD_trend.ss',selex='a',Nmorphs=1,overwrite=T,estimated=estimated,fixed=F,seltrend=T,note='CaseD with trend in selectivity')
    }

    ## file.copy('dat_master.ss','dat_simA.ss')
    ## file.copy('dat_master.ss','dat_fitA.ss')

    ## f1 <- SS_readforecast()
    ## SS_writeforecast(f1,dir='fits')
    ## SS_writeforecast(f1,dir='sims')
    file.copy('forecast.ss','sims/forecast.ss')
    file.copy('forecast.ss','fits/forecast.ss')

    s1sim <- s1fit <- SS_readstarter()

    s1sim$ctlfile <- "ctl_isim.ss"
    s1sim$datfile <- "dat_isim.ss"

    s1fit$ctlfile <- "ctl_ifit.ss"
    s1fit$datfile <- "dat_ifit.ss"

    s1sim$run_display_detail <- 0
    s1fit$run_display_detail <- 0

    s1sim$N_bootstraps <- 3
    s1fit$N_bootstraps <- 1
    s1sim$last_estimation_phase <- 0
    s1fit$last_estimation_phase <- 25
    SS_writestarter(s1fit, dir='fits', overwrite=T)
    SS_writestarter(s1sim, dir='sims', overwrite=T)
  }
} # end of function
