#' write control file
#'
#' write Stock Synthesis control file from list object in R which was probably
#' created using \code{\link{SS_readctl}}
#'
#'
#' @param ctllist  List object created by \code{\link{SS_readctl}}.
#' @param outfile Filename for where to write new data file.
#' @param overwrite Should existing files be overwritten? Default=FALSE.
#' @param verbose Should there be verbose output while running the file?
#' @param nseas number of season in the model. This information is not
#'  explicitly available in control file
#' @param N_areas number of spatial areas in the model. This information is also not
#'  explicitly available in control file
#' @param Do_AgeKey Flag to indicate if 7 additional ageing error parameters to be read
#'  set 1 (but in fact any non zero numeric in R) or TRUE to enable to read them 0 or FALSE (default)
#'  to disable them. This information is not explicitly available in control file, too.
#' @author Yukio Takeuchi
#' @importFrom gdata write.fwf
#' @importFrom stringr str_c
#' @export
#' @seealso \code{\link{SS_readctl}}, \code{\link{SS_readctl_3.24}},\code{\link{SS_readstarter}},
# ' \code{\link{SS_readforecast}},
# ' \code{\link{SS_writestarter}}, \code{\link{SS_writeforecast}},
# ' \code{\link{SS_writedat}}
SS_writectl_3.24 <- function(ctllist,outfile,overwrite=FALSE,verbose=TRUE,
## Parameters that are not defined in control file
## if ctllist is an output of SS_readctl these three inputs will be overriden by
## nseas,N_areas and Do_AgeKey in ctllist
    nseas=1,
    N_areas=1,
    Do_AgeKey=FALSE
){
#  require("gdata")
#  require("magrittr")
#  require("stringr")
  # function to write Stock Synthesis ctl files
  if(verbose) cat("running SS_writectl\n")

  if(ctllist$type!="Stock_Synthesis_control_file"){
    stop("input 'ctllist' should be a list with $type=='Stock_Synthesis_control_file'")
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  nseas<-ifelse(is.null(ctllist$nseas),nseas,ctllist$nseas)
  N_areas<-ifelse(is.null(ctllist$N_areas),N_areas,ctllist$N_areas)
  Do_AgeKey<-ifelse(is.null(ctllist$Do_AgeKey),Do_AgeKey,ctllist$Do_AgeKey)

  if(file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }

  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  #  on.exit({if(sink.number()>0) sink();close(zz)})
   on.exit(close(zz))
#  sink(zz)
  writeComment<-function(text,...){
    if(length(grep(x=text,pattern="^#"))!=length(text))text<-paste("#_",text,sep="")
    writeLines(text=text,con=zz,...)
  }
  wl <- function(name,comment=NULL,con=stdout){
    # simple function to clean up many repeated commands
    value = ctllist[names(ctllist)==name]
    if(is.null(comment)){
      writeLines(paste(value," #_",name,sep="",collapse="_"),con=zz)
    }else{
      if(length(grep(comment,pattern="^#"))!=0){
        writeLines(paste(value,comment),con=zz)
      }else{
        writeLines(paste(value," #_",comment,sep="",collapse="_"),con=zz)
      }
    }
  }

   wl.vector <- function(name,comment=NULL){
     # simple function to clean up many repeated commands
     value = ctllist[names(ctllist)==name][[1]]
     if(is.null(comment)){
       writeLines(paste(paste(value,collapse=" ")," #_",name,sep=""),con=zz)
 #      write.table(file=zz,x=t(value),append=TRUE,sep=" ",quote=FALSE,row.names=FALSE)
     }else{
       writeLines(paste(paste(value,collapse=" "),comment),con=zz)
 #      write.table(file=zz,x=t(value),append=TRUE,sep=" ",quote=FALSE,row.names=FALSE)
     }
   }

  wl.list<-function(name,comment=NULL,header=NULL){
    if(!is.null(header)){
      writeLines(paste0("#_",header),con=zz)
    }
    value = ctllist[names(ctllist)==name][[1]]
    value1<-sapply(value,function(x){stringr::str_c(paste(x),collapse=" ")},simplify=TRUE)
    writeLines(value1,con=zz)
  }

## Internal function to write formatted data.frame
  printdf <- function(dataframe,header=TRUE,headerLine=NA){
    # function to print data frame with hash mark before first column name
    if(is.character(dataframe)){
      tmp<-ctllist[names(ctllist)==dataframe]
      if(length(tmp)>0){
        dataframe<-tmp[[1]]
      }else{
        dataframe<-NULL
      }
    }
    if(!is.null(dataframe)){
      if(header){
        names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
        writeLines(paste(names(dataframe),collapse="\t"),con=zz)
      }
      if(!is.na(headerLine))xxx<-2
  #  print.data.frame(dataframe, row.names=FALSE, strip.white=TRUE,header)
      if(!is.null(rownames(dataframe))){
        rownames(dataframe)<-sapply(rownames(dataframe),function(z){ifelse(length(grep(x=z,pattern="^#"))==1,z,paste0("#_",z))})
        dataframe$comments<-rownames(dataframe)
      }
      #     write.table(file=zz,x=dataframe,append=TRUE,sep=" ",quote=FALSE,
      #                 row.names=FALSE,col.names=FALSE)
      gdata::write.fwf(file=zz,x=dataframe,append=TRUE,sep="\t",quote=FALSE,
                       rownames=FALSE,colnames=FALSE,digits=6)
    #  write_delim(path=zz,x=dataframe,append=TRUE,delim=" ",col_names=TRUE)
    }
  }

  # write a header
  writeComment("#C control file created using the SS_writectl function in the R package r4ss")
  writeComment(paste("#C should work with SS version:",ctllist$SSversion))
  writeComment(paste("#C file write time:",Sys.time()))
  writeComment("#")

  # write the contents
  wl("N_GP",comment="# N_Growth_Patterns") # N_Growth_Patterns
  wl("N_platoon",comment="#_N_Morphs_Within_GrowthPattern") # number of platoons  1, 3, 5 are best values to use
  if(ctllist$N_platoon>1){
    stop("currently sub morphs are not supported yet in this R code")
    wl("sd_ratio")
    wl("submorphdist")
  }
  if(ctllist$N_GP*nseas*N_areas>1) {
    wl("recr_dist_read",comment="#_number of recruitment assignments (overrides GP*area*seas parameter values)")
    wl("recr_dist_inx",comment="#_recruitment interaction requested")
    printdf("recr_dist_pattern")
  }
  if(N_areas>1){
  #  stop("Multi areas are not yet implemented")
    wl("N_moveDef",comment="#_N_movement_definitions goes here if N_areas > 1")
    wl("firstAgeMove",comment="#_first age that moves (real age at begin of season, not integer) also cond on do_migration>0")
    writeComment("move definition for seas, morph, source, dest, age1, age2")
    printdf("moveDef",header=FALSE)

  }else{
    writeComment("#_Cond 0 # N_movement_definitions goes here if N_areas > 1")
    writeComment("#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0")
    writeComment("#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10")
  }
  wl("N_Block_Designs",comment="#_Nblock_Patterns")
  if(ctllist$N_Block_Designs>0){
    wl.vector("blocks_per_pattern",comment="#_blocks_per_pattern")
    wl.list("Block_Design",header="#_begin and end years of blocks")
  }else{
    writeComment("#_Cond 0 #_blocks_per_pattern")
    writeComment("# begin and end years of blocks")
    writeComment("#")
  }
  wl("fracfemale") #_fracfemale
  wl("natM_type",comment="#_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate")
  writeComment("#_Age_natmort_by gender x growthpattern")
  if(ctllist$natM_type==1){
    wl("N_natM",comment="#_Number of M_segments")
    wl.vector("M_ageBreakPoints",comment="# age(real) at M breakpoints")
  }else if(ctllist$natM_type==2){
    wl.vector("Lorenzen_refage",comment="#_reference age for Lorenzen M; read 1P per morph")
  }else if(ctllist$natM_type %in% c(3,4)){
    printdf("natM")
  }else if(ctllist$natM_type==0){
    # Just to skip
  }else{
    stop("natM_type :",ctllist$natM_type, "is not supported")
  }
  ## Growth ##
  wl("GrowthModel",comment="# GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_speciific_K; 4=not implemented")
  wl("Growth_Age_for_L1")
  wl("Growth_Age_for_L2",comment="#_Growth_Age_for_L2 (999 to use as Linf)")

  wl("SD_add_to_LAA",comment="#_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)")
  wl("CV_Growth_Pattern",comment="#_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)")
  wl("maturity_option",comment=
    "#_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity by GP; 4=read age-fecundity by GP; 5=read fec and wt from wtatage.ss; 6=read length-maturity by GP")
  if(ctllist$maturity_option %in% c(3,4)){
    printdf("Age_Maturity")
  }
  if(ctllist$maturity_option==6){
    printdf("Len_Maturity")
  }
  wl("First_Mature_Age")
  wl("fecundity_option",comment=
      "fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W")
  if(is.null(ctllist$hermaphroditism_option))ctllist$hermaphroditism_option<-1
  wl("hermaphroditism_option",comment="hermaphroditism option:  0=none; 1=age-specific fxn")
  wl("parameter_offset_approach",comment=
    "parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)")
  wl("env_block_dev_adjust_method",comment=
    "env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)")
  writeComment(c("#","#_growth_parms"))
  #writeComment("#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn")
  printdf("MG_parms")

  writeComment("#")
  writeComment("#_Cond 0  #custom_MG-env_setup (0/1)")
  writeComment("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-environ parameters")
  writeComment("#")
  writeComment("#_Cond 0  #custom_MG-block_setup (0/1)")
  writeComment("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters")
  writeComment("#_Cond No MG parm trends")
  writeComment("#")
  writeComment("#_seasonal_effects_on_biology_parms")
  wl.vector("MGparm_seas_effects",comment=
    "#_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K")
  N_seas_effects<-sum(ctllist$MGparm_seas_effects)
  if(N_seas_effects>0){
    printdf("MG_parms_seas")
    writeComment("#")
  }else{
    writeComment(c("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters","#"))
  }
  DoParmDev<-sum(ctllist$MG_parms[,9])
  if(DoParmDev>0){
    if(is.null(ctllist$MGparm_Dev_Phase))ctllist$MGparm_Dev_Phase<- -4
    wl("MGparm_Dev_Phase")
    writeComment("#")
  }
  # SRR
  writeComment("#_Spawner-Recruitment")
  wl("SR_function",comment="#_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm")
  # writeComment("#_LO HI INIT PRIOR PR_type SD PHASE")
  printdf("SRparm")
  wl("SR_env_link")
  wl("SR_env_target")
  wl("do_recdev")
  wl("MainRdevYrFirst",comment="first year of main recr_devs; early devs can preceed this era")
  wl("MainRdevYrLast",comment="last year of main recr_devs; forecast devs start in following year")
  wl("recdev_phase",comment="recdev phase")
  wl("recdev_adv",comment="(0/1) to read 13 advanced options")
  if(ctllist$recdev_adv){
  #  cat("writing 13 advanced SRR options\n")
    wl("recdev_early_start",comment="#_recdev_early_start (0=none; neg value makes relative to recdev_start)")
    wl("recdev_early_phase",comment="#_recdev_early_phase")
    wl("Fcast_recr_phase",comment="#_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)")
    wl("lambda4Fcast_recr_like",comment="#_lambda for Fcast_recr_like occurring before endyr+1")
    wl("last_early_yr_nobias_adj",comment="#_last_early_yr_nobias_adj_in_MPD")
    wl("first_yr_fullbias_adj",comment="#_first_yr_fullbias_adj_in_MPD")
    wl("last_yr_fullbias_adj",comment="#_last_yr_fullbias_adj_in_MPD")
    wl("first_recent_yr_nobias_adj",comment="#_first_recent_yr_nobias_adj_in_MPD")
    wl("max_bias_adj",comment="#_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)")
    wl("period_of_cycles_in_recr",comment="#_period of cycles in recruitment (N parms read below)")
    wl("min_rec_dev",comment="#min rec_dev")
    wl("max_rec_dev",comment="#max rec_dev")
    wl("N_Read_recdevs",comment="#_read_recdevs")
  }
  writeComment("#_end of advanced SR options")
  writeComment("#_placeholder for full parameter lines for recruitment cycles")
  writeComment("# read specified recr devs")
  writeComment("#_Yr Input_value")

  if(ctllist$recdev_adv && ctllist$N_Read_recdevs>0){
    printdf("recdev_input")
  }

##
# F part
  writeComment("#Fishing Mortality info")
  wl("F_ballpark",comment="F ballpark for annual F (=Z-M) for specified year")
  wl("F_ballpark_year",comment="F ballpark year (neg value to disable)")
  wl("F_Method",comment="F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)")
  wl("maxF",comment="max F or harvest rate, depends on F_Method")
  if(ctllist$F_Method==1){
  #  Fmethod:1 does not need any additional information
  #  stop("stop currently F_method:1 is not implemented")
  }else if(ctllist$F_Method==2){
  #  stop("stop currently F_method:2 is not implemented")
    writeComment("overall start F value; overall phase; N detailed inputs to read")
    wl.vec("F_setup")
    writeComment("fleet, yr, seas, Fvalue, se, phase")
    printdf("F_setup2")
  }else if(ctllist$F_Method==3){
    wl("F_iter",comment="N iterations for tuning F in hybrid method (recommend 3 to 7)")
  }
  writeComment(c("#","#_initial_F_parms"))
 # writeComment("#_LO HI INIT PRIOR PR_type SD PHASE")
  printdf("init_F")
  writeComment("#_Q_setup")
  writeComment("# Q_type options:  <0=mirror, 0=float_nobiasadj, 1=float_biasadj, 2=parm_nobiasadj, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm")
  writeComment("#_for_env-var:_enter_index_of_the_env-var_to_be_linked")
#  writeComment("#_Den-dep  env-var  extra_se  Q_type")
  printdf("Q_setup")
##
## First of all to check if random Q parameters are used.
## If yes, read 1 number for flag to see if to read single parameter for each random Q or
## one parameter for each data point
  if(sum(ctllist$Q_setup[,4] %in% c(3,4))>0){
    wl("Do_Q_detail",comment=
        "If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index")
  }else{
    writeComment("#")
    writeComment("#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index")
  }
  writeComment("#_Q_parms(if_any);Qunits_are_ln(q)")
  header<-TRUE
  # Density dependant Q(Q-power)
  if(sum(ctllist$Q_setup[,1])>0){
    if(any(ctllist$Q_setup[(ctllist$Q_setup[,1]>0),4]<2)){
      cat("must create base Q parm to use Q_power for fleet: ",which(ctllist$Q_setup[(ctllist$Q_setup[,1]>0),4]<2))
      stop()
    }
    printdf("Q_power",header=header);header<-FALSE
  }
# Q-env
  if(sum(ctllist$Q_setup[,2])>0){
    if(any(ctllist$Q_setup[(ctllist$Q_setup[,2]>0),4]<2)){
      cat("must create base Q parm to use Q_env for fleet: ",which(ctllist$Q_setup[(ctllist$Q_setup[,2]>0),4]<2))
      stop()
    }
    printdf("Q_env",header=header);header<-FALSE
  }
# Q_extraSD
  if(sum(ctllist$Q_setup[,3])>0){
    printdf("Q_extraSD",header=header);header<-FALSE
  }
# Q-type
  if(!is.null(ctllist$Q_parms))printdf("Q_parms",header=header);header<-FALSE

  writeComment("#_size_selex_types")
  writeComment("#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead")
#  writeComment("#_Pattern Discard Male Special")
  printdf("size_selex_types")
  writeComment("#")
  writeComment("#_age_selex_types")
#  writeComment("#_Pattern ___ Male Special")
  printdf("age_selex_types")
  writeComment("#")
  #selex parameters
  writeComment("SizeSelex")
  if(!is.null(ctllist$size_selex_parms)){
  #  writeComment("#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn")
    printdf("size_selex_parms")
  }else{
    writeComment("#_No size_selex_parm")
  }
  writeComment("AgeSelex")
  if(!is.null(ctllist$age_selex_parms)){
  #  writeComment("#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn")
    printdf("age_selex_parms")
  }else{
    writeComment("#_No age_selex_parm")
  }
  #########################
  ## Following parts are not yet implemented in this code
  writeComment("#_Cond 0 #_custom_sel-env_setup (0/1)")
  writeComment("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns")
  if(sum(ctllist$age_selex_parms[,13])+sum(ctllist$size_selex_parms[,13])>0){
    wl("DoCustom_sel_blk_setup",comment="custom_sel-blk_setup (0/1) ")
    if(ctllist$DoCustom_sel_blk_setup){
      printdf("custom_sel_blk_setup")
    }
  }else{
    writeComment("#_Cond 0 #_custom_sel-blk_setup (0/1) ")
    writeComment("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage")
  }
  writeComment("#_Cond No selex parm trends ")

  if(!is.null(ctllist$selparm_Dev_Phase) && ctllist$selparm_Dev_Phase){
    wl("selparm_Dev_Phase",comment="#selparm_dev_PH")
  }else{
    writeComment("#_Cond -4 # placeholder for selparm_Dev_Phase")
  }
  if(ctllist$DoAdjust){
    wl("selex_adjust_method",comment="env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)")
  }else{
    writeComment("#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)")
  }

  writeComment("#")
  #################
  ## Tag model parameters
  ## Later they will be copyed from frq2dat
  writeComment("# Tag loss and Tag reporting parameters go next")
  wl("TG_custom",comment="TG_custom:  0=no read; 1=read if tags exist")
  if(ctllist$TG_custom==0){
    writeComment(c("#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters","#"))
  }else{
  #########################################
    printdf("TG_Loss_init")
    printdf("TG_Loss_chronic")
    printdf("TG_overdispersion")
    printdf("TG_Report_fleet")
    printdf("TG_Report_fleet_decay")
  ###########################
  }
  if(is.null(ctllist$DoVar_adjust))ctllist$Var_adjust<- 0
  wl("DoVar_adjust",comment="Variance_adjustments_to_input_values")


  if(ctllist$DoVar_adjust>0){
    printdf("Variance_adjustments")
  }
  wl("maxlambdaphase")
  wl("sd_offset")
  wl("N_lambdas",comment="number of changes to make to default Lambdas (default value is 1.0)")
  writeComment("# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch;")
  writeComment("# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark")
#  writeComment("#like_comp fleet/survey  phase  value  sizefreq_method")
  if(ctllist$N_lambdas>0){
  #  lambda_pettern_vec<-ctlist$lambda[,1]
  #  if(sum(lambda_pettern_vec %in% c(15,16))==0){
  #
  #  }
    printdf(ctllist$lambdas)
  #############
  ## Summary outputs of lambdas as comments is planed to follow in the future version
  }

  if(is.null(ctllist$more_stddev_reporting))ctllist$more_stddev_reporting<-0
  wl("more_stddev_reporting")
  if(ctllist$more_stddev_reporting != 0){
    wl.vector("stddev_reporting_specs",comment="# selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages")
    ## Selex bin
    if(ctllist$stddev_reporting_specs[4]>0){
      wl.vector("stddev_reporting_selex",comment="# selex bins to be reported (-1 in first bin to self-generate)")
    }
    ## Growth bin
    if(ctllist$stddev_reporting_specs[6]>0){
      wl.vector("stddev_reporting_growth",comment="# growth bins to be reported (-1 in first bin to self-generate)")
    }
    ## N at age
    if(ctllist$stddev_reporting_specs[9]>0){
      wl.vector("stddev_reporting_N_at_A",comment="# N@A to be reported (-1 in first bin to self-generate)")
    }
  }else{
    writeComment("# 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages")
    writeComment("# placeholder for vector of selex bins to be reported")
    writeComment("# placeholder for vector of growth ages to be reported")
    writeComment("# placeholder for vector of NatAges ages to be reported")
  }
  writeComment("#")
  writeLines("999",con=zz)
  options(width=oldwidth,max.print=oldmax.print)
  #sink()
  #close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
