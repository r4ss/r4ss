#' read control file from SS version 3.24
#'
#' Read Stock Synthesis (version 3.24) control file into list object in R.
#' This function comes with its wrapper function SS_readctl
#' that calls SS_readctl_3.24 (this function) or SS_readctl_3.30 (to be available in future).
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param nseas number of seasons in the model. This information is not
#'  explicitly available in control file
#' @param N_areas number of spatial areas in the model. Default = 1. This information is also not
#'  explicitly available in control file
#' @param Nages oldest age in the model. This information is also not
#'  explicitly available in control file
#' @param Ngenders number of genders in the model. This information is also not
#'  explicitly available in control file
#' @param Npopbins number of population bins in the model. This information is also not
#'  explicitly available in control file and this information is only required if length based
#'  maturity vector is directly supplied (Maturity option of 6), and not yet tested
#' @param Nfleet number of fisheries in the model. This information is also not
#'  explicitly available in control file
#' @param Nsurveys number of survey fleets in the model. This information is also not
#'  explicitly available in control file
#' @param Do_AgeKey Flag to indicate if 7 additional ageing error parameters to be read
#'  set 1 (but in fact any non zero numeric in R) or TRUE to enable to read them 0 or FALSE (default)
#'  to disable them. This information is not explicitly available in control file, too.
#' @param N_tag_groups number of tag release group. Default =NA. This information is not explicitly available
#'  control file. This information is only required if custom tag parameters is enabled (TG_custom=1)
#' @param N_CPUE_obs numeric vector of length=Nfleet+Nsurveys containing number of data points of each CPUE time series
#' @param use_datlist LOGICAL if TRUE, use datlist to derive parameters which can not be
#'        determined from control file
#' @param datlist list or character. if list : produced from SS_writedat or character : file name of dat file.
#' @author Yukio Takeuchi
#' @export
#' @seealso \code{\link{SS_readctl}}, \code{\link{SS_readdat}}
#' \code{\link{SS_readdat_3.24}},\code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}
SS_readctl_3.24 <- function(file,verbose=TRUE,echoall=FALSE,
## Parameters that are not defined in control file
    nseas=4,
    N_areas=1,
    Nages=20,
    Ngenders=1,
    Npopbins=NA,
    Nfleet=2,
    Nsurveys=2,
    Do_AgeKey=FALSE,
    N_tag_groups=NA,
    N_CPUE_obs=c(0,0,9,12), # This information is needed if Q_type of 3 or 4 is used
##################################
    use_datlist=FALSE,
    datlist=NULL
    ){
  # function to read Stock Synthesis data files

  if(verbose) cat("running SS_readctl_3.24\n")
  dat <- readLines(file,warn=FALSE)

  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2]," ")[[1]][1]
  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in 1:length(dat)){
    # split along blank spaces
    mysplit <- strsplit(dat[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    # if final value is a number is followed immediately by a pound ("1#"),
    # this needs to be split
    nvals <- length(mysplit)
    if(nvals>0) mysplit[nvals] <- strsplit(mysplit[nvals],"#")[[1]][1]
    # convert to numeric
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
    else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  # Function to add vector to ctllist

  add_vec<-function(ctllist,length,name,verbose=TRUE,comments=NULL){
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
    ctllist$temp<-dat[i+1:length-1]
    ctllist$'.i'<-i+length
    if(is.null(comments)){
      names(ctllist$temp)<-paste0(paste0("#_",name,"_",collapse=""),1:length)
    }else{
      names(ctllist$temp)<-comments
    }
    if(!is.na(name))names(ctllist)[names(ctllist)=="temp"]<-name
    if(verbose){cat(name,",i=",ctllist$'.i',"\n");print(ctllist[name])}
    return(ctllist)
  }
  # Function to add data as data.frame to ctllist
  add_df<-function(ctllist,nrow,ncol,col.names,name,verbose=TRUE,comments=NULL){
    k<-nrow*ncol
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
    df0<-as.data.frame(matrix(dat[i+1:k-1],nrow=nrow,ncol=ncol,byrow=TRUE))
    colnames(df0)<-col.names
    if(is.null(comments)){
      rownames(df0)<-paste0(paste0("#_",name,collapse=""),1:nrow)
    }else{
      rownames(df0)<-comments
    }
    i <- i+k
    ctllist$temp<-df0
    ctllist$'.i'<-i
    if(!is.na(name))names(ctllist)[names(ctllist)=="temp"]<-name
    if(verbose){
      cat(name,",i=",ctllist$'.i',"\n")
      print(ctllist[[which(names(ctllist)==name)]])
    }
    return(ctllist)
  }
  ## function to add an element to ctllist
  add_elem<-function(ctllist=NA,name,verbose=TRUE){
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
    ctllist$temp<-dat[i]
    ctllist$'.i'<-i+1
    if(!is.na(name))names(ctllist)[names(ctllist)=="temp"]<-name
    if(verbose)cat(name,",i=",ctllist$'.i'," ;",ctllist[[which(names(ctllist)==name)]],"\n")
    return(ctllist)
  }

  ## function to add list  to ctllist
  add_list<-function(ctllist=NA,name,length,length_each,verbose=TRUE){
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
     ctllist$temp<-list()
    for(j in 1:length){
      ctllist$temp[[j]]<-dat[i+1:length_each[j]-1]; i <- i+length_each[j]
    }
    ctllist$'.i'<-i
    if(!is.null(name))names(ctllist)[names(ctllist)=="temp"]<-name
    if(verbose)cat(name,",i=",ctllist$'.i',"\n")
    return(ctllist)
  }

  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  ctllist <- list()
  ctllist$'.i'<-i
  ctllist$'.dat'<-allnums
  if(!use_datlist){
    ctllist$nseas<-nseas
    ctllist$N_areas<-N_areas
    ctllist$Nages<-Nages
    ctllist$Ngenders<-Ngenders
    ctllist$Npopbins<-Npopbins
    ctllist$Nfleet<-Nfleet
    ctllist$Nsurveys<-Nsurveys
    ctllist$Do_AgeKey<-Do_AgeKey
    ctllist$N_tag_groups<-N_tag_groups
    ctllist$N_CPUE_obs<-N_CPUE_obs
    ctllist$fleetnames<-fleetnames<-c(paste0("FL",1:Nfleet),paste0("S",1:Nsurveys))
  }else{
    if(is.character(datlist))datlist<-SS_readdat(file=datlist)
    if(is.null(datlist))stop("datlist from SS_readdat is needed is use_datlist is TRUE")
    ctllist$nseas<-nseas<-datlist$nseas
    ctllist$N_areas<-N_areas<-datlist$N_areas
    ctllist$Nages<-Nages<-datlist$Nages
    ctllist$Ngenders<-Ngenders<-datlist$Ngenders
    ctllist$Npopbins<-Npopbins<-datlist$Npopbins
    ctllist$Nfleet<-Nfleet<-datlist$Nfleet
    ctllist$Nsurveys<-Nsurveys<-datlist$Nsurveys
    if(datlist$N_ageerror_definition>0){
      ctllist$Do_AgeKey<-Do_AgeKey<-ifelse(any(datlist$ageerror[1:(nrow(datlist$ageerror)/2)*2,1]<0),1,0)
    }
    ctllist$N_tag_groups<-N_tag_groups<-datlist$N_tag_groups
    N_CPUE_obs<-vector(mode="numeric",length=Nfleet+Nfleet)
    N_CPUE_obs<-sapply(1:(Nfleet+Nfleet),function(i){sum(datlist$CPUE[,"index"]==i)})
    ctllist$N_CPUE_obs<-N_CPUE_obs
    fleetnames<-datlist$fleetnames
  }
  # specifications
  ctllist$sourcefile <- file
  ctllist$type <- "Stock_Synthesis_control_file"
  ctllist$SSversion <- "SSv3.24" # NULL # "SSv3.21"

  if(verbose) cat("SSversion =",ctllist$SSversion,"\n")

  # model dimensions
  ctllist<-add_elem(ctllist,"N_GP")
  # Currently I do not how MGparms are sorted when N_GP>1
  if(ctllist$N_GP>1)stop("this function not yet written for models with multiple growth patterns")
  ctllist<-add_elem(ctllist,"N_platoon")
  if(ctllist$N_platoon>1){
    stop("currently sub morphs are not supported yet")
#    ctllist<-add_elem(ctllist,"N_platoon")
    ctllist<-add_elem(ctllist,"submorphdist")
  }else{
    ctllist$sd_ratio<- 1.
    ctllist$submorphdist<-1.
  }
  if(ctllist$submorphdist[1]<0.){
    if(ctllist$N_platoon==1){
      ctllist$submorphdist<- 1.;
    }else if (ctllist$N_platoon==3){
      ctllist$submorphdist<-c(0.15,0.70,0.15)
    }else if (ctllist$N_platoon==5){
      ctllist$submorphdist<-c(0.031, 0.237, 0.464, 0.237, 0.031)
    }
  }
  ctllist$submorphdist<-ctllist$submorphdist/sum(ctllist$submorphdist)
  if(ctllist$N_GP*ctllist$nseas*ctllist$N_areas>1) {
    ctllist<-add_elem(ctllist,"recr_dist_read")
    recr_dist_read<-ctllist$recr_dist_read
    #  number of recruitment assignments (overrides GP*area*seas parameter values)
    ctllist<-add_elem(ctllist,"recr_dist_inx")
    # recruitment interaction requested
    ctllist<-add_df(ctllist,"recr_dist_pattern",nrow=recr_dist_read,ncol=3,
      col.names=c("GP","seas","area"))
  }else{
    ctllist$recr_dist_inx<-FALSE
  }
  if(ctllist$N_areas>1){
    #stop("Multi areas are not yet implemented")
    ctllist<-add_elem(ctllist,"N_moveDef") #_N_movement_definitions goes here if N_areas > 1
    ctllist<-add_elem(ctllist,"firstAgeMove") #_first age that moves (real age at begin of season, not integer) also cond on do_migration>0
    ctllist<-add_df(ctllist,"moveDef",nrow=ctllist$N_moveDef,ncol=6,col.names=c("seas", "morph", "source", "dest", "age", "age2"))
  }

  ctllist<-add_elem(ctllist,"N_Block_Designs") #_Nblock_Patterns
  if(ctllist$N_Block_Designs>0){
    ctllist<-add_vec(ctllist,name="blocks_per_pattern",length=ctllist$N_Block_Designs)
    #_blocks_per_pattern
  # begin and end years of blocks
    ctllist<-add_list(ctllist,name="Block_Design",length=ctllist$N_Block_Designs,
      length_each=ctllist$blocks_per_pattern*2)
  }
  ctllist<-add_elem(ctllist,"fracfemale") #_fracfemale
  ctllist<-add_elem(ctllist,"natM_type") #_natM_type
  if(ctllist$natM_type==0){
    N_natMparms<-1
  }else if(ctllist$natM_type==1){
    ctllist<-add_elem(ctllist,name="N_natM") #_Number of M_segments
    ctllist<-add_vec(ctllist,name="M_ageBreakPoints",length=ctllist$N_natM) # age(real) at M breakpoints
    N_natMparms<-ctllist$N_natM
  }else if(ctllist$natM_type==2){
#    stop("natM_type =2 is not yey implemented in this script")
    N_natMparms<-1 ## 2016-12-8
    comments<-if(ctllist$N_GP==1){
      "#_reference age for Lorenzen M; read 1P per morph"
    }else{
      paste0("#_reference age for Lorenzen M_GP",1:ctllist$N_GP)
    }
    ctllist<-add_vec(ctllist,name="Lorenzen_refage",length=ctllist$N_GP,
      comments=comments) ## Reference age for Lorenzen M
  }else if(ctllist$natM_type %in% c(3,4)){
    N_natMparms<-0
    ctllist<-add_df(ctllist,name="natM",nrow=ctllist$N_GP*ctllist$Ngenders,ncol=Nages+1,col.names=paste0("Age_",0:Nages))
  }else{
    stop("natM_type =", ctllist$natM_type," is not yet implemented in this script")
  }
  cat("N_natMparms=",N_natMparms,"\n")
  ctllist<-add_elem(ctllist,name="GrowthModel")
    # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
  ctllist<-add_elem(ctllist,name="Growth_Age_for_L1") #_Growth_Age_for_L1
  ctllist<-add_elem(ctllist,name="Growth_Age_for_L2") #_Growth_Age_for_L2 (999 to use as Linf)
  if(ctllist$GrowthModel==3){
    ctllist<-add_elem(ctllist,name="N_ageK")
  }
  if(ctllist$GrowthModel==1)  # 1=vonBert with L1&L2
  {
    N_growparms<-5
  #  AFIX=tempvec5(1);
  #  AFIX2=tempvec5(2);
  }
  else if(ctllist$GrowthModel==2) # 2=Richards with L1&L2
  {
    N_growparms<-6
  #  AFIX=tempvec5(1);
  #  AFIX2=tempvec5(2);
  }else if(ctllist$GrowthModel==3){ # 3=age_specific_K
  #  AFIX=tempvec5(1);
  #  AFIX2=tempvec5(2);
    Age_K_count<-ctllist$N_ageK
    N_growparms=5+Age_K_count
    ctllist<-add_vec(ctllist,name="Age_K_points",length=Age_K_count)
    #  points at which age-specific multipliers to K will be applied

  }else if(ctllist$GrowthModel==4){
  ##  I found some portion of source code for GrowthModel=4
  ##  But for now I disabled it
    stop("GrowthModel==4 is not implemented")
    N_growparms<-2  # for the two CV parameters
    k1<-ctllist$N_GP*ctllist$Ngenders  # for reading age_natmort
    ctllist<-add_df(ctllist,name="Len_At_Age_rd",nrow=k1,ncol=Nages+1,col.names=paste0("Age_",0:Nages))
  #!!if(k1>0) echoinput<<"  Len_At_Age_rd"<<Len_At_Age_rd<<endl; Need check
  }else{
    cat("GrowthModel;",ctllist$GrowthModel," ")
    stop("is not supported yet")
  }
  MGparm_per_def<-N_natMparms+N_growparms

  ctllist<-add_elem(ctllist,name="SD_add_to_LAA") #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
  ctllist<-add_elem(ctllist,name="CV_Growth_Pattern")
    #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
  ctllist<-add_elem(ctllist,name="maturity_option")
    #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity by GP; 4=read age-fecundity by GP; 5=read fec and wt from wtatage.ss; 6=read length-maturity by GP
  if(ctllist$maturity_option %in% c(3,4)){
    ctllist<-add_df(ctllist,name="Age_Maturity",nrow=ctllist$N_GP,ncol=Nages+1,col.names=paste0("Age_",0:Nages))
  }else if(ctllist$maturity_option==6){
    ctllist<-add_df(ctllist,dat=allnums,"Length_Maturity",nrow=ctllist$N_GP,ncol=Npopbins)
  }
  ctllist<-add_elem(ctllist,"First_Mature_Age") #_First_Mature_Age
  ctllist<-add_elem(ctllist,"fecundity_option")  #_fecundity_option
  ctllist<-add_elem(ctllist,"hermaphroditism_option")   #_hermaphroditism_option
  ctllist<-add_elem(ctllist,"parameter_offset_approach")    #_parameter_offset_approach
  ctllist<-add_elem(ctllist,"env_block_dev_adjust_method")   #_env/block/dev_adjust_method

  N_MGparm<-MGparm_per_def*ctllist$N_GP*ctllist$Ngenders  ## Parmeters for M and Growth multiplied by N_GP and Ngenders
  MGparmLabel<-list()
  cnt<-1
  GenderLabel<-c("Fem","Mal")
  for(i in 1:ctllist$Ngenders){
    for(j in 1:ctllist$N_GP){
      if(N_natMparms>0){
        MGparmLabel[1:N_natMparms+cnt-1]<-paste0("NatM_p_",1:N_natMparms,"_",GenderLabel[i],"_GP_",j)
        cnt<-cnt+N_natMparms
      }
      if(ctllist$GrowthModel==1){ # VB
        tmp<-c("L_at_Amin_","L_at_Amax_","VonBert_K_","CV_young_","CV_old_")
        MGparmLabel[1:5+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        cnt<-cnt+5
      }else if(ctllist$GrowthModel==2){ # Richards
        tmp<-c("L_at_Amin_","L_at_Amax_","VonBert_K_","Richards_","CV_young_","CV_old_")
        MGparmLabel[1:6+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        cnt<-cnt+6
      }else if(ctllist$GrowthModel==3){
        tmp<-c("L_at_Amin_","L_at_Amax_","VonBert_K_",paste0("Age_K_",1:Age_K_count),"CV_young_","CV_old_")
        MGparmLabel[1:(5+Age_K_count)+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        cnt<-cnt+5+Age_K_count
      }
    }
  }
  N_MGparm<-N_MGparm+2*ctllist$Ngenders+2+2 #add for wt-len(by gender), mat-len parms; eggs
  MGparmLabel[cnt]<-paste0("Wtlen_1_",GenderLabel[1]);cnt<-cnt+1
  MGparmLabel[cnt]<-paste0("Wtlen_2_",GenderLabel[1]);cnt<-cnt+1
  MGparmLabel[cnt]<-paste0("Mat50%_",GenderLabel[1]);cnt<-cnt+1
  MGparmLabel[cnt]<-paste0("Mat_slope_",GenderLabel[1]);cnt<-cnt+1
  if(ctllist$maturity_option==1){
    MGparmLabel[cnt]<-paste0("Eggs/kg_inter_",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Eggs/kg_slope_wt_",GenderLabel[1]);cnt<-cnt+1
  }else if(ctllist$maturity_option==2){
    MGparmLabel[cnt]<-paste0("Eggs_scalar_",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Eggs_exp_len_",GenderLabel[1]);cnt<-cnt+1
  }else  if(ctllist$maturity_option==3){
    MGparmLabel[cnt]<-paste0("Eggs_scalar_",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Eggs_exp_wt_",GenderLabel[1]);cnt<-cnt+1
  }else  if(ctllist$maturity_option==4){
    MGparmLabel[cnt]<-paste0("Eggs_intercept_",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Eggs_slope_len_",GenderLabel[1]);cnt<-cnt+1
  }else   if(ctllist$maturity_option==5){
    MGparmLabel[cnt]<-paste0("Eggs_intercept_",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Eggs_slope_wt_",GenderLabel[1]);cnt<-cnt+1
  }else{
    cat("Maturity option : ",ctllist$maturity_option," ")
    stop("is not supported")
  }
  if(ctllist$Ngenders==2){
    MGparmLabel[cnt]<-paste0("Wtlen_1_",GenderLabel[2]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Wtlen_2_",GenderLabel[2]);cnt<-cnt+1
  }
  if(ctllist$hermaphroditism_option){
    MGparmLabel[cnt]<-paste0("Herm_Infl_age",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Herm_stdev",GenderLabel[1]);cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Herm_asymptote",GenderLabel[1]);cnt<-cnt+1
    N_MGparm<-N_MGparm+3
  }
  N_MGparm<-N_MGparm+ctllist$N_GP+N_areas+nseas         # add for the assignment to areas
  MGparmLabel[cnt+1:ctllist$N_GP-1]<-paste0("RecrDist_GP_",1:ctllist$N_GP);cnt<-cnt+ctllist$N_GP
  MGparmLabel[cnt+1:N_areas-1]<-paste0("RecrDist_Area_",1:N_areas);cnt<-cnt+N_areas
  MGparmLabel[cnt+1:nseas-1]<-paste0("RecrDist_Seas_",1:nseas);cnt<-cnt+nseas
  ## number of recruiment distribution parameters
  N_RecrDist_parms<-ctllist$N_GP+ctllist$N_areas+ctllist$nseas
  if(ctllist$recr_dist_inx){
  ## Interactions
    N_MGparm<-N_MGparm+ctllist$N_GP*N_areas*nseas
    N_RecrDist_parms<-N_RecrDist_parms+ctllist$N_GP*N_areas*nseas
    for(i in 1:ctllist$N_GP){
      for(j in 1:nseas){
        for(k in 1:N_areas){
          MGparmLabel[cnt]<-paste0("RecrDist_interaction_GP_",i,"_seas_",j,"_area_",k);cnt<-cnt+1
        }
      }
    }
    # add for the morph assignments within each area
  }
#  MGparms
  N_MGparm<-N_MGparm+1 # add 1 parameter for cohort-specific growth parameter
  MGparmLabel[cnt]<-"CohortGrowDev";cnt<-cnt+1
  if(N_areas>1){
    N_MGparm<-N_MGparm+ctllist$N_moveDef*2 # add 2 * N_moveDef for movement params
#    M_Move_parms<-ctllist$N_moveDef*2
    for(i in 1:ctllist$N_moveDef){
      seas<-ctllist$moveDef[i,1]
      GP<-ctllist$moveDef[i,2]
      from<-ctllist$moveDef[i,3]
      to<-ctllist$moveDef[i,4]
      MGparmLabel[cnt+0:1]<-paste0("MoveParm_",c("A","B"),"_seas_",seas,"_GP_",GP,"_from_",from,"_to_",to);cnt<-cnt+2
    }
  }

  if(Do_AgeKey){
    MGparmLabel[cnt+0:6]<-paste0("AgeKeyParm",1:7);cnt<-cnt+7
    N_MGparm<-N_MGparm+7
  }

  ctllist<-add_df(ctllist,name="MG_parms",nrow=N_MGparm,ncol=14,
                    col.names=c("LO", "HI", "INIT", "PRIOR",
                                "PR_type", "SD", "PHASE",
                                "env_var","use_dev", "dev_minyr",
                                "dev_maxyr", "dev_stddev", "Block", "Block_Fxn"),
                                comments=MGparmLabel)

  ctllist<-add_vec(ctllist,name="MGparm_seas_effects",length=10)

  N_seas_effects<-sum(ctllist$MGparm_seas_effects)
  if(N_seas_effects>0){
    ctllist<-add_df(ctllist,"MG_parms_seas",nrow=N_seas_effects,ncol=7,
                    col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"))
  }
#  DoParmDev<-sum(ctllist$M_parms[,9])+
#             sum(ctllist$G_parms[,9])+sum(ctllist$cohortG_parm[,9])+
#             sum(ctllist$RecrDist_parms[,9])+sum(ctllist$Move_parms[,9])
  DoParmDev<-sum(ctllist$MG_parms[,9])
  if(DoParmDev>0){
    ctllist<-add_elem(ctllist,"MGparm_Dev_Phase") #_MGparm_Dev_Phase
  }

 # SRR
  ctllist<-add_elem(ctllist,"SR_function")   #_SR_function
  N_SRparm<-c(0,2,2,2,3,2,3,3,0,0)
  N_SRparm2<-N_SRparm[as.numeric(ctllist$SR_function)]+4
  SRparmsLabels<-if(ctllist$SR_function ==3){
    # B-H SRR
    c("SR_LN(R0)","SR_BH_steep","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")
  }else if(ctllist$SR_function==2){
    # Ricker SRR
    c("SR_LN(R0)","SR_Ricker","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")  ## Need to rivise with example inputs
  }else if(ctllist$SR_function==4){
    # SCAA
    c("SR_LN(R0)","SR_SCAA_null","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")
  }else if(ctllist$SR_function==5){
    # Hockey stick
    c("SR_LN(R0)","SR_hockey_infl","SR_hockey_min_R","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")
  }else if(ctllist$SR_function ==6){
    # B-H-flat SRR
    c("SR_LN(R0)","SR_BH_flat_steep","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")
  }else if(ctllist$SR_function==7){
    # survival_3Parm
    c("SR_LN(R0)","SR_surv_Sfrac","SR_surv_Beta","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")
  }else if(ctllist$SR_function==8){
    # Shepard_3Parm
    c("SR_LN(R0)","SR_steepness","SR_Shepard_c","SR_sigmaR","SR_envlink","SR_R1_offset","SR_autocorr")
  }else{
    cat("SR_function=",ctllist$SR_function," is not supported yet.");stop()
  }
  ctllist<-add_df(ctllist,name="SRparm",nrow=N_SRparm2,ncol=7,
            col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"),comments=SRparmsLabels)
  ctllist<-add_elem(ctllist,"SR_env_link") #_SR_env_link
  ctllist<-add_elem(ctllist,"SR_env_target") #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
  ctllist<-add_elem(ctllist,"do_recdev") #do_recdev:  0=none; 1=devvector; 2=simple deviations
  ctllist<-add_elem(ctllist,"MainRdevYrFirst") # first year of main recr_devs; early devs can preceed this era
  ctllist<-add_elem(ctllist,"MainRdevYrLast") # last year of main recr_devs; forecast devs start in following year
  ctllist<-add_elem(ctllist,"recdev_phase") #_recdev phase
  ctllist<-add_elem(ctllist,"recdev_adv") # (0/1) to read 13 advanced options

  if(ctllist$recdev_adv){
    ctllist<-add_elem(ctllist,"recdev_early_start") #_recdev_early_start (0=none; neg value makes relative to recdev_start)
    ctllist<-add_elem(ctllist,"recdev_early_phase") #_recdev_early_phase
    ctllist<-add_elem(ctllist,"Fcast_recr_phase") #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
    ctllist<-add_elem(ctllist,"lambda4Fcast_recr_like") #_lambda for Fcast_recr_like occurring before endyr+1
    ctllist<-add_elem(ctllist,"last_early_yr_nobias_adj") #_last_early_yr_nobias_adj_in_MPD
    ctllist<-add_elem(ctllist,"first_yr_fullbias_adj") #_first_yr_fullbias_adj_in_MPD
    ctllist<-add_elem(ctllist,"last_yr_fullbias_adj") #_last_yr_fullbias_adj_in_MPD
    ctllist<-add_elem(ctllist,"first_recent_yr_nobias_adj") #_first_recent_yr_nobias_adj_in_MPD
    ctllist<-add_elem(ctllist,"max_bias_adj") #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
    ctllist<-add_elem(ctllist,"period_of_cycles_in_recr") #_period of cycles in recruitment (N parms read below)
    ctllist<-add_elem(ctllist,"min_rec_dev") #min rec_dev
    ctllist<-add_elem(ctllist,"max_rec_dev") #max rec_dev
    ctllist<-add_elem(ctllist,"N_Read_recdevs") #_read_recdevs
    #_end of advanced SR options
#
    if(ctllist$period_of_cycles_in_recr>0){
      stop("Reading full parameters for recr cycles is not yet coded")
    }
    if(ctllist$N_Read_recdevs>0){
  #    stop("Reading specific recdev is not coded in this R code")
      ctllist<-add_df(ctllist,"recdev_input",ncol=2,nrow=ctllist$N_Read_recdevs,col.names=c("Year","recdev"))
    }
  }
  # F Part
  ctllist<-add_elem(ctllist,"F_ballpark") # F ballpark for annual F (=Z-M) for specified year
  ctllist<-add_elem(ctllist,"F_ballpark_year") # F ballpark year (neg value to disable)
  ctllist<-add_elem(ctllist,"F_Method") # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
  ctllist<-add_elem(ctllist,"maxF") # max F or harvest rate, depends on F_Method
  if(ctllist$F_Method==1){
 #   stop("stop currently F_method:1 is not implemented")
  }else if(ctllist$F_Method==2){
 #   stop("stop currently F_method:2 is not implemented")
    ctllist<-add_vec(ctllist,"F_setup",length=3) # overall start F value; overall phase; N detailed inputs to read
    ctllist<-add_df(ctllist,name="F_setup2",nrow=ctllist$F_setup[3],ncol=6,
                    col.names=c("fleet", "yr", "seas", "Fvalue", "se", "phase"))
  }else if(ctllist$F_Method==3){
    ctllist<-add_elem(ctllist,"F_iter") # N iterations for tuning F in hybrid method (recommend 3 to 7)
  }
  #
  #_initial_F_parms
  comments_initF<-paste0("InitF_",1:Nfleet,"_",fleetnames[1:Nfleet])

  #_LO HI INIT PRIOR PR_type SD PHASE
  ctllist<-add_df(ctllist,name="init_F",nrow=Nfleet,ncol=7,
    col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"),comments=comments_initF)
  #
  comments_selex_types<-paste0(1:(Nfleet+Nsurveys)," ",fleetnames)
  #_Q_setup
  # Q_type options:  <0=mirror, 0=float_nobiasadj, 1=float_biasadj, 2=parm_nobiasadj, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm
  ## currently only float_nobiasadj (Q_type==0) is suppoerted
#_for_env-var:_enter_index_of_the_env-var_to_be_linked
#_Den-dep  env-var  extra_se  Q_type
  ctllist<-add_df(ctllist,name="Q_setup",nrow=Nfleet+Nsurveys,ncol=4,
              col.names=c("Den_dep","env_var","extra_se","Q_type"),comments=comments_selex_types)
## Then check if random Q parameters are used.
## If yes, read 1 number for flag to see if to read single parameter for each random Q or
## one parameter for each data point
  Do_Q_detail<-FALSE
  if(sum(ctllist$Q_setup[,4] %in% c(3,4))>0){
    ctllist<-add_elem(ctllist,name="Do_Q_detail")  ##
    Do_Q_detail<-ctllist$Do_Q_detail
  }

# Density dependant Q(Q-power)
  if(sum(ctllist$Q_setup[,1])>0){
    if(any(ctllist$Q_setup[(ctllist$Q_setup[,1]>0),4]<2)){
      cat("must create base Q parm to use Q_power for fleet: ",which(ctllist$Q_setup[(ctllist$Q_setup[,1]>0),4]<2))
      stop()
    }
    N_Q_power<-sum(ctllist$Q_setup[,1]>0)
    ctllist<-add_df(ctllist,name="Q_power",nrow=N_Q_power,ncol=7,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"))
  }
# Q-env
  if(sum(ctllist$Q_setup[,2])>0){
    if(any(ctllist$Q_setup[(ctllist$Q_setup[,2]>0),4]<2)){
      cat("must create base Q parm to use Q_power for fleet: ",which(ctllist$Q_setup[(ctllist$Q_setup[,2]>0),4]<2))
      stop()
    }
    N_Q_env<-sum(ctllist$Q_setup[,2]>0)
    ctllist<-add_df(ctllist,name="Q_env",nrow=N_Q_env,ncol=7,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"))
  }
# extra-se
  if(sum(ctllist$Q_setup[,3])>0){
    tmp<-which(ctllist$Q_setup[,3]>0)
    comments_Q_extraSD<-paste0("Q_extraSD_",tmp,"_",fleetnames[tmp])
    ctllist<-add_df(ctllist,name="Q_extraSD",nrow=sum(ctllist$Q_setup[,3]),ncol=7,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"),
              ,comments=comments_Q_extraSD)
  }
# Q-type
  N_Q_parms<-0
  comments_Q_type<-list()
  i<-1
  cat("Setting up Q_parameters comments\n")
  for(fl in 1:(Nfleet+Nsurveys)){
    flname<-fleetnames[fl]
    .Q_type<-ctllist$Q_setup[fl,4]
    cat(flname,";",fl,"\n")
    if(.Q_type==2){ # One Q parameter
      N_Q_parms<-N_Q_parms+1
      cat("i=",i,"\n")
      comments_Q_type[[i]]<-paste0("LnQ_base_",fl,"_",flname,collapse="");i<-i+1
    }else if(.Q_type==3){ # Random Q deviations
      N_Q_parms<-N_Q_parms+1
      cat("i=",i,"\n")
      comments_Q_type[[i]]<-paste0("LnQ_base_",fl,"_",flname,collapse="")
      if(Do_Q_detail){
        N_Q_parms<-N_Q_parms+N_CPUE_obs[fl]
        comments_Q_type[[i]][1:N_CPUE_obs[fl]+1]<-paste0("Q_dev_",1:N_CPUE_obs[fl],"_",fl,"_",flname)
      };i<-i+1
    }else if(.Q_type==4){# Random walk W
      N_Q_parms<-N_Q_parms+1
      comments_Q_type[[i]]<-paste0("LnQ_base_",fl,"_",flname,collapse="");i<-i+1
      if(Do_Q_detail){
        N_Q_parms<-N_Q_parms+N_CPUE_obs[fl]-1
        comments_Q_type[[i]][2:N_CPUE_obs[fl]]<-paste0("Q_walk_",1:(N_CPUE_obs[fl]-1),"_",fl,"_",flname)
      };i<-i+1
    }else if(.Q_type==5){
      N_Q_parms<-N_Q_parms+1
      comments_Q_type[[i]]<-paste0("LnQ_base_",fl,"_",flname,collapse="");i<-i+1
    }
  }
  if(N_Q_parms>0){
    ctllist<-add_df(ctllist,name="Q_parms",nrow=N_Q_parms,ncol=7,
                col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"),
                comments=unlist(comments_Q_type))
  }
# size_selex_types
  ctllist<-add_df(ctllist,name="size_selex_types",nrow=Nfleet+Nsurveys,ncol=4,
              col.names=c("Pattern","Discard","Male","Special"),comments=comments_selex_types)
  size_selex_pattern_vec<-as.vector(ctllist$size_selex_types[,1])
#
#_age_selex_types
#_Pattern ___ Male Special

  ctllist<-add_df(ctllist,name="age_selex_types",nrow=Nfleet+Nsurveys,ncol=4,
              col.names=c("Pattern","___","Male","Special"),comments=comments_selex_types)
  age_selex_pattern_vec<-as.vector(ctllist$age_selex_types[,1])


  #                 0 1 2 3 4 5 6 7 8 9 10
  selex_patterns<-c(0,2,8,6,0,2,2,8,8,6, 0,
  #                11 12 13 14      15 16 17      18 19 20
                    2, 2, 8,Nages+1, 0, 2,Nages+1, 8, 6, 6,
  #                21 22 23 24 25 26 27 28 29 30
                   NA, 4, 6, 6, 3, 3, 3,NA,NA, 0,
  #                31 32 33 34
                    0, 0, 0, 0)
#########################################################
  size_selex_Nparms<-vector(mode="numeric",length=Nfleet+Nsurveys)
  size_selex_label<-list()
  for(j in 1:(Nfleet+Nsurveys)){
       #    size_selex_pattern_vec
    size_selex_Nparms[j]<-selex_patterns[size_selex_pattern_vec[j]+1]
    ## spline needs special treatment
    if(size_selex_pattern_vec[j]==27){
      size_selex_Nparms[j]<-size_selex_Nparms[j]+ctllist$size_selex_types[j,4]*2
 #     size_selex_label[j]<-paste0("SizeSel_",j,"P_",1:size_selex_Nparms[j],"_",fleetnames[j])
      size_selex_label[[j]]<-c(paste0("SizeSpline_Code_",fleetnames[j],"_",j),
                             paste0("SizeSpline_GradLo_",fleetnames[j],"_",j),
                             paste0("SizeSpline_GradHi_",fleetnames[j],"_",j),
                             paste0("SizeSpline_Knot_",1:ctllist$size_selex_types[j,4],"_",fleetnames[j],"_",j),
                             paste0("SizeSpline_Val_",1:ctllist$size_selex_types[j,4],"_",fleetnames[j],"_",j))
    }else{
      size_selex_label[[j]]<-if(size_selex_Nparms[j]>0){
        paste0("SizeSel_",j,"P_",1:size_selex_Nparms[j],"_",fleetnames[j])
      }else{
        NULL
      }
    }
  }
  age_selex_Nparms<-vector(mode="numeric",length=Nfleet+Nsurveys)
  age_selex_label<-list()
  for(j in 1:(Nfleet+Nsurveys)){
    age_selex_Nparms[j]<-selex_patterns[age_selex_pattern_vec[j]+1]
    ## spline needs special treatment
    if(age_selex_pattern_vec[j]==27){
      age_selex_Nparms[j]<-age_selex_Nparms[j]+ctllist$age_selex_types[j,4]*2
    #  age_selec_label[j]<-paste0("AgeSel_",j,"P_",1:agee_selex_Nparms[j],"_",fleetnames[j])
      age_selex_label[[j]]<-c(paste0("AgeeSpline_Code_",fleetnames[j],"_",j),
                             paste0("AgeSpline_GradLo_",fleetnames[j],"_",j),
                             paste0("AgeeSpline_GradHi_",fleetnames[j],"_",j),
                             paste0("AgeeSpline_Knot_",1:ctllist$age_selex_types[j,4],"_",fleetnames[j],"_",j),
                             paste0("AgeeSpline_Val_",1:ctllist$age_selex_types[j,4],"_",fleetnames[j],"_",j))
    }else{
      age_selex_label[[j]]<-if(age_selex_Nparms[j]>0){
        paste0("AgeSel_",j,"P_",1:age_selex_Nparms[j],"_",fleetnames[j])
      }else{
        NULL
      }
    }
  }
  if(verbose){cat("size_selex_Nparms\n");print(size_selex_Nparms)}
  if(verbose){cat("age_selex_Nparms\n");print(age_selex_Nparms)}
# Selex parameters
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
# Size selex
  if(sum(size_selex_Nparms)>0){
    ctllist<-add_df(ctllist,name="size_selex_parms",nrow=sum(size_selex_Nparms),ncol=14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "env_var","use_dev", "dev_minyr", "dev_maxyr", "dev_stddev",
                           "Block", "Block_Fxn"),comments=unlist(size_selex_label))
  }
# Age selex
  if(sum(age_selex_Nparms)>0){
    ctllist<-add_df(ctllist,name="age_selex_parms",nrow=sum(age_selex_Nparms),ncol=14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "env_var","use_dev", "dev_minyr", "dev_maxyr", "dev_stddev",
                           "Block", "Block_Fxn"),comments=unlist(age_selex_label))
  }
##########################
## Following pars are not yet  implemented in this R code
#_Cond 0 #_custom_sel-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
  DoAdjust<-FALSE
  if(sum(ctllist$age_selex_parms[,13])+sum(ctllist$size_selex_parms[,13])>0){
    DoAdjust<-TRUE
    ctllist<-add_elem(ctllist,"DoCustom_sel_blk_setup") #_custom_sel-blk_setup (0/1)
    if(ctllist$DoCustom_sel_blk_setup){

        # FIND relevant blocks
        blks <- ctllist$blocks_per_pattern

        # SUBSET params for Block > 0
        pbks <- subset(ctllist$age_selex_parms, Block > 0)

        # MULTIPLY number of blocks by number of params per block
        rbks <- blks[blks > 1] * pbks[!duplicated(pbks$Block), "Block_Fxn"]

        # No. of rows
        k0 <- sum(rbks)

      ctllist<-add_df(ctllist,name="custom_sel_blk_setup",nrow=k0,ncol=7,
        col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE"))
    }
  }else{
    #_Cond 0 #_custom_sel-blk_setup (0/1)
    #_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
  }

#_Cond No selex parm trends
  if(sum(ctllist$age_selex_parms[,9])+sum(ctllist$size_selex_parms[,9])>0){
    DoAdjust<-TRUE
    ctllist<-add_elem(ctllist,name="selparm_Dev_Phase") #  selparm_Dev_Phase
  }else{
#_Cond -4 # placeholder for selparm_Dev_Phase
  }
  ctllist$DoAdjust<-DoAdjust
  if(DoAdjust){
    ctllist<-add_elem(ctllist,name="selex_adjust_method")
    #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
  }
#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
  ctllist<-add_elem(ctllist,name="TG_custom") # TG_custom:  0=no read; 1=read if tags exist
  if(ctllist$TG_custom){
    ##  Following parameters are to be read
    ##  Fro each SS's tag release group
    ##  . one initial Tag loss parameter
    ##  . one continuos Tag loss parameter
    ##  . NB over-dispersion paramater
    ##  For each fleet
    ##  . one tag reporting rate paramater
    ##  . one tag reporting rate decay paramater
    ##
    ##  In total N_tag_groups*3+ Nfleet*2 parameters are needed to read
    ##
    # Initial tag loss
    ctllist<-add_df(ctllist,name="TG_Loss_init",nrow=N_tag_groups,ncol=14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "Dum","Dum", "Dum", "Dum", "Dum", "Dum", "Dum"))
    # continuous tag loss
    ctllist<-add_df(ctllist,name="TG_Loss_chronic",nrow=N_tag_groups,ncol=14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "Dum","Dum", "Dum", "Dum", "Dum", "Dum", "Dum"),
                           comments=paste0("#_TG_Loss_chronic_",1:N_tag_groups))

    # NB over-dispersion
    ctllist<-add_df(ctllist,name="TG_overdispersion",nrow=N_tag_groups,ncol=14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "Dum","Dum", "Dum", "Dum", "Dum", "Dum", "Dum"),
              comments=paste0("#_TG_overdispersion_",1:N_tag_groups))

    # TG_Report_fleet
    ctllist<-add_df(ctllist,name="TG_Report_fleet",nrow=Nfleet,14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "Dum","Dum", "Dum", "Dum", "Dum", "Dum", "Dum"))

    # TG_Report_fleet_decay
    ctllist<-add_df(ctllist,name="TG_Report_fleet_decay",nrow=Nfleet,14,
              col.names=c("LO", "HI", "INIT", "PRIOR", "PR_type", "SD", "PHASE",
                           "Dum","Dum", "Dum", "Dum", "Dum", "Dum", "Dum"))
  }else{
    #_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
    #
  }
  ctllist<-add_elem(ctllist,"DoVar_adjust")  #_Variance_adjustments_to_input_values
  if(ctllist$DoVar_adjust>0){
    ctllist<-add_df(ctllist,name="Variance_adjustments",nrow=6,ncol=Nfleet+Nsurveys,
                      col.names=paste0("Fleet",1:(Nfleet+Nsurveys)))
    rownames(ctllist$Variance_adjustments)<-paste0("#_",paste(c("add_to_survey_CV",
                                                          "add_to_discard_stddev",
                                                          "add_to_bodywt_CV",
                                                          "mult_by_lencomp_N",
                                                          "mult_by_agecomp_N",
                                                          "mult_by_size-at-age_N")))
  }
  ctllist<-add_elem(ctllist,"maxlambdaphase") #_maxlambdaphase
  ctllist<-add_elem(ctllist,"sd_offset")  #_sd_offset

  ctllist<-add_elem(ctllist,"N_lambdas")   # number of changes to make to default Lambdas (default value is 1.0)
  if(ctllist$N_lambdas>0){
    ctllist<-add_df(ctllist,name="lambdas",nrow=ctllist$N_lambdas,ncol=5,
                      col.names=c("like_comp","fleet/survey","phase","value","sizefreq_method"))
    for(i in 1:ctllist$N_lambdas){
      like_comp<-ctllist$lambdas[i,1]
      fl<-ctllist$lambdas[i,2]
      phz<-ctllist$lambdas[i,3]
      value<-ctllist$lambdas[i,4]
      sizefreq_method<-ctllist$lambdas[i,5]
      if(like_comp==1){
        rownames(ctllist$lambdas)[i]<-paste0("Surv_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==4){
        rownames(ctllist$lambdas)[i]<-paste0("length_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==5){
        rownames(ctllist$lambdas)[i]<-paste0("age_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==5){
        sizefreq_method<-ctllist$lambdas[i,5]
        rownames(ctllist$lambdas)[i]<-paste0("SizeFreq_",fleetnames[fl],"_sizefreq_method_",sizefreq_method,"_Phz",phz)
      }else if(like_comp==8){
        rownames(ctllist$lambdas)[i]<-paste0("catch_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==9){
        rownames(ctllist$lambdas)[i]<-paste0("init_equ_catch_",fleetnames[fl],"_lambda_for_init_equ_catch_can_only_enable/disable for_all_fleets_Phz",phz)
      }else if(like_comp==10){
        rownames(ctllist$lambdas)[i]<-paste0("recrdev_Phz",phz)
      }else if(like_comp==11){
        rownames(ctllist$lambdas)[i]<-paste0("parm_prior_Phz",phz)
      }else if(like_comp==12){
        rownames(ctllist$lambdas)[i]<-paste0("parm_dev_Phz",phz)
      }else if(like_comp==13){
        rownames(ctllist$lambdas)[i]<-paste0("CrashPen_Phz",phz)
      }else if(like_comp==15){
        rownames(ctllist$lambdas)[i]<-paste0("Tag-comp-likelihood-",fl,"_Phz",phz)
      }else if(like_comp==16){
        rownames(ctllist$lambdas)[i]<-paste0("Tag-negbin-likelihood-",fl,"_Phz",phz)
      }
    }
  }
  # Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch;
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
  ctllist<-add_elem(ctllist,"more_stddev_reporting")  # (0/1) read specs for more stddev reporting
  if(ctllist$more_stddev_reporting!=0){
  #  stop("Currently additional reporting of derived quantities is not implemented in this R code")
    ctllist<-add_vec(ctllist,name="stddev_reporting_specs",length=9)
    ## Selex bin
    if(ctllist$stddev_reporting_specs[4]>0){
      ctllist<-
        add_vec(ctllist,name="stddev_reporting_selex",length=ctllist$stddev_reporting_specs[4])
    }
    ## Growth bin
    if(ctllist$stddev_reporting_specs[6]>0){
      ctllist<-
        add_vec(ctllist,name="stddev_reporting_growth",length=ctllist$stddev_reporting_specs[6])
    }
    ## N at age
    if(ctllist$stddev_reporting_specs[9]>0){
      ctllist<-
        add_vec(ctllist,name="stddev_reporting_N_at_A",length=ctllist$stddev_reporting_specs[9])
    }
  }
  if(ctllist$'.dat'[ctllist$'.i']==999){
    if(verbose) cat("read of control file complete (final value = 999)\n")

  }else{
    cat("Error: final value is", ctllist$'.dat'[ctllist$'.i']," but should be 999\n")
  }
  ctllist$'.dat'<-NULL
  ctllist$'.i'<-NULL
  # return the result
  return(ctllist)
}
