#' read control file from SS version 3.30
#'
#' Read Stock Synthesis (version 3.30) control file into list object in R.
#' This function comes with its wrapper function SS_readctl
#' that calls SS_readctl_3.24  or SS_readctl_3.30 (this function).
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param verbose Should there be verbose output while running the file?
#' Default=TRUE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param version SS version number. Currently only "3.24" or "3.30" are supported,
#' either as character or numeric values (noting that numeric 3.30  = 3.3).
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
#' @param Nfleets number of fishery and survey fleets in the model. This information is also not
#'  explicitly available in control file
#' @param Do_AgeKey Flag to indicate if 7 additional ageing error parameters to be read
#'  set 1 (but in fact any non zero numeric in R) or TRUE to enable to read them 0 or FALSE (default)
#'  to disable them. This information is not explicitly available in control file, too.
#' @param N_tag_groups number of tag release group. Default =NA. This information is not explicitly available
#'  control file. This information is only required if custom tag parameters is enabled (TG_custom=1)
#' @param N_CPUE_obs integer vector of length=Nfleets containing number of data points of each CPUE time series
#' @param catch_mult_fleets integer vector of fleets using the catch multiplier 
#'   option. Defaults to NULL and should be left as such if 1) the catch 
#'   multiplier option is not used for any fleets or 2) use_datlist = TRUE and 
#'   datlist is specified.
#' @param N_rows_equil_catch Integer value of the number of parmeter lines to 
#' read for equilibrium catch. Defaults to 0.
#' @param N_dirichlet_parms Integer value of the number of Dirichlet multinomial
#' parameters. Defaults to 0.
#' @param use_datlist LOGICAL if TRUE, use datlist to derive parameters which can not be
#'        determined from control file
#' @param datlist list or character. If list, should be a list produced from 
#'   \code{\link{SS_writedat}}. If character, should be the file name of an
#'   SS data file.
#' @author Neil Klaer, Yukio Takeuchi, Watal M. Iwasaki, and Kathryn Doering
#' @export
#' @seealso \code{\link{SS_readctl}}, \code{\link{SS_readdat}}
#' \code{\link{SS_readdat_3.24}},\code{\link{SS_readdat_3.30}}
#' \code{\link{SS_readctl_3.24}},
#' \code{\link{SS_readstarter}}, \code{\link{SS_readforecast}},
#' \code{\link{SS_writestarter}},
#' \code{\link{SS_writeforecast}}, \code{\link{SS_writedat}}
SS_readctl_3.30 <- function(file,verbose=TRUE,echoall=FALSE,version="3.30",
## Parameters that are not defined in control file
    nseas=4,
    N_areas=1,
    Nages=20,
    Ngenders=1,
    Npopbins=NA,
    Nfleets=2,
    Do_AgeKey=FALSE,
    N_tag_groups=NA,
    N_CPUE_obs=c(0,0,9,12), # This information is needed if Q_type of 3 or 4 is used
    catch_mult_fleets = NULL,
    N_rows_equil_catch = 0, 
    N_dirichlet_parms = 0,
    ##################################
    use_datlist=FALSE,
    datlist=NULL
    ){
  
  
  # function to read Stock Synthesis data files

  if(verbose) cat("running SS_readctl_3.30\n")
  dat <- readLines(file,warn=FALSE)

  nver=as.numeric(substring(version,1,4))
  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2]," ")[[1]][1]
  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in seq_len(length(dat))) {
    # First split between input and comments
    mysplit <- strsplit(dat[i],split="#")[[1]]
    if(!is.na(mysplit[1]))
    {
      # split along blank spaces
      mysplit <- strsplit(mysplit[1],split="[[:blank:]]+")[[1]]
      mysplit <- mysplit[mysplit!=""]
      # convert to numeric
      nums <- suppressWarnings(as.numeric(mysplit))
      nums <- nums[!is.na(nums)]
      # append new values to allnums vector
      if(length(nums) > 0){
        allnums <- c(allnums, nums)
      }
    }
  }
  
  # internally used fun definitions ----
  # Function to add vector to ctllist

  add_vec<-function(ctllist,length,name,comments=NULL){
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

  find.index <- function(dat, ind, str){
    ## Find the first line at position ind or later that
    ## contains the string str and return the index of that
    ## line. If the end of the data is reached, an error
    ## will be shown.
    while(ind < length(dat) & !length(grep(str, dat[ind]))){
      ind <- ind + 1
    }
    if(ind == length(dat)){
      stop("SS_readctl_3.30-find.index: Error - ",
           "the value of ", str, " was not found. ",
           "Check the control file and make sure all ",
           "data frames are correctly formed.\n")
    }
    ind
  }

  # Function to add data as data.frame to ctllist
  add_df<-function(ctllist,nrows=NULL,ncol,col.names,name,comments=NULL){
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
    if(is.null(nrows))
    {
      end.ind <- find.index(dat, i, "-9999")
      nrow<-as.integer((end.ind-i)/ncol)
      if(nrow==0) # there isn't any data so just return
      {
        ctllist$'.i'<-ctllist$'.i'+ncol
        return(ctllist)
      }
    }
    else nrow<-nrows
    
    k<-nrow*ncol
    
    df0<-as.data.frame(matrix(dat[i+1:k-1],nrow=nrow,ncol=ncol,byrow=TRUE))
    colnames(df0)<-col.names
    if(is.null(comments)){
      rownames(df0)<-paste0(paste0("#_",name,collapse=""),1:nrow)
    }else{
      rownames(df0)<-comments
    }
    i <- i+k
    
    if(is.null(nrows))i <- i+ncol
    
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
  add_elem<-function(ctllist=NA,name){
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
    ctllist$temp<-dat[i]
    ctllist$'.i'<-i+1
    if(!is.na(name))names(ctllist)[names(ctllist)=="temp"]<-name
    if(verbose)cat(name,",i=",ctllist$'.i'," ;",ctllist[[which(names(ctllist)==name)]],"\n")
    return(ctllist)
  }

  ## function to add list  to ctllist
  add_list<-function(ctllist=NA,name,length,length_each){
    i<-ctllist$'.i'
    dat<-ctllist$'.dat'
     ctllist$temp<-list()
    for(j in seq_len(length)){
      ctllist$temp[[j]]<-dat[i+1:length_each[j]-1]; i <- i+length_each[j]
    }
    ctllist$'.i'<-i
    if(!is.null(name))names(ctllist)[names(ctllist)=="temp"]<-name
    if(verbose)cat(name,",i=",ctllist$'.i',"\n")
    return(ctllist)
  }
  
  # internally used commmon values ----
  lng_par_colnames <- c("LO", "HI", "INIT", "PRIOR", "PR_SD", "PR_type", "PHASE",
                        "env_var&link","dev_link", "dev_minyr", "dev_maxyr", "dev_PH", 
                        "Block", "Block_Fxn")
  srt_par_colnames <- c("LO", "HI", "INIT", "PRIOR","PR_SD", "PR_type", "PHASE")

  # setup ----
  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  ctllist <- list()
  ctllist$'.i'<-i
  ctllist$'.dat'<-allnums
  ctllist$warnings<-""
  if(!use_datlist){
    ctllist$nseas<-nseas
    ctllist$N_areas<-N_areas
    ctllist$Nages<-Nages
    ctllist$Ngenders<-Ngenders
    ctllist$Npopbins<-Npopbins
    ctllist$Nfleets<-Nfleets
    ctllist$Do_AgeKey<-Do_AgeKey
    ctllist$N_tag_groups<-N_tag_groups
    ctllist$N_CPUE_obs<-N_CPUE_obs
    fleetnames<-paste0("FL",1:Nfleets)

    ctllist$fleetnames<-fleetnames
  }else{
    if(is.character(datlist))datlist<-SS_readdat(file=datlist)
    if(is.null(datlist))stop("datlist from SS_readdat is needed if use_datlist is TRUE")
    ctllist$nseas<-nseas<-datlist$nseas
    ctllist$N_areas<-N_areas<-datlist$N_areas
    ctllist$Nages<-Nages<-datlist$Nages
    ctllist$Ngenders<-Ngenders<-datlist$Ngenders
    if(datlist$lbin_method == 1) {
      ctllist$Npopbins <- Npopbins <- datlist$N_lbins # b/c method 1 uses data length bins
    } else if (datlist$lbin_method == 2) {
      #calculate number of bins (max Lread - min Lread)/(bin width) + 1. 
      # formula according to SS manual.
      ctllist$Npopbins <- Npopbins <- 
        (datlist$maximum_size - datlist$minimum_size)/(datlist$binwidth) + 1
    } else if (datlist$lbin_method == 3) {
      ctllist$Npopbins <- Npopbins <- datlist$N_lbinspop # read actual input
    } else {
      stop("datlist$lbin_method is ", datlist$lbin_method, "but only can be 1", 
           ", 2, or 3.")
    }
    ctllist$Nfleets<-Nfleets<-datlist$Nfleets
    #ctllist$Nsurveys<-Nsurveys<-datlist$Nsurveys
    # short circuit logic to avoid error if it is null.
    if(!is.null(datlist$N_ageerror_definition) && 
       datlist$N_ageerror_definition > 0) {
      ctllist$Do_AgeKey <- ifelse(
        any(datlist$ageerror[1:(nrow(datlist$ageerror)/2)*2, 1] < 0), 1, 0)
    } else {
      ctllist$Do_AgeKey <- 0
    }
    ctllist$N_tag_groups <- N_tag_groups <- datlist$N_tag_groups
    ctllist$N_CPUE_obs <- N_CPUE_obs <- datlist$N_cpue
    ctllist$fleetnames <- fleetnames<-datlist$fleetnames
  }
  # specifications ----
  ctllist$sourcefile <- file
  ctllist$type <- "Stock_Synthesis_control_file"
  ctllist$ReadVersion <- "3.30"
  
  ctllist$eof <- FALSE
  
  if(verbose) cat("SS_readctl_3.30 - read version = ",ctllist$ReadVersion,"\n")
  
  # beginning of ctl ----
  # weight at age option
  ctllist<-add_elem(ctllist,"EmpiricalWAA")

  # model dimensions
  ctllist<-add_elem(ctllist,"N_GP")
  
  ctllist<-add_elem(ctllist,"N_platoon")
  if(ctllist$N_platoon>1){
    
    ctllist<-add_elem(ctllist,"sd_ratio")
    ctllist<-add_vec(ctllist,name="submorphdist",length=ctllist$N_platoon)
  }
  
  # recruitment timing and distribution ----
  ctllist<-add_elem(ctllist,"recr_dist_method")
  if(ctllist$recr_dist_method == "1") {
    warning("recr_dist_method 1 should not be used in SS version 3.30. Please use 2, 3, or 4. \n")
  }
  if(ctllist$recr_dist_method == "4" && (ctllist$N_GP != 1 || ctllist$N_areas != 1)) {
    stop("recr_dist_method 4 should only be used when GPxSettlementxArea=1. \n")
  }
  
  ctllist<-add_elem(ctllist,"recr_global_area")
  
  ctllist<-add_elem(ctllist,"recr_dist_read")
  recr_dist_read<-ctllist$recr_dist_read
  ctllist<-add_elem(ctllist,"recr_dist_inx") # recruitment interaction requested
  if(ctllist$recr_dist_inx>0){
    #give warning but don't stop for now
    warning("Recr_dist_inx should not be used in SS version 3.30. Please set to 0. \n")
  }
  ctllist<-add_df(ctllist,"recr_dist_pattern",nrow=recr_dist_read,ncol=4,
      col.names=c("GPattern","month","area","age"))
  # movement ----
  if(ctllist$N_areas>1){
    ctllist<-add_elem(ctllist,"N_moveDef") #_N_movement_definitions goes here if N_areas > 1
    if(ctllist$N_moveDef>0)
    {
      ctllist<-add_elem(ctllist,"firstAgeMove") #_first age that moves (real age at begin of season, not integer) also cond on do_migration>0
      ctllist<-add_df(ctllist,"moveDef",nrow=ctllist$N_moveDef,ncol=6,col.names=c("seas", "morph", "source", "dest", "age", "age2"))
    }
  }
  # block setup ----
  ctllist<-add_elem(ctllist,"N_Block_Designs") #_Nblock_Patterns
  if(ctllist$N_Block_Designs>0){
    ctllist<-add_vec(ctllist,name="blocks_per_pattern",length=ctllist$N_Block_Designs)
    #_blocks_per_pattern
  # begin and end years of blocks
    ctllist<-add_list(ctllist,name="Block_Design",length=ctllist$N_Block_Designs,
      length_each=ctllist$blocks_per_pattern*2)
  }
  # timevary ctls ----
  ctllist<-add_elem(ctllist,"time_vary_adjust_method") 
  ctllist<-add_vec(ctllist,name="time_vary_auto_generation",length=5) 
  # MG setup ----
  # M setup ----
  ctllist<-add_elem(ctllist,"natM_type") #_natM_type
  if(ctllist$natM_type==0){
    N_natMparms<-1
  }else if(ctllist$natM_type==1){
    ctllist<-add_elem(ctllist,name="N_natM") #_Number of M_segments
    ctllist<-add_vec(ctllist,name="M_ageBreakPoints",length=ctllist$N_natM) # age(real) at M breakpoints
    N_natMparms<-ctllist$N_natM
  }else if(ctllist$natM_type==2){
    N_natMparms<-ctllist$N_GP*abs(ctllist$Ngenders)
    ctllist<-add_elem(ctllist,name="Lorenzen_refage") ## Reference age for Lorenzen M
  }else if(ctllist$natM_type %in% c(3,4)){
    N_natMparms<-0
    ctllist<-add_df(ctllist,name="natM",nrow=ctllist$N_GP*abs(ctllist$Ngenders),ncol=Nages+1,col.names=paste0("Age_",0:Nages))
  }else{
    stop("natM_type =", ctllist$natM_type," is not yet implemented in this script")
  }
  if(verbose) message("N_natMparms =",N_natMparms,"\n")
  
  # growth setup ----
  ctllist<-add_elem(ctllist,name="GrowthModel")
    # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
  ctllist<-add_elem(ctllist,name="Growth_Age_for_L1") #_Growth_Age_for_L1
  ctllist<-add_elem(ctllist,name="Growth_Age_for_L2") #_Growth_Age_for_L2 (999 to use as Linf)
  ctllist<-add_elem(ctllist,name="Exp_Decay") #Exponential decay for growth above maximum age
  ctllist<-add_elem(ctllist,name="Growth_Placeholder") #_for_future_use
  if(ctllist$GrowthModel %in% c(3,4,5)) {
    ctllist<-add_elem(ctllist,name="N_ageK")
    
  }
  
  if(ctllist$GrowthModel==1)  # 1=vonBert with L1&L2
  {
    N_growparms<-5
  } else if(ctllist$GrowthModel %in% c(2,8)) # 2=Richards with L1&L2, growth cess.
  {
    N_growparms<-6
  } else if(ctllist$GrowthModel %in% c(3,4,5)) { # 3,4=age_specific_K
    Age_K_count<-ctllist$N_ageK
    N_growparms <- 5 + Age_K_count
    ctllist<-add_vec(ctllist,name="Age_K_points",length=Age_K_count)
    #  points at which age-specific multipliers to K will be applied
  } else {
    stop("Growth Model ", ctllist$GrowthModel, " is not supported yet")
  }
  MGparm_per_def<-N_natMparms+N_growparms
  ctllist$N_natMparms<-N_natMparms
  #Add growth inputs that all growth types have:
  ctllist<-add_elem(ctllist,name="SD_add_to_LAA") #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
  ctllist<-add_elem(ctllist,name="CV_Growth_Pattern")
  
  # maturity and MG options setup ----
  ctllist<-add_elem(ctllist,name="maturity_option")
    #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity by GP; 4=read age-fecundity by GP; 5=read fec and wt from wtatage.ss; 6=read length-maturity by GP
  if(ctllist$maturity_option %in% c(3,4)){
    ctllist<-add_df(ctllist,name="Age_Maturity",nrow=ctllist$N_GP,ncol=Nages+1,col.names=paste0("Age_",0:Nages))
  }else if(ctllist$maturity_option==6){
    ctllist<-add_df(ctllist,name="Length_Maturity",nrow=ctllist$N_GP,ncol=Npopbins,col.names=paste0("Len_",1:Npopbins))
  }
  
  ctllist<-add_elem(ctllist,"First_Mature_Age") #_First_Mature_Age
  ctllist<-add_elem(ctllist,"fecundity_option")  #_fecundity_option
  ctllist<-add_elem(ctllist,"hermaphroditism_option")   #_hermaphroditism_option
  if(ctllist$hermaphroditism_option!=0)
  {
    ctllist<-add_elem(ctllist,"Herm_season") #_Hermaphroditism_season
    ctllist<-add_elem(ctllist,"Herm_MalesInSSB")  #_Hermaphroditism_males_in_SSB
  }
  ctllist<-add_elem(ctllist,"parameter_offset_approach")    #_parameter_offset_approach

  # MG parlines -----
  N_MGparm<-MGparm_per_def*ctllist$N_GP*abs(ctllist$Ngenders)  ## Parmeters for M and Growth multiplied by N_GP and Ngenders
  MGparmLabel<-list()
  cnt<-1
  PType<-array() # store parameter types M=1, Growth=2, WtLn = 3, Maturity = 4, Fecundity = 5, 
                 # Hermaph = 6, RecDevs GP = 7 Areas = 8 Seas= 9, RecDev Interactions = 10, 
                 # GrowthDevs = 11, Movement = 12, AgeKey = 13, Frac female = 14,
                 # catch mult = NA (but could assign in the future)
  
  GenderLabel<-c("Fem","Mal")
  for(i in seq_len(abs(ctllist$Ngenders))) {
    for(j in seq_len(ctllist$N_GP)) {
      if(N_natMparms>0){
        MGparmLabel[1:N_natMparms+cnt-1]<-paste0("NatM_p_",1:N_natMparms,"_",GenderLabel[i],"_GP_",j)
        PType[cnt:(N_natMparms+cnt-1)]<-1
        cnt<-cnt+N_natMparms
      }
      if(ctllist$GrowthModel==1){ # VB
        tmp<-c("L_at_Amin","L_at_Amax","VonBert_K","CV_young","CV_old")
        MGparmLabel[1:5+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        PType[cnt:(5+cnt-1)]<-2
        cnt<-cnt+5
      }else if(ctllist$GrowthModel==2){ # Richards
        tmp<-c("L_at_Amin","L_at_Amax","VonBert_K","Richards","CV_young","CV_old")
        MGparmLabel[1:6+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        PType[cnt:(6+cnt-1)]<-2
        cnt<-cnt+6
      }else if(ctllist$GrowthModel %in% 3:5) {
        tmp <- c("L_at_Amin","L_at_Amax","VonBert_K",
                 paste0("Age_K_",ctllist$Age_K_points),"CV_young","CV_old")
        MGparmLabel[1:(5+Age_K_count)+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        PType[cnt:((5+Age_K_count)+cnt-1)]<-2
        cnt<-cnt+5+Age_K_count
      } else if(ctllist$GrowthModel == 8) { 
        tmp<-c("L_at_Amin","L_at_Amax","VonBert_K","Cessation","CV_young","CV_old")
        MGparmLabel[1:6+cnt-1]<-paste0(tmp,"_",GenderLabel[i],"_GP_",j)
        PType[cnt:(6+cnt-1)]<-2
        cnt<-cnt+6
      }
      MGparmLabel[cnt]<-paste0("Wtlen_1_",GenderLabel[i],"_GP_",j);PType[cnt]<-3;cnt<-cnt+1
      MGparmLabel[cnt]<-paste0("Wtlen_2_",GenderLabel[i],"_GP_",j);PType[cnt]<-3;cnt<-cnt+1
      if(i==1){
      MGparmLabel[cnt]<-paste0("Mat50%_",GenderLabel[1],"_GP_",j);PType[cnt]<-4;cnt<-cnt+1
      MGparmLabel[cnt]<-paste0("Mat_slope_",GenderLabel[1],"_GP_",j);PType[cnt]<-4;cnt<-cnt+1
      if(ctllist$maturity_option==1){
        MGparmLabel[cnt]<-paste0("Eggs/kg_inter_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
        MGparmLabel[cnt]<-paste0("Eggs/kg_slope_wt_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
      }else if(ctllist$maturity_option==2){
        MGparmLabel[cnt]<-paste0("Eggs_scalar_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
        MGparmLabel[cnt]<-paste0("Eggs_exp_len_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
      }else  if(ctllist$maturity_option==3){
        MGparmLabel[cnt]<-paste0("Eggs_scalar_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
        MGparmLabel[cnt]<-paste0("Eggs_exp_wt_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
      }else  if(ctllist$maturity_option==4){
        MGparmLabel[cnt]<-paste0("Eggs_intercept_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
        MGparmLabel[cnt]<-paste0("Eggs_slope_len_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
      }else   if(ctllist$maturity_option==5){
        MGparmLabel[cnt]<-paste0("Eggs_intercept_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
        MGparmLabel[cnt]<-paste0("Eggs_slope_wt_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
      }else   if(ctllist$maturity_option==6){  # check what to do with option 6
        MGparmLabel[cnt]<-paste0("Eggs_intercept_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
        MGparmLabel[cnt]<-paste0("Eggs_slope_wt_",GenderLabel[1],"_GP_",j);PType[cnt]<-5;cnt<-cnt+1
      }else{
        stop("Maturity option : ",ctllist$maturity_option," is not supported")
      }
      }
    }
  }
  N_MGparm <- cnt - 1
  
  if(ctllist$hermaphroditism_option!=0){
    MGparmLabel[cnt]<-paste0("Herm_Infl_age",GenderLabel[1]);PType[cnt]<-6;cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Herm_stdev",GenderLabel[1]);PType[cnt]<-6;cnt<-cnt+1
    MGparmLabel[cnt]<-paste0("Herm_asymptote",GenderLabel[1]);PType[cnt]<-6;cnt<-cnt+1
    N_MGparm<-N_MGparm+3
  }
  #Recruitment Distribution
  # get the labels for RecrDist, number and names depend on method
  RecrDistLabel <- switch(ctllist$recr_dist_method,
                          "1"= { # included for back compatibility only.
                                lab <- c(paste0("RecrDist_GP_", 1:ctllist$N_GP),
                                         paste0("RecrDist_Area_", 1:ctllist$N_areas),
                                         paste0("RecrDist_Bseas_", 1:ctllist$nseas))
                                if(ctllist$recr_dist_inx){ # interactions
                                  #Note:labels and order consistent with SS source 
                                  #(3.30 and 3.24 control file read in 3.30.10)
                                  for(i in seq_len(ctllist$N_GP)) {
                                    for(j in seq_len(ctllist$N_areas)) {
                                      for(k in seq_len(ctllist$nseas)) { # goes with settle
                                        tmp_lab_inx <-paste0("RecrDist_interaction_GP_",i,
                                                             "_area_",j,
                                                             "_settle_",k)
                                        lab <- c(lab, tmp_lab_inx)
                                      }
                                    }
                                  }
                                }
                                lab
                          },
                          "2"=  {
                            #get the number of times settlement occurs
                            N_settle_timings <- length(unique(ctllist$recr_dist_pattern[ ,2]))
                                  lab <- c(paste0("RecrDist_GP_",1:ctllist$N_GP), 
                                           paste0("RecrDist_Area_",1:ctllist$N_areas), 
                                           paste0("RecrDist_month_",1:N_settle_timings))
                                  if(ctllist$recr_dist_inx){ # interactions
                                    #Note:labels and order consistent with SS source 
                                    #(3.30 and 3.24 control file read in 3.30.10)
                                    for(i in seq_len(ctllist$N_GP)) {
                                      for(j in seq_len(ctllist$N_areas)) {
                                        for(k in seq_len(N_settle_timings)) {
                                          tmp_lab_inx <-paste0("RecrDist_interaction_GP_",i,
                                                               "_area_",j,
                                                               "_month_",k)
                                          lab <- c(lab, tmp_lab_inx)
                                        }
                                      }
                                    }
                                  }
                                  lab
                          },
                          "3"= { 
                                lab <- NULL
                                for(i in seq_len(nrow(ctllist$recr_dist_pattern))) {
                                  tmp_lab <- paste0("RecrDist_GP_", 
                                               ctllist$recr_dist_pattern[i,1], 
                                               "_area_", 
                                               ctllist$recr_dist_pattern[i,3], 
                                               "_month_", 
                                               ctllist$recr_dist_pattern[i,2])
                                  lab <- c(lab, tmp_lab)
                                 }
                                 lab
                          },
                          "4"= NULL
                          )
# Get the parameter type for recruitment dist params, type depends on method.
  RecrDist_PType <- switch(ctllist$recr_dist_method,
                           "1" = {
                                  tmp_PType <- rep(c(7,8,9),
                                                   times = c(ctllist$N_GP,
                                                             ctllist$N_areas,
                                                             ctllist$nseas
                                                             )
                                                   )
                                  if(ctllist$recr_dist_inx){ #interactions
                                    tmp_PType <- c(tmp_PType, rep(10, times = ctllist$N_GP*ctllist$N_areas*ctllist$nseas))
                                  }
                                  tmp_PType
                           },
                           "2" = {
                                  tmp_PType <- rep(c(7,8,9), times = c(ctllist$N_GP,
                                                                       ctllist$N_areas,
                                                                       N_settle_timings))
                                    if(ctllist$recr_dist_inx){ #interactions
                                      tmp_PType <- c(tmp_PType, rep(10, times = ctllist$N_GP*ctllist$N_areas*N_settle_timings))
                                    }
                                  tmp_PType
                            },
                           # may want a different PType for 3 in the future?
                           "3"=rep(10, times = nrow(ctllist$recr_dist_pattern)),
                           "4"= NULL # because no params
                           )

  #Add the labels, their parameter type and then adjust the count. 
  N_RecrDist_parms <- length(RecrDistLabel)
  if(N_RecrDist_parms>0) {
    MGparmLabel[cnt:(cnt+N_RecrDist_parms-1)] <- RecrDistLabel
    PType[cnt:(cnt+N_RecrDist_parms-1)] <- RecrDist_PType
    cnt <- cnt + N_RecrDist_parms # add on to the count
    N_MGparm <- N_MGparm + N_RecrDist_parms # add on to number of MGparms 
  }
  
  N_MGparm<-N_MGparm+1 # add 1 parameter for cohort-specific growth parameter
  MGparmLabel[cnt]<-"CohortGrowDev";PType[cnt]<-11;cnt<-cnt+1
  if((N_areas>1)&&(ctllist$N_moveDef>0)){
    N_MGparm<-N_MGparm+ctllist$N_moveDef*2 # add 2 * N_moveDef for movement params
#    M_Move_parms<-ctllist$N_moveDef*2
    for(i in seq_len(ctllist$N_moveDef)) {
      seas<-ctllist$moveDef[i,1]
      GP<-ctllist$moveDef[i,2]
      from<-ctllist$moveDef[i,3]
      to<-ctllist$moveDef[i,4]
      MGparmLabel[cnt+0:1]<-paste0("MoveParm_",c("A","B"),"_seas_",seas,"_GP_",GP,"_from_",from,"_to_",to);PType[cnt:(cnt+1)]<-12;cnt<-cnt+2
    }
  }
  # age error parameters
  if(ctllist$Do_AgeKey) {
    MGparmLabel[cnt+0:6]<-paste0("AgeKeyParm",1:7);PType[cnt:(cnt+6)]<-13;cnt<-cnt+7
    N_MGparm<-N_MGparm+7
  }
  # Parameter lines for catch multiplier: 
  # figure out if need to use the catch multiplier
  if(use_datlist == TRUE) {
    if(any(datlist$fleetinfo$need_catch_mult == 1)) {
      use_catch_mult <- TRUE
    } else {
      use_catch_mult <- FALSE
    }
  } else if(!is.null(catch_mult_fleets)) {
    use_catch_mult <- TRUE
  } else {
    use_catch_mult <- FALSE
  }
  if(use_catch_mult == TRUE) {
    if(use_datlist == TRUE) {
      catch_mult_fleets <- which(datlist$fleetinfo$need_catch_mult == 1)
    }
  }
  # specify the parameter lines for catch multiplier: 
  if(!is.null(catch_mult_fleets)) {
    MGparmLabel[cnt+0:(length(catch_mult_fleets)-1)] <- 
      paste0("Catch_Mult:_", catch_mult_fleets)
    PType[cnt:(cnt+(length(catch_mult_fleets)-1))] <- NA
    cnt <- cnt + length(catch_mult_fleets)
    N_MGparm<-N_MGparm + length(catch_mult_fleets)
  }
  MGparmLabel[cnt+1:ctllist$N_GP-1]<-paste0("FracFemale_GP_",1:ctllist$N_GP);PType[cnt:(cnt+ctllist$N_GP-1)]<-14;cnt<-cnt+ctllist$N_GP
  N_MGparm<-N_MGparm+ctllist$N_GP
  
  ctllist<-add_df(ctllist,name="MG_parms",nrow=N_MGparm,ncol=14,
                    col.names=lng_par_colnames,
                                comments=MGparmLabel)
  
  ctllist$MG_parms<-cbind(ctllist$MG_parms,PType)

  # MG timevarying parlines ------
  if(any(ctllist$MG_parms[,c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[1] == 0) {
    warning("There are time varying MG parameters, and AUTOGEN for MG is 0, so",
            " not expecting any short parameter lines.")
  }
  if(any(ctllist$MG_parms[,c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[1] != 0) {
    tmp_parlab <- get_tv_parlabs(full_parms = ctllist[["MG_parms"]], ctllist[["Block_Design"]])
    ctllist <- add_df(ctllist, 
                      name = "MG_parms_tv", 
                      nrow = length(tmp_parlab), 
                      ncol = 7, 
                      col.names = srt_par_colnames,
                      comments = tmp_parlab)
  }

  # Read seasonal effects ----
  ctllist<-add_vec(ctllist,name="MGparm_seas_effects",length=10)
  PType<-array()
  N_seas_effects<-length(ctllist$MGparm_seas_effects[ctllist$MGparm_seas_effects>0])
  if(N_seas_effects>0){
    ctllist<-add_df(ctllist,"MG_parms_seas",nrow=N_seas_effects*ctllist$nseas,ncol=7,
                    col.names=srt_par_colnames)
    PType[1:(N_seas_effects*ctllist$nseas)]<-16
    ctllist$MG_parms_seas<-cbind(ctllist$MG_parms_seas,PType)
    
  }
  
 # SR -----
  ctllist<-add_elem(ctllist,"SR_function")   #_SR_function
  N_SRparm<-c(0,2,2,2,3,2,3,3,3,3)
  N_SRparm2<-N_SRparm[as.numeric(ctllist$SR_function)]+3
  
  if(is.na(ctllist$SR_function)) {
    stop("SR_function is NA, which is not valid.")
  }
  
  ctllist<-add_elem(ctllist,"Use_steep_init_equi")   # 0/1 to use steepness in initial equ recruitment calculation
  ctllist<-add_elem(ctllist,"Sigma_R_FofCurvature")   #  future feature:  0/1 to make realized sigmaR a function of SR curvature
  
  # SR parms ----
  SRparmsLabels<-if(ctllist$SR_function ==3){
    # B-H SRR
    c("SR_LN(R0)","SR_BH_steep","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==2){
    # Ricker SRR
    c("SR_LN(R0)","SR_Ricker_steep","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==4){
    # SCAA
    c("SR_LN(R0)","SR_SCAA_null","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==5){
    # Hockey stick
    c("SR_LN(R0)","SR_hockey_infl","SR_hockey_min_R","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function ==6){
    # B-H-flat SRR
    c("SR_LN(R0)","SR_BH_flat_steep","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==7){
    # survival_3Parm
    c("SR_LN(R0)","SR_surv_Sfrac","SR_surv_Beta","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==8){
    # Shepard_3Parm
    c("SR_LN(R0)","SR_steepness","SR_Shepard_c","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==9){
    # Shepard_reparam_beta
    c("SR_LN(R0)","SR_re_steepness","SR_Shepard_c","SR_sigmaR","SR_regime","SR_autocorr")
  }else if(ctllist$SR_function==10){
    # Ricker SRR reparam beta
    c("SR_LN(R0)","SR_Ricker_re_steep","SR_Ricker_re_power","SR_sigmaR","SR_regime","SR_autocorr")
  }else{
    cat("SR_function=",ctllist$SR_function," is not supported yet.");return(ctllist)
  }
  
  PType<-array()
  ctllist<-add_df(ctllist,name="SR_parms",nrow=N_SRparm2,ncol=14,
            col.names=lng_par_colnames,comments=SRparmsLabels)
  PType[1:N_SRparm2]<-17
  ctllist$SR_parms<-cbind(ctllist$SR_parms,PType)
  
  #SR timevarying parlines ----
  # time block, environmental link, and parm devs parameters
  if(any(ctllist$SR_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[2] == 0) {
    warning("There are time varying SR parameters, and AUTOGEN for SR is 0, so",
            " not expecting any short parameter lines.")
  }
  if(any(ctllist$SR_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[2] != 0) {
    tmp_parlab <- get_tv_parlabs(full_parms = ctllist[["SR_parms"]], ctllist[["Block_Design"]])
    ctllist <- add_df(ctllist, 
                      name = "SR_parms_tv", 
                      nrow = length(tmp_parlab), 
                      ncol = 7, 
                      col.names = srt_par_colnames,
                      comments = tmp_parlab)
    ctllist$SR_parms_tv$PType <- 17
  }
  # recdevs ----
  ctllist<-add_elem(ctllist,"do_recdev") #do_recdev:  0=none; 1=devvector; 2=simple deviations; 3=deviation vector; 4=3 with summed deviation penalty
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
    if(ctllist$period_of_cycles_in_recr > 0) {
      ctllist <- add_df(ctllist, 
                        name = "recr_cycle_pars", 
                        nrow = ctllist$period_of_cycles_in_recr,
                        ncol = 14,
                        col.names = lng_par_colnames,
                        comments = paste0("Recr_period_", 
                                          1:ctllist$period_of_cycles_in_recr))
    }
    if(ctllist$N_Read_recdevs>0){
      ctllist<-add_df(ctllist,"recdev_input",ncol=2,nrow=ctllist$N_Read_recdevs,col.names=c("Year","recdev"))
    }
  }
  
  # F setup ----
  ctllist<-add_elem(ctllist,"F_ballpark")
  ctllist<-add_elem(ctllist,"F_ballpark_year")
  ctllist<-add_elem(ctllist,"F_Method") 
  ctllist<-add_elem(ctllist,"maxF")
  # Additional inputs for F method 2 or 3 must be read. None for method 1.
  if(ctllist$F_Method == 2) {
    # overall start F value; overall phase; N detailed inputs to read
    ctllist <- add_vec(ctllist, "F_setup", length = 3)
    if(ctllist$F_setup[3] > 0) {
      ctllist<-add_df(ctllist, name = "F_setup2", nrow = ctllist$F_setup[3],
                      ncol = 6, 
                      col.names = c("fleet", "yr", "seas", "Fvalue", "se",
                                    "phase"))
    }
  }
  if(ctllist$F_Method == 3) {
    ctllist <- add_elem(ctllist,"F_iter")
  }
  #_initial_F_parms - get them for fleet/seasons with non-zero initial equilbrium catch
  if(use_datlist == FALSE & N_rows_equil_catch  > 0  ) {
    #TODO: modeify code so there is an appraoch that does not require the datlist.{
    #To achieve this I think we will need to write a script to check row length and 
    #identify all short parameter lines until catchability section is reached
    #I don't think there is any way to identify which Fleets/Seasons the F's match up with.
    stop("Cannot yet read in init_F (which should be done if ", 
         "N_rows_equil_catch > 0) if use_datalist == F")
  }
  
  if(any(datlist$catch[, "year"] == -999)) {
    tmp_equil <- datlist$catch[datlist$catch[,"year"] == -999, ]
    if(any(tmp_equil[, "catch"] > 0)) {
      tmp_equil <- tmp_equil[tmp_equil$catch > 0, ]
      # reorder as ss expects.
      tmp_equil_sort <- tmp_equil[order(tmp_equil$fleet, tmp_equil$seas), ]
      comments_initF <- paste0("InitF_seas_", tmp_equil_sort$seas, 
                                "_flt_", tmp_equil_sort$fleet, 
                                fleetnames[tmp_equil_sort$fleet])
      ctllist<-add_df(ctllist, name = "init_F", nrow = length(comments_initF), 
                      ncol = 7, col.names=srt_par_colnames,
                      comments = comments_initF)
      ctllist$init_F$PType <- 18
    }
  }
  
  # Q_setup ----
  ctllist<-add_df(ctllist,name="Q_options",ncol=6,
              col.names=c("fleet","link","link_info","extra_se","biasadj","float")) # no nrow, so read to -9999
  if(!is.null(ctllist$Q_options)) {
  rownames(ctllist$Q_options) <- ctllist$fleetnames[ctllist$Q_options$fleet]
  # create 3.24 compatible Q_setup ----
  comments_fl<-paste0(1:(Nfleets)," ",fleetnames)
  ctllist$Q_setup<-data.frame(matrix(data=0,nrow=(Nfleets),ncol=4),row.names=comments_fl)
  colnames(ctllist$Q_setup)<-c("Den_dep","env_var","extra_se","Q_type")
  }
  # q parlines ----
  N_Q_parms <- 0
  i<-1
  if(!is.null(ctllist$Q_options)) {
    comments_Q_type<-list()
  
    for(j in seq_len(nrow(ctllist$Q_options)))
    {
      if((ctllist$Q_options[j,]$float==0)||(ctllist$Q_options[j,]$float==1)) # handle float 0 or 1 as 1 parm sel 
      {
        ctllist$Q_setup[ctllist$Q_options[j,]$fleet,]$Q_type <- 2
        flname <- fleetnames[ctllist$Q_options[j,]$fleet]
        comments_Q_type[[i]] <- paste0("LnQ_base_", flname,"(",
                                     ctllist$Q_options[j,]$fleet,")",collapse="")
        i <- i + 1
        N_Q_parms<-N_Q_parms+1
      }
      if(ctllist$Q_options[j,]$link==2)  # mirrored
      {
        ctllist$Q_setup[ctllist$Q_options[j,]$fleet,]$Q_type<--abs(ctllist$Q_options[j,]$link_info)
      }
      if(ctllist$Q_options[j,]$link==3)  # do power
      {
        ctllist$Q_setup[ctllist$Q_options[j,]$fleet,]$Den_dep<-1
        flname<-fleetnames[ctllist$Q_options[j,]$fleet]
        comments_Q_type[[i]]<-paste0("Q_power_", flname, "(", 
                                     ctllist$Q_options[j,]$fleet ,")", 
                                     collapse="")
        i <- i + 1
        N_Q_parms<-N_Q_parms+1
      }
      if(ctllist$Q_options[j,]$link==4)  # mirrored with offset
      {
        ctllist$Q_setup[ctllist$Q_options[j,]$fleet,]$Den_dep<--abs(ctllist$Q_options[j,]$link_info)
        flname<-fleetnames[ctllist$Q_options[j,]$fleet]
        comments_Q_type[[i]]<-paste0("Q_Mirror_offset_", flname, "(", 
                                     ctllist$Q_options[j,]$fleet, ")",
                                     collapse="")
        i <- i + 1
        N_Q_parms<-N_Q_parms+1
      }
      if(ctllist$Q_options[j,]$extra_se==1)  # do extra se
      {
        ctllist$Q_setup[ctllist$Q_options[j,]$fleet,]$extra_se<-1
        flname<-fleetnames[ctllist$Q_options[j,]$fleet]
        comments_Q_type[[i]]<-paste0("Q_extraSD_", flname, "(",
                                     ctllist$Q_options[j,]$fleet, ")",
                                     collapse="")
        i <- i + 1
        N_Q_parms<-N_Q_parms+1
      }
    }
  }

  if(N_Q_parms>0){
    ctllist<-add_df(ctllist,name="Q_parms",nrow=N_Q_parms,ncol=14,
                    col.names=lng_par_colnames,
                comments=unlist(comments_Q_type))
    # q timevarying parlines----
    # time block, environmental link, and parm devs parameters
    # provide a warning if AUTOGEN is being used for MG.
    if(any(ctllist$Q_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
       ctllist$time_vary_auto_generation[3] == 0) {
      warning("There are time varying q parameters, and AUTOGEN for ",
              "q is 0, so not expecting any short parameter lines.")
    }
    if(any(ctllist$Q_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
       ctllist$time_vary_auto_generation[3] != 0) {
      tmp_parlab <- get_tv_parlabs(full_parms = ctllist[["Q_parms"]], ctllist[["Block_Design"]])
      ctllist <- add_df(ctllist, 
                        name = "Q_parms_tv", 
                        nrow = length(tmp_parlab), 
                        ncol = 7, 
                        col.names = srt_par_colnames,
                        comments = tmp_parlab)
      #Need Ptype?
    }
  }

# selecitivty -----  
  # size setup
  #TODO: make sure that this will work when special fleet types are used 
  #(e.g., units 30 fleet)
  ctllist <- add_df(ctllist, 
                    name = "size_selex_types",
                    nrow = Nfleets,
                    ncol = 4,
                    col.names=c("Pattern", "Discard", "Male", "Special"),
                    comments = fleetnames)
  # age setup
  ctllist<-add_df(ctllist,
                  name = "age_selex_types", 
                  nrow = Nfleets,
                  ncol = 4, 
                  col.names = c("Pattern", "Discard", "Male", "Special"),
                  comments = fleetnames)
  # selectivity parlines -----
  # This contains the number of params for each pattern (the name)
  selex_patterns <- c("0" = 0,"1" = 2,"2" = 8,"3" = 6,"4" = 0, "5" = 2,
                        "6" = 2, "7" = 8, "8" = 8, "9"= 6, "10" = 0, "11" = 2,
                        "12" = 2, "13" = 8, "14" = Nages + 1, "15" = 0, 
                        "16" = 2, "17" = Nages + 1, "18" = 8, "19" = 6,
                        "20" = 6, "21" = 2, "22" = 4, "23" = 6, "24" =  6,
                        "25" = 3, "26" = 3, "27" = 3, "28" = NA, "29" = NA, 
                        "30" = 0, "31" = 0, "32" = 0, "33" = 0, "34" = 0,
                        "35" = 0, "36" = NA, "37" = NA, "38" = NA, "39" = NA,
                        "40" = NA, "41" = 2 + Nages + 1, "42" = 5, "43" = 4,
                        "44" = 4 + Nages, "45" = 4 + Nages)
  # note thatspecial cases are 27, which is 3 + 2*N_nodes and 42, which is 
  # 5+2*N_nodes, and 6 which is 2+special, so the npars listed is not exactly 
  # correct. This will be addressed in special cases below.NAs are unused codes.
  size_selex_label<- vector("list", Nfleets) # do this to initialize size
  # loop through fishing fleets and surveys to assign names
  
  # make a labeling function, that way this 1 function can be changed in the
  # future instead of changing individual selectivity lines.
  # @param sel_type should be "s" or "a" for selectivity or age,
  # @param par_num is optional.
  # @param n_labs is optional. If provided, a basic check to see if the length
  # of the labels generated matches will be done and a warning will be generated
  # if the length of the labels is not equal to n_labs.
  make_sel_lab <- function(sel_type = "s", par_type, par_num = NULL, fltname, 
                           fltnum, n_labs = NULL) {
    if (!sel_type %in% c("s", "a")) {
      stop("sel_type cannot be ", sel_type, ". Please change sel_type to 's' ", 
           "for size or 'a' for age.")
    }
    if(sel_type == "s") sel_name <- "SizeSel"
    if(sel_type == "a") sel_name <- "AgeSel"
    if(!is.null(par_num)) {
      par_num <- paste0("_", par_num)
    }
    labs <- paste0(sel_name, "_", par_type, par_num, "_", fltname, "(", 
                   fltnum, ")")
    if(!is.null(n_labs)){
      if(length(labs) != n_labs) { # do a check and generate warning if input provided
        warning("the length of labs generated was not equal to the prespecified",
                "n_labs")
      }
    }
    labs
  }
  
  for(j in seq_len(Nfleets)) {

    jn <- fleetnames[j] # to use a shorter name throughout loop. jn for "j name)
    # get the number of parameters to use in making the generic labels. 
    #Not used for anything else.
    tmp_size_selex_Nparms <- 
      selex_patterns[as.character(ctllist$size_selex_types[j, "Pattern"])]
    if(is.na(tmp_size_selex_Nparms)) {
      stop("Pattern ", as.character(ctllist$size_selex_types[j, "Pattern"]),
           "was used for the size selectivity pattern fleet or survey, but it ",
           " is not valid.")
    }
    # pattern 6 is a special case of number of params, so account for here.
    if(ctllist$size_selex_types[j, "Pattern"] == 6) {
      tmp_size_selex_Nparms <- tmp_size_selex_Nparms + 
                               ctllist$size_selex_types[j, "Special"]
    }
    ## spline needs special treatment
    if(ctllist$size_selex_types[j, "Pattern"] == 27) {
      tmp_names <- paste0("Spline_", c("Code", "GradLo", "GradHi"))
      size_selex_label[[j]] <-
        c(make_sel_lab("s", tmp_names, NULL, jn, j), 
          make_sel_lab("s", "Spline_Knot",
                       1:ctllist$size_selex_types[j, "Special"], jn, j),
          make_sel_lab("s", "Spine_Val", 
                       1:ctllist$size_selex_types[j, "Special"], jn, j))
    } else if(ctllist$size_selex_types[j, "Pattern"] == 42) {
      # produce the labels
      tmp_names <- paste0("Spline_", c("ScaleBinLo", "ScaleBinHi", "Code", "GradLo",
                                  "GradHi"))
      size_selex_label[[j]] <-
        c(make_sel_lab("s", tmp_names, NULL, jn, j), 
          make_sel_lab("s", "Spline_Knot",
                       1:ctllist$size_selex_types[j, "Special"], jn, j), 
          make_sel_lab("s", "Spline_Val", 
                       1:ctllist$size_selex_types[j, "Special"], jn,j))
    } else {
      if(tmp_size_selex_Nparms > 0) { 
         size_selex_label[[j]] <-
           c(size_selex_label[[j]], 
             make_sel_lab("s", "P", 1:tmp_size_selex_Nparms, jn, j))
      }
    }
    # do extra retention parameters (4 extra parameters)
    if(ctllist$size_selex_types[j, "Discard"] %in% 1:2) { #add 4 retention parameters
      size_selex_label[[j]] <- c(size_selex_label[[j]], 
          make_sel_lab("s", "PRet", 1:4, jn, j))
    }
    # note that for Discard = 3, no extra parameters are needed.
    if(ctllist$size_selex_types[j, "Discard"] == 4) { #add 7 dome shaped retention parameters
      size_selex_label[[j]] <- c(size_selex_label[[j]], 
        make_sel_lab("s", "PRet", 1:7, jn, j))
    }
    
    if(ctllist$size_selex_types[j, "Discard"] %in% c(2,4)) { #add 4 discard mortality parameters
      size_selex_label[[j]] <- c(size_selex_label[[j]], 
        make_sel_lab("s", "PDis", 1:4, jn, j))
    }
    
    # do extra offset parameters
    if(ctllist$size_selex_types[j, "Male"] == 1) {
      size_selex_label[[j]] <- c(size_selex_label[[j]],
        make_sel_lab("s", "PMale", 1:4, jn, j))
    }
    if(ctllist$size_selex_types[j, "Male"] == 2) {
      size_selex_label[[j]] <- c(size_selex_label[[j]],
        make_sel_lab("s", "PFemOff", 1:4, jn, j))
    }
    
    if(ctllist$size_selex_types[j, "Male"] %in% 3) { # has value 3 or 5 - differs by select pattern
      if(ctllist$size_selex_types[j, "Pattern"] == 1) {
        size_selex_label[[j]] <- c(size_selex_label[[j]],
          make_sel_lab("s", "PMalOff", 1:3, jn, j))
      }else if(ctllist$size_selex_types[j, "Pattern"] %in% 23:24) {
        size_selex_label[[j]] <- c(size_selex_label[[j]],
          make_sel_lab("s", "PMalOff", 1:5, jn, j))
      }
    }else if(ctllist$size_selex_types[j, "Male"] %in% 4) { # has value 3 or 5 - differs by select pattern
      if(ctllist$size_selex_types[j, "Pattern"] == 1) {
        size_selex_label[[j]] <- c(size_selex_label[[j]],
            make_sel_lab("s", "PFemOff", 1:3, jn, j))
      }else if(ctllist$size_selex_types[j, "Pattern"] %in% 23:24) {
        size_selex_label[[j]] <- c(size_selex_label[[j]],
          make_sel_lab("s", "PFemOff", 1:5, jn, j))
      }
    }
  }
  # age selex parlines----
  age_patterns <- as.character(ctllist$age_selex_types[,"Pattern"])
  if(any(ctllist$age_selex_types[,"Pattern"] > 100)) {
    # this code deals with the option to give fish below a certain age 0 
    # selectivity; implemented in SS 3.30.15.
    age_patterns[age_patterns > 100] <-
      as.character(as.integer(substr(age_patterns[age_patterns > 100], 2, 3)))
  }
  # age selectivity parlines - count the number, not accounting for the 
  # extra special parameters for 17, 27, and 42.
  age_selex_Nparms <- selex_patterns[age_patterns]
  # pattern 17 is a special case of number of params, so account for here.
  age_selex_Nparms <-ifelse(age_patterns == "17" & 
                            ctllist$age_selex_types[,"Special"] > 0, 
                            ctllist$age_selex_types[,"Special"] + 1, 
                            age_selex_Nparms)

  age_selex_label <- vector("list", length = Nfleets)
  for(j in seq_len(Nfleets)) {
    jn <- fleetnames[j]
    ## spline needs special treatment
    if(age_patterns[j] == "27") {
      tmp_names <- paste0("Spline_", c("Code", "GradLo", "GradHi"))
      age_selex_label[[j]]<- c(make_sel_lab("a", tmp_names, NULL, jn, j ), 
                               make_sel_lab("a", c("Spline_Knot"), 
                                            1:ctllist$age_selex_types[j,"Special"], 
                                            jn, j), 
                               make_sel_lab("a", "Spline_Val",
                                            1:ctllist$age_selex_types[j,"Special"], 
                                            jn, j))
    } else if(age_patterns[j] == "42") {
      tmp_names <- paste0("Spline_", 
                   c("ScaleAgeLo", "ScaleAgeHi", "Code", "GradLo", "GradHi"))
      
      age_selex_label[[j]] <- 
        c(make_sel_lab("a", tmp_names, NULL, jn, j), 
          make_sel_lab("a", "Spline_Knot", 
                       1:ctllist$age_selex_types[j, "Special"], jn, j), 
          make_sel_lab("a", "Spine_Val",
                       1:ctllist$age_selex_types[j, "Special"], jn, j))
    } else {
      if(age_selex_Nparms[j] > 0) {
        age_selex_label[[j]] <- make_sel_lab("a", "P", 1:age_selex_Nparms[j],
                                             jn, j)
      }
      #Note that age_selex_Nparams[j] not used beyond this point.
    }
    # do extra offset parameters
    if(ctllist$age_selex_types[j,"Male"] == 1) {
      age_selex_label[[j]] <- c(age_selex_label[[j]], 
        make_sel_lab("a", "PMale", 1:4, jn, j))
    }
    if(ctllist$age_selex_types[j,"Male"] == 2) {
      age_selex_label[[j]] <- c(age_selex_label[[j]],
        make_sel_lab("a", "PFemOff_", 1:4, jn, j))
    }
    
    if(ctllist$age_selex_types[j, "Male"] %in% 3) { # has value 3 or 5 - differs by select pattern
      if(age_patterns[j] == "20") {
        age_selex_label[[j]] <- c(age_selex_label[[j]],
          make_sel_lab("a", "PMalOff", 1:5, jn, j))
      }
    }else if(ctllist$age_selex_types[j, "Male"] %in% 4) { # has value 3 or 5 - differs by select pattern
      if(age_patterns[j] == "20") {
        age_selex_label[[j]] <- c(age_selex_label[[j]],
          make_sel_lab("a", "PFemOff", 1:5, jn, j))
      }
    }
  }
  if(verbose) {
    cat("size_selex_Nparms\n")
    print(unlist(lapply(size_selex_label, function(x) length(x))))
    cat("age_selex_Nparms\n")
    print(unlist(lapply(age_selex_label, function(x) length(x))))
  }
# Selex parameters
  if(sum(length(unlist(size_selex_label))) > 0) {
    ctllist <- add_df(ctllist,
                      name = "size_selex_parms",
                      nrow = length(unlist(size_selex_label)),
                      ncol = 14,
                      col.names = lng_par_colnames,
                      comments = unlist(size_selex_label))
  }
# Age selex
  if(sum(length(unlist(age_selex_label))) > 0) {
    ctllist <- add_df(ctllist,
                      name = "age_selex_parms",
                      nrow = length(unlist(age_selex_label)),
                      ncol = 14,
                      col.names = lng_par_colnames,
                      comments = unlist(age_selex_label))
  }
  # Dirichlet MN pars -----
  # this is turned on in the data file.
  if(use_datlist == TRUE) {
    if(any(datlist$len_info$CompError == 1 ) | 
       any(datlist$age_info$CompError == 1)) {
      N_dirichlet_parms <-  max(c(datlist$len_info$ParmSelect, datlist$age_info$ParmSelect))
    }
  }
  if(N_dirichlet_parms > 0) {
    ctllist <- add_df(ctllist, 
                      name = "dirichlet_parms",
                      nrow = N_dirichlet_parms,
                      ncol = 14,
                      col.names = lng_par_colnames,
                      comments = paste0("ln(DM_theta)_", 1:N_dirichlet_parms))
  }
  
  # sel timevarying parlines----
  if(any(ctllist$size_selex_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[5] == 0) {
    warning("There are time varying size selectivity  parameters, and AUTOGEN ",
            "for selectivity is 0, so not expecting any short parameter lines.")
  }  
  if(any(ctllist$age_selex_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
        ctllist$time_vary_auto_generation[5] == 0) {
    warning("There are time varying size selectivity  parameters, and AUTOGEN ",
            "for selectivity is 0, so not expecting any short parameter lines.")
  }
  if(any(ctllist$size_selex_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[5] != 0) {
    tmp_parlab <- get_tv_parlabs(full_parms = ctllist[["size_selex_parms"]], ctllist[["Block_Design"]])
    ctllist <- add_df(ctllist, 
                      name = "size_selex_parms_tv", 
                      nrow = length(tmp_parlab), 
                      ncol = 7, 
                      col.names = srt_par_colnames,
                      comments = tmp_parlab)
  }
  if(any(ctllist$age_selex_parms[, c("env_var&link", "dev_link", "Block")] != 0) &
     ctllist$time_vary_auto_generation[5] != 0) {
    tmp_parlab <- get_tv_parlabs(full_parms = ctllist[["age_selex_parms"]], ctllist[["Block_Design"]])
    ctllist <- add_df(ctllist, 
                      name = "age_selex_parms_tv", 
                      nrow = length(tmp_parlab), 
                      ncol = 7, 
                      col.names = srt_par_colnames,
                      comments = tmp_parlab)
  }
  #2DAR ----
  ctllist<-add_elem(ctllist,name="Use_2D_AR1_selectivity")
  if (ctllist$Use_2D_AR1_selectivity == 1) {
    add_2dar <- function(x) {
      end <- x$.i
      while(length(grep("^-9999", x$.dat[end])) == 0) {
        end <- end + 1
      }
      n2dfleets <- length(x$.dat[x$.i:(end-1)]) / (11 + 3 * 7)
      par2D <- matrix(x$.dat[(end - 3*7*n2dfleets):(end-1)],
        nrow = 3*n2dfleets, byrow = TRUE)
      colnames(par2D) <- c("LO", "HI", "INIT", "PRIOR", "PR_SD", "PR_type", "PHASE")
      names2D <- c("sigma_sel", "rho_year", "rho_age")
      rownames(par2D) <- sprintf(paste0(names2D, ":%d"), 
        rep(1:n2dfleets, each=3))
      specs2D <- matrix(x$.dat[x$.i:(x$.i+10*n2dfleets)],
        nrow = n2dfleets, byrow = TRUE)
      colnames(specs2D) <- c("fleet", "ymin", "ymax", "amin", "amax", 
        "sig_amax", "use_rho", "l1/a2", "devphase", "before_range", "after_range")
      rownames(specs2D) <- paste0("2d_AR specs:", 1:n2dfleets)
      x[["specs_2D_AR"]] <- as.data.frame(specs2D, stringsAsFactors = FALSE)
      x[["pars_2D_AR"]] <- as.data.frame(par2D, stringsAsFactors = FALSE)
      x[[".i"]] <- end + 11
      return(x)
    }
    ctllist <- add_2dar(x = ctllist)
  }

# tagging ----  
  # TG_custom:  0=no read; 1=read if tags exist
  ctllist <- add_elem(ctllist, name = "TG_custom")
  if(ctllist$TG_custom) {
    #  The Following parameters are to be read
    #  For each SS tag release group
    # . one initial Tag loss parameter
    # . one continuos Tag loss parameter
    # . NB over-dispersion paramater
    # For each fleet
    # . one tag reporting rate paramater
    # . one tag reporting rate decay paramater
    #
    #  In total N_tag_groups*3+ Nfleets*2 parameters are needed to read
    ctllist <- add_df(ctllist,
                      name = "TG_Loss_init",
                      nrow = ctllist$N_tag_groups,
                      ncol = 14,
                      col.names = lng_par_colnames,
                      comments = 
                        paste0("_TG_Loss_init_", 1:ctllist$N_tag_groups))
    ctllist <- add_df(ctllist, 
                      name = "TG_Loss_chronic",
                      nrow = ctllist$N_tag_groups,
                      ncol = 14,
                      col.names = lng_par_colnames,
                      comments = 
                        paste0("#_TG_Loss_chronic_", 1:ctllist$N_tag_groups))
    ctllist <- add_df(ctllist,
                      name = "TG_overdispersion", 
                      nrow = ctllist$N_tag_groups,
                      ncol = 14, 
                      col.names = lng_par_colnames,
                      comments = 
                        paste0("#_TG_overdispersion_", 1:ctllist$N_tag_groups))
    ctllist <- add_df(ctllist, 
                      name = "TG_Report_fleet", 
                      nrow = ctllist$Nfleets, 
                      ncol = 14, 
                      col.names = lng_par_colnames,
                      comments = 
                        paste0("#_TG_report_fleet_par_",1:ctllist$Nfleets))
    ctllist <- add_df(ctllist,
                      name = "TG_Report_fleet_decay",
                      nrow = ctllist$Nfleets,
                      ncol = 14,
                      col.names=lng_par_colnames,
                      comments = 
                       paste0("#_TG_report_decay_par_", 1:ctllist$Nfleets))
  }
  
  # Var adj ----
  ctllist$DoVar_adjust <- 0
  # Create 3.30 variance adjustments and reset DoVar_adjust if true.
  ctllist <- add_df(ctllist,name="Variance_adjustment_list",nrow=NULL,ncol=3,
                    col.names=c("Factor","Fleet","Value"))
  if(!is.null(ctllist$Variance_adjustment_list)) ctllist$DoVar_adjust <- 1
  # create version 3.24 variance adjustments
  ctllist$Variance_adjustments<-as.data.frame(matrix(data=0,nrow=6,ncol=(Nfleets)))
  ctllist$Variance_adjustments[4:6,]<-1
  colnames(ctllist$Variance_adjustments)<-fleetnames
  rownames(ctllist$Variance_adjustments)<-paste0("#_",paste(c("add_to_survey_CV",
                                                        "add_to_discard_stddev",
                                                        "add_to_bodywt_CV",
                                                        "mult_by_lencomp_N",
                                                        "mult_by_agecomp_N",
                                                        "mult_by_size-at-age_N")))
  if(!is.null(ctllist$Variance_adjustment_list)) {
    if(nrow(ctllist$Variance_adjustment_list) > 0) {
      for(j in seq_len(nrow(ctllist$Variance_adjustment_list))){
        ctllist$Variance_adjustments[ctllist$Variance_adjustment_list[j,]$Factor,ctllist$Variance_adjustment_list[j,]$Fleet]<-
          ctllist$Variance_adjustment_list[j,]$Value
      }
    }
  }
  
  # Lambdas ----
  ctllist<-add_elem(ctllist,"maxlambdaphase") #_maxlambdaphase
  ctllist<-add_elem(ctllist,"sd_offset")  #_sd_offset

  ctllist<-add_df(ctllist,name="lambdas",nrow=NULL,ncol=5,
                  col.names=c("like_comp","fleet","phase","value","sizefreq_method"))
  
  if(!is.null(ctllist$lambdas)) ctllist$N_lambdas<-nrow(ctllist$lambdas)   # number of changes to make to default Lambdas
  else ctllist$N_lambdas<-0
  
  if(ctllist$N_lambdas>0){
    # find and delete duplicates
    chk1<-duplicated(ctllist$lambdas)
    if(any(chk1)) # there are duplicates
    {
      ctllist$lambdas<-ctllist$lambdas[!chk1,]
      ctllist$N_lambdas<-nrow(ctllist$lambdas)
      ctllist$warnings<-paste(ctllist$warnings,"Duplicate_lambdas",sep=",")
    }

    for(i in seq_len(ctllist$N_lambdas)){
      like_comp<-ctllist$lambdas[i,1]
      fl<-ctllist$lambdas[i,2]
      phz<-ctllist$lambdas[i,3]
      value<-ctllist$lambdas[i,4]
      sizefreq_method<-ctllist$lambdas[i,5]
      if(like_comp==1){
        rownames(ctllist$lambdas)[i]<-paste0("Surv_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==2){
        rownames(ctllist$lambdas)[i]<-paste0("discard_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==3){
        rownames(ctllist$lambdas)[i]<-paste0("mean-weight_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==4){
        rownames(ctllist$lambdas)[i]<-paste0("length_",fleetnames[fl],"_sizefreq_method_",sizefreq_method,"_Phz",phz)
      }else if(like_comp==5){
        rownames(ctllist$lambdas)[i]<-paste0("age_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==6){
        rownames(ctllist$lambdas)[i]<-paste0("SizeFreq_",fleetnames[fl],"_sizefreq_method_",sizefreq_method,"_Phz",phz)
      }else if(like_comp==7){
        rownames(ctllist$lambdas)[i]<-paste0("SizeAge_",fleetnames[fl],"_sizefreq_method_",sizefreq_method,"_Phz",phz)
      }else if(like_comp==8){
        rownames(ctllist$lambdas)[i]<-paste0("catch_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==9){
        rownames(ctllist$lambdas)[i]<-paste0("init_equ_catch_",fleetnames[fl],"_lambda_for_init_equ_catch_can_only_enable/disable for_all_fleets_Phz",phz)
      }else if(like_comp==10){
        rownames(ctllist$lambdas)[i]<-paste0("recrdev_Phz",phz)
      }else if(like_comp==11){
        rownames(ctllist$lambdas)[i]<-paste0("parm_prior_",fleetnames[fl],"_Phz",phz)
      }else if(like_comp==12){
        rownames(ctllist$lambdas)[i]<-paste0("parm_dev_Phz",phz)
      }else if(like_comp==13){
        rownames(ctllist$lambdas)[i]<-paste0("CrashPen_Phz",phz)
      }else if(like_comp==14){
        rownames(ctllist$lambdas)[i]<-paste0("Morph-Comp_Phz",phz)
      }else if(like_comp==15){
        rownames(ctllist$lambdas)[i]<-paste0("Tag-comp-likelihood-",fl,"_Phz",phz)
      }else if(like_comp==16){
        rownames(ctllist$lambdas)[i]<-paste0("Tag-negbin-likelihood-",fl,"_Phz",phz)
      }else if(like_comp==17){
        rownames(ctllist$lambdas)[i]<-paste0("F-ballpark-",fl,"_Phz",phz)
      }else if(like_comp==18){
        rownames(ctllist$lambdas)[i]<-paste0("Regime-shift-",fl,"_Phz",phz)
      }
    }
  }
  
  # more sd reporting ----
  # Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch;
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
  ctllist<-add_elem(ctllist,"more_stddev_reporting")  # (0/1) read specs for more stddev reporting
  if(ctllist$more_stddev_reporting != 0 ) {
    if(ctllist$more_stddev_reporting == 1) {
      ctllist<-add_vec(ctllist, name = "stddev_reporting_specs", length = 9)
    } else if(ctllist$more_stddev_reporting == 2) { # adds option for M
      ctllist<-add_vec(ctllist, name = "stddev_reporting_specs", length = 11)
    } else {
      stop("more_stddev_reporting read as ", ctllist$more_stddev_reporting, 
           ", but this is either not a valid SS option or not yet implemented ", 
           "in the SS_readctl function.")
    }
    ## Selex bin
    if(ctllist$stddev_reporting_specs[4] > 0) {
        ctllist<-
          add_vec(ctllist, name = "stddev_reporting_selex",
                  length=ctllist$stddev_reporting_specs[4])
    }
    ## Growth bin
    # if using wt at age, this is not read.
    if(ctllist$EmpiricalWAA != 0 & ctllist$stddev_reporting_specs[6] > 0) {
      warning("Additional stddev reporting being used with a model using ", 
              "empirical weight at age. Note that even if number of growth", 
              " ages > 0, SS will ignore these and not expect any input for ", 
              "the line stddev_reporting_growth. Changing ",
              "ctllist$stdev_reporting_specs[6] to 0 to make this clear.")
      ctllist$stddev_reporting_specs[6] <- 0
    }
    if(ctllist$stddev_reporting_specs[6] > 0  & ctllist$EmpiricalWAA == 0) {
        ctllist <- add_vec(ctllist,
                           name = "stddev_reporting_growth",
                           length = ctllist$stddev_reporting_specs[6])
    }
    ## N at age
    if(ctllist$stddev_reporting_specs[9] > 0) {
      ctllist<- add_vec(ctllist,
                        name = "stddev_reporting_N_at_A",
                        length = ctllist$stddev_reporting_specs[9])
    }
    # M at age - only available with more_stddev_reporting option 2 in 
    # SS>= 3.30.15
    if(ctllist$more_stddev_reporting == 2 && 
      ctllist$stddev_reporting_specs[11] > 0) {
        ctllist<- add_vec(ctllist,
                          name = "stddev_reporting_M_at_A",
                          length = ctllist$stddev_reporting_specs[11])
    }
  }
  if(ctllist$'.dat'[ctllist$'.i']==999){
    if(verbose) message("read of control file complete (final value = 999)\n")
    ctllist$eof <- TRUE
  }else{
    warning("Error: final value is", ctllist$'.dat'[ctllist$'.i'], " but ",
            "should be 999\n")
    ctllist$eof <- FALSE
  }
  ctllist$'.dat'<-NULL
  ctllist$'.i'<-NULL
  # return the result
  return(ctllist)
}

#' Get time varying parameter labels
#' 
#' function to add get the names of short time varying parameter lines
#' @param full_parms the dataframe with the full parameter lines in the control
#'  file as read in by r4ss.
#' @param block_design The block design in the control file as read in by r4ss.
get_tv_parlabs <- function(full_parms, 
                           block_design) {
  # Figure out parameters are time varying
  tmp_tv <- list(env   = full_parms[,"env_var&link"], 
                 dev   = full_parms[,"dev_link"],
                 block = full_parms[,"Block"])
  par_num <- lapply(tmp_tv, function(x) which(x != 0))
  loop_pars <- unlist(par_num)
  loop_pars <- loop_pars[order(loop_pars)]
  parlab <- vector() # a maybe faster approach is if we could initialize with the correct size.
  for(i in loop_pars) {
    tmp_parname <- rownames(full_parms)[i]
    # Add lines to the data frame as you go. (maybe can use the same approach as long parlines)
    if(i %in% par_num[["env"]]) {
      parlab <- c(parlab, paste0("# ", tmp_parname, "_ENV_add"))
    }
    if(i %in% par_num[["block"]]) {
      n_blk <- full_parms$Block[i]
      tmp_blk_design <- block_design[[n_blk]]
      # Get the start year for each block
      blk_start_yrs <- tmp_blk_design[seq(1, length(tmp_blk_design), by = 2)]
      parlab <- c(parlab,
                  paste0("# ", 
                         rep(tmp_parname,times = length(blk_start_yrs)),
                         "_BLK", n_blk, "add", blk_start_yrs))
    }
    if(i %in% par_num[["dev"]]) {
      # parameter name if there is devs
      parlab <- c(parlab, 
                  paste0("# ", 
                         rep(tmp_parname, times = 2), 
                         c("_dev_se", "_dev_autocorr")))
    }
  }
  invisible(parlab)
}

