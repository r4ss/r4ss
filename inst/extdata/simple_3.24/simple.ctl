#V3.21e
#C growth parameters are estimated
#C spawner-recruitment bias adjustment Not tuned For optimality
#_data_and_control_files: simple.dat // simple.ctl
#_SS-V3.21d-safe;_06/09/2011;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_10
1  #_N_Growth_Patterns
1 #_N_Morphs_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
#_Cond 0  #  N recruitment designs goes here if N_GP*nseas*area>1
#_Cond 0  #  placeholder for recruitment interaction request
#_Cond 1 1 1  # example recruitment design element for GP=1, seas=1, area=1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
0.5 #_fracfemale 
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=not implemented; 4=not implemented
0 #_Growth_Age_for_L1
25 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=read fec and wt from wtatage.ss
#_placeholder for empirical age-maturity by growth pattern
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
2 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
 0.05 0.15 0.1 0.1 -1 0.8 -3 0 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1
 -10 45 21.6552 36 0 10 2 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 40 90 71.6492 70 0 10 4 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.05 0.25 0.147282 0.15 0 0.8 4 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.05 0.25 0.1 0.1 -1 0.8 -3 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.05 0.25 0.1 0.1 -1 0.8 -3 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
 0.05 0.15 0.1 0.1 -1 0.8 -3 0 0 0 0 0 0 0 # NatM_p_1_Mal_GP_1
 1 45 0 36 -1 10 -3 0 0 0 0 0 0 0 # L_at_Amin_Mal_GP_1
 40 90 69.5361 70 0 10 4 0 0 0 0 0 0 0 # L_at_Amax_Mal_GP_1
 0.05 0.25 0.163516 0.15 0 0.8 4 0 0 0 0 0 0 0 # VonBert_K_Mal_GP_1
 0.05 0.25 0.1 0.1 -1 0.8 -3 0 0 0 0 0 0 0 # CV_young_Mal_GP_1
 0.05 0.25 0.1 0.1 -1 0.8 -3 0 0 0 0 0 0 0 # CV_old_Mal_GP_1
 -3 3 2.44e-006 2.44e-006 -1 0.8 -3 0 0 0 0 0 0 0 # Wtlen_1_Fem
 -3 4 3.34694 3.34694 -1 0.8 -3 0 0 0 0 0 0 0 # Wtlen_2_Fem
 50 60 55 55 -1 0.8 -3 0 0 0 0 0 0 0 # Mat50%_Fem
 -3 3 -0.25 -0.25 -1 0.8 -3 0 0 0 0 0 0 0 # Mat_slope_Fem
 -3 3 1 1 -1 0.8 -3 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
 -3 3 0 0 -1 0.8 -3 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
 -3 3 2.44e-006 2.44e-006 -1 0.8 -3 0 0 0 0 0 0 0 # Wtlen_1_Mal
 -3 4 3.34694 3.34694 -1 0.8 -3 0 0 0 0 0 0 0 # Wtlen_2_Mal
 0 0 0 0 -1 0 -4 0 0 0 0 0 0 0 # RecrDist_GP_1
 0 0 0 0 -1 0 -4 0 0 0 0 0 0 0 # RecrDist_Area_1
 0 0 0 0 -1 0 -4 0 0 0 0 0 0 0 # RecrDist_Seas_1
 0 0 0 0 -1 0 -4 0 0 0 0 0 0 0 # CohortGrowDev
#
#_Cond 0  #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-environ parameters
#
#_Cond 0  #custom_MG-block_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters
#_Cond No MG parm trends 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Cond -4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm
#_LO HI INIT PRIOR PR_type SD PHASE
 3 31 8.81544 10.3 -1 10 1 # SR_LN(R0)
 0.2 1 0.613717 0.7 1 0.05 4 # SR_BH_steep
 0 2 0.6 0.8 -1 0.8 -4 # SR_sigmaR
 -5 5 0.1 0 -1 1 -3 # SR_envlink
 -5 5 0 0 -1 1 -4 # SR_R1_offset
 0 0 0 0 -1 0 -99 # SR_autocorr
0 #_SR_env_link
0 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1971 # first year of main recr_devs; early devs can preceed this era
2001 # last year of main recr_devs; forecast devs start in following year
2 #_recdev phase 
1 # (0/1) to read 13 advanced options
 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 -4 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1900 #_last_early_yr_nobias_adj_in_MPD
 1900 #_first_yr_fullbias_adj_in_MPD
 2001 #_last_yr_fullbias_adj_in_MPD
 2002 #_first_recent_yr_nobias_adj_in_MPD
 1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -5 #min rec_dev
 5 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#DisplayOnly 0.127813 # Main_RecrDev_1971
#DisplayOnly -0.0629072 # Main_RecrDev_1972
#DisplayOnly 0.0999946 # Main_RecrDev_1973
#DisplayOnly -0.173914 # Main_RecrDev_1974
#DisplayOnly 0.0306906 # Main_RecrDev_1975
#DisplayOnly 0.714754 # Main_RecrDev_1976
#DisplayOnly -0.0229372 # Main_RecrDev_1977
#DisplayOnly 0.00347081 # Main_RecrDev_1978
#DisplayOnly 0.260891 # Main_RecrDev_1979
#DisplayOnly 0.173281 # Main_RecrDev_1980
#DisplayOnly 0.0891999 # Main_RecrDev_1981
#DisplayOnly -0.227374 # Main_RecrDev_1982
#DisplayOnly -0.440643 # Main_RecrDev_1983
#DisplayOnly -0.312905 # Main_RecrDev_1984
#DisplayOnly 0.391936 # Main_RecrDev_1985
#DisplayOnly 0.551136 # Main_RecrDev_1986
#DisplayOnly 0.218287 # Main_RecrDev_1987
#DisplayOnly 0.15476 # Main_RecrDev_1988
#DisplayOnly -0.384699 # Main_RecrDev_1989
#DisplayOnly 0.596713 # Main_RecrDev_1990
#DisplayOnly -0.68218 # Main_RecrDev_1991
#DisplayOnly -0.273103 # Main_RecrDev_1992
#DisplayOnly -0.829262 # Main_RecrDev_1993
#DisplayOnly 0.365425 # Main_RecrDev_1994
#DisplayOnly -0.604348 # Main_RecrDev_1995
#DisplayOnly 0.455566 # Main_RecrDev_1996
#DisplayOnly 1.11144 # Main_RecrDev_1997
#DisplayOnly -0.545922 # Main_RecrDev_1998
#DisplayOnly -0.65609 # Main_RecrDev_1999
#DisplayOnly 0.172111 # Main_RecrDev_2000
#DisplayOnly -0.301188 # Main_RecrDev_2001
#DisplayOnly 0 # ForeRecr_2002
#DisplayOnly 0 # ForeRecr_2003
#DisplayOnly 0 # ForeRecr_2004
#DisplayOnly 0 # ForeRecr_2005
#DisplayOnly 0 # ForeRecr_2006
#DisplayOnly 0 # ForeRecr_2007
#DisplayOnly 0 # ForeRecr_2008
#DisplayOnly 0 # ForeRecr_2009
#DisplayOnly 0 # ForeRecr_2010
#DisplayOnly 0 # ForeRecr_2011
#DisplayOnly 0 # Impl_err_2002
#DisplayOnly 0 # Impl_err_2003
#DisplayOnly 0 # Impl_err_2004
#DisplayOnly 0 # Impl_err_2005
#DisplayOnly 0 # Impl_err_2006
#DisplayOnly 0 # Impl_err_2007
#DisplayOnly 0 # Impl_err_2008
#DisplayOnly 0 # Impl_err_2009
#DisplayOnly 0 # Impl_err_2010
#DisplayOnly 0 # Impl_err_2011
#
#Fishing Mortality info 
0.3 # F ballpark for tuning early phases
-2001 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
2.9 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
4  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
 0 1 0 0.01 0 99 -1 # InitF_1FISHERY1
#
#_Q_setup
 # Q_type options:  <0=mirror, 0=median_float, 1=mean_float, 2=parameter, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm
 #_Den-dep  env-var  extra_se  Q_type
 0 0 0 0 # 1 FISHERY1
 0 0 1 2 # 2 SURVEY1
 0 0 0 2 # 3 SURVEY2
#
#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any)
# LO HI INIT PRIOR PR_type SD PHASE
 0 0.5 0 0.05 1 0 -4 # Q_extraSD_2_SURVEY1
 -7 5 0.515263 0 -1 1 1 # Q_base_2_SURVEY1
 -7 5 -6.62828 0 -1 1 1 # Q_base_3_SURVEY2
#
#_size_selex_types
#_Pattern Discard Male Special
 1 0 0 0 # 1 FISHERY1
 1 0 0 0 # 2 SURVEY1
 0 0 0 0 # 3 SURVEY2
#
#_age_selex_types
#_Pattern ___ Male Special
 11 0 0 0 # 1 FISHERY1
 11 0 0 0 # 2 SURVEY1
 11 0 0 0 # 3 SURVEY2
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
 19 80 53.6526 50 1 0.01 2 0 0 0 0 0 0 0 # SizeSel_1P_1_FISHERY1
 0.01 60 18.9204 15 1 0.01 3 0 0 0 0 0 0 0 # SizeSel_1P_2_FISHERY1
 19 70 36.6499 30 1 0.01 2 0 0 0 0 0 0 0 # SizeSel_2P_1_SURVEY1
 0.01 60 6.58921 10 1 0.01 3 0 0 0 0 0 0 0 # SizeSel_2P_2_SURVEY1
 0 40 0 5 -1 99 -1 0 0 0 0 0 0 0 # AgeSel_1P_1_FISHERY1
 0 40 40 6 -1 99 -1 0 0 0 0 0 0 0 # AgeSel_1P_2_FISHERY1
 0 40 0 5 -1 99 -1 0 0 0 0 0 0 0 # AgeSel_2P_1_SURVEY1
 0 40 40 6 -1 99 -1 0 0 0 0 0 0 0 # AgeSel_2P_2_SURVEY1
 0 40 0 5 -1 99 -1 0 0 0 0 0 0 0 # AgeSel_3P_1_SURVEY2
 0 40 0 6 -1 99 -1 0 0 0 0 0 0 0 # AgeSel_3P_2_SURVEY2
#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
#_Cond 0 #_custom_sel-blk_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
#_Cond No selex parm trends 
#_Cond -4 # placeholder for selparm_Dev_Phase
#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet: 1 2 3 
  0 0 0 #_add_to_survey_CV
  0 0 0 #_add_to_discard_stddev
  0 0 0 #_add_to_bodywt_CV
  1 1 1 #_mult_by_lencomp_N
  1 1 1 #_mult_by_agecomp_N
  1 1 1 #_mult_by_size-at-age_N
#
4 #_maxlambdaphase
1 #_sd_offset
#
3 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
#like_comp fleet/survey  phase  value  sizefreq_method
 1 2 2 1 1
 4 2 2 1 1
 4 2 3 1 1
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 #_CPUE/survey:_2
#  1 1 1 1 #_CPUE/survey:_3
#  1 1 1 1 #_lencomp:_1
#  1 1 1 1 #_lencomp:_2
#  0 0 0 0 #_lencomp:_3
#  1 1 1 1 #_agecomp:_1
#  1 1 1 1 #_agecomp:_2
#  0 0 0 0 #_agecomp:_3
#  1 1 1 1 #_size-age:_1
#  1 1 1 1 #_size-age:_2
#  0 0 0 0 #_size-age:_3
#  1 1 1 1 #_init_equ_catch
#  1 1 1 1 #_recruitments
#  1 1 1 1 #_parameter-priors
#  1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 #_crashPenLambda
1 # (0/1) read specs for more stddev reporting 
 1 1 -1 5 1 5 1 -1 5 # selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 5 15 25 35 43 # vector with selex std bin picks (-1 in first bin to self-generate)
 1 2 14 26 40 # vector with growth std bin picks (-1 in first bin to self-generate)
 1 2 14 26 40 # vector with NatAge std bin picks (-1 in first bin to self-generate)
999

