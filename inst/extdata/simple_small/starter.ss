#V3.30.24.00;_safe;_compile_date:_Sep 10 2025;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:_https://groups.google.com/g/ss3-forum_and_NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:_https://nmfs-ost.github.io/ss3-website/
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#C starter file written by R function SS_writestarter
#C rerun model to get more complete formatting in starter.ss_new
#C should work with SS version: 3.30
#C file write time: 2022-06-15 15:53:34
data.ss #_datfile
control.ss #_ctlfile
0 #_init_values_src:  0 (use init values in control file); 1 (use ss3.par)
0 #_screen_display:  0 (minimal); 1 (one line per iter); 2 (each logL)
1 #_report_table_selection:  0 (minimal; no wtatage.ss_new); 1 (all tables); 2 (brief), 3 (custom, read list) 
# COND: custom report options: -100 to start with minimal; -101 to start with all; -number to remove, +number to add, -999 to end
0 #_checkup:  write more 1st iteration age-specific details to echoinput.sso file (0,1) 
0 #_parmtrace:  write parm values to ParmTrace.sso:  0 (no); 1 (good_iter,active_parms); 2 (good,all); 3 (every,all); 4 (every,active)
0 #_cumreport:  write to cumreport.sso: 0 (no); 1 (like&timeseries); 2 (add survey fits)
1 #_prior_like:  include prior_like for non-estimated parameters (0,1) 
1 #_soft_bounds:  Use Soft Boundaries to aid convergence (0,1) (recommended)
#
3 #_N_bootstraps:  Number of datafiles to produce:  0 turns off all *.ss_new; 1st is data_echo.ss_new, 2nd is data_expval.ss, 3rd and higher are data_boot_**N.ss,
10 #_last_estimation_phase:  turn off estimation for parameters entering after this phase
#
0 #_MCMCburn
1 #_MCMCthin
0 # jitter_fraction:  jitter within parameter bounds
-1 #_minyr_sdreport:  min year for sdreport outputs (-1 for styr); #_2009
-2 #_maxyr_sdreport:  max year for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs); #_2032
0 #_N_STD_yrs:  N individual STD years 
#COND: vector of year values if N>0

0.0001 #_converge_criterion: (e.g. 1.0e-04) 
0 #_retro_yr:  retrospective year relative to end year (e.g. -4)
1 #_min_age_summary_bio
1 #_depl_basis:  Depletion denom is: 0=skip; 1=X*SSB_virgin; 2=X*SSB_msy; 3=X*SSB_styr; 4=X*SSB_endyr; 5=X*dyn_Bzero; 6=X*SSB_unf_bmark; values>=11 invoke N multiyr with 10s & 100s digit; append .1 to invoke log(ratio); e.g. 122.1 produces log(12 yr trailing average of B/Bmsy)
# If value = 1, then Btarget in benchmark will be a fraction of SSB_virgin, else will be a fraction of SSB_benchmark
1 #_depl_denom_frac:  fraction (X) for Depletion denominator (e.g. 0.4)
1 #_SPR_basis:  0 (skip); 1 (1-SPR)/(1-SPR_tgt); 2 (1-SPR)/(1-SPR_MSY); 3 (1-SPR)/(1-SPR_Btarget); 4 (1-SPR); 5 (SPR)
3 # F_std_units:  0 (skip); 1 (exploitation(Bio)); 2 (exploitation(Num)); 3 (sum(Apical_F's)); 4 (mean F for range of ages (numbers weighted)); 5 (unweighted mean F for range of ages)
#COND 10 15 #_min and max age over which mean F will be calculated with F_reporting=4 or 5
0 # F_std_basis: 0=no scaling; 1 (F/Fspr); 2 (F/Fmsy); 3 (F/Fbtgt); where F means annual F_std, Fmsy means F_std@msy; values >=11 invoke N multiyr using 10s and 100s digit; append .1 to invoke log(ratio)
0 #_MCMC_output_detail: integer part (0=default; 1=adds obj func components; 2= +write_report_for_each_mceval); and decimal part (added to SR_LN(R0) on first call to mcmc)
0 #_deprecated:  ALK tolerance ***disabled in code
-1 #_seed:  random number seed for bootstrap data (-1 to use long(time) as seed): # 1757545233
0 #_Compatibility:  flag for legacy (0) vs improved (1) impact of timevary biology on benchmark SRR calcs >=3.30.24
3.30 #_final:  check value for end of file and for version control
