#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#C starter file written by R function SS_writestarter
#C rerun model to get more complete formatting in starter.ss_new
#C should work with SS version: 3.30
#C file write time: 2022-06-15 15:53:34
data.ss
control.ss
0 # 0=use init values in control file; 1=use ss.par
0 # run display detail (0,1,2)
1 # detailed output (0=minimal for data-limited, 1=high (w/ wtatage.ss_new), 2=brief, 3=custom) 
# custom report options: -100 to start with minimal; -101 to start with all; -number to remove, +number to add, -999 to end
0 # write 1st iteration details to echoinput.sso file (0,1) 
0 # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)
0 # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)
1 # Include prior_like for non-estimated parameters (0,1) 
1 # Use Soft Boundaries to aid convergence (0,1) (recommended)
#
3 # Number of datafiles to produce:  0 turns off all *.ss_new; 1st is data_echo.ss_new, 2nd is data_expval.ss, 3rd and higher are data_boot_**N.ss,
10 # Turn off estimation for parameters entering after this phase
#
0 # MCeval burn interval
1 # MCeval thin interval
0 # jitter initial parm value by this fraction
-1 # min yr for sdreport outputs (-1 for styr); #_2009
-2 # max yr for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs); #_2032
0 # N individual STD years 
#vector of year values 

0.0001 # final convergence criteria (e.g. 1.0e-04) 
0 # retrospective year relative to end year (e.g. -4)
1 # min age for calc of summary biomass
1 # Depletion basis:  denom is: 0=skip; 1=X*SPBvirgin; 2=X*SPBmsy; 3=X*SPB_styr; 4=X*SPB_endyr; 5=X*dyn_Bzero;  values>=11 invoke N multiyr (up to 9!) with 10's digit; >100 invokes log(ratio)
1 # Fraction (X) for Depletion denominator (e.g. 0.4)
1 # SPR_report_basis:  0=skip; 1=(1-SPR)/(1-SPR_tgt); 2=(1-SPR)/(1-SPR_MSY); 3=(1-SPR)/(1-SPR_Btarget); 4=rawSPR
3 # F_reporting_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Apical_F's); 4=true F for range of ages; 5=unweighted avg. F for range of ages
#COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4 or 5
0 # F_std_basis: 0=raw_annual_F; 1=F/Fspr; 2=F/Fmsy; 3=F/Fbtgt; where F means annual_F; values >=11 invoke N multiyr (up to 9!) with 10's digit; >100 invokes log(ratio)
0 # MCMC output detail: integer part (0=default; 1=adds obj func components; 2= +write_report_for_each_mceval); and decimal part (added to SR_LN(R0) on first call to mcmc)
0 # ALK tolerance ***disabled in code (example 0.0001)
-1 # random number seed for bootstrap data (-1 to use long(time) as seed): # 1677620645
3.30 # check value for end of file and for version control
