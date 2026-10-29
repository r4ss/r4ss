#V3.30.26.00;_safe;_compile_date:_Oct 29 2026;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:_https://groups.google.com/g/ss3-forum_and_NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:_https://nmfs-ost.github.io/ss3-website/
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#_Start_time: Thu Oct 29 09:14:21 2026
#_bootstrap
#C data file for simple example
#_bootstrap file: 1  irand_seed: 1793290461 first rand#: 0.0219391
#V3.30.26.00;_safe;_compile_date:_Oct 29 2026;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
2011 #_StartYr
2022 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
2 #_Nsexes: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
20 #_Nages=accumulator age, first age is always age 0
1 #_Nareas
3 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=predator(M2) 
#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type fishery_timing area catch_units need_catch_mult fleetname
 1 -1 1 1 0 FISHERY  # 1
 3 1 1 2 0 SURVEY1  # 2
 3 1 1 2 0 SURVEY2  # 3
#Bycatch_fleet_input_goes_next
#a:  fleet index
#b:  1=include dead bycatch in total dead catch for F0.1 and MSY optimizations and forecast ABC; 2=omit from total catch for these purposes (but still include the mortality)
#c:  1=Fmult scales with other fleets; 2=bycatch F constant at input value; 3=bycatch F from range of years
#d:  F or first year of range
#e:  last year of range
#f:  not used
# a   b   c   d   e   f 
#_Catch data: year, seas, fleet, catch, catch_se
#_catch_se:  standard error of log(catch)
#_NOTE:  catch data is ignored for survey fleets
-999 1 1 4124.72 0.2
2011 1 1 10033.6 0.01
2012 1 1 10164.8 0.01
2013 1 1 10128.2 0.01
2014 1 1 10003.2 0.01
2015 1 1 9957 0.01
2016 1 1 9833.12 0.01
2017 1 1 10084.6 0.01
2018 1 1 9004.5 0.01
2019 1 1 7911.31 0.01
2020 1 1 6926.92 0.01
2021 1 1 6078.55 0.01
2022 1 1 3998.64 0.01
-9999 0 0 0 0
#
#_CPUE_and_surveyabundance_and_index_observations
#_units: 0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=exp(recdev); 36=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)
#_errtype:  -1=normal; 0=lognormal; 1=lognormal with bias correction; >1=df for T-dist
#_SD_report: 0=not; 1=include survey expected value with se
#_note that link functions are specified in Q_setup section of control file
#_dataunits = 36 and 35 should use Q_type 5 to provide offset parameter
#_fleet units errtype SD_report
1 1 0 0 # FISHERY
2 1 0 1 # SURVEY1
3 0 0 0 # SURVEY2
#_year month index obs err
2013 7 2 192636 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 85107.8 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 101382 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 68991.6 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 9.03199 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 6.25844 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 21.9648 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 4.58754 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 44.5577 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 16.872 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 6.68642 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 9.11952 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 10.1453 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 6.02394 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 5.0553 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 15.3961 0.7 #_orig_obs: 15.8286 SURVEY2
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note: only enter units and errtype for fleets with discard 
# note: discard data is the total for an entire season, so input of month here must be to a month in that season
#_Fleet units errtype
# -9999 0 0 0.0 0.0 # terminator for discard data 
#
0 #_use meanbodysize_data (0/1)
#_COND_0 #_DF_for_meanbodysize_T-distribution_like
# note:  type=1 for mean length; type=2 for mean body weight 
#_year month fleet part type obs stderr
#  -9999 0 0 0 0 0 0 # terminator for mean body size data 
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp 
10 # minimum size in the population (lower edge of first bin and size at age 0.00) 
94 # maximum size in the population (lower edge of last bin) 
1 # use length composition data (0/1/2) where 2 invokes new comp_comtrol format
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined sex below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  consecutive index for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_Using old format for composition controls
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.0001 0 0 0 0 0.1 #_fleet:1_FISHERY
-1 0.0001 0 0 0 0 0.1 #_fleet:2_SURVEY1
-1 0.0001 0 0 0 0 0.1 #_fleet:3_SURVEY2
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  (0=combined; 1=discard; 2=retained
25 #_N_LengthBins
 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 68 72 76 80 90
#_year month fleet sex part Nsamp datavector(female-male)
 2011 7 1 3 0 50  0 0 0 0 0 2 0 0 0 0 0 0 2 2 1 3 2 2 3 4 4 1 3 0 0 0 0 0 0 0 0 0 1 1 1 0 0 2 3 2 0 0 1 3 3 3 1 0 0 0
 2012 7 1 3 0 50  0 0 0 0 0 0 1 1 0 1 0 3 1 1 1 2 1 2 0 5 1 2 1 1 0 0 0 0 0 0 0 0 0 0 0 1 3 0 1 0 1 0 2 4 3 3 5 2 1 0
 2013 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 0 2 0 2 1 2 2 4 1 2 4 0 2 0 0 0 0 0 0 0 0 1 0 0 0 1 3 1 2 3 1 3 1 0 5 3 2 1 0 0
 2014 7 1 3 0 50  0 0 0 0 1 0 0 1 1 0 1 0 2 3 2 2 5 2 1 4 4 0 2 0 0 0 0 0 0 1 0 0 0 1 0 0 1 2 1 1 0 3 1 1 2 1 4 0 0 0
 2015 7 1 3 0 50  0 0 0 0 0 1 0 1 1 0 2 2 1 3 2 4 1 0 1 2 4 0 0 0 0 1 0 0 0 0 1 1 1 0 1 0 1 1 0 1 1 3 4 2 3 0 1 2 1 0
 2016 7 1 3 0 50  0 0 0 0 0 0 0 0 2 1 0 3 0 1 4 2 0 4 1 1 3 1 1 1 0 0 0 0 0 0 1 1 0 1 1 0 3 0 3 1 2 2 1 3 2 2 1 0 1 0
 2017 7 1 3 0 50  0 0 0 0 1 0 0 0 4 0 2 0 3 2 1 3 3 1 0 4 1 0 1 1 0 0 0 0 0 0 1 1 1 3 2 1 0 0 1 1 1 0 2 1 3 2 1 2 0 0
 2018 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 1 1 0 1 2 2 2 2 3 0 4 1 0 1 0 0 0 0 0 0 0 0 1 0 0 2 0 1 2 2 3 3 2 2 5 1 4 0 1 0
 2019 7 1 3 0 50  0 1 0 0 0 0 1 1 1 0 3 2 3 1 1 2 2 2 1 3 0 1 0 0 0 1 0 0 1 0 0 0 0 0 1 1 0 3 1 2 1 2 2 3 3 0 2 1 1 0
 2020 7 1 3 0 50  0 0 1 2 0 0 0 0 1 1 1 2 6 2 3 1 1 2 0 2 3 2 0 0 0 0 1 0 0 0 0 0 0 1 1 1 2 1 2 2 1 1 1 1 2 1 1 1 0 0
 2021 7 1 3 0 50  0 0 0 1 0 1 0 0 0 1 1 1 3 3 1 1 2 3 1 1 2 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 4 0 3 1 3 1 0 2 6 3 2 0 0 0
 2022 7 1 3 0 50  0 0 0 0 0 0 1 1 0 0 2 0 0 4 2 2 1 5 1 4 2 1 0 0 0 0 0 2 0 0 0 0 0 0 1 0 2 1 0 3 2 6 2 1 2 1 1 0 0 0
 2013 7 2 3 0 50  0 0 0 2 1 2 2 2 2 2 0 2 0 0 1 0 1 0 2 2 0 4 0 0 0 1 0 0 1 0 0 4 1 0 2 2 1 3 1 0 0 1 4 2 1 0 0 1 0 0
 2016 7 2 3 0 50  0 0 0 0 0 1 3 2 3 3 3 1 0 1 1 0 2 0 2 1 0 0 0 0 0 0 0 0 0 1 1 0 0 1 3 5 1 2 1 5 0 3 2 2 0 0 0 0 0 0
 2019 7 2 3 0 50  0 0 0 0 2 0 1 2 3 2 1 3 1 1 2 6 1 0 2 4 0 1 0 0 0 0 0 0 0 0 1 1 0 0 1 1 1 4 2 1 2 1 0 1 0 1 1 0 0 0
 2022 7 2 3 0 50  0 0 0 0 0 1 0 1 0 3 3 2 2 0 0 2 1 1 1 0 4 0 0 0 0 0 0 0 0 1 0 2 1 2 2 4 1 3 1 2 2 2 1 1 1 2 1 0 0 0
-9999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#
15 #_N_age_bins
 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
2 #_N_ageerror_definitions
 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 20.5
 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001 0.001
 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 20.5
 0.5 0.65 0.67 0.7 0.73 0.76 0.8 0.84 0.88 0.92 0.97 1.03 1.09 1.16 1.23 1.32 1.41 1.51 1.62 1.75 1.89
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined sex below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  consecutive index for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.0001 0 0 0 0 0.1 #_fleet:1_FISHERY
-1 0.0001 0 0 0 0 0.1 #_fleet:2_SURVEY1
-1 0.0001 0 0 0 0 0.1 #_fleet:3_SURVEY2
1 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_year month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)
2011  7 1  3 0 2 1 -1 25  0 0 0 0 1 0 1 2 1 1 0 0 0 0 4 0 2 1 0 1 0 0 1 1 0 1 1 2 1 4
2012  7 1  3 0 2 1 -1 25  0 1 1 1 2 0 3 1 1 0 0 0 2 0 0 0 0 1 0 0 4 1 1 0 0 0 0 1 0 5
2013  7 1  3 0 2 1 -1 25  0 0 0 1 0 1 3 0 0 0 0 0 0 0 3 0 0 1 1 3 0 0 2 0 0 0 1 0 3 6
2014  7 1  3 0 2 1 -1 25  1 1 2 0 1 0 0 0 0 0 0 0 1 1 2 0 0 0 1 0 1 3 0 1 0 1 2 0 1 6
2015  7 1  3 0 2 1 -1 25  0 0 0 0 2 2 2 1 1 1 0 2 0 0 3 0 0 1 0 2 1 1 0 1 1 0 0 1 1 2
2016  7 1  3 0 2 1 -1 25  0 0 0 1 1 1 1 1 1 0 1 2 1 1 1 0 0 1 3 0 2 1 2 0 0 0 0 0 0 4
2017  7 1  3 0 2 1 -1 25  1 0 3 0 0 1 1 0 0 1 0 0 2 0 0 0 2 1 0 1 1 3 2 0 0 0 0 2 0 4
2018  7 1  3 0 2 1 -1 25  0 1 0 0 0 0 2 4 0 1 1 0 0 1 2 0 0 1 1 0 0 1 2 2 2 1 0 0 1 2
2019  7 1  3 0 2 1 -1 25  0 0 2 2 2 1 1 2 0 1 0 0 1 0 0 0 0 2 1 3 0 1 0 4 0 0 0 0 0 2
2020  7 1  3 0 2 1 -1 25  0 1 2 1 2 0 0 1 2 0 1 0 0 0 0 0 0 0 2 2 0 3 4 2 0 2 0 0 0 0
2021  7 1  3 0 2 1 -1 25  0 0 2 2 1 1 2 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 4 1 2 0 0 2 0 0
2022  7 1  3 0 2 1 -1 25  0 0 1 1 1 4 1 1 0 0 1 1 0 0 2 0 1 2 0 1 1 2 1 1 1 0 1 1 0 0
2013  7 2  3 0 2 1 -1 25  0 1 1 1 2 1 0 0 1 0 1 1 1 1 1 3 0 0 1 3 2 0 0 0 0 1 0 1 0 2
2016  7 2  3 0 2 1 -1 25  1 0 2 2 0 1 1 0 0 0 0 0 0 0 0 1 3 4 3 2 2 0 0 1 0 0 0 1 0 1
2019  7 2  3 0 2 1 -1 25  3 2 0 0 3 2 0 1 0 0 1 0 0 0 1 0 1 1 2 1 0 1 2 0 2 0 1 0 0 1
2022  7 2  3 0 2 1 -1 25  0 2 1 2 2 1 0 1 0 2 0 0 0 0 1 1 1 1 2 1 0 1 4 1 0 1 0 0 0 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_year month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 34.2723 39.2112 46.1263 48.0253 52.6239 51.8878 60.1768 61.0648 63.2599 59.5374 63.8943 66.8299 62.4343 70.26 74.2673 29.3894 37.4962 39.8964 52.8009 52.8525 58.3155 63.336 57.6296 59.3916 60.8839 63.0119 66.5547 70.6207 67.5452 72.8892
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 30.9586 39.3493 47.6529 45.4621 54.5022 53.6372 55.924 59.036 63.8658 67.7212 68.8231 63.2041 67.0005 66.9035 71.8319 32.1267 40.8643 46.6104 48.8746 52.0646 55.9955 57.1328 58.7925 60.1463 66.0603 66.4417 68.1068 63.5864 71.3359 68.5322
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 37.4944 40.787 41.9739 47.9689 52.0533 53.1244 55.9527 54.8478 62.9734 60.9855 64.2645 60.1952 64.2466 67.6646 71.1006 35.8968 37.1713 43.9952 46.8506 48.5278 54.4499 57.7171 62.2973 56.3288 59.8203 63.3631 64.1186 69.0423 64.4766 69.0191
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 35.1108 40.6354 45.4469 46.2786 46.3424 55.9448 58.0802 58.5488 57.8291 58.6785 65.447 67.1073 66.9973 72.804 67.9229 38.0006 36.2542 43.4563 47.9039 49.2963 55.5913 57.574 55.8197 62.198 62.1188 59.2358 68.4716 66.7757 63.5456 71.8116
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_N_environ_variables
# -2 in year will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#_year variable value
#
# Sizefreq data. Defined by method because a fleet can use multiple methods
0 # N sizefreq methods to read (or -1 for expanded options)
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#_year, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
#_year, seas, fleet, age/size, bin, selex_prior, prior_sd
# feature not yet implemented
#
999

