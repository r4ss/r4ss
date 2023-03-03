#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Tue Feb 28 13:44:05 2023
#_bootdata:_3
#C data file for simple example
#_bootstrap file: 1  irand_seed: 1677620645 first rand#: -0.34011
#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
2011 #_StartYr
2022 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
2 #_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
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
#_catch_biomass(mtons):_columns_are_fisheries,year,season
#_catch:_columns_are_year,season,fleet,catch,catch_se
#_Catch data: yr, seas, fleet, catch, catch_se
-999 1 1 3409.54 0.2
2011 1 1 9916.52 0.01
2012 1 1 9873.4 0.01
2013 1 1 10077.9 0.01
2014 1 1 10023.1 0.01
2015 1 1 10184.2 0.01
2016 1 1 9754.11 0.01
2017 1 1 10201.2 0.01
2018 1 1 8946.36 0.01
2019 1 1 7909.71 0.01
2020 1 1 7144.75 0.01
2021 1 1 5932.55 0.01
2022 1 1 4052.77 0.01
-9999 0 0 0 0
#
 #_CPUE_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_SD_Report: 0=no sdreport; 1=enable sdreport
#_Fleet Units Errtype SD_Report
1 1 0 0 # FISHERY
2 1 0 1 # SURVEY1
3 0 0 0 # SURVEY2
#_year month index obs err
2013 7 2 137980 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 58108.4 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 33919.4 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 74570.7 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 8.51273 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 20.4924 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 4.03336 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 17.5192 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 5.39849 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 5.16863 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 9.84962 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 16.8969 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 32.7503 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 13.0082 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 8.50488 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 26.7545 0.7 #_orig_obs: 15.8286 SURVEY2
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
#_yr month fleet part type obs stderr
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
#_combM+F: males and females treated as combined gender below this bin number 
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
25 #_N_LengthBins
 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 68 72 76 80 90
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_yr month fleet sex part Nsamp datavector(female-male)
 2011 7 1 3 0 50  0 0 0 0 0 1 0 1 0 0 0 2 3 0 2 1 2 5 1 3 3 2 0 0 0 0 0 0 0 0 0 0 1 0 0 2 0 4 2 1 4 2 0 4 2 1 1 0 0 0
 2012 7 1 3 0 50  0 1 0 0 0 1 0 1 0 1 0 2 3 1 3 1 3 3 3 6 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 1 3 2 1 3 0 3 1 1 0 0 0
 2013 7 1 3 0 50  0 1 0 0 0 0 1 0 1 0 4 0 1 2 3 1 2 1 1 4 2 2 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 3 2 1 1 2 2 3 2 5 0 0 0
 2014 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 0 0 1 1 3 4 3 4 1 1 2 2 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 2 0 1 3 1 1 2 8 5 1 1 0 0
 2015 7 1 3 0 50  0 0 1 1 1 0 1 0 0 1 1 1 2 1 1 3 1 1 1 6 4 1 0 2 0 0 0 0 0 1 0 1 0 0 0 1 1 0 3 1 0 2 3 2 2 2 1 0 0 0
 2016 7 1 3 0 50  0 0 0 1 1 1 0 0 0 0 3 1 0 3 1 3 2 4 3 1 3 0 0 0 0 1 0 0 0 0 0 1 0 0 2 2 3 1 0 1 2 2 0 1 3 2 1 0 1 0
 2017 7 1 3 0 50  0 0 0 1 0 0 0 0 1 0 1 2 2 1 3 0 2 2 2 8 4 1 0 0 0 0 0 0 1 0 0 0 0 1 1 0 2 1 2 1 1 2 0 3 3 2 0 0 0 0
 2018 7 1 3 0 50  0 0 0 0 0 0 2 1 0 1 4 0 2 1 0 3 3 3 1 2 1 2 1 0 0 0 0 0 0 0 0 1 1 1 0 1 0 1 1 0 3 1 1 2 4 3 2 1 0 0
 2019 7 1 3 0 50  1 0 0 0 0 1 0 0 0 1 1 2 3 1 1 1 1 2 0 3 0 5 0 0 0 0 0 0 0 0 0 0 0 0 2 0 2 1 2 1 0 4 1 4 2 4 3 1 0 0
 2020 7 1 3 0 50  0 0 0 1 1 0 1 0 0 2 0 2 1 2 1 1 1 3 4 3 1 1 1 0 0 0 0 0 0 0 2 0 1 0 1 3 0 1 1 1 3 3 0 1 4 2 1 0 0 0
 2021 7 1 3 0 50  1 0 0 0 0 1 2 0 1 1 0 3 1 1 1 3 2 2 1 4 3 2 0 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 2 2 1 2 0 2 5 3 0 0 0 0
 2022 7 1 3 0 50  0 0 0 1 0 0 0 0 1 1 0 2 1 1 1 1 5 0 0 4 2 1 0 1 0 0 0 0 3 0 0 0 0 3 1 0 1 0 1 3 2 2 2 1 4 2 1 2 0 0
 2013 7 2 3 0 50  0 0 0 0 0 0 1 0 4 3 2 1 0 1 1 1 0 1 0 1 5 2 0 0 0 0 0 0 0 0 2 1 1 1 2 2 0 3 1 0 6 2 1 0 3 1 0 1 0 0
 2016 7 2 3 0 50  0 0 0 0 1 0 1 3 1 0 2 2 1 2 1 1 1 1 0 2 0 1 1 0 0 0 0 0 0 0 0 0 2 0 3 2 5 5 2 0 3 0 3 0 3 0 0 1 0 0
 2019 7 2 3 0 50  0 0 0 0 1 1 0 2 3 1 2 0 1 1 1 2 0 0 2 2 0 0 0 0 0 0 0 1 1 0 1 1 1 7 2 1 1 3 3 4 2 0 1 0 0 2 0 0 0 0
 2022 7 2 3 0 50  0 0 0 1 0 3 0 2 2 2 4 3 1 3 1 1 3 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0 3 2 0 2 1 2 1 0 2 1 2 2 0 1 0 1 0 0
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
#_combM+F: males and females treated as combined gender below this bin number 
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
#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)
2011  7 1  3 0 2 1 -1 25  0 0 2 0 0 1 1 1 3 2 0 1 0 2 1 0 0 0 1 1 1 1 0 0 2 1 1 2 0 1
2012  7 1  3 0 2 1 -1 25  1 2 0 0 1 2 1 0 3 0 1 1 0 0 1 0 2 0 1 1 0 3 0 0 2 1 0 0 1 1
2013  7 1  3 0 2 1 -1 25  0 1 1 3 0 0 2 0 1 1 2 1 3 0 1 0 0 0 3 0 0 0 2 0 0 1 0 0 2 1
2014  7 1  3 0 2 1 -1 25  0 0 0 2 2 0 0 1 1 0 1 1 0 2 1 0 1 1 0 2 1 1 0 0 2 0 1 1 0 4
2015  7 1  3 0 2 1 -1 25  0 2 0 1 1 1 0 0 0 1 1 0 0 0 4 0 0 1 0 2 2 1 1 0 1 1 1 1 0 3
2016  7 1  3 0 2 1 -1 25  0 1 1 2 1 0 0 0 0 1 0 0 0 1 2 1 0 1 0 2 1 0 1 1 1 1 2 2 0 3
2017  7 1  3 0 2 1 -1 25  0 0 0 1 1 1 0 1 2 2 0 0 1 0 1 1 0 0 1 0 1 4 1 1 1 1 0 1 1 2
2018  7 1  3 0 2 1 -1 25  0 1 1 4 2 2 1 1 2 0 0 1 0 0 0 0 1 0 0 2 2 0 1 1 0 1 0 1 1 0
2019  7 1  3 0 2 1 -1 25  1 0 2 2 1 2 1 0 1 0 0 1 0 0 1 1 0 3 0 2 3 0 2 1 0 0 0 0 0 1
2020  7 1  3 0 2 1 -1 25  0 0 0 2 0 4 1 1 1 0 0 0 1 0 0 0 0 4 1 0 3 0 3 2 1 1 0 0 0 0
2021  7 1  3 0 2 1 -1 25  0 0 0 1 1 2 0 1 2 1 0 1 1 0 1 1 0 0 1 2 1 2 2 2 1 0 1 0 1 0
2022  7 1  3 0 2 1 -1 25  1 1 2 3 3 0 2 0 1 1 1 1 0 1 1 1 0 0 1 1 1 1 2 0 0 0 0 0 0 0
2013  7 2  3 0 2 1 -1 25  2 0 1 0 0 2 1 2 0 0 1 0 1 1 0 0 1 1 3 2 0 1 1 3 1 0 1 0 0 0
2016  7 2  3 0 2 1 -1 25  0 2 2 1 0 1 0 1 2 2 1 0 1 0 0 1 0 1 1 4 2 0 1 2 0 0 0 0 0 0
2019  7 2  3 0 2 1 -1 25  0 0 0 4 1 2 1 2 0 1 0 1 0 0 0 0 1 1 4 2 1 0 0 2 0 0 0 0 0 2
2022  7 2  3 0 2 1 -1 25  1 1 1 1 1 3 0 0 1 1 0 1 0 0 1 0 2 2 2 0 2 1 0 0 1 2 0 0 1 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 32.8825 39.2695 45.9173 48.5713 51.1626 57.1421 58.1227 58.8299 60.4261 60.4647 63.2659 65.289 66.2336 69.7947 72.2087 30.7856 42.9951 40.9492 48.4579 56.4144 49.7241 57.771 61.3854 58.3284 64.9646 64.1707 71.0845 73.4437 68.1756 69.1855
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 30.1449 40.3524 45.1516 47.3694 52.185 56.34 58.5093 63.8373 63.406 65.4427 64.9815 67.8515 69.0314 69.2582 69.2341 27.93 39.7614 41.9513 48.8708 53.6364 57.0885 57.2448 58.0703 63.7672 65.0098 63.62 60.949 68.4948 62.4898 68.7292
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 36.2867 39.9912 43.5078 45.4485 49.7887 53.9694 55.4816 57.8705 62.5697 63.5608 60.6517 67.6629 65.7414 69.0798 72.2994 34.1978 36.4511 44.5061 47.9729 46.7788 56.9646 60.6328 59.4798 65.034 62.6869 59.3057 63.4198 60.3777 67.3679 73.1289
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 36.7221 36.804 41.1153 47.4056 50.9581 54.3473 56.9535 52.2227 56.1596 65.5149 59.4022 63.9374 70.1288 65.3967 62.9125 36.0588 36.0381 39.8029 49.0036 49.3316 52.9527 54.5451 56.4964 60.8483 59.0934 62.9501 66.8575 68.906 70.4597 74.3774
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_N_environ_variables
# -2 in yr will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#Yr Variable Value
#
# Sizefreq data. Defined by method because a fleet can use multiple methods
0 # N sizefreq methods to read (or -1 for expanded options)
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#  yr, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
 # Yr, Seas, Fleet,  Age/Size,  Bin,  selex_prior,  prior_sd
 # feature not yet implemented
#
999

