#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Tue Feb 28 13:44:05 2023
#_bootdata:_10
#C data file for simple example
#_bootstrap file: 8  irand_seed: 1677620645 first rand#: 0.306277
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
-999 1 1 5323.27 0.2
2011 1 1 9919.46 0.01
2012 1 1 10053 0.01
2013 1 1 10128.1 0.01
2014 1 1 10031.8 0.01
2015 1 1 9924.96 0.01
2016 1 1 9925.95 0.01
2017 1 1 10174.4 0.01
2018 1 1 9113.64 0.01
2019 1 1 7855.92 0.01
2020 1 1 7070.5 0.01
2021 1 1 5975.83 0.01
2022 1 1 3994.79 0.01
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
2013 7 2 64561.5 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 65024.8 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 102698 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 67055.5 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 26.0883 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 9.79114 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 5.32839 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 15.3552 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 7.07044 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 22.253 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 10.6165 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 4.76244 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 20.7939 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 5.7871 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 6.43117 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 18.9979 0.7 #_orig_obs: 15.8286 SURVEY2
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
 2011 7 1 3 0 50  0 0 0 0 0 0 0 0 2 1 1 2 0 1 0 4 0 4 1 6 2 3 0 0 0 0 0 0 0 1 0 0 0 0 2 1 0 0 2 1 1 1 3 1 3 3 1 2 1 0
 2012 7 1 3 0 50  0 0 0 0 0 1 0 0 0 2 1 2 1 0 1 1 2 1 3 5 2 1 3 3 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 3 1 1 0 2 3 5 1 1 2 0
 2013 7 1 3 0 50  0 0 0 2 0 0 0 1 0 1 0 1 1 1 2 0 4 2 1 4 6 1 0 1 0 0 0 0 0 0 1 0 0 1 2 2 1 0 1 1 1 1 0 1 3 6 0 0 1 0
 2014 7 1 3 0 50  0 0 0 0 0 1 0 0 0 0 1 0 0 1 0 1 2 3 2 3 2 1 1 0 0 0 0 0 1 0 0 0 0 0 3 1 2 3 1 1 5 0 0 3 6 4 0 1 1 0
 2015 7 1 3 0 50  0 0 0 1 0 0 0 1 1 0 0 0 2 1 2 3 3 1 3 5 0 3 1 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 2 1 0 2 2 1 0 4 4 2 2 0
 2016 7 1 3 0 50  0 0 0 0 0 0 0 0 0 0 1 0 0 6 4 0 2 1 2 3 2 1 0 0 0 0 0 0 0 1 0 1 0 0 1 0 4 1 3 3 0 2 0 0 4 5 1 2 0 0
 2017 7 1 3 0 50  0 0 0 0 0 0 2 2 0 0 1 1 0 3 1 0 3 1 1 4 0 0 1 0 0 0 0 1 1 1 0 1 0 3 2 2 1 0 2 1 4 2 0 2 3 2 1 0 0 1
 2018 7 1 3 0 50  0 0 0 0 0 0 0 1 0 2 1 0 0 1 2 3 4 2 2 3 3 2 0 0 0 0 0 0 1 0 0 0 0 0 3 3 2 1 1 3 2 2 2 1 3 0 0 0 0 0
 2019 7 1 3 0 50  1 0 0 0 0 0 2 0 2 2 0 1 2 4 2 1 1 3 1 5 1 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 4 2 0 1 2 3 0 0 3 1 0
 2020 7 1 3 0 50  0 0 0 0 0 0 3 0 0 1 0 2 2 2 5 0 1 1 2 3 1 2 0 0 0 0 0 0 0 0 0 2 0 1 0 3 1 0 2 1 4 2 2 3 2 1 1 0 0 0
 2021 7 1 3 0 50  0 0 0 0 1 0 1 2 1 3 2 0 1 1 0 3 2 0 3 1 5 1 0 1 0 0 0 0 0 1 0 0 1 1 1 0 1 1 0 2 1 1 4 1 1 3 1 1 1 0
 2022 7 1 3 0 50  0 1 1 0 0 0 1 0 2 4 2 0 3 1 1 2 4 5 1 0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 2 3 2 1 0 3 3 2 1 0 0
 2013 7 2 3 0 50  0 1 1 1 0 0 2 1 3 0 2 0 2 2 2 3 2 0 0 4 1 0 0 0 0 0 0 0 1 0 3 0 1 2 3 2 2 1 1 1 3 0 2 1 0 0 0 0 0 0
 2016 7 2 3 0 50  0 0 0 1 0 1 2 1 1 0 1 1 2 1 2 3 3 2 1 3 1 1 0 0 0 0 0 0 0 0 2 1 5 1 1 2 1 1 0 2 1 1 0 0 3 1 1 0 0 0
 2019 7 2 3 0 50  0 0 0 0 0 1 1 1 1 2 1 0 1 3 2 0 1 0 0 4 1 1 0 0 0 0 0 0 1 0 0 2 1 4 6 3 1 3 0 2 1 1 2 0 1 2 0 0 0 0
 2022 7 2 3 0 50  0 0 0 0 0 1 1 1 3 3 2 1 1 3 2 1 2 0 1 4 1 1 1 0 0 0 0 0 0 0 1 0 3 3 2 1 2 0 0 1 2 0 1 1 3 1 0 0 0 0
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
2011  7 1  3 0 2 1 -1 25  0 0 2 1 0 1 4 0 1 0 1 0 0 0 3 0 1 1 1 1 0 0 1 1 2 0 0 1 0 3
2012  7 1  3 0 2 1 -1 25  0 0 0 1 1 1 2 2 0 1 0 1 1 0 2 0 0 0 1 0 1 1 1 2 2 0 0 1 0 4
2013  7 1  3 0 2 1 -1 25  0 0 1 1 1 1 1 1 1 0 0 0 1 0 5 0 0 1 0 1 1 3 3 2 0 0 0 1 0 0
2014  7 1  3 0 2 1 -1 25  0 0 0 0 0 1 3 2 1 0 2 2 0 0 0 0 0 1 0 1 0 0 3 0 1 1 1 3 0 3
2015  7 1  3 0 2 1 -1 25  0 0 0 2 3 1 1 0 0 1 2 2 2 0 2 0 0 0 0 1 3 2 0 1 0 0 0 0 0 2
2016  7 1  3 0 2 1 -1 25  0 0 1 0 0 2 2 4 2 2 0 0 1 0 2 0 0 0 0 0 1 1 1 1 0 0 1 0 2 2
2017  7 1  3 0 2 1 -1 25  0 0 1 1 1 3 0 1 1 1 0 1 0 0 1 0 1 0 2 1 1 1 1 1 0 2 1 1 0 2
2018  7 1  3 0 2 1 -1 25  1 1 0 2 1 2 1 1 2 1 0 1 0 0 3 1 0 0 0 1 1 1 0 3 0 2 0 0 0 0
2019  7 1  3 0 2 1 -1 25  1 0 0 0 1 1 2 2 1 2 0 1 1 0 2 0 0 0 1 3 1 2 1 0 0 0 0 1 0 2
2020  7 1  3 0 2 1 -1 25  0 0 1 2 1 3 0 1 1 1 0 1 0 0 3 0 1 4 0 0 0 0 0 3 0 0 1 0 1 1
2021  7 1  3 0 2 1 -1 25  0 1 0 2 0 0 1 0 0 0 1 1 2 0 1 0 0 3 1 1 2 3 0 0 0 2 0 1 1 2
2022  7 1  3 0 2 1 -1 25  0 0 0 0 2 0 3 0 0 1 0 0 1 0 0 0 1 0 1 1 4 4 3 0 1 2 0 1 0 0
2013  7 2  3 0 2 1 -1 25  0 2 0 3 1 3 1 1 0 0 0 0 0 0 0 3 0 1 0 0 4 1 1 0 0 3 0 0 0 1
2016  7 2  3 0 2 1 -1 25  0 1 1 2 6 1 0 1 1 0 1 0 0 0 1 0 1 2 1 2 0 1 1 0 1 0 0 0 0 1
2019  7 2  3 0 2 1 -1 25  0 3 1 1 0 0 1 1 0 0 0 1 0 1 1 0 2 3 3 1 1 2 2 0 0 0 0 0 0 1
2022  7 2  3 0 2 1 -1 25  1 2 3 2 2 0 1 1 1 0 0 0 0 0 1 2 2 0 3 0 0 0 1 1 0 1 1 0 0 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 33.701 42.3709 47.8245 50.1815 53.112 54.1972 59.2216 58.1697 66.0473 66.6387 60.8393 68.2605 68.385 69.7844 66.9154 31.8692 39.3493 47.4117 50.7724 51.6415 59.7464 62.2966 62.0384 65.6484 60.2338 69.3841 66.955 66.1634 65.564 70.5817
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 29.6184 38.4351 44.9793 51.4668 48.6287 53.3231 58.8747 62.9236 62.0297 66.5259 60.1713 67.5823 61.9359 67.8519 73.7473 27.9422 39.4036 42.3821 50.7748 54.0963 54.2309 54.7318 57.902 64.1811 61.8133 64.2717 69.2353 66.5534 67.4206 71.7738
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 36.8061 40.0196 43.3091 46.4359 50.5391 53.7504 54.4084 60.4786 61.9023 62.3422 61.9431 66.7996 67.1781 56.4696 69.7015 35.2809 42.1876 42.698 48.0765 45.7067 53.0725 55.14 63.9145 58.4894 58.8709 63.5292 62.7612 67.1928 69.9899 65.2816
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 33.7554 40.5001 42.1658 45.1258 50.8625 54.6118 57.9726 58.3152 61.1183 61.9031 67.9504 65.3114 64.3327 70.0183 69.8869 36.2947 39.4894 44.5837 46.7073 49.9635 54.6312 56.0702 61.3989 60.0952 59.1246 65.552 61.9447 64.6327 63.0555 62.8515
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

