#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Tue Feb 28 13:44:05 2023
#_bootdata:_5
#C data file for simple example
#_bootstrap file: 3  irand_seed: 1677620645 first rand#: -1.17338
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
-999 1 1 4588.09 0.2
2011 1 1 9804.8 0.01
2012 1 1 10126.3 0.01
2013 1 1 10101.1 0.01
2014 1 1 10103.2 0.01
2015 1 1 9973.6 0.01
2016 1 1 10014.6 0.01
2017 1 1 10012.2 0.01
2018 1 1 8865.42 0.01
2019 1 1 7779.99 0.01
2020 1 1 7054.3 0.01
2021 1 1 5987.03 0.01
2022 1 1 3992.03 0.01
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
2013 7 2 76241.4 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 91801.9 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 40125.2 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 70465.5 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 9.68768 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 5.46306 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 18.0889 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 28.7809 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 4.7862 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 6.50361 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 7.84378 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 7.55385 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 4.93013 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 19.1314 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 7.62078 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 9.10292 0.7 #_orig_obs: 15.8286 SURVEY2
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
 2011 7 1 3 0 50  0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 1 1 2 3 6 2 0 1 0 0 0 0 0 0 0 2 0 1 0 0 2 4 2 1 1 3 5 2 4 1 2 1 0 0
 2012 7 1 3 0 50  0 0 0 1 0 0 0 0 0 1 0 0 0 2 1 2 2 2 4 2 2 3 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 2 2 4 3 2 2 1 3 3 1 0
 2013 7 1 3 0 50  0 0 0 0 0 0 0 1 0 1 1 4 2 1 4 3 1 3 1 7 3 2 0 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 3 2 2 1 2 1 0 1 0 0
 2014 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 1 2 3 2 1 1 2 1 0 0 0 0 0 0 1 0 0 0 0 0 0 3 2 3 3 3 3 3 5 1 3 2 1 0
 2015 7 1 3 0 50  0 0 0 0 0 1 0 0 0 2 0 2 1 1 0 4 3 1 1 1 6 3 0 0 0 0 0 0 0 0 1 0 0 0 1 2 1 0 2 1 0 1 3 1 5 4 0 1 1 0
 2016 7 1 3 0 50  0 0 0 1 1 0 1 0 0 2 1 3 1 2 0 1 2 0 1 3 2 2 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 2 3 1 5 4 4 3 3 0 0
 2017 7 1 3 0 50  0 0 0 0 0 0 1 0 0 0 0 3 0 1 2 3 3 0 3 5 1 0 0 0 0 0 0 0 1 0 0 0 2 0 0 1 2 0 3 4 0 1 6 1 2 0 4 0 1 0
 2018 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 1 0 4 3 1 3 2 2 3 1 3 2 0 0 0 0 0 0 1 1 0 0 0 1 0 0 2 2 2 2 0 1 2 2 3 4 1 0 0 0
 2019 7 1 3 0 50  0 0 0 0 0 1 0 0 0 0 1 2 1 2 2 1 2 4 1 4 2 2 1 0 0 0 0 0 0 0 0 0 0 0 2 1 0 0 3 2 0 2 4 2 3 3 1 1 0 0
 2020 7 1 3 0 50  0 0 0 0 0 0 1 0 1 0 3 1 0 2 1 1 3 4 0 2 1 1 2 0 0 0 0 0 0 1 1 1 1 0 2 1 3 2 2 1 3 0 2 1 2 3 0 0 1 0
 2021 7 1 3 0 50  0 0 0 0 1 0 1 0 0 1 1 3 1 1 3 0 4 3 4 0 0 2 0 0 0 1 0 0 0 1 0 0 0 0 1 0 4 0 2 4 1 2 0 1 3 2 1 1 1 0
 2022 7 1 3 0 50  0 1 0 0 0 0 2 0 0 0 1 1 1 2 2 2 2 2 2 4 2 1 0 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1 2 2 0 6 2 4 3 2 0 0 0 0
 2013 7 2 3 0 50  0 0 0 0 1 2 1 3 2 2 0 1 2 2 0 2 0 1 1 1 2 1 0 0 0 0 0 0 0 1 0 0 1 0 1 7 2 3 3 1 1 0 2 0 1 3 0 0 0 0
 2016 7 2 3 0 50  0 0 0 0 0 1 3 1 4 4 0 2 2 0 0 1 0 2 1 4 2 0 0 0 0 0 1 0 1 0 1 0 2 1 2 1 1 1 4 0 0 0 2 0 1 3 2 0 0 0
 2019 7 2 3 0 50  0 0 0 0 0 0 1 2 2 2 1 1 4 3 2 1 1 1 2 2 0 0 0 0 0 0 0 0 0 0 0 3 3 2 1 2 2 4 1 1 1 0 0 4 0 0 1 0 0 0
 2022 7 2 3 0 50  0 0 0 0 0 0 1 3 0 0 1 2 2 1 0 4 1 1 1 2 1 0 0 0 0 0 0 0 0 1 0 4 1 3 2 2 2 1 1 1 2 2 1 2 2 1 1 0 1 0
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
2011  7 1  3 0 2 1 -1 25  0 0 0 0 0 1 0 1 0 0 3 0 0 0 5 1 1 0 1 1 0 3 1 0 0 1 2 1 1 2
2012  7 1  3 0 2 1 -1 25  0 0 0 0 0 1 0 2 0 0 0 1 0 0 3 0 0 1 0 7 3 1 3 1 0 0 0 0 2 0
2013  7 1  3 0 2 1 -1 25  0 1 0 0 1 1 1 1 1 0 1 0 1 0 1 0 0 1 0 1 1 1 2 2 1 1 2 2 0 2
2014  7 1  3 0 2 1 -1 25  0 0 1 0 0 0 0 1 1 1 1 1 1 0 4 0 0 1 0 0 0 1 3 1 0 1 0 0 0 7
2015  7 1  3 0 2 1 -1 25  0 0 1 1 0 1 0 2 1 1 1 1 0 0 0 0 1 0 2 0 3 0 1 2 1 1 1 2 0 2
2016  7 1  3 0 2 1 -1 25  1 0 2 0 0 0 0 1 1 2 1 0 0 0 3 1 1 1 1 1 3 0 1 0 0 0 3 0 1 1
2017  7 1  3 0 2 1 -1 25  0 0 1 2 0 1 4 1 0 0 1 0 0 0 0 0 1 3 1 1 3 0 1 1 1 1 0 0 0 2
2018  7 1  3 0 2 1 -1 25  0 1 0 0 3 1 0 1 2 0 0 1 1 1 4 0 0 0 0 2 1 1 2 0 1 0 0 0 0 3
2019  7 1  3 0 2 1 -1 25  0 0 0 1 3 0 4 0 1 0 0 0 1 0 1 0 2 0 0 1 3 2 1 1 2 0 0 1 0 1
2020  7 1  3 0 2 1 -1 25  0 2 2 2 2 1 1 0 1 0 0 0 2 0 0 1 0 0 0 2 1 0 1 1 1 0 2 2 0 1
2021  7 1  3 0 2 1 -1 25  0 0 1 0 1 2 0 2 0 1 0 0 0 0 0 0 1 2 1 2 1 2 3 1 2 2 0 0 0 1
2022  7 1  3 0 2 1 -1 25  0 1 0 0 2 2 4 2 0 0 0 0 1 0 1 0 2 0 2 2 3 1 0 0 0 0 1 0 0 1
2013  7 2  3 0 2 1 -1 25  0 1 2 2 1 0 2 4 0 0 0 0 0 0 0 0 2 1 2 1 4 0 1 1 0 0 0 0 0 1
2016  7 2  3 0 2 1 -1 25  0 1 1 2 1 0 1 1 0 0 0 1 0 0 1 0 2 2 1 5 0 1 2 0 0 1 0 1 0 1
2019  7 2  3 0 2 1 -1 25  0 0 2 5 1 2 1 1 0 1 0 0 0 0 0 1 1 0 0 0 0 2 0 2 2 2 1 0 0 1
2022  7 2  3 0 2 1 -1 25  1 2 0 4 1 2 1 1 0 0 0 0 0 0 0 0 3 1 2 1 3 0 0 2 1 0 0 0 0 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 31.193 39.1639 44.1124 51.9164 52.0434 57.7003 58.8483 59.7804 60.1647 61.6211 64.0945 67.612 67.7564 65.9539 73.9855 31.4235 40.9479 48.378 49.7304 53.4287 53.943 59.9469 64.6827 64.3614 66.1802 65.1859 63.9016 71.3334 68.3366 71.2648
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 29.8948 37.2769 45.5953 46.6348 52.9272 53.4562 59.1033 56.028 63.3977 58.3972 63.3827 67.2044 68.9099 71.8912 70.6141 30.8329 37.9579 46.2392 49.8366 51.9021 54.8744 61.6949 61.159 63.3393 63.6307 63.1537 67.0087 65.1093 65.1788 67.7374
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 35.4136 39.7608 43.1449 51.1951 54.4699 54.0112 55.9927 54.3805 59.2528 60.8469 58.7615 64.7537 60.4983 69.2671 66.4615 33.6003 40.2638 44.8195 46.649 55.6542 52.2709 60.1911 54.5891 63.5523 66.1886 63.0397 61.7092 66.8907 76.3045 74.6553
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 35.0269 40.9647 44.9686 45.8818 50.8732 51.1841 50.9846 58.7256 61.9886 61.3917 60.4992 68.4512 63.2937 67.5633 72.1037 37.7391 39.4545 41.7567 49.9855 50.7869 53.5801 54.1579 62.7834 59.532 59.9281 64.8695 67.031 69.2211 69.305 68.8722
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

