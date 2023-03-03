#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Tue Feb 28 13:44:05 2023
#_bootdata:_4
#C data file for simple example
#_bootstrap file: 2  irand_seed: 1677620645 first rand#: -0.384923
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
-999 1 1 4703.71 0.2
2011 1 1 9940.78 0.01
2012 1 1 10125.1 0.01
2013 1 1 10293.1 0.01
2014 1 1 9900.22 0.01
2015 1 1 10191.2 0.01
2016 1 1 9855.67 0.01
2017 1 1 10203.3 0.01
2018 1 1 8927 0.01
2019 1 1 7944.08 0.01
2020 1 1 6871.23 0.01
2021 1 1 5976.65 0.01
2022 1 1 4016.39 0.01
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
2013 7 2 163822 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 84859.8 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 49913.9 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 41436.8 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 9.47742 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 6.03431 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 5.9735 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 6.99308 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 95.3411 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 20.4374 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 11.9928 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 22.4307 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 17.6025 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 14.1934 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 6.82021 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 9.95527 0.7 #_orig_obs: 15.8286 SURVEY2
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
 2011 7 1 3 0 50  0 0 0 0 1 0 0 1 0 0 0 0 2 0 1 0 4 0 2 5 1 5 1 0 0 0 0 0 0 0 2 0 2 0 1 1 1 2 2 0 1 0 1 4 6 2 1 0 1 0
 2012 7 1 3 0 50  0 0 1 0 0 0 0 1 1 2 0 1 0 0 1 1 4 2 1 1 7 1 4 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 6 1 1 1 1 3 2 3 2 0 1 0
 2013 7 1 3 0 50  0 0 0 1 0 0 0 0 0 0 1 0 0 1 0 0 2 2 2 5 5 2 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 2 4 2 1 0 0 7 5 1 2 2 0 1
 2014 7 1 3 0 50  0 0 0 0 0 0 0 1 0 2 0 0 1 1 4 3 3 0 0 6 2 1 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 1 1 2 1 2 4 7 2 3 0 0 0
 2015 7 1 3 0 50  0 0 0 0 0 0 0 1 0 2 1 1 1 1 2 5 1 2 1 1 5 3 1 1 0 0 0 0 0 1 1 0 0 0 0 0 0 2 1 1 1 1 4 1 1 4 2 0 1 0
 2016 7 1 3 0 50  0 0 0 0 0 0 0 1 1 1 1 1 3 3 2 0 3 2 3 5 1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 2 3 1 2 6 0 0 1 1 2 2 1 0
 2017 7 1 3 0 50  0 0 0 0 1 1 1 0 0 1 0 1 1 0 0 0 0 5 3 3 1 1 1 1 0 0 0 0 0 0 0 0 0 2 0 2 2 2 3 1 1 2 3 2 5 2 1 1 0 0
 2018 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 0 2 1 3 2 1 3 2 3 5 1 0 0 0 0 2 0 0 1 1 0 2 0 0 3 0 0 3 1 0 2 1 1 3 5 1 0 0 0 0
 2019 7 1 3 0 50  0 0 0 0 0 0 0 1 1 0 1 2 2 2 0 2 2 0 0 2 3 2 0 0 0 0 0 0 0 0 0 0 0 1 1 2 0 2 5 5 1 1 2 2 6 1 0 0 1 0
 2020 7 1 3 0 50  0 1 0 0 1 0 0 1 0 1 2 2 1 1 3 4 3 1 3 4 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 1 5 2 0 0 3 1 2 2 0 0 0 0
 2021 7 1 3 0 50  0 1 1 0 1 2 1 1 1 0 1 1 1 2 1 2 0 1 5 0 3 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 3 2 2 1 4 2 2 1 1 3 1 0 0 0
 2022 7 1 3 0 50  0 0 0 0 0 0 0 0 0 2 1 0 0 1 4 0 3 2 0 3 1 2 0 0 0 0 1 1 1 0 0 2 1 0 2 1 1 0 1 3 3 4 3 0 3 3 1 0 0 0
 2013 7 2 3 0 50  0 1 1 0 0 2 1 0 1 2 2 1 0 4 2 0 2 1 0 3 2 0 2 0 0 0 0 0 1 0 2 1 1 1 0 2 0 4 0 2 0 0 5 1 2 0 1 0 0 0
 2016 7 2 3 0 50  0 0 0 0 0 1 2 0 0 2 1 0 2 1 5 2 0 3 2 1 1 0 1 0 0 0 0 0 0 0 0 0 0 2 4 1 3 4 2 1 1 1 1 0 3 1 0 1 1 0
 2019 7 2 3 0 50  0 0 0 0 0 2 0 2 1 2 0 2 0 1 2 2 0 1 2 3 1 0 0 0 0 0 0 0 0 0 1 1 3 2 2 1 3 5 2 0 0 2 1 2 1 1 1 0 1 0
 2022 7 2 3 0 50  0 0 0 0 0 2 1 0 4 1 2 2 3 4 2 2 1 0 3 0 1 1 0 0 0 0 1 0 1 0 1 1 2 0 1 1 1 0 1 0 2 1 3 2 0 3 0 0 0 0
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
2011  7 1  3 0 2 1 -1 25  1 1 0 0 1 0 3 0 0 0 1 0 1 0 2 0 0 0 1 0 3 3 2 0 0 2 1 0 1 2
2012  7 1  3 0 2 1 -1 25  0 0 0 0 0 2 1 1 1 2 1 0 0 0 4 1 0 0 0 0 1 1 2 0 2 0 0 1 1 4
2013  7 1  3 0 2 1 -1 25  1 1 0 1 0 0 0 0 2 0 0 0 1 0 5 1 0 2 5 0 1 0 0 0 1 0 1 0 0 3
2014  7 1  3 0 2 1 -1 25  1 1 0 0 0 0 1 3 0 0 1 1 1 0 1 2 0 0 1 2 2 1 0 1 1 0 1 1 1 2
2015  7 1  3 0 2 1 -1 25  0 2 3 0 0 0 0 0 0 0 0 2 0 0 3 0 1 0 0 0 2 1 3 1 1 1 2 1 0 2
2016  7 1  3 0 2 1 -1 25  0 0 0 1 2 1 1 0 0 4 0 0 0 1 1 0 1 0 1 2 0 2 0 1 1 1 1 1 0 3
2017  7 1  3 0 2 1 -1 25  1 0 0 0 0 2 0 3 4 0 0 0 0 0 2 0 1 0 0 2 2 2 1 2 1 1 0 0 0 1
2018  7 1  3 0 2 1 -1 25  0 2 0 0 3 0 0 2 3 1 0 0 1 0 3 0 0 0 2 0 1 1 3 1 0 0 1 0 0 1
2019  7 1  3 0 2 1 -1 25  0 0 1 0 1 2 1 2 1 1 1 1 1 0 0 0 0 0 4 2 0 0 1 2 0 0 0 0 1 3
2020  7 1  3 0 2 1 -1 25  0 1 1 1 0 1 2 1 1 1 1 0 0 2 1 0 0 1 2 2 1 1 1 2 0 0 0 1 0 1
2021  7 1  3 0 2 1 -1 25  1 0 1 0 0 1 0 1 1 1 0 0 1 0 1 1 1 3 5 1 1 1 1 1 1 0 0 0 0 1
2022  7 1  3 0 2 1 -1 25  0 0 2 0 0 3 0 0 4 1 1 0 0 0 0 1 0 1 1 2 1 1 3 1 1 1 0 0 0 1
2013  7 2  3 0 2 1 -1 25  0 1 3 1 0 1 2 0 0 1 1 0 0 0 1 2 1 1 2 0 0 4 1 1 0 0 0 0 0 2
2016  7 2  3 0 2 1 -1 25  2 0 1 2 4 0 0 1 0 0 1 0 0 0 1 0 1 2 3 0 1 0 0 2 0 0 2 0 1 1
2019  7 2  3 0 2 1 -1 25  0 0 4 0 3 1 2 0 0 1 0 0 1 0 0 1 1 1 0 2 3 3 0 0 0 0 1 0 0 1
2022  7 2  3 0 2 1 -1 25  1 4 1 2 1 2 1 1 0 0 0 0 0 0 0 0 2 2 3 0 2 2 0 0 0 0 1 0 0 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 33.605 39.8127 47.1862 50.5085 53.8812 54.3597 60.1565 57.5551 57.6685 64.1512 66.3065 66.8282 70.0966 68.0517 66.1579 29.5345 39.7785 44.9174 51.4602 55.2811 56.5807 58.5801 60.552 65.4409 66.5305 64.9426 65.4615 70.1266 68.8079 67.1004
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 32.9979 40.7382 44.6627 46.9404 51.7597 55.5388 59.9951 63.2799 61.5756 62.2523 67.3776 67.131 69.6148 71.2982 70.6775 33.0702 41.9064 44.3083 50.4224 53.8024 56.9641 59.2312 61.5166 61.815 68.0084 65.5347 68.8721 65.3866 70.1429 69.4398
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 34.6722 40.5075 43.3096 49.7284 49.3366 55.9363 58.1219 59.453 59.5802 63.5521 63.7495 66.6752 60.9534 61.8565 67.294 36.1921 38.5704 42.6306 48.6312 49.4844 52.5928 55.8534 59.7522 56.9995 60.3359 63.5941 64.4153 64.2357 64.9589 70.8413
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 36.0438 38.7959 41.4469 44.1353 50.6663 52.9434 57.5907 56.4249 61.344 63.7686 57.3751 61.8738 66.6252 70.1388 67.397 36.4733 37.6786 45.3074 47.3111 52.1143 54.6926 58.216 60.9792 59.7572 54.9601 68.0269 60.0535 68.7943 67.806 69.2772
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

