#V3.30.23.00;_safe;_compile_date:_Nov  4 2024;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#_Start_time: Wed Nov  6 18:50:16 2024
#_echo_input_data
#C data file for simple example
#V3.30.23.00;_safe;_compile_date:_Nov  4 2024;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
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
-999 1 1 4153.75 0.2
2011 1 1 10015.9 0.01
2012 1 1 10026.4 0.01
2013 1 1 10124.8 0.01
2014 1 1 10036.8 0.01
2015 1 1 9969.51 0.01
2016 1 1 9893.53 0.01
2017 1 1 10107 0.01
2018 1 1 9003.8 0.01
2019 1 1 7882.21 0.01
2020 1 1 7053.95 0.01
2021 1 1 6000.76 0.01
2022 1 1 4024.62 0.01
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
#_year month fleet obs stderr
2013 7 2 144745 0.3 #_ SURVEY1
2016 7 2 63760.3 0.3 #_ SURVEY1
2019 7 2 59242.9 0.3 #_ SURVEY1
2022 7 2 49649.7 0.3 #_ SURVEY1
2011 7 3 11.5668 0.7 #_ SURVEY2
2012 7 3 13.9955 0.7 #_ SURVEY2
2013 7 3 12.5783 0.7 #_ SURVEY2
2014 7 3 16.7479 0.7 #_ SURVEY2
2015 7 3 7.7595 0.7 #_ SURVEY2
2016 7 3 9.36206 0.7 #_ SURVEY2
2017 7 3 16.9079 0.7 #_ SURVEY2
2018 7 3 6.90196 0.7 #_ SURVEY2
2019 7 3 14.6227 0.7 #_ SURVEY2
2020 7 3 7.4737 0.7 #_ SURVEY2
2021 7 3 7.60085 0.7 #_ SURVEY2
2022 7 3 15.8286 0.7 #_ SURVEY2
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note: only enter units and errtype for fleets with discard 
# note: discard data is the total for an entire season, so input of month here must be to a month in that season
#_fleet units errtype
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
1 # use length composition data (0/1/2) where 2 invokes new comp_control format
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
25 #_N_LengthBins; then enter lower edge of each length bin
 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 68 72 76 80 90
#_year month fleet sex part Nsamp datavector(female-male)
 2011 7 1 3 0 50 0 0 0 0 0 0 0 1 0 1 1 2 0 0 0 0 3 5 1 3 5 1 2 0 1 0 0 1 0 0 0 0 0 0 2 2 1 2 0 3 0 0 0 3 5 2 3 0 0 0
 2012 7 1 3 0 50 0 0 0 0 0 0 0 1 1 1 0 1 0 0 0 3 1 4 1 5 1 3 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 2 2 3 4 4 4 2 0 1 0 0
 2013 7 1 3 0 50 0 0 0 0 0 0 0 0 0 0 2 0 3 0 0 2 0 4 3 1 2 2 0 1 0 0 0 0 0 1 1 1 1 0 2 0 0 1 2 1 2 0 2 6 6 1 1 1 1 0
 2014 7 1 3 0 50 0 0 0 0 1 0 0 0 0 1 5 0 1 1 2 3 2 0 2 3 3 1 1 0 1 0 0 1 0 0 0 1 1 0 2 0 0 1 2 1 1 2 1 1 5 1 1 1 1 0
 2015 7 1 3 0 50 0 0 0 0 0 1 0 0 0 0 1 0 0 2 0 0 1 0 2 4 4 2 2 0 0 0 0 1 1 0 0 0 0 1 2 0 3 1 1 2 1 1 3 4 2 4 1 2 1 0
 2016 7 1 3 0 50 1 0 0 0 0 0 0 0 0 2 0 2 1 2 5 3 1 0 4 1 5 2 0 0 0 0 0 0 0 0 0 0 0 0 2 0 1 2 0 0 2 1 2 3 3 1 2 2 0 0
 2017 7 1 3 0 50 0 0 0 0 1 0 0 0 0 0 1 0 1 2 3 2 1 3 2 6 2 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1 1 2 0 1 1 2 3 3 3 2 2 0 2 0
 2018 7 1 3 0 50 0 0 0 0 0 0 1 0 0 1 0 3 2 1 7 3 2 2 0 4 4 1 0 1 0 0 0 0 0 1 0 0 1 0 1 2 0 1 1 2 1 0 0 1 3 2 2 0 0 0
 2019 7 1 3 0 50 1 0 0 0 0 0 0 0 1 2 0 0 1 4 5 1 6 1 1 3 4 0 1 0 0 0 0 0 0 0 1 1 0 0 3 0 0 0 1 1 1 0 1 2 3 4 1 0 0 0
 2020 7 1 3 0 50 0 0 0 0 1 2 0 0 1 2 1 1 1 2 4 1 2 1 0 6 0 1 0 0 0 0 0 0 1 0 1 0 1 1 1 0 1 0 3 4 0 2 4 1 0 1 1 1 1 0
 2021 7 1 3 0 50 0 0 0 1 1 0 0 1 1 2 2 0 1 1 1 2 2 2 1 2 3 1 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 3 4 2 2 3 1 2 3 3 0 0 0 0
 2022 7 1 3 0 50 0 0 0 0 0 0 1 0 0 2 2 0 1 2 2 1 0 2 1 4 0 1 1 0 0 1 0 0 0 0 0 0 1 1 0 2 1 1 3 3 2 3 1 2 2 2 4 1 0 0
 2013 7 2 3 0 50 0 0 0 0 0 2 0 3 1 4 2 2 0 4 1 2 2 0 1 3 2 0 0 2 0 0 0 0 1 1 0 0 1 1 1 2 2 0 1 3 0 2 0 0 2 1 1 0 0 0
 2016 7 2 3 0 50 0 0 0 0 2 0 1 0 0 2 0 2 2 2 1 0 1 4 1 2 3 1 0 0 0 0 0 0 0 1 1 2 1 4 2 1 0 4 0 2 1 0 2 1 0 3 0 1 0 0
 2019 7 2 3 0 50 0 0 0 0 1 1 0 4 1 1 4 0 0 3 1 2 2 1 0 2 0 0 0 0 0 1 0 1 0 0 2 1 2 3 1 3 4 3 1 0 2 1 0 1 0 0 1 0 0 0
 2022 7 2 3 0 50 0 0 0 1 3 0 2 0 0 0 0 1 6 2 2 0 4 0 1 4 1 0 1 0 0 0 0 0 0 1 1 0 4 0 1 1 1 3 1 2 2 1 0 1 1 2 0 0 0 0
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
 2011 7 1 3 0 2 1 -1 25 0 1 0 0 0 2 2 0 0 1 0 0 0 0 4 0 0 2 0 2 1 1 1 1 0 2 0 0 1 4
 2012 7 1 3 0 2 1 -1 25 0 0 2 0 1 1 1 2 1 1 0 0 1 0 2 0 1 1 1 0 1 0 2 1 0 2 1 1 1 1
 2013 7 1 3 0 2 1 -1 25 1 0 2 1 2 1 1 1 0 1 1 0 0 0 3 0 1 0 0 1 1 3 0 1 0 0 1 0 2 1
 2014 7 1 3 0 2 1 -1 25 1 1 1 2 1 1 1 0 0 1 2 0 1 0 3 0 0 1 2 2 0 0 1 0 1 0 1 0 0 2
 2015 7 1 3 0 2 1 -1 25 0 0 0 1 0 2 1 0 2 0 2 1 0 1 3 0 0 0 3 1 0 1 0 3 0 0 0 1 0 3
 2016 7 1 3 0 2 1 -1 25 1 1 1 1 2 2 3 2 1 1 0 0 2 1 1 0 0 1 0 1 0 0 0 0 0 1 1 0 0 2
 2017 7 1 3 0 2 1 -1 25 0 1 2 2 0 2 1 2 0 0 0 0 1 1 1 0 0 1 0 1 1 2 2 0 2 1 0 0 1 1
 2018 7 1 3 0 2 1 -1 25 0 1 1 3 3 0 2 0 1 1 1 0 1 0 1 0 1 0 2 0 2 0 0 2 0 1 0 1 0 1
 2019 7 1 3 0 2 1 -1 25 0 0 3 2 0 1 2 0 2 0 0 0 0 0 1 0 1 0 1 2 0 1 1 2 3 1 1 0 0 1
 2020 7 1 3 0 2 1 -1 25 0 0 2 1 0 0 3 0 1 1 2 0 0 1 2 0 0 1 0 0 0 1 4 0 1 1 1 1 0 2
 2021 7 1 3 0 2 1 -1 25 0 1 3 0 2 2 1 0 1 0 0 1 0 0 1 0 0 1 2 2 3 0 1 0 0 1 0 0 1 2
 2022 7 1 3 0 2 1 -1 25 0 0 0 1 0 4 1 2 4 2 0 0 1 0 0 0 1 0 0 0 1 3 1 2 0 1 1 0 0 0
 2013 7 2 3 0 2 1 -1 25 0 2 0 0 0 3 1 0 0 0 0 0 0 0 1 1 0 2 3 2 2 0 2 0 1 3 0 0 0 2
 2016 7 2 3 0 2 1 -1 25 0 2 3 4 0 2 1 3 0 1 1 0 0 0 2 0 0 1 1 2 0 1 0 0 1 0 0 0 0 0
 2019 7 2 3 0 2 1 -1 25 2 0 2 4 3 3 1 0 0 1 0 0 0 0 0 1 0 0 2 2 2 0 1 0 1 0 0 0 0 0
 2022 7 2 3 0 2 1 -1 25 0 5 1 2 0 0 1 1 0 0 1 0 0 0 1 1 2 1 2 1 1 1 1 0 2 0 0 0 0 1
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_year month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
 2011 7 1 3 0 1 2 31.1001 37.6247 44.5123 49.1577 55.1767 54.8937 57.8764 60.9767 63.407 65.7124 66.0334 66.4773 68.9837 66.6942 71.1547 33.5091 39.7331 44.3663 51.8999 58.3478 56.2476 56.8415 61.0979 62.6753 66.6534 63.2961 68.0089 70.3394 63.6221 64.0669 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
 2022 7 1 3 0 1 2 34.0009 39.7221 47.1123 45.7161 53.0591 55.0899 59.624 59.0393 60.7682 62.2977 66.8939 69.1026 64.3979 70.8878 70.3055 29.1412 38.5441 43.7912 49.411 53.6436 51.6203 56.6552 58.4004 57.7949 60.8038 60.1757 68.7121 65.4931 71.1546 68.5105 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
 2011 7 2 3 0 1 2 37.0771 42.2374 45.8496 46.4639 48.0909 53.0877 51.8078 53.4516 59.9566 63.2883 65.2691 61.7347 65.8296 62.7968 65.2321 35.7322 39.2453 41.9728 43.2482 51.1391 51.5264 56.5467 57.8468 59.1265 55.1627 63.584 64.4084 66.1779 67.2428 76.4436 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
 2022 7 2 3 0 1 2 35.8298 39.9798 43.9049 47.5526 51.4533 55.7381 58.9463 55.3783 64.3036 61.418 59.7164 63.6052 65.2001 65.1717 62.5297 34.9269 40.0321 40.2621 46.1732 49.9184 54.0177 50.9587 56.7169 62.4114 65.9328 63.2853 65.8266 71.7125 69.1363 71.1985 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_N_environ_variables
# -2 in year will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#_year variable value
#
# Sizefreq data. Defined by method because a fleet can use multiple methods
0 # N sizefreq methods to read (or -1 for expanded options)
# 
0 # do tags (0/1/2); where 2 allows entry of TG_min_recap
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

