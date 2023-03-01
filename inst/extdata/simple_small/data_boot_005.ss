#V3.30.21.00;_safe;_compile_date:_Feb 10 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Tue Feb 28 13:44:05 2023
#_bootdata:_7
#C data file for simple example
#_bootstrap file: 5  irand_seed: 1677620645 first rand#: -0.652367
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
-999 1 1 4445.52 0.2
2011 1 1 10046.4 0.01
2012 1 1 9949.3 0.01
2013 1 1 9975.12 0.01
2014 1 1 10020.5 0.01
2015 1 1 9933.62 0.01
2016 1 1 9947.25 0.01
2017 1 1 10213.5 0.01
2018 1 1 8957.2 0.01
2019 1 1 8003.17 0.01
2020 1 1 7024.19 0.01
2021 1 1 6071.97 0.01
2022 1 1 4091.4 0.01
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
2013 7 2 85550.7 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 48568.3 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 43244.6 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 55283.6 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 20.6135 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 13.9434 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 11.0666 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 28.2256 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 5.15495 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 12.1611 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 6.61491 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 4.56276 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 10.1015 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 24.457 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 6.43553 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 11.2851 0.7 #_orig_obs: 15.8286 SURVEY2
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
 2011 7 1 3 0 50  0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 2 1 3 1 1 7 2 0 0 0 0 0 0 0 0 0 0 0 2 0 0 2 1 0 5 2 4 2 0 4 3 2 1 1 0
 2012 7 1 3 0 50  0 0 0 0 0 0 0 0 0 0 0 1 1 0 3 3 2 1 1 5 3 2 0 0 0 0 0 0 0 0 0 0 0 0 1 1 2 1 3 2 2 1 2 0 7 1 5 0 0 0
 2013 7 1 3 0 50  1 0 0 0 0 0 0 0 1 1 0 1 1 2 2 1 1 1 4 5 3 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 1 0 2 1 4 0 4 4 2 0 1 0
 2014 7 1 3 0 50  1 0 0 0 0 0 0 0 2 1 0 1 1 2 2 2 3 2 5 0 2 1 2 2 0 0 0 0 0 0 0 1 0 0 0 0 1 1 2 2 2 1 2 1 4 0 2 2 0 0
 2015 7 1 3 0 50  0 0 0 0 0 0 0 0 1 1 1 2 3 1 0 0 3 1 3 4 1 1 0 0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 2 3 2 2 3 0 4 5 2 0 1 0
 2016 7 1 3 0 50  0 0 0 0 0 2 0 0 0 0 1 2 0 2 2 2 1 0 6 4 2 1 1 0 0 0 0 0 0 0 0 1 0 2 0 0 1 0 1 1 3 3 2 1 1 4 1 1 2 0
 2017 7 1 3 0 50  0 0 0 1 0 0 0 1 1 3 3 2 1 4 1 0 4 3 1 0 2 1 1 0 0 0 0 0 0 0 0 1 0 1 0 0 1 2 4 1 1 0 2 1 4 3 0 0 0 0
 2018 7 1 3 0 50  1 0 0 0 0 0 1 2 0 1 1 2 2 3 1 4 1 2 1 0 5 2 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 2 2 2 1 2 0 1 3 3 0 2 0 0
 2019 7 1 3 0 50  0 0 0 0 0 0 0 0 0 0 0 2 0 1 3 2 5 2 3 2 1 2 0 0 0 0 0 0 1 1 0 1 0 1 0 0 1 0 4 0 1 3 4 3 1 3 2 0 1 0
 2020 7 1 3 0 50  0 0 0 0 0 0 0 1 2 1 2 0 2 1 1 3 1 1 1 2 3 1 0 0 0 0 0 1 0 0 0 1 0 2 0 3 1 1 0 1 3 3 1 0 2 4 0 3 2 0
 2021 7 1 3 0 50  0 0 0 0 1 1 1 0 0 2 1 0 1 1 2 2 2 1 4 4 3 1 2 1 0 0 0 1 0 0 0 0 1 0 1 1 1 1 0 1 3 3 2 1 1 0 2 0 1 0
 2022 7 1 3 0 50  0 0 2 0 0 0 0 0 1 0 2 0 4 2 4 3 1 2 3 3 0 0 1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 3 3 2 1 1 3 0 3 1 1 0 0 0
 2013 7 2 3 0 50  0 0 0 1 0 0 0 0 2 2 0 1 6 1 1 1 2 1 1 3 1 0 0 0 0 0 0 0 0 0 1 1 1 0 2 2 2 0 1 2 2 0 1 3 1 5 2 0 1 0
 2016 7 2 3 0 50  0 0 0 1 0 2 0 1 0 3 4 0 5 2 1 0 2 0 0 0 1 0 0 0 0 0 0 0 0 0 1 2 1 1 2 2 2 3 0 1 3 1 1 1 2 4 1 0 0 0
 2019 7 2 3 0 50  0 0 0 0 1 0 0 2 1 2 3 0 4 4 2 0 0 0 1 5 1 0 0 0 0 0 0 0 0 0 1 1 3 1 1 3 0 3 2 0 1 3 1 1 2 0 0 0 1 0
 2022 7 2 3 0 50  0 0 0 0 0 2 2 2 1 2 1 1 0 1 1 2 1 2 2 1 2 0 0 0 0 0 0 0 1 0 1 4 5 0 4 3 1 1 2 1 0 0 0 2 1 1 0 0 0 0
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
2011  7 1  3 0 2 1 -1 25  1 1 0 0 1 2 0 1 1 0 1 0 0 0 4 0 0 0 0 2 0 1 1 0 0 3 1 2 1 2
2012  7 1  3 0 2 1 -1 25  0 1 0 0 0 0 2 1 0 1 2 1 0 0 2 0 0 0 0 0 1 5 0 1 0 2 1 0 2 3
2013  7 1  3 0 2 1 -1 25  0 0 1 0 1 2 1 3 0 1 0 0 1 1 1 0 0 0 1 1 3 2 1 0 1 1 0 0 0 3
2014  7 1  3 0 2 1 -1 25  2 0 0 2 1 2 3 2 0 0 0 1 0 1 2 1 0 0 0 3 1 0 0 0 0 0 0 0 1 3
2015  7 1  3 0 2 1 -1 25  1 0 0 2 0 1 1 1 0 1 1 0 1 0 1 3 0 2 2 0 1 1 0 2 0 0 1 0 0 3
2016  7 1  3 0 2 1 -1 25  0 0 0 0 2 1 4 0 0 0 0 1 0 1 3 1 0 0 0 1 0 2 2 0 2 0 0 0 2 3
2017  7 1  3 0 2 1 -1 25  0 0 2 2 2 1 1 2 0 0 1 1 0 0 2 0 0 0 3 0 1 1 1 0 1 0 0 2 0 2
2018  7 1  3 0 2 1 -1 25  1 1 0 0 2 2 1 2 2 1 0 2 0 0 2 1 1 0 1 0 0 2 1 1 0 0 0 0 0 2
2019  7 1  3 0 2 1 -1 25  0 1 1 1 0 0 3 1 1 1 0 0 1 2 2 0 0 0 0 1 3 3 1 0 0 0 1 0 1 1
2020  7 1  3 0 2 1 -1 25  0 0 3 1 4 1 2 0 1 2 0 4 0 0 1 0 1 2 1 1 0 0 0 0 1 0 0 0 0 0
2021  7 1  3 0 2 1 -1 25  0 0 2 1 0 1 1 1 0 0 2 1 1 0 1 0 0 2 1 1 1 3 3 1 0 1 0 0 0 1
2022  7 1  3 0 2 1 -1 25  3 0 0 1 1 1 0 1 1 0 0 0 0 0 0 0 1 0 0 1 2 3 3 0 2 0 1 1 1 2
2013  7 2  3 0 2 1 -1 25  2 0 1 2 2 1 0 1 0 2 2 1 0 1 1 1 0 0 2 1 0 1 0 0 0 0 1 0 1 2
2016  7 2  3 0 2 1 -1 25  0 2 1 0 1 2 2 0 1 1 0 0 0 1 0 0 1 1 3 0 1 1 3 0 1 0 0 2 0 1
2019  7 2  3 0 2 1 -1 25  1 0 3 3 1 0 1 0 0 1 1 1 0 0 0 1 2 2 0 2 2 1 1 1 0 0 1 0 0 0
2022  7 2  3 0 2 1 -1 25  0 1 2 4 0 2 1 1 0 0 0 0 0 0 0 0 3 4 2 0 1 1 2 0 1 0 0 0 0 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 33.6999 40.4565 44.797 45.7282 52.4081 50.9854 56.011 63.798 57.4781 60.8847 60.0041 70.9319 66.764 69.4888 71.6535 31.0638 39.508 44.0944 49.5948 48.6316 56.0466 57.1527 62.936 63.0635 61.7639 66.8515 68.5781 65.2066 68.8404 68.6679
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 32.0822 40.1318 45.0101 50.1848 51.6916 54.3321 60.3682 64.4248 59.5362 64.6406 64.6693 73.0363 71.7565 65.1373 74.1994 30.5291 41.1417 43.3602 49.7865 54.6088 57.5034 59.2182 57.5923 60.6943 63.4373 62.9123 67.8047 62.1011 72.2485 67.8661
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 32.9739 37.8379 41.4891 44.7218 50.3507 54.2016 58.9792 58.2961 60.4672 61.0074 59.6989 64.4502 73.4005 61.6755 63.4204 36.0895 40.4498 43.2957 45.0457 47.9284 55.9703 58.61 56.9496 59.4999 62.2177 62.9452 65.9559 59.772 65.8989 70.4214
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 36.0227 38.4275 42.2553 47.6681 52.6244 59.8631 56.4906 58.3411 60.2263 61.7867 70.6505 65.7025 71.2794 66.7616 71.57 35.4124 39.2909 45.5653 50.8624 48.8846 56.4192 53.5718 59.9985 62.7976 59.9764 58.54 62.0249 70.0683 63.4316 68.4669
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

