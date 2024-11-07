#V3.30.23.00;_safe;_compile_date:_Nov  4 2024;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#_Start_time: Wed Nov  6 18:50:16 2024
#_bootstrap
#C data file for simple example
#_bootstrap file: 1  irand_seed: 1730947816 first rand#: 0.253475
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
-999 1 1 5628.62 0.2
2011 1 1 10271.9 0.01
2012 1 1 10207.6 0.01
2013 1 1 10232.4 0.01
2014 1 1 10127.5 0.01
2015 1 1 9818.09 0.01
2016 1 1 9979.67 0.01
2017 1 1 10133 0.01
2018 1 1 9021.57 0.01
2019 1 1 7866.84 0.01
2020 1 1 7103.25 0.01
2021 1 1 5979.02 0.01
2022 1 1 4032.88 0.01
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
2013 7 2 128014 0.3 #_orig_obs: 144745 SURVEY1
2016 7 2 58552.9 0.3 #_orig_obs: 63760.3 SURVEY1
2019 7 2 42846 0.3 #_orig_obs: 59242.9 SURVEY1
2022 7 2 53626 0.3 #_orig_obs: 49649.7 SURVEY1
2011 7 3 8.85417 0.7 #_orig_obs: 11.5668 SURVEY2
2012 7 3 9.2356 0.7 #_orig_obs: 13.9955 SURVEY2
2013 7 3 22.2243 0.7 #_orig_obs: 12.5783 SURVEY2
2014 7 3 11.6085 0.7 #_orig_obs: 16.7479 SURVEY2
2015 7 3 9.77738 0.7 #_orig_obs: 7.7595 SURVEY2
2016 7 3 24.1198 0.7 #_orig_obs: 9.36206 SURVEY2
2017 7 3 6.30126 0.7 #_orig_obs: 16.9079 SURVEY2
2018 7 3 12.3709 0.7 #_orig_obs: 6.90196 SURVEY2
2019 7 3 4.51183 0.7 #_orig_obs: 14.6227 SURVEY2
2020 7 3 9.22408 0.7 #_orig_obs: 7.4737 SURVEY2
2021 7 3 14.0971 0.7 #_orig_obs: 7.60085 SURVEY2
2022 7 3 21.7353 0.7 #_orig_obs: 15.8286 SURVEY2
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
 2011 7 1 3 0 50  0 0 0 0 1 0 0 1 1 0 0 0 3 0 1 4 0 3 0 2 3 3 1 1 0 0 0 0 0 1 0 0 0 0 0 2 2 1 1 0 1 3 0 4 2 5 2 0 2 0
 2012 7 1 3 0 50  0 0 0 0 0 1 1 1 0 0 1 0 1 1 1 3 2 1 2 4 5 2 1 0 0 0 0 0 1 0 0 1 0 2 0 2 0 0 0 0 1 0 3 4 5 1 2 0 1 0
 2013 7 1 3 0 50  0 0 0 0 0 1 0 0 1 0 0 2 1 0 1 1 1 1 2 5 3 0 2 1 0 0 0 0 1 0 0 0 0 2 0 2 1 1 1 3 2 4 3 2 4 1 1 0 0 0
 2014 7 1 3 0 50  0 0 0 0 0 0 0 0 0 1 1 1 1 1 2 4 0 4 2 4 2 1 2 0 0 0 0 0 0 0 0 0 1 0 0 1 2 0 0 1 0 4 4 1 3 3 2 2 0 0
 2015 7 1 3 0 50  0 0 0 0 0 1 0 0 0 2 0 4 4 0 3 2 4 2 1 3 0 2 0 0 0 1 0 0 0 1 0 0 0 0 0 1 3 3 2 1 0 1 0 2 4 1 1 1 0 0
 2016 7 1 3 0 50  0 0 0 0 0 0 0 0 1 0 0 3 2 3 2 1 4 2 3 3 3 1 0 1 0 0 0 0 0 0 0 0 0 0 0 2 1 0 2 2 2 3 1 1 2 4 0 0 0 1
 2017 7 1 3 0 50  0 0 0 0 0 0 0 1 0 1 0 1 1 3 0 2 3 0 1 4 1 2 0 0 0 0 1 0 2 0 2 0 1 0 3 1 1 2 2 3 3 0 3 2 1 1 1 1 0 0
 2018 7 1 3 0 50  0 0 0 1 0 1 0 0 2 1 0 1 2 5 2 2 2 3 1 2 2 3 1 0 0 0 0 0 0 0 0 0 0 1 0 0 2 2 1 1 2 1 0 1 6 2 0 0 0 0
 2019 7 1 3 0 50  0 0 0 0 0 2 1 0 1 1 0 1 0 2 2 4 1 6 2 1 2 3 1 1 0 0 1 0 0 1 0 0 2 1 1 1 1 0 0 3 2 1 0 0 4 1 0 0 0 0
 2020 7 1 3 0 50  0 0 0 0 0 1 1 0 1 0 0 1 0 2 1 2 0 4 0 3 0 2 2 0 0 0 0 1 0 0 1 2 0 0 0 1 2 2 6 2 1 3 0 3 3 3 0 0 0 0
 2021 7 1 3 0 50  1 0 0 0 0 0 0 1 3 0 2 1 0 5 1 3 0 1 2 3 2 1 0 0 0 0 0 0 1 0 0 1 0 1 1 1 1 0 1 0 3 2 2 3 3 4 0 0 0 0
 2022 7 1 3 0 50  0 1 1 0 0 0 0 0 0 1 0 1 0 4 2 2 3 5 3 2 2 0 0 0 0 0 0 0 0 0 0 0 1 0 2 3 0 1 2 2 2 1 3 1 1 2 0 0 1 1
 2013 7 2 3 0 50  0 0 1 0 0 0 0 0 5 3 1 1 0 1 2 0 1 0 2 1 4 0 1 0 0 0 0 0 0 1 1 2 1 2 2 1 1 2 3 0 2 3 1 3 1 0 0 0 1 0
 2016 7 2 3 0 50  0 0 0 0 0 1 2 2 1 1 2 4 1 0 3 0 1 1 1 2 2 0 0 0 0 0 0 0 0 2 1 0 0 1 3 2 3 2 1 1 2 0 2 0 3 0 3 0 0 0
 2019 7 2 3 0 50  0 0 0 1 3 0 1 1 2 6 0 1 1 0 0 1 0 1 1 0 0 0 0 0 0 0 0 1 1 0 1 1 2 3 1 1 1 1 0 4 2 4 2 4 1 1 0 0 0 0
 2022 7 2 3 0 50  0 0 0 0 1 2 1 2 3 3 3 1 2 2 3 1 2 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 2 0 2 2 2 3 1 3 1 0 0 0 2 0 0 0 0
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
2011  7 1  3 0 2 1 -1 25  0 0 0 0 1 0 2 1 3 0 1 1 0 1 2 0 0 0 1 0 2 0 1 2 1 0 0 1 1 4
2012  7 1  3 0 2 1 -1 25  1 1 0 1 1 0 0 2 1 1 0 1 0 1 3 0 0 0 1 1 2 0 0 1 2 1 0 1 0 3
2013  7 1  3 0 2 1 -1 25  0 0 0 1 0 0 4 0 0 0 0 0 0 1 2 1 1 1 2 1 3 0 0 1 0 0 2 2 0 3
2014  7 1  3 0 2 1 -1 25  0 1 0 0 1 0 1 2 0 1 0 0 0 0 2 1 0 3 2 1 0 0 2 2 0 0 0 0 0 6
2015  7 1  3 0 2 1 -1 25  0 1 0 0 3 1 0 0 0 2 0 1 1 0 3 0 1 2 0 1 1 0 0 1 0 1 1 2 2 1
2016  7 1  3 0 2 1 -1 25  0 0 0 0 1 2 3 2 1 0 0 0 0 0 1 0 0 0 1 1 1 2 1 1 1 1 0 1 1 4
2017  7 1  3 0 2 1 -1 25  1 2 1 0 0 2 0 0 0 1 2 1 0 2 1 0 0 0 0 2 1 2 0 1 1 0 2 1 0 2
2018  7 1  3 0 2 1 -1 25  0 0 0 2 1 1 1 0 1 3 0 2 0 0 0 0 0 4 3 0 0 1 0 1 0 0 0 0 2 3
2019  7 1  3 0 2 1 -1 25  1 1 0 0 1 2 0 1 3 3 1 1 0 0 2 0 0 0 0 0 1 2 1 1 2 0 0 1 0 1
2020  7 1  3 0 2 1 -1 25  1 2 0 0 0 1 2 2 1 1 0 0 0 1 2 0 0 1 1 3 1 0 1 0 0 2 1 0 0 2
2021  7 1  3 0 2 1 -1 25  0 2 0 0 0 2 1 1 0 2 0 0 0 0 0 0 1 1 2 1 2 3 2 1 3 1 0 0 0 0
2022  7 1  3 0 2 1 -1 25  0 0 1 2 0 2 2 0 0 1 0 0 0 2 0 0 1 1 5 1 1 2 2 1 0 0 0 0 0 1
2013  7 2  3 0 2 1 -1 25  0 0 4 1 0 1 0 2 0 1 0 0 0 0 2 1 3 0 0 0 1 3 0 1 0 0 0 1 0 4
2016  7 2  3 0 2 1 -1 25  2 1 1 1 3 1 1 1 0 0 0 2 0 0 2 0 2 0 1 1 1 0 1 1 1 2 0 0 0 0
2019  7 2  3 0 2 1 -1 25  1 1 1 4 0 0 3 1 0 0 0 0 0 0 1 0 0 1 2 1 1 2 0 2 1 0 1 0 0 2
2022  7 2  3 0 2 1 -1 25  0 2 2 4 2 2 2 1 2 1 0 0 0 0 0 0 0 0 2 1 1 0 0 0 2 0 0 0 0 1
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
1 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_year month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
2011  7 1  3 0 1 2 33.7436 38.8411 43.7493 48.8877 47.7292 58.6128 60.654 56.3711 63.6748 67.8787 70.928 68.3411 66.5978 68.1932 76.825 30.423 40.7107 47.1441 51.2112 49.3149 57.6373 56.5191 60.8299 58.6191 63.8761 62.9683 66.9254 68.4149 69.1219 64.2063
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 1  3 0 1 2 30.5918 41.106 45.925 48.608 47.8013 56.3381 56.0096 58.0903 58.9384 61.9529 60.9764 65.6052 69.7733 72.0141 70.536 34.9328 40.9798 43.9636 49.3117 55.5663 57.0719 60.0975 63.9534 61.0531 65.4105 68.4531 62.5428 68.1065 67.3887 73.2912
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2011  7 2  3 0 1 2 35.2003 38.3812 46.1599 46.564 50.6512 52.7971 55.1227 52.4077 58.9486 63.2215 65.8317 70.0671 65.3121 64.0599 66.0345 35.8056 37.8698 45.692 48.8988 48.9367 46.5907 57.109 55.6201 56.7264 61.3989 62.0778 61.9723 66.9538 67.3048 73.0392
 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
2022  7 2  3 0 1 2 38.4503 36.948 42.6301 47.3749 56.0248 54.2534 57.1985 58.8009 59.9905 61.8156 61.3722 63.3709 61.4754 70.7262 72.1616 36.4589 40.9684 41.6998 48.6687 51.6816 53.5569 57.5825 61.6642 61.3959 62.7914 62.3114 65.8531 68.1913 67.9108 70.5524
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

