SS_makedatlist <-
    function(styr=1971,
             endyr=2001,
             nseas=1,
             months_per_seas=12,
             spawn_seas=1,
             Nfleet=1,
             Nsurveys=1,
             N_areas=1,
             fleetnames=c("fishery1","survey1"),
             surveytiming=0.5,
             areas=1,
             units_of_catch=1,
             se_log_catch=0.01,
             Ngenders=2,
             Nages=40,
             init_equil=0,
             catch=NULL,
             CPUE=NULL,
             N_discard_fleets=0,
             discard_data=NULL,
             meanbodywt=NULL,
             DF_for_meanbodywt=30,
             lbin_method=2,
             binwidth=2,
             minimum_size=2,
             maximum_size=90,
             comp_tail_compression= -0.0001,
             add_to_comp=0.0001,
             max_combined_lbin=0,
             lbin_vector=seq(22, 90, 2),
             lencomp=NULL,
             agebin_vector=1:25,
             ageerror=data.frame(rbind(0:40+.5,.001,0:40+.5,seq(0.525,2.525,0.05))),
             agecomp=NULL,
             Lbin_method=3,
             max_combined_age=1,
             MeanSize_at_Age_obs=NULL,
             N_environ_variables=0,
             N_environ_obs=0,
             N_sizefreq_methods=0,
             do_tags=0,
             morphcomp_data=0
             ){
        SSversion <- "SSv3.24B"
        N_lbins <- length(lbin_vector)
        N_agebins <- length(agebin_vector)

        N_catch <- ifelse(is.null(catch), 0, nrow(catch))
        N_cpue <- ifelse(is.null(CPUE), 0, nrow(CPUE))
        CPUEinfo <- data.frame(Fleet=1:(Nfleet+Nsurveys),Units=1,Errtype=0)
        N_discard <- ifelse(is.null(discard_data), 0, nrow(discard_data))
        N_meanbodywt <- ifelse(is.null(meanbodywt), 0, nrow(meanbodywt))
        N_lencomp <- ifelse(is.null(lencomp), 0, nrow(lencomp))
        N_ageerror_definitions <- ifelse(is.null(ageerror), 0, nrow(ageerror)/2)
        N_agecomp <- ifelse(is.null(agecomp), 0, nrow(agecomp))
        N_MeanSize_at_Age_obs <- ifelse(is.null(MeanSize_at_Age_obs), 0, nrow(MeanSize_at_Age_obs))


        fleetinfo1 <- data.frame(rbind(rep(surveytiming,Nfleet+Nsurveys),
                                        rep(areas,Nfleet+Nsurveys)))
        names(fleetinfo1) <- fleetnames
        names(fleetinfo1)[1] <- paste("#",names(fleetinfo1)[1],sep="")
        fleetinfo1$input <- c("#_surveytiming","#_areas")

        fleetinfo2 <- data.frame(rbind(rep(units_of_catch,Nfleet),
                                        rep(se_log_catch,Nfleet)))
        names(fleetinfo2) <- fleetnames[1:Nfleet]
        names(fleetinfo2)[1] <- paste("#",names(fleetinfo2)[1],sep="")
        fleetinfo2$input <- c("#_units_of_catch","#_se_log_catch")

        names(ageerror) <- c("#_age0",paste("age",1:Nages,sep=""))

        datlist <- list(SSversion = SSversion,
                        type = "Stock_Synthesis_data_file",
                        styr = styr,
                        endyr = endyr,
                        nseas = nseas,
                        months_per_seas = months_per_seas,
                        spawn_seas = spawn_seas,
                        Nfleet = Nfleet,
                        Nsurveys = Nsurveys,
                        N_areas = N_areas,
                        fleetnames = fleetnames,
                        surveytiming = surveytiming,
                        areas = areas,
                        units_of_catch = units_of_catch,
                        se_log_catch = se_log_catch,
                        fleetinfo1 = fleetinfo1,
                        fleetinfo2 = fleetinfo2,
                        Ngenders = Ngenders,
                        Nages = Nages,
                        init_equil = init_equil,
                        N_catch = N_catch,
                        catch = catch,
                        N_cpue = N_cpue,
                        CPUEinfo = CPUEinfo,
                        CPUE = CPUE,
                        N_discard_fleets = N_discard_fleets,
                        N_discard = N_discard,
                        discard_data = discard_data,
                        N_meanbodywt = N_meanbodywt,
                        meanbodywt = meanbodywt,
                        DF_for_meanbodywt=DF_for_meanbodywt,
                        lbin_method = lbin_method,
                        binwidth = binwidth,
                        minimum_size = minimum_size,
                        maximum_size = maximum_size,
                        comp_tail_compression = comp_tail_compression,
                        add_to_comp = add_to_comp,
                        max_combined_lbin = max_combined_lbin,
                        N_lbins = N_lbins,
                        lbin_vector = lbin_vector,
                        N_lencomp = N_lencomp,
                        lencomp = lencomp,
                        N_agebins = N_agebins,
                        agebin_vector = agebin_vector,
                        N_ageerror_definitions = N_ageerror_definitions,
                        ageerror = ageerror,
                        N_agecomp = N_agecomp,
                        agecomp = agecomp,
                        Lbin_method = Lbin_method,
                        max_combined_age = max_combined_age,
                        N_MeanSize_at_Age_obs = N_MeanSize_at_Age_obs,
                        MeanSize_at_Age_obs = MeanSize_at_Age_obs,
                        N_environ_variables = N_environ_variables,
                        N_environ_obs = N_environ_obs,
                        N_sizefreq_methods = N_sizefreq_methods,
                        do_tags = do_tags,
                        morphcomp_data = morphcomp_data
                        )
        return(datlist)
    }

