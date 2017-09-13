# plot cohort contributions to total biomass in each year

# pull often used objects from replist created by SS_output
startyr <- replist$startyr
endyr  <- replist$endyr
accuage <- replist$accuage
batage <- replist$batage

# define ranges and counts of things
cohort_start <- startyr - accuage
cohort_end <- endyr
cohort_vec <- cohort_start:cohort_end
Ncohorts <- length(cohort_vec)
year_vec <- startyr:endyr
Nyears <- length(year_vec)
# choose beginning of year vs middle-year values
period <- "B" # could be "M" instead

# empty matrix with dimensions years by cohorts
bio_by_cohort_year <- matrix(0, nrow = Nyears, ncol = Ncohorts)
rownames(bio_by_cohort_year) <- year_vec
colnames(bio_by_cohort_year) <- cohort_vec

# loop over years to fill in values
for(iyear in 1:Nyears){
  year <- year_vec[iyear]
  # biomass by age within each year
  bio_vec <- batage[batage$"Beg/Mid" == period &
                      batage$Era != "VIRG" &
                        batage$Yr == year,
                    names(batage) %in% 0:accuage]
  # aggregate over sexes to truly get a vector
  bio_vec <- apply(rev(bio_vec), 2, sum)
  # map vector into matrix after adjusting for which age goes to which cohort
  bio_by_cohort_year[iyear, cohort_vec %in% (year - 0:accuage)] <- bio_vec
    
}

# reverse the order of cohorts to make the plots come out right
bio_by_cohort_year2 <- bio_by_cohort_year[,ncol(bio_by_cohort_year):1]

# make plot
barplot(t(bio_by_cohort_year2/1000), col=rich.colors.short(10),
        space=0, xlab="Year", ylab="Total biomass (1000s of tons)", las=1,
        mar=c(4,4,1,1), xlim=c(80,102))
axis(1, at=1:Nyears-.5, lab=rep("", Nyears))

#legend('top', title="Birth year ends in:", legend=0:9, fill=rich.colors.short(10), bty='n', ncol=2)
