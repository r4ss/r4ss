## harmean <- function(x){
##   1/mean(1/x)
## }

## tab <- SS_tune_comps(y1)
## tab$Punt_mult <- NA
## irow <- 1

## dat <- y1$len_comp_fit_table
## for(ifleet in unique(y1$len_comp_fit_table$Fleet)){
##   sub <- dat$Fleet==ifleet & dat$Nsamp > 0 & dat$effN > 0
##   print(harmean(dat$effN[sub]/dat$Nsamp[sub]))
##   tab$Punt_mult[irow] <- harmean(dat$effN[sub]/dat$Nsamp[sub])
##   irow <- irow+1
## }

## dat <- y1$age_comp_fit_table
## for(ifleet in unique(y1$age_comp_fit_table$Fleet)){
##   sub <- dat$Fleet==ifleet & dat$Nsamp > 0 & dat$effN > 0
##   print(ifleet)
##   print(harmean(dat$effN[sub]/dat$Nsamp[sub]))
##   tab$Punt_mult[irow] <- harmean(dat$effN[sub]/dat$Nsamp[sub])
##   irow <- irow+1
## }

