# Ian's notes on splitting apart the plotting function
dummyfunction <- function(){
# putting inside a function so it won't run on its own

library(r4ss)
update_r4ss_files(br=T)

setwd('c:/SS/R/r4ss/trunk/newplots/')
files = dir()
for(i in 1:length(files)){
    print(files[i])
    source(files[i])
}

s1  <- SSoutput('c:/SS/SSv3.10c_Mar3/old_simple',printstats=F)
ex1 <- SSoutput('C:/ss/toolboxFTP/Version_3_10b/Example_1',printstats=F)
ex2 <- SSoutput('C:/ss/toolboxFTP/Version_3_10b/Example_2',printstats=F)
a1 <- SSoutput('C:/ss/AndreTags/SSv3.10b/',printstats=F)

SSplots(ex2)



# end dummy function
}
