# Ian's notes on splitting apart the plotting function
dummyfunction <- function(){
# putting inside a function so it won't run on its own

library(r4ss)
update_r4ss_files(br=T)
s1 <- SSv3_output('/home/ian/SS/SSv3.10c_Mar3/old_simple',printstats=F)
s2 <- SSv3_output('/home/ian/SS/Sablefish/sable_SSv3.10c',printstats=F)

s1 <- SSv3_output('c:/SS/SSv3.10c_Mar3/old_simple',printstats=F)
s2 <- SSv3_output('c:/SS/Sablefish/sable_SSv3.10c',printstats=F)

ex1 <- SSv3_output('C:/ss/toolboxFTP/Version_3_10b/Example_1',printstats=F)
ex2 <- SSv3_output('C:/ss/toolboxFTP/Version_3_10b/Example_2',printstats=F)

setwd('/home/ian/SS/R/r4ss/branches/individual_plot_functions')

setwd('c:/SS/R/r4ss/branches/individual_plot_functions')

setwd('c:/SS/R/r4ss/trunk/individual_plot_functions')
files = dir()
for(i in 1:length(files)){
    print(files[i])
    source(files[i])
}


SSplots(ex2)



# end dummy function
}
