#_HMChallenge.R
########################
######DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO
#########################

#Main Set Up
MAINCODEDIR <- "C:/Users/User/Google Drive/z_ALLHM/_MainCode_Demo"; setwd(MAINCODEDIR)
source("_HM_alldirectory.R"); source("_HM_allfunction.R")
DONE <- vector()

#Start Distribute
yy <- 0; setwd(maincode.dir); source("D0_Distribution.R")

##END OF YEAR COMPUTE:

#next = END of YEAR 1
preyy <- 0 ; setwd(maincode.dir); source("__ProgressYear.R")

rm(list=ls())
setwd(MAINCODEDIR); source("T_tempdeleteTeam.R")

########################
######DEMO DEMO DEMO DEMO DEMO DEMO DEMO DEMO
#########################


# #next = END of YEAR 2
# preyy <- 1 ; setwd(maincode.dir); source("__ProgressYear.R")
# 
# 
# 
# #next = END of YEAR 3
# preyy <- 2 ; setwd(maincode.dir); source("__ProgressYear.R")
# 
# #next = END of YEAR 4
# preyy <- 3 ; setwd(maincode.dir); source("__ProgressYear.R")
# 
# #next = END of YEAR 5 #END
# preyy <- 4 ; setwd(maincode.dir); source("__ProgressYear.R")



# setwd(maincode.dir); source("_HMFinalReport_3.R")