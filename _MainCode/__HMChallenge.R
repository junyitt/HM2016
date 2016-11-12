#_HMChallenge.R  ##safesunway@outlook.com - safest2016

############################################################
#Main Set Up
MAINCODEDIR <- "C:/Users/User/Google Drive/z_ALLHM/_MainCode"; setwd(MAINCODEDIR)
source("_HM_alldirectory.R"); source("_HM_allfunction.R")
DONE <- vector(); success.yy <- -0.1

############################################################
####RESTART???
setwd(maincode.dir); source("T_tempdeleteTeam.R")

#Start Distribute
yy <- 0; setwd(maincode.dir); source("D0_Distribution.R")

############################################################

##END OF YEAR COMPUTE:

#next = END of YEAR 1
yy <- 0 ; setwd(maincode.dir); source("__ProgressYear.R")

#next = END of YEAR 2
yy <- 1 ; setwd(maincode.dir); source("__ProgressYear.R")

#next = END of YEAR 3
yy <- 2 ; setwd(maincode.dir); source("__ProgressYear.R")

#next = END of YEAR 4
yy <- 3 ; setwd(maincode.dir); source("__ProgressYear.R")

#next = END of YEAR 5 #END
yy <- 4 ; setwd(maincode.dir); source("__ProgressYear.R")

############################################################
##FINAL REPORT yy = 5
############################################################
setwd(maincode.dir); source("__Q_FinalScore_yy5.R") #output pdf and csv score_Round1


