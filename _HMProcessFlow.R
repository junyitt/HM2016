source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")
library(reshape2)

############################
# 4_ConvUser
############################

EXCraw2full.R_dir <- "C:/Users/User/Google Drive/z_ALLHM"
OTCraw2full.R_dir <- "C:/Users/User/Google Drive/z_ALLHM"
EXTRAraw2full.R_dir <- "C:/Users/User/Google Drive/z_ALLHM"

#set year to "collect" data from, i.e. EXC_0 >> yy <- 0

yy <- 0
##EXCHANGE
setwd(EXCraw2full.R_dir); source("01_EXC_raw_full.R")
      #output: 
      head(EXCfulltrandf)

##OTC
setwd(OTCraw2full.R_dir); source("02_OTC_raw_full.R")
      #output: 
      head(OTCfulltrandf)
      
##EXTRA -> SPECIAL links to META-FULL
setwd(EXTRAraw2full.R_dir); source("03_EXTRA_raw_full.R")
      #output:
      head(spectrandf)  #yr0, yr1 PV
      head(extrafulltrandf) #yr2 arbitrage, yr3 bond, yr4 harryM - cash
      
###COMBINED########33333333333333
      ufullfdf <- rbind(EXCfulltrandf, OTCfulltrandf, extrafulltrandf)
      uspecfdf <- spectrandf
      ############################
      # 4_ConvUser -- backup files, move files
      ############################
      fulldf_csvdir <- "C:/Users/User/OneDrive/yy_YearlyFullTran"; b_fulldf_csvdir <- "C:/Users/User/OneDrive/yy_b_YearlyFullTran"
      setwd(fulldf_csvdir); write.csv(ufullfdf, paste0("ufullfdf_", yy, ".csv"), row.names = F); write.csv(uspecfdf, paste0("uspecfdf_", yy, ".csv"), row.names = F)
      setwd(b_fulldf_csvdir); write.csv(ufullfdf, paste0("b_ufullfdf_", yy, ".csv"), row.names = F); write.csv(uspecfdf, paste0("b_uspecfdf_", yy, ".csv"), row.names = F)
      
      
      ############################
      # 5_Routine
      ############################
      
      #refer to fulltran_(0 up to yy), and spectran(0 up to yy)
      
      ##ADD EXTRA
      #rbind FDF <- fulltran 0 to yy, SPDF <- spectran 0 to yy, 
            #add spectran 0 to yy related extra transactions of that year yy to full tran >>> refer meta >> FDF <- rbind(FDF, specconv2full)
      
      
      #ADD core and scenario
            #FDF <- rbind(FDF, COREfull, SCENARIOfull)
      
      #output: sfdf, cfdf, e0fdf
      
############################
# 6_1BGNcalc
###########################

##copy paste from 00_FullTran.R  !!
      
      
      ############################
      # 7_BGNstloan
      ###########################

      
############################
# 8_2BGNcalc
###########################
      
      
      
      ############################
      # 9_ENDcalc
      ###########################
      

############################
# A1_Report
###########################
      
      