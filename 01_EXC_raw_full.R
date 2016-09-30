#!!!!!!!!! Fix: read.csv change to read_excel

#############
startcut.f.dir <- "C:/Users/User/Google Drive/z_ALLHM/"
setwd(startcut.f.dir); source("01A_startcutoff_f.R")
########

#DIR
EXCrawdir <- "C:/Users/User/OneDrive/1_Form_EXC"
if(real_run == T){EXCrawdir<-gsub("1", "f", EXCrawdir)}else{}

excficmetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"
tkeymetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta/"
      EXCfdir <- "C:/Users/User/Google Drive/z_ALLHM/"
#FUNCTION
      setwd(EXCfdir)
      source("01_01_EXC_functions.R")
#####SECTION EXC-to-FULL-1#####
  
#set yy
# yy <- 0
# yy <- 1  
      
#RAW-EXC-TRAN
setwd(EXCrawdir)
EXCrawdf <- as.data.frame(read_excel(paste0("EXC_", yy, ".xlsx")))
EXCrawdf <- startcutoff.f(EXCrawdf) ##Added startjan

      ##FIXED META
            #get excficmetadf
            setwd(excficmetadir)
            excficmetadf <- as.data.frame(read_excel("meta-FIC-EXC.xlsx"))  ###NOT CHANGED: next: change to meta-FIC-A, and use ONE xlsx meta file only
      
            #get tkeydf from meta-tradingkey.csv
            setwd(tkeymetadir)
            tkeydf <- as.data.frame(read_excel("meta-tradingkey.xlsx"))
      ##END-FIXED META
            
      #write a function that convert EXCrawdf to full tran #with error handler
      EXCfulltrandf <- EXCfullconv_f(EXCrawdf, excficmetadf, tkeydf)
      EXCfulltrandf <- subcutdf_f(EXCfulltrandf)
            #add track no
            EXCfulltrandf[,"TrackNo"] <- trackno_f(EXCfulltrandf)


     