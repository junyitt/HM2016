#!!!!!!!!! Fix: read.csv change to read_excel


#DIR
EXCrawdir <- "C:/Users/User/OneDrive/1_Form_EXC"
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

      ##FIXED META
            #get excficmetadf
            setwd(excficmetadir)
            excficmetadf <- as.data.frame(read_excel("meta-FIC-EXC.xlsx"))
      
            #get tkeydf from meta-tradingkey.csv
            setwd(tkeymetadir)
            tkeydf <- as.data.frame(read_excel("meta-tradingkey.xlsx"))
      ##END-FIXED META
            
      #write a function that convert EXCrawdf to full tran #with error handler
      EXCfulltrandf <- EXCfullconv_f(EXCrawdf, excficmetadf, tkeydf)
      EXCfulltrandf <- subcutdf_f(EXCfulltrandf)
            #add track no
            EXCfulltrandf[,"TrackNo"] <- trackno_f(EXCfulltrandf)


     