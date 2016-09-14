#DIR
EXCrawdir <- "C:/Users/User/Google Drive/z_ALLHM/v5U_UserTranRaw/EXC"
excficmetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"
tkeymetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta/"
      EXCfdir <- "C:/Users/User/Google Drive/z_ALLHM/"
#FUNCTION
      setwd(EXCfdir)
      source("01_01_EXC_functions.R")
#####SECTION EXC-to-FULL-1#####
  
#set yy
yy <- 0
      
#RAW-EXC-TRAN
setwd(EXCrawdir)
EXCrawdf <- read.csv(paste0("EXC_", yy, ".csv"))

      ##FIXED META
            #get excficmetadf
            setwd(excficmetadir)
            exficmetadf <- read.csv("meta-FIC-EXC.csv")
      
            #get tkeydf from meta-tradingkey.csv
            setwd(tkeymetadir)
            tkeydf <- read.csv("meta-tradingkey.csv")
      ##END-FIXED META
            
      #write a function that convert EXCrawdf to full tran #with error handler
      EXCfulltrandf <- EXCfullconv_f(EXCrawdf, excficmetadf, tkeydf)
      
      


     