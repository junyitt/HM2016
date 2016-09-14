#02_OTC_raw_full.R

#DIR
OTCrawdir <- "C:/Users/User/Google Drive/z_ALLHM/v5U_UserTranRaw/OTC"
EXCfdir <- "C:/Users/User/Google Drive/z_ALLHM/"
                  EXTRArawdir <- "C:/Users/User/Google Drive/z_ALLHM/v5U_UserTranRaw/EXTRA"
#FUNCTION
setwd(EXCfdir)
source("01_EXC_raw_full.R")

#####SECTION OTC-to-FULL-1#####

#set yy
yy <- 0

#RAW-OTC-TRAN
setwd(OTCrawdir)
OTCrawdf <- read.csv(paste0("OTC_", yy, ".csv"))
      #duplicate and fix df
      dupotcrawdf <- duplicate_f(OTCrawdf)
            #final full-tran-OTC df
            OTCfulltrandf <- OTCfullconv_f(dupotcrawdf)
            OTCfulltrandf <- subcutdf_f(OTCfulltrandf)            
                  #add track no
                  OTCfulltrandf[,"TrackNo"] <- trackno_f(OTCfulltrandf)