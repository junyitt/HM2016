#02_OTC_raw_full.R

#DIR
OTCrawdir <- "C:/Users/User/Google Drive/z_ALLHM/v5U_UserTranRaw/OTC"
OTCfdir <- "C:/Users/User/Google Drive/z_ALLHM/"

#FUNCTION
setwd(OTCfdir)
source("02_01_OTC_functions.R")

#####SECTION OTC-to-FULL-1#####

#set yy
# yy <- 0
# yy <- 0

#RAW-OTC-TRAN
setwd(OTCrawdir)
OTCrawdf <- as.data.frame(read_excel(paste0("OTC_", yy, ".xlsx")))

      #duplicate and fix df
      dupotcrawdf <- duplicate_f(OTCrawdf)
            #final full-tran-OTC df
            OTCfulltrandf <- OTCfullconv_f(dupotcrawdf)
                  #add track no
                  OTCfulltrandf[,"TrackNo"] <- trackno_f(OTCfulltrandf)
                  