#02_OTC_raw_full.R

#DIR Required
#raw_OTC.dir, maincode.dir, meta.dir;; yy

#FUNCTION (general) Required
#cutoff.f, importmeta.f

      #ENVIRONMENT OUTPUT:
      #preOTCraw.df, OTCraw.df, meta.und.price.df, OTCfulltran.df

#Internal FUNCTION
setwd(maincode.dir)
source("02_01_OTC_functions.R")  #importrawOTC.f ##duplicate.f, flippos.f, posfix.f, loanNAfix.f  #OTCfullconv.f ##VLandVLR.OTC.v2.f, addcurr.f

#####SECTION OTC-to-FULL-1#####

#Import RAW-OTC-TRAN
preOTCraw.df <- importrawOTC.f(raw_OTC.dir, yy)
OTCraw.df <- duplicate.f(preOTCraw.df)   #duplicate and fix df

#Import meta.und.price.df
meta.und.price.df <- importmeta.f(meta.dir, "meta-underlyingprice.xlsx")
      
##final full-tran-OTC df
OTCfulltran.df <- OTCfullconv.f(OTCraw.df, meta.und.price.df)
     
            