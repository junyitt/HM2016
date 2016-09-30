#03_EXTRA_raw_full.R
#############
startcut.f.dir <- "C:/Users/User/Google Drive/z_ALLHM/"
setwd(startcut.f.dir); source("01A_startcutoff_f.R")
########


######################
      #4_ConvUser EXTRA
######################

#DIR
EXTRArawdir <- "C:/Users/User/OneDrive/1_Form_EXTRA"
if(real_run == T){EXTRArawdir<-gsub("1", "f", EXTRArawdir)}else{}

EXTRAfdir <- "C:/Users/User/Google Drive/z_ALLHM/"
tkeymetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta/"
cvmetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta/"
      excficmetadir <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"

#META-tkeydf
      setwd(tkeymetadir)
      tkeydf <- as.data.frame(read_excel("meta-tradingkey.xlsx"))
#META-yr4-rr and cv, covariance matrix of 4 assets
      setwd(cvmetadir)
      rr <- as.data.frame(read_excel("meta-extra-4-harrym.xlsx", sheet = 1)); rr <- t(t(rr[,2]))
      cv <- as.matrix(as.data.frame(read_excel("meta-extra-4-harrym.xlsx", sheet = 2)))
#META-FIC
      setwd(excficmetadir)
      excficmetadf <- as.data.frame(read_excel("meta-FIC-EXC.xlsx"))
      
#FUNCTION
setwd(EXTRAfdir)
source("03_01_EXTRA_functions.R")

{
      #Create empty extrafulltrandf
            fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                          "cType", "Underlying", "Currency", "kPrice", 
                          "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                          "VL", "VLRemarks")
            #create empty vector for fullvar1
            for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}; 
            extrafulltrandf <- as.data.frame(cbind(TrackNo, classf, FIC, rClass, sClass, TeamName, cParty, 
                                    cType, Underlying, Currency, kPrice, 
                                    pos1, Units, tDate, mDate, tKey, Remarks,
                                    VL, VLRemarks))
      #create empty spectrandf
            fullvar2 <- c("ExtraName", "tDate", "Service", "tName", "tKey", "Remarks")
            for(i in 1:length(fullvar2)){assign(fullvar2[i], value = vector())}; 
            spectrandf <- as.data.frame(cbind(ExtraName, tDate, Service, tName, tKey, Remarks))
            
      } #create empty df

#set yy
# yy <- 0
# yy <- 4
if(yy %in% c(0,1)){
      setwd(EXTRArawdir)
      EXTRArawdfa <- as.data.frame(read_excel(paste0("EXTRA_", yy, "A.xlsx"))); EXTRArawdfa <- startcutoff.f(EXTRArawdfa) ##Added startjan
      EXTRArawdfb <- as.data.frame(read_excel(paste0("EXTRA_", yy, "B.xlsx"))); EXTRArawdfb <- startcutoff.f(EXTRArawdfb) ##Added startjan
      EXTRArawdf <- rbind(EXTRArawdfa, EXTRArawdfb)
}else{
      setwd(EXTRArawdir)
      EXTRArawdf <- as.data.frame(read_excel(paste0("EXTRA_", yy, ".xlsx")))
      EXTRArawdf <- startcutoff.f(EXTRArawdf) ##Added startjan
}

if(yy %in% c(0,1)){ 
      #belongs to yr 00, 01, -- PV, PV,  Extra Events ->> special treatment > SPEC DF
      spectrandf <- conv1extra_f(EXTRArawdf, tkeydf)
      
}else if(yy %in% c(2)){
      #yr2 - arbitrage - FIC for options
      setwd(EXTRAfdir)
      source("03_01_EXTRA_functions.R")
      extrafulltrandf <- conv2extra_f(EXTRArawdf, excficmetadf, tkeydf)
      
}else if(yy %in% c(3)){
      #yr3 - bond
      extrafulltrandf <- conv3extra_f(EXTRArawdf, tkeydf)
            
}else if(yy %in% c(4)){
      #yr4 - harry m
      extrafulltrandf <- conv4extra_f(EXTRArawdf, tkeydf)
}

