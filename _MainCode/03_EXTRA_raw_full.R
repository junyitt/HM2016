#03_EXTRA_raw_full.R

######################
      #4_ConvUser EXTRA
######################

#DIR
#maincode.dir, raw_EXTRA.dir, meta.dir

#FUNCTION (general) Required
#importmeta.f, cutoff.f ;returnmetaval.f

      #ENVIRONMENT Output
      #tkeydf, ficmeta.df
      #maxsharpe.c, rr, cv
            #$spectran.df, extrafulltran.df

#FUNCTION (internal)  
      #VLandVLR.EXTRA.v2.f, maxfour.f, check.tkey.f
      #outbd.df.f
      #compsharpe, maxsharpe, validrawdf.f
      #conv1extra.f,conv2extra.f, conv3extra.f, conv4extra.f

setwd(maincode.dir)
source("03_01_EXTRA_functions.R")


#####################
# full conv extra
#####################
#META-tkeydf
      tkeydf <- importmeta.f(meta.dir, "meta-tradingkey.xlsx")
                             
#META-yr4-rr and cv, covariance matrix of 4 assets
      setwd(meta.dir)
      rr <- as.data.frame(read_excel("meta-extra-4-harrym.xlsx", sheet = 1)); rr <- t(t(rr[,2]))
      cv <- as.matrix(as.data.frame(read_excel("meta-extra-4-harrym.xlsx", sheet = 2)))
      
#META-FIC
      ficmeta.df <- importmeta.f(meta.dir, "meta-FIC.xlsx")
#meta-employservice
      employserv.meta.df <- importmeta.f(meta.dir, "meta-employservice.xlsx")
      
{
      #Create empty extrafulltran.df
            fullvar1 <- fullvar.f()
            #create empty vector for fullvar1
            for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}; 
            extrafulltran.df <-   data.frame(TrackNo, classf, FIC, TeamName, cParty, 
                                            cType, Underlying, Currency, kPrice, 
                                            pos1, cff, Units, 
                                            tDate, mDate, tKey, 
                                            Remarks, VL, VLRemarks, stringsAsFactors = F)
      #create empty spectran.df
            fullvar2 <- c("ExtraName", "tDate", "Service", "TeamName", "tKey", "Remarks")
            for(i in 1:length(fullvar2)){assign(fullvar2[i], value = vector())}; 
            spectran.df <- data.frame(ExtraName, tDate, Service, TeamName, tKey, Remarks, stringsAsFactors = F)
            
} #create empty df

##Import EXTRAraw.df      
if(yy %in% c(0,1)){
      setwd(raw_EXTRA.dir)
      EXTRArawdfa <- as.data.frame(read_excel(paste0("EXTRA_", yy, "A.xlsx"))); EXTRArawdfa <- cutoff.f(EXTRArawdfa) ##Added startjan
      EXTRArawdfb <- as.data.frame(read_excel(paste0("EXTRA_", yy, "B.xlsx"))); EXTRArawdfb <- cutoff.f(EXTRArawdfb) ##Added startjan
      EXTRAraw.df <- rbind(EXTRArawdfa, EXTRArawdfb)
}else{
      setwd(raw_EXTRA.dir)
      EXTRArawdf <- as.data.frame(read_excel(paste0("EXTRA_", yy, ".xlsx")))
      EXTRAraw.df <- cutoff.f(EXTRArawdf)
}

##CONVERT EXTRAraw.df to spectran.df/extrafulltran.df
if(yy %in% c(0,1)){ 
      #belongs to yr 00, 01, -- PV, PV,  Extra Events ->> special treatment > SPEC DF
      spectran.df <- conv1extra.f(EXTRAraw.df, tkeydf, emptydf = spectran.df)
      extrafulltran.df <- conv1extra.employ.f(EXTRAraw.df, tkeydf, employserv.meta.df, emptydf = extrafulltran.df)
}else if(yy %in% c(2)){
      #yr2 - arbitrage - FIC for options
      extrafulltran.df <- conv2extra.f(EXTRAraw.df, ficmeta.df, tkeydf)
      
}else if(yy %in% c(3)){
      #yr3 - bond
      extrafulltran.df <- conv3extra.f(EXTRAraw.df, tkeydf)
            
}else if(yy %in% c(4)){
      #yr4 - harry m
      extrafulltran.df <- conv4extra.f(EXTRAraw.df, tkeydf, rr, cv)
      maxsharpe.c <<- maxsharpe(rr, cv)
}

