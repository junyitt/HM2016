#01_01_EXC_functions.R

#import RAW-EXC-TRAN
importrawEXC.f <- function(raw_EXC.dir, yy){
      setwd(raw_EXC.dir)
      EXCrawdf <- as.data.frame(read_excel(paste0("EXC_", yy, ".xlsx")))
      EXCrawdf <- cutoff.f(EXCrawdf)   
      EXCrawdf
}

#MAIN CONVERT FUNCTION, that CONVERT RAW-EXCDF to FULL-TRAN-EXCDF
EXCfullconv.f <- function(rawdf, ficmetadf, tkeydf, yy, meta.undprice.df){
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
      
      N <- nrow(rawdf)
      colnames(rawdf) <- c("FIC", "TeamName", "pos1", "Units", "tKey", "Remarks")
      
if(N > 0){      
            #EACH VARIABLES - treatment
            TrackNo <- rep(NA, N)
            classf <- rep("EXC", N)
            FIC <- rawdf[,"FIC"]
            TeamName <- rawdf[,"TeamName"]
            cParty <- rep("Exchange", N)
                  cType <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmetadf, "cType")})           #require meta
                  Underlying <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmetadf, "Underlying")}) #require meta
                  Currency <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmetadf, "Currency")})     #meta
                  kPrice <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmetadf, "kPrice")})         #meta
            pos1 <- rawdf[, "pos1"]
            cff <- sapply(X = 1:N, FUN = function(j){  cff.f(cType[j], pos1[j])  })
            Units <- round(rawdf[,"Units"],0)
                  tDate <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmetadf, "tDate")})    #meta
                  mDate <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmetadf, "mDate")})    #meta
                  tKey <- substr(rawdf[,"tKey"], 4, 7)
            Remarks <- rawdf[,"Remarks"]
      #Notionalvalue
            NVal <- sapply(X = 1:N, function(j){ notionalvalue.f(cType[j], Units[j], Underlying[j], Currency[j], yy, meta.undprice.df) })
            VL <-  sapply(X = 1:N, function(j){  v1 <- VLandVLRexc.v2.f(TeamName[j], rawdf[j,"tKey"], tkeydf, cType[j], pos1[j], Units[j], NVal[j]);  v1[1]   }) 
            VLRemarks <- sapply(X = 1:N, function(j){  v1 <- VLandVLRexc.v2.f(TeamName[j], rawdf[j,"tKey"], tkeydf, cType[j], pos1[j], Units[j], NVal[j]);  v1[2]   }) 
}else{
      TrackNo <- classf<- FIC<- TeamName <- cParty <- cType <- Underlying <- Currency <- kPrice <- pos1 <- cff <- Units <- tDate <- mDate <- tKey <- Remarks <- VL <- VLRemarks <- vector()
      
}

      #output fulldf
      data.frame(TrackNo, classf, FIC, TeamName, cParty, 
                  cType, Underlying, Currency, kPrice, 
                  pos1, cff, Units, 
                  tDate, mDate, tKey, 
                  Remarks, VL, VLRemarks, stringsAsFactors = F)
}

#check validitiy (EXC)
VLandVLRexc.v2.f <- function(TeamName.c, tKey.c, tkey.df, cType.c, pos1.c, Units.c, NVal.c){

      tryCatch({
      allteamname.v <- tkey.df[, "TeamName"]
            uu <- allteamname.v %in% TeamName.c 
            correct.tkey <- tkey.df[uu, "tKey"]
      
      if(!(tKey.c == correct.tkey)){ #invalid if wrong trading key
            c(0, "Wrong Trading Key")
      }else if(cType.c == "Bond" & !(pos1.c == "Long")){ #invalid if short bond
            c(0, "Short Bond is NOT allowed")
      }else if(Units.c < 0){
            c(0, "Negative Number of Units")
      }else if(NVal.c > 10e6){
            c(0, "Notional Value exceeds RM 10,000,000")
      }else{
            c(1, "OK")
      }
}, error = function(e){
      print("VLandVLRexc.v2.f - error in getting VL and VLRemarks under EXCfullconv.f")
})
      
}

notionalvalue.f <- function(cType.c, Units.c, Underlying.c, Currency.c, yy, meta.undprice.df){
      if(Currency.c %in% "MYR"){
            exss0 <- 1
      }else{
            exss0 <- und.price.f(Currency.c, yy, meta.undprice.df)
      }
      
      if(cType.c %in% c("Bond")){
          
            Units.c*exss0*1000
      }else{
            ss0 <- und.price.f(Underlying.c, yy, meta.undprice.df)
            Units.c*exss0*ss0
      }
}

