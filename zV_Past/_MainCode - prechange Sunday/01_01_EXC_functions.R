#01_01_EXC_functions.R

#import RAW-EXC-TRAN
importrawEXC.f <- function(raw_EXC.dir, yy){
      setwd(raw_EXC.dir)
      EXCrawdf <- as.data.frame(read_excel(paste0("EXC_", yy, ".xlsx")))
      EXCrawdf <- cutoff.f(EXCrawdf)   
      EXCrawdf
}

#MAIN CONVERT FUNCTION, that CONVERT RAW-EXCDF to FULL-TRAN-EXCDF
EXCfullconv.f <- function(rawdf, ficmetadf, tkeydf){
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
      
      N <- nrow(rawdf)
      colnames(rawdf) <- c("FIC", "TeamName", "pos1", "Units", "tKey", "Remarks")
      
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
            VL <-  sapply(X = 1:N, function(j){  v1 <- VLandVLRexc.v2.f(TeamName[j], rawdf[j,"tKey"], tkeydf, cType[j], pos1[j], Units[j]);  v1[1]   }) 
            VLRemarks <- sapply(X = 1:N, function(j){  v1 <- VLandVLRexc.v2.f(TeamName[j], rawdf[j,"tKey"], tkeydf, cType[j], pos1[j], Units[j]);  v1[2]   }) 
      
      #output fulldf
      data.frame(TrackNo, classf, FIC, TeamName, cParty, 
                  cType, Underlying, Currency, kPrice, 
                  pos1, cff, Units, 
                  tDate, mDate, tKey, 
                  Remarks, VL, VLRemarks, stringsAsFactors = F)
}

#check validitiy (EXC)
VLandVLRexc.v2.f <- function(TeamName.c, tKey.c, tkey.df, cType.c, pos1.c, Units.c){

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
      }else{
            c(1, "OK")
      }
}, error = function(e){
      print("VLandVLRexc.v2.f - error in getting VL and VLRemarks under EXCfullconv.f")
})
      
}


