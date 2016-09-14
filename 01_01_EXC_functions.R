#01_01_EXC_functions.R

#MAIN CONVERT FUNCTION, that CONVERT RAW-EXCDF to FULL-TRAN-EXCDF
##INPUT: rawdf, tkeydf, yy
EXCfullconv_f <- function(excdf, excficmetadf, tkeydf){
      #FULL-TRAN required variables list
      fullvar1 <- c("classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                    "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                    "VL", "VLRemarks")
      
      #create empty vector for fullvar1
      for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}  
      N <- nrow(excdf)
      colnames(excdf) <- c("FIC", "tName", "pos1", "Units", "tKey", "Remarks")
      
            #EACH VARIABLES - treatment
            classf <- rep("EXC", N)
            FIC <- excdf[,"FIC"]
            rClass <- rep(NA, N) #######TO BE DETERMINED #####!!!!!!!!!!!!
            sClass <- rep(NA,N) #######TO BE DETERMINED #####!!!!!!!!!!!!
            TeamName <- excdf[,"tName"]
            cParty <- rep("Exchange", N)
                  cType <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "cType")})           #require meta
                  Underlying <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "Underlying")}) #require meta
                  Currency <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "Currency")})     #meta
                  kPrice <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "kPrice")})         #meta
                  pos1 <- excdf[, "pos1"]
                  Units <- round(excdf[,"Units"],0)
                  tDate <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "tDate")})    #meta
                  mDate <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "mDate")})    #meta
                  tKey <- substr(excdf[,"tKey"], 4, 7)
                  Remarks <- excdf[,"Remarks"]
                        VL <-  sapply(X=1:N, function(j){VLf(TeamName[j], tKey[j], tkeydf)}) 
                        VLRemarks <- sapply(X=1:N, function(j){VLRf(VL[j])}) 
      
      #output fulldf
      cbind(classf, FIC, rClass, sClass, TeamName, cParty, 
            cType, Underlying, Currency, kPrice, 
            pos1, Units, tDate, mDate, tKey, Remarks,
            VL, VLRemarks)
}




#return the value of the desired variable, input: FIC, metadf, variablename
exc_valf <- function(FIC, metadf, vname){
      uu <- metadf[,"FIC"] == FIC
      #vname can take: Underlying, Currency, kPrice, tDate, mDate
      tryCatch(metadf[uu, vname], error = function(e){NA})
}

#check validitiy (EXC)
VLf <- function(tName, tKey, tKeydf){
      teamname1 <- tKeydf[, "Team Name"]
      teamname1 <- gsub(tolower(teamname1), pattern = "[[:space:]]", replacement = "")     
      tname2 <- gsub(tolower(tName), pattern = "[[:space:]]", replacement = "") 
            uu <- teamname1 == tname2
            tkey1 <- tKeydf[uu, "Trading Key"]
            if(tKey == tkey1){1}else{0}
}

#return validitiy remarks
VLRf <- function(vl){
      if(vl==1){"OK"}else{"Invalid Trading Key"}
}