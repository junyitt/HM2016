#01_01_EXC_functions.R

#MAIN CONVERT FUNCTION, that CONVERT RAW-EXCDF to FULL-TRAN-EXCDF
##INPUT: rawdf, tkeydf
EXCfullconv_f <- function(excdf, excficmetadf, tkeydf){
      #FULL-TRAN required variables list
      fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                    "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                    "VL", "VLRemarks")
      
      #create empty vector for fullvar1
      for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}  
      N <- nrow(excdf)
      colnames(excdf) <- c("FIC", "tName", "pos1", "Units", "tKey", "Remarks")
      
            #EACH VARIABLES - treatment
            TrackNo <- rep(1, N)
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
                        VL <-  sapply(X=1:N, function(j){VLf(TeamName[j], tKey[j], tkeydf, cType[i], pos1[i])}) 
                        VLRemarks <- sapply(X=1:N, function(j){VLRf(VL[j])}) 
      
      #output fulldf
      cbind(classf, FIC, rClass, sClass, TeamName, cParty, 
            cType, Underlying, Currency, kPrice, 
            pos1, Units, tDate, mDate, tKey, Remarks,
            VL, VLRemarks)
}

#CUTOFF function: Cutoff at the n-1 transactions, where the nth transaction' remarks = "cutoffapril"
subcutdf_f <- function(excfulldf){
      rem1 <- excfulldf[, "Remarks"]
      rem2 <- gsub(tolower(rem1), pattern = "[[:space:]]", replacement = "")
      if("cutoffapril" %in% aa){
            nx <- grep("cutoffapril", rem2)
            excfulldf[1:(nx-1),]
      }else{
            excfulldf
      }
}

#return the value of the desired variable, input: FIC, metadf, variablename
exc_valf <- function(FIC, metadf, vname){
      uu <- metadf[,"FIC"] == FIC
      #vname can take: Underlying, Currency, kPrice, tDate, mDate
      tryCatch(metadf[uu, vname], error = function(e){NA})
}

#check validitiy (EXC)
VLf <- function(tName, tKey, tKeydf, cType, pos1){
      teamname1 <- tKeydf[, "Team Name"]
      teamname1 <- gsub(tolower(teamname1), pattern = "[[:space:]]", replacement = "")     
      tname2 <- gsub(tolower(tName), pattern = "[[:space:]]", replacement = "") 
            uu <- teamname1 == tname2
            tkey1 <- tKeydf[uu, "Trading Key"]
            #invalid if wrong trading key
            if(!(tKey == tkey1)){
                  0
            #invalid if short bond
            }else if(cType == "Bond" & !(pos1 == "Long")){
                  0
            }else{
                  1
            }
}

#return validitiy remarks
VLRf <- function(vl){
      if(vl==1){"OK"}else{"Invalid Trading Key or Short Bond"}
}

#TRACKNO function
trackno_f <- function(fulltrandf){
      trackv <- rep(1, nrow(fulltrandf))
      yearuni <- unique(fulltrandf[,"tDate"])
      classfuni <- unique(fulltrandf[,"classf"])
      for(yrr in yearuni){
            for(clf in classfuni){
                  Y <- fulltrandf[,"tDate"] == yrr; Z <- fulltrandf[,"classf"] == clf
                  nt <- length(trackv[Y & Z])
                  start <- yrr+100; mid <- substr(clf, 1,3)
                  trackv[Y & Z] <- sapply(1:nt, FUN = function(j){
                        paste0(start, mid, (1000+j))      
                  })
            }
      }
      #output: track number vector
      trackv
      
}