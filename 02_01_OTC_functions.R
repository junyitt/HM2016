
#####OTC FUNCTIONS

#NUMBER ONE PRIORITY, DUPLICATE FIRST and FIX POSITION

##DUPLICATE: Main: DUPf
duplicate_f <- function(rawdf){
      colnames(rawdf) <- c("cType", "Team1", "Team2", "posTeam1",
                           "Underlying", "kPrice", "Units", "tDate",
                           "mDate", "Remarks")
      rawdf2 <- rawdf
      rawdf2[,"Team1"] <- rawdf[, "Team2"]  #flip team 1 and team 2 
      rawdf2[,"Team2"] <- rawdf[, "Team1"]
      rawdf2[,"posTeam1"] <- sapply(rawdf[,"posTeam1"], FUN = function(j){flippos_f(j)})
      rawdf0 <- rbind(rawdf, rawdf2)
            #FIX
            rawdf0[, "Units"] <- round(rawdf0[, "Units"],0) #round
            rawdf0[,"posTeam1"] <- sapply(1:nrow(rawdf0), FUN = function(j){posfix_f(rawdf0[j,"cType"], rawdf0[j, "posTeam1"])}) #fix position
            rawdf0[,"Underlying"] <- sapply(1:nrow(rawdf0), FUN = function(j){loanNAfix_f(rawdf0[j,"cType"], rawdf0[j,"Underlying"])}) #fix underlying NA for loan
                  #output, duplicated and fixed df
                  rawdf0
}

#DUPf: flip Long to Short, Lend to Borrow and vice versa
      flippos_f <- function(pos1){
      p1 <- c("Long", "Short"); p2 <- c("Lend", "Borrow")
      if(pos1 %in% p1){
            uu <- !(pos1 == p1)
            p1[uu]
      }else{
            uu <- !(pos1 == p2)
            p2[uu]
      }
}
      
#DUPf: FIX Long and Short to Borrow and Lend for loan and forward
      posfix_f <- function(cType, pos){
            p1 <- c("Long", "Short"); p2 <- c("Lend", "Borrow")
            if(cType %in% "Forward"){
                  if(pos %in% p2){
                        p1[pos == p2]
                  }else{pos}             
            }else if(cType %in% "Loan"){
                  if(pos %in% p1){
                        p2[pos == p1]     
                  }else{pos}     
            }else{pos}
      }

#DUPf: loanunderlying -> chg to NA
      loanNAfix_f <- function(cType, Underlying){
      if(cType %in% "Loan"){
            NA
      }else{
            Underlying
      }
}
      
#############################################################################
#CONVf: add currrency
      curr_f <- function(Underlying){
            if(Underlying %in% "GOL"){
                  "USD"     
            }else{
                  "MYR"
            }
      }
      
#CONVf: VL      
      VLOTCf <- function(cType, Underlying, kPrice, tDate, mDate, yy){
            #1) no underlying for Forward - ignore for loan
            if(cType == "Forward" & !Underlying %in% c("GOL", "CRU", "PAL", "USD", "EUR")){
                  0
                  #2) kPrice boundary
            }else if(cType == "Loan" & !(kPrice >= 0.01 & kPrice <= 0.15)){
                  0
            }else if(Underlying %in% "GOL" & !(kPrice >= 500 & kPrice <= 2000)){
                  0
            }else if(Underlying %in% "CRU" & !(kPrice >= 100 & kPrice <= 400)){
                  0
            }else if(Underlying %in% "PAL" & !(kPrice >= 1000 & kPrice <= 3000)){
                  0
            }else if(Underlying %in% "USD" & !(kPrice >= 2.5 & kPrice <= 5.0)){
                  0
            }else if(Underlying %in% "EUR" & !(kPrice >= 3.0 & kPrice <= 6.0)){
                  0
                  #3) tDate, mDate boundary
            }else if(tDate >= mDate | !(tDate >= 0 & tDate <= 4) | !(mDate >= 1 & mDate <= 5)){
                  0
                  #4) tDate not during yy
            }else if(!(tDate == yy)){
                  0
            }else{
                  1
            }
            
            #FORCED#3) remarks? -"CONFIRM"
            # -GOL [500,2000]
            # -CRU [100, 400]
            # -PAL [1000,3000]
            # -USD [2.5, 5.0]
            # -EUR [3.0, 6.0]
            # -interest rate on loan [1%, 15%]
      }

#CONVf: VLRemarks
      VLROTCf <- function(cType, Underlying, kPrice, tDate, mDate, yy){
            p <- vector(); p <- rep(F,4)
            if(cType == "Forward" & !Underlying %in% c("GOL", "CRU", "PAL", "USD", "EUR")){
                  p[1] <- TRUE
                  
            }
            
            if(cType == "Loan" & !(kPrice >= 0.01 & kPrice <= 0.15)){
                  p[2] <- TRUE
            }else if(Underlying %in% "GOL" & !(kPrice >= 500 & kPrice <= 2000)){
                  p[2] <- TRUE
            }else if(Underlying %in% "CRU" & !(kPrice >= 100 & kPrice <= 400)){
                  p[2] <- TRUE
            }else if(Underlying %in% "PAL" & !(kPrice >= 1000 & kPrice <= 3000)){
                  p[2] <- TRUE
            }else if(Underlying %in% "USD" & !(kPrice >= 2.5 & kPrice <= 5.0)){
                  p[2] <- TRUE
            }else if(Underlying %in% "EUR" & !(kPrice >= 3.0 & kPrice <= 6.0)){
                  p[2] <- TRUE
            }
            
            if(tDate >= mDate | !(tDate >= 0 & tDate <= 4) | !(mDate >= 1 & mDate <= 5)){
                  p[3] <- TRUE
            }
            
            if(!(tDate == yy)){
                  p[4] <- TRUE
            }
            
            
            emsg <- c("Invalid Underlying for Forward", "Invalid kPrice", "Invalid tDate/mDate", "tDate <> currentyr")
            paste(emsg[p], sep="", collapse="--") 
      }

##CONV-TO-FULL-TRAN-DF: Main: CONVf
OTCfullconv_f <- function(dupdf){
      #FULL-TRAN required variables list
      fullvar1 <- c("TrackNo", "classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                    "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                    "VL", "VLRemarks")
      
      #create empty vector for fullvar1
      for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}  
      N <- nrow(dupdf)
      colnames(dupdf) <- c("cType", "Team1", "Team2", "posTeam1",
                          "Underlying", "kPrice", "Units", "tDate",
                          "mDate", "Remarks")
      
      #EACH VARIABLES - treatment
      TrackNo <- rep(1, N)
      classf <- rep("OTC", N)
      FIC <- rep(NA, N)
      rClass <- rep(NA, N) #######TO BE DETERMINED #####!!!!!!!!!!!!
      sClass <- rep(NA,N) #######TO BE DETERMINED #####!!!!!!!!!!!!
      TeamName <- dupdf[,"Team1"]
      cParty <- dupdf[,"Team2"]
            cType <- dupdf[,"cType"]
            Underlying <- dupdf[,"Underlying"]
            Currency <- sapply(X = Underlying, FUN = function(x){curr_f(x)})   
            kPrice <- dupdf[,"kPrice"]
                  pos1 <- dupdf[, "posTeam1"]
                  Units <- dupdf[, "Units"]
                  tDate <- dupdf[, "tDate"]
                  mDate <- dupdf[, "mDate"]
                  tKey <- rep(NA, N)
                  Remarks <- dupdf[,"Remarks"]
                        VL <-  sapply(X=1:N, function(j){VLOTCf(cType[j], Underlying[j], kPrice[j], tDate[j], mDate[j],yy)}) 
                        VLRemarks <- sapply(X=1:N, function(j){VLROTCf(cType[j], Underlying[j], kPrice[j], tDate[j], mDate[j],yy)}) 
      
      #output fulldf
      data.frame(TrackNo, classf, FIC, rClass, sClass, TeamName, cParty, 
            cType, Underlying, Currency, kPrice, 
            pos1, Units, tDate, mDate, tKey, Remarks,
            VL, VLRemarks)
}

#general, duplicate: TRACKNO function
trackno_f <- function(fulltrandf){
      trackv <- rep(1, nrow(fulltrandf))
      yearuni <- as.integer(unique(fulltrandf[,"tDate"]))
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