
#General functions
      #DUPLICATE: CUTOFF function: Cutoff at the n-1 transactions, where the nth transaction' remarks = "cutoffapril"
      subcutdf_f <- function(excfulldf){
            rem1 <- excfulldf[, "Remarks"]
            rem2 <- gsub(tolower(rem1), pattern = "[[:space:]]", replacement = "")
            if("cutoffapril" %in% rem1){
                  nx <- grep("cutoffapril", rem2)
                  excfulldf[1:(nx-1),]
            }else{
                  excfulldf
            }
      } 
      
      #DUPLICATE: check validitiy (EXC)
      VLf <- function(tName, tKey, tKeydf){
            teamname1 <- tKeydf[, "TeamName"]
            teamname1 <- gsub(tolower(teamname1), pattern = "[[:space:]]", replacement = "")     
            tname2 <- gsub(tolower(tName), pattern = "[[:space:]]", replacement = "") 
            uu <- teamname1 %in% tname2
            tkey1 <- tKeydf[uu, "tKey"]
            #invalid if wrong trading key
            if(length(tKey) == 0){
                  0
            }else if(is.na(tKey)){
                  0
            }else if(!(tKey %in% tkey1)){
                  0
            }else{
                  1
            }
      } #Validity check on tkey for yr3 (bond extra), yr 4 (harrym)
      
      #DUPLICATE: return validitiy remarks
      VLRf <- function(vl){
            if(vl==1){"OK"}else{"Invalid Trading Key"}
      }

########YR 0,1 - PV     #must be unique teamname? one only!
      conv1extra_f <- function(EXTRArawdf, tkeydf){
            EXTRArawdf<- subcutdf_f(EXTRArawdf)
            edf <- EXTRArawdf
            N <- nrow(edf)
            colnames(edf) <- c("ExtraName", "tDate", "Service", "tName", "tKey", "Remarks")
                  vl_v <- sapply(1:N, FUN = function(j){VLf(edf[j, "tName"], edf[j, "tKey"], tkeydf)})
                  #Output: spectran
                  edf <- edf[vl_v==1,]
                  #unique team?
                  edf2 <- lapply(tkeydf[,"TeamName"], FUN = function(k){
                        u <- edf[, "tName"] == k
                        edf[u, ][sum(u),]
                  })
                  do.call(rbind,edf2)
      }
      
########YR 2 - arbitrage 

      #DUPLICATE: return the value of the desired variable, input: FIC, metadf, variablename
      exc_valf <- function(FIC, metadf, vname){
            uu <- metadf[,"FIC"] %in% FIC
            #vname can take: Underlying, Currency, kPrice, tDate, mDate
            tryCatch(metadf[uu, vname], error = function(e){NA})
      }

      conv2extra_f <- function(EXTRArawdf, excficmetadf, tkeydf){
            EXTRArawdf <- subcutdf_f(EXTRArawdf) 
if(nrow(EXTRArawdf) == 0){

}else{
      
      #FULL-TRAN required variables list
      fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                    "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                    "VL", "VLRemarks")
      
      #create empty vector for fullvar1
      for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}  
      edf <- EXTRArawdf
      N <- nrow(edf)
      colnames(edf) <- c("FIC", "tName", "pos1", "Units", "tKey", "Remarks")
      
      #EACH VARIABLES - treatment
      TrackNo <- rep(1, N) 
      classf <- rep("EXTRA", N)
      FIC <- edf[,"FIC"]
      rClass <- rep(NA, N) #######TO BE DETERMINED #####!!!!!!!!!!!!
      sClass <- rep(NA,N) #######TO BE DETERMINED #####!!!!!!!!!!!!
      TeamName <- edf[,"tName"]
            cParty <- rep("Banker E2", N)
            cType <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "cType")})           #require meta
            Underlying <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "Underlying")}) #require meta
            Currency <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "Currency")})     #meta
            kPrice <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "kPrice")})         #meta
                  pos1 <- edf[, "pos1"]
                  Units <- round(edf[,"Units"],0)
                  tDate <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "tDate")})    #meta
                  mDate <- sapply(X = FIC, FUN = function(x){exc_valf(x, excficmetadf, "mDate")})    #meta
                  tKey <- substr(edf[,"tKey"], 4, 7)
                  Remarks <- edf[,"Remarks"]
                        VL <-  sapply(X=1:N, function(j){VLf(TeamName[j], edf[j,"tKey"], tkeydf)}) 
                        VLRemarks <- sapply(X=1:N, function(j){VLRf(VL[j])}) 
                        
      #output fulldf
      efdf <<- data.frame(TrackNo, classf, FIC, rClass, sClass, TeamName, cParty, 
            cType, Underlying, Currency, kPrice, 
            pos1, Units, tDate, mDate, tKey, Remarks,
            VL, VLRemarks)
      colnames(efdf) <- fullvar1
      efdf
}
}

########YR 3  BOND FAIR XV

      conv3extra_f <- function(EXTRArawdf, tkeydf){

      #belongs to yr 03 - bond bidding Extra Events ->> meta to full ##meta-FIC-A 
      #need filter calculation - openIPO method
      
      #subcut
      EXTRArawdf <- subcutdf_f(EXTRArawdf) 
      
      colnames(EXTRArawdf) <- c("ExtraName", "tDate", "BondCode", "BiddingPrice", "Units", "tName", "tKey", "Remarks")      
     
      vl_v <- sapply(1:nrow(EXTRArawdf), FUN = function(j){
            VLf(EXTRArawdf[j,"tName"], EXTRArawdf[j,"tKey"], tkeydf)
      }) #validity check on tkey
      EXTRArawdf1 <- EXTRArawdf[vl_v==1,]
      
            bcode <- unique(as.character(EXTRArawdf[,"BondCode"])) #to loop through all unique bond code
            bdf_list <- lapply(bcode, FUN = function(bc){
                  u <- EXTRArawdf1[,"BondCode"] == bc; bdf <- EXTRArawdf1[u,]
                  bdf <- bdf[order(-bdf[,"BiddingPrice"]),]
                  cmsum <- cumsum(bdf[,"Units"]); v <- cmsum >= 100000 #cap issue size = 100k for all bond here
                  if(sum(v) == 0){  #lack in subscription, demand less than issue size 100k
                        price <- min(bdf[,"BiddingPrice"]); 
                        bdf[,"BiddingPrice"] <- rep(price, nrow(bdf))
                  }else{
                        price <- bdf[v,"BiddingPrice"][1]; prorata_u <- bdf[,"BiddingPrice"] == price; pbelow_i <- bdf[,"BiddingPrice"] > price
                        sbelow <- sum(bdf[pbelow_i,"Units"]); ss <- sum(bdf[prorata_u,"Units"]) 
                        rrem <- 100000-sbelow
                        bdf[prorata_u,"Units"] <- round(bdf[prorata_u,"Units"]/ss*rrem,0)
                        uu <- bdf[,"BiddingPrice"] >= price
                        bdf <- bdf[uu, ]
                        bdf[,"BiddingPrice"] <- rep(price, nrow(bdf))
                  }
                        bdf
            })#ipo method, output = valid bond transactions, with "prorata" adjusted units transacted
            bdf <- do.call(rbind,bdf_list) #pre-full transactions
      
      #convert2fulltransactions:
            #FULL-TRAN required variables list
            fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                          "cType", "Underlying", "Currency", "kPrice", 
                          "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                          "VL", "VLRemarks")
            for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}  
            N <- nrow(bdf)
                  TrackNo <- rep("EXTRA", N)   #by the end only add?
                  classf <- rep("EXTRA", N)
                  FIC <- bdf[,"BondCode"] 
                  rClass <- rep(NA, N) #######TO BE DETERMINED #####!!!!!!!!!!!!
                  sClass <- rep(NA,N) #######TO BE DETERMINED #####!!!!!!!!!!!!
                  TeamName <- bdf[, "tName"]
                  cParty <- bdf[, "ExtraName"]
                        cType <- rep("Bond-E", N) 
                        Underlying <- rep(NA,N) 
                        Currency <- rep("MYR",N)
                        kPrice <- bdf[,"BiddingPrice"]     
                        pos1 <- rep("Long",N)
                        Units <- bdf[, "Units"]
                        tDate <- rep(3,N)
                        mDate <- rep(5,N)
                        tKey <- substr(bdf[,"tKey"], 4, 7)
                        Remarks <- bdf[,"Remarks"]
                        VL <-  rep(1,N)
                        VLRemarks <- rep(1,N)
            
            #output fulldf
            data.frame(TrackNo, classf, FIC, rClass, sClass, TeamName, cParty, 
                  cType, Underlying, Currency, kPrice, 
                  pos1, Units, tDate, mDate, tKey, Remarks,
                  VL, VLRemarks)
}


########YR 4 HARRY M: convert EXTRArawdf to fulltran

      compsharpe <- function(weight, rr, cv){
            W <- weight
            n <- nrow(cv)
            VarP <- sum(t(W) %*% cv %*% W)
            RP <- sum(rr*W)
            sharpe <- RP/sqrt(VarP)
            sharpe
      }
      
      maxsharpe <- function(r, cv){
            cv_inv <- solve(cv)
            J <- cv_inv %*% r
            sj <- sum(J)
            
            #compute Weightage, W
            W <- J/sum(J)
            tW <- t(W)
            #compute portfolio expected return
            RP <- sum(W*r)
            #compute portfolio variance
            VarP <- sum(tW %*% cv %*% W) 
            #compute maximized sharpe ratio
            sharpe <- (RP)/sqrt(VarP)
            #output max sharpe ratio
            sharpe
      }
      
      conv4extra_f <- function(EXTRArawdf, tkeydf){
            #yr 04, HarryM.
            #subcut
            EXTRArawdf <- subcutdf_f(EXTRArawdf) 
            
            colnames(EXTRArawdf) <- c("ExtraName", "tDate", "Wa", "Wb", "Wc", "Wd", "tName", "tKey", "Remarks")
            vl_v <- sapply(1:nrow(EXTRArawdf), FUN = function(j){
                  VLf(EXTRArawdf[j,"tName"], EXTRArawdf[j,"tKey"], tkeydf)
            }) #check tkey
            edf <- EXTRArawdf[vl_v == 1,] #only get those valid transactions
            
            #do not use edf, now recreate "temp values" for Alpha 1 to Beta 6
            edfn <- sapply(tkeydf[,1], FUN = function(jj){ #loop through team name
                  u <- edf[, "tName"] == jj
                  e <- edf[u,][sum(u),]
                  w <- t(t(as.numeric((e[,3:6]))))
                  compsharpe(w, rr, cv)
            }) #sharpe ratio for all teams, teams doesn't exist or invalid, will have NA
            cashrewardn <- sapply(edfn, FUN = function(j){ 
                  if(is.na(j)){
                        0
                  }else{
                        maxs <- maxsharpe(rr, cv)
                        dev <- abs((maxs - j)/maxs)
                        threshold <- c(0.01, 0.05, 0.10, 0.20, 0.50)
                        cashr <- c(1e7, 8e6, 5e6, 2e6, 1e6, 0)
                        tr <- dev > threshold
                        cashr[sum(tr)+1]
                  }
            }) #loop through team name #cash
            scoren <- sapply(edfn, FUN = function(j){ #loop through team name
                  if(is.na(j)){
                        0
                  }else{
                        maxs <- maxsharpe(rr, cv); maxsharpeR1 <<- maxs
                        dev <- abs((maxs - j)/maxs)
                        threshold <- c(0.01, 0.05, 0.10, 0.20, 0.50)
                        score <- c(20, 18, 15, 10, 5, 0)/20*15
                        tr <- dev > threshold
                        score[sum(tr)+1]
                  }
            }) #score 
            rem2 <- lapply(tkeydf[,1], FUN = function(jj){ 
                  u <- edf[, "tName"] == jj
                  if(sum(u)==0){
                        "NA"
                  }else{
                        edf[u,"Remarks"][sum(u)]
                  }
            }); rem <- as.character(do.call(rbind, rem2)[,1]) #loop through team name for remarks
            
            #FULL-TRAN required variables list
            fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                          "cType", "Underlying", "Currency", "kPrice", 
                          "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                          "VL", "VLRemarks")
            for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}  
            N <- length(edfn)
                  TrackNo <- rep("EXTRA", N)   #by the end only add?
                  classf <- rep("EXTRA", N)
                  FIC <- rep(NA, N) #cashreward FIC?
                  rClass <- rep(NA, N) #######TO BE DETERMINED #####!!!!!!!!!!!!
                  sClass <- rep(NA,N) #######TO BE DETERMINED #####!!!!!!!!!!!!
                  TeamName <- names(scoren)
                  cParty <- rep("Harry M.", N)
                        cType <- rep("Cash", N) 
                        Underlying <- rep(NA,N) 
                        Currency <- rep("MYR",N)
                        kPrice <- round(edfn,6)       #sharpe ratio
                              pos1 <- rep("Receive",N)
                              Units <- cashrewardn
                              tDate <- rep(4,N)
                              mDate <- rep(5,N)
                              tKey <- rep("tkey", N) #fixed later
                              Remarks <- rem
                                    VL <-  rep(1,N)
                                    VLRemarks <- scoren #; score.extra4 <<- scoren
      
                              #output fulldf
                              data.frame(TrackNo, classf, FIC, rClass, sClass, TeamName, cParty, 
                                    cType, Underlying, Currency, kPrice, 
                                    pos1, Units, tDate, mDate, tKey, Remarks,
                                    VL, VLRemarks)
      }


