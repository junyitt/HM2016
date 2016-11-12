#tkey check: return 0 or 1
check.tkey.f <- function(TeamName.c, tKey.c, tkey.df){
            allteamname.v <- tkey.df[, "TeamName"]
            uu <- allteamname.v %in% TeamName.c 
            correct.tkey <- tkey.df[uu, "tKey"]
            
            if(!(tKey.c == correct.tkey)){ #invalid if wrong trading key
                  0
            }else{
                  1
            }
} 
      
#yr2: max N=6 transactions
maxfour.f <- function(EXTRAraw.df, tkeydf, maxntran = 6){
      N <- nrow(EXTRAraw.df)
      vl.v <- sapply(1:N, FUN = function(j){    check.tkey.f(EXTRAraw.df[j, "TeamName"], EXTRAraw.df[j, "tKey"], tkeydf)      })
      truedf <- EXTRAraw.df[vl.v %in% 1,]
            uniqueteam.v <- unique(truedf[,"TeamName"])
            rawtran.team.df.list <- lapply(uniqueteam.v, FUN = function(x){
                                    u <- truedf[,"TeamName"] %in% x
                                    if(sum(u) > maxntran){
                                          tdf <- truedf[u,]; N2 <- nrow(tdf)
                                          tdf[(N2-maxntran+1):N2,] #latest 6 transactions will be chosen
                                    }else{
                                          tdf <- truedf[u,]
                                          tdf
                                    }
                              })
      do.call(rbind, rawtran.team.df.list)
}

#Validity check and validity remarks
VLandVLR.EXTRA.v2.f <- function(TeamName.c, tKey.c, tkey.df){
      if(check.tkey.f(TeamName.c, tKey.c, tkey.df) == 0){
            c(0, "Wrong trading key")
      }else{
            
            c(1, "EXTRA: OK")
      }
}
      

########YR 0,1 - PV     #must be unique teamname? one only!
conv1extra.f <- function(EXTRAraw.df, tkeydf, emptydf){ #SPECTRAN ##filter out excess/invalid raw extra transactions.
N <- nrow(EXTRAraw.df)
if(N == 0){
      emptydf
}else{
      colnames(EXTRAraw.df) <- c("ExtraName", "tDate", "Service", "TeamName", "tKey", "Remarks")
      
      #Check Trading Key
            vl.v <- sapply(1:N, FUN = function(j){    check.tkey.f(EXTRAraw.df[j, "TeamName"], EXTRAraw.df[j, "tKey"], tkeydf)      })
            
            #Output: valid spectran1
            edf1 <- EXTRAraw.df[vl.v %in% 1,]
            
      #1 Contract per team
      uniqueteam.v <- unique(EXTRAraw.df[, "TeamName"])
      
      edf2 <- lapply(uniqueteam.v, FUN = function(k){
            u <- edf1[, "TeamName"] %in% k
            edf1[u, ][sum(u),]
      })
      
      edf3 <- do.call(rbind,edf2)
      edf3
}
}


conv1extra.employ.f <- function(EXTRAraw.df, tkeydf, employserv.meta.df, emptydf){ #employ service pay transactions
      N <- nrow(EXTRAraw.df)
      if(N == 0){
            emptydf
      }else{
            colnames(EXTRAraw.df) <- c("ExtraName", "tDate", "Service", "TeamName", "tKey", "Remarks")
            
            #Check Trading Key
            vl.v <- sapply(1:N, FUN = function(j){    check.tkey.f(EXTRAraw.df[j, "TeamName"], EXTRAraw.df[j, "tKey"], tkeydf)      })
      if(sum(vl.v) == 0){
            emptydf
      }else{
            u1 <- vl.v %in% 1
            
            u2 <- !EXTRAraw.df[, "Service"] %in% c("None")
                  
            #Output: valid spectran1
            edf1 <- EXTRAraw.df[u1 & u2,]
            
            #1 Contract per team
            uniqueteam.v <- unique(EXTRAraw.df[, "TeamName"])
            
            edf2 <- lapply(uniqueteam.v, FUN = function(k){
                  u <- edf1[, "TeamName"] %in% k
                  edf1[u, ][sum(u),]
            })
            
            edf3 <- do.call(rbind,edf2)
            
            #compare and get the employ service pay transaction
            service.v <- edf3[,"Service"]
            servtran.df.list <- lapply(service.v, FUN = function(serv){
                  u <- employserv.meta.df[, "FIC"] %in% serv
                  employserv.meta.df[u, 1:18]
            })
            servtran.df <- do.call(rbind, servtran.df.list)
      N <- nrow(servtran.df)
      servtran.df[, "TeamName"] <- edf3[, "TeamName"]
      servtran.df[,"Remarks"] <- servtran.df[,"tKey"] <- rep(NA,N) ; servtran.df[,"VL"] <- servtran.df[,"VLRemarks"] <- rep(1, N)   #add remaining columns
      servtran.df[,"cff"] <- sapply(X = 1:N, FUN = function(j){ cff.f(cType.c = servtran.df[j,"cType"], pos1.c = servtran.df[j,"pos1"])   }) #add cff
            servtran.df
      }
      }
}


########YR 2 - arbitrage 
      
conv2extra.f <- function(EXTRAraw.df, employserv.meta.df, tkeydf){ #required function: #returnmetaval.f(fic, ficmeta.df, vname)
if(nrow(EXTRAraw.df) == 0){
      EXTRAraw.df
}else{
      
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
      
      #subset only latest 4 transactions per team
      colnames(EXTRAraw.df) <- c("FIC", "TeamName", "pos1", "Units", "tKey", "Remarks")
            edf <- maxfour.f(EXTRAraw.df, tkeydf, 6)
                  N <- nrow(edf)
            
      
      #EACH VARIABLES - treatment
      TrackNo <- rep(1, N) 
      classf <- rep("EXTRA", N)
      FIC <- edf[,"FIC"]
      TeamName <- edf[,"TeamName"]
      cParty <- rep("Banker(EXTRA)", N)
            cType <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmeta.df, "cType")})           #require meta
            Underlying <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmeta.df, "Underlying")}) #require meta
            Currency <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmeta.df, "Currency")})     #meta
            kPrice <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmeta.df, "kPrice")})         #meta
      pos1 <- edf[, "pos1"]
      cff <- sapply(X = 1:N, FUN = function(j){    cff.f(cType.c = cType[j], pos1.c = pos1[j])   })
      Units <- round(edf[,"Units"],0)
            tDate <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmeta.df, "tDate")})    #meta
            mDate <- sapply(X = FIC, FUN = function(x){returnmetaval.f(x, ficmeta.df, "mDate")})    #meta
            tKey <- substr(edf[,"tKey"], 4, 7)
      Remarks <- edf[,"Remarks"]
      VL <-  sapply(X=1:N, function(j){     v3 <- VLandVLR.EXTRA.v2.f(TeamName[j], edf[j,"tKey"], tkeydf); v3[1]       }) 
      VLRemarks <- sapply(X=1:N, function(j){     v3 <- VLandVLR.EXTRA.v2.f(TeamName[j], edf[j,"tKey"], tkeydf); v3[2]       })  
                        
      #output fulldf
      data.frame(TrackNo, classf, FIC, TeamName, cParty, 
                 cType, Underlying, Currency, kPrice, 
                 pos1, cff, Units, 
                 tDate, mDate, tKey, 
                 Remarks, VL, VLRemarks, stringsAsFactors = F)
}
}

########YR 3  BOND FAIR XV

#output: filtered trading key, post-OPENIPO method, PRE-fulltran, b.df
outbd.df.f <- function(EXTRAraw.df, tkeydf){
      colnames(EXTRAraw.df) <- c("ExtraName", "tDate", "FIC", "BiddingPrice", "Units", "TeamName", "tKey", "Remarks")      
      
      #Filter Trading Key
      N <- nrow(EXTRAraw.df)
      vl.v <- sapply(1:N, FUN = function(j){    check.tkey.f(EXTRAraw.df[j, "TeamName"], EXTRAraw.df[j, "tKey"], tkeydf)      })
      u1 <- vl.v %in% 1; u2 <- EXTRAraw.df[, "Units"] > 0;  u3 <- EXTRAraw.df[, "BiddingPrice"] >= 50; 
      EXTRAraw.df1 <- EXTRAraw.df[u1 & u2 & u3,]
      
      
      FIC <- unique(as.character(EXTRAraw.df1[,"FIC"])) #to loop through all unique bond code
      bdf_list <- lapply(FIC, FUN = function(bc){ #loop through all unique bond code; return final df 
            u <- EXTRAraw.df1[,"FIC"] %in% bc; bdf <- EXTRAraw.df1[u,]
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
      bdf2 <- do.call(rbind,bdf_list) #pre-full transactions
      bdf2
}

conv3extra.f <- function(EXTRAraw.df, tkeydf){

if(nrow(EXTRAraw.df) == 0){
      EXTRAraw.df
}else{
      
#filtered trading key, post-OPENIPO method -> bdf
bdf <- outbd.df.f(EXTRAraw.df, tkeydf)

#convert2fulltransactions:
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
      N <- nrow(bdf)
            
      TrackNo <- rep(NA, N) 
      classf <- rep("EXTRA", N)
      FIC <- bdf[,"FIC"] 
      TeamName <- bdf[, "TeamName"]
      cParty <- bdf[, "ExtraName"]
            cType <- rep("Bond-E", N) 
            Underlying <- rep(NA,N) 
            Currency <- rep("MYR",N)
            kPrice <- bdf[,"BiddingPrice"]     
      pos1 <- rep("Long",N)
      cff <- sapply(X = 1:N, FUN = function(j){    cff.f(cType[j], pos1[j])     })
      Units <- bdf[, "Units"]
            tDate <- rep(3,N)
            mDate <- rep(5,N)
            tKey <- substr(bdf[,"tKey"], 4, 7)
            Remarks <- bdf[,"Remarks"]
            VL <-  rep(1,N)
            VLRemarks <- rep("EXTRA3: OK",N)
            
#output fulldf
data.frame(TrackNo, classf, FIC, TeamName, cParty, 
           cType, Underlying, Currency, kPrice, 
           pos1, cff, Units, 
           tDate, mDate, tKey, 
           Remarks, VL, VLRemarks, stringsAsFactors = F)

}
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

validrawdf.f <- function(EXTRAraw.df, tkeydf){
      colnames(EXTRAraw.df) <- c("ExtraName", "tDate", "Wa", "Wb", "Wc", "Wd", "TeamName", "tKey", "Remarks")
      N <- nrow(EXTRAraw.df)
      vl.v <- sapply(1:N, FUN = function(j){    check.tkey.f(EXTRAraw.df[j, "TeamName"], EXTRAraw.df[j, "tKey"], tkeydf)      })
            edf0 <- EXTRAraw.df[vl.v == 1,] #only get those valid transactions
            uniqueteam.v <- edf0[,"TeamName"]
            edf1 <- lapply(uniqueteam.v, FUN = function(x){
                  u <- edf0[,"TeamName"] %in% x
                  edf0[u,][sum(u),]         #get latest entry 
            })
            edf2 <- do.call(rbind,edf1)
            edf2
}
      
conv4extra.f <- function(EXTRAraw.df, tkeydf, rr, cv){
if(nrow(EXTRAraw.df) == 0){
      for(i in 1:length(fullvar1)){assign(fullvar1[i], value = vector())}; 
      data.frame(TrackNo, classf, FIC, TeamName, cParty, 
                 cType, Underlying, Currency, kPrice, 
                 pos1, cff, Units, 
                 tDate, mDate, tKey, 
                 Remarks, VL, VLRemarks, stringsAsFactors = F)
}else{
      edf <- validrawdf.f(EXTRAraw.df, tkeydf); n2 <- nrow(edf) #valid entry extrarawdf
      
      sharpe.entry.v <- sapply(1:n2, FUN = function(jj){ 
            #c("ExtraName", "tDate", "Wa", "Wb", "Wc", "Wd", "TeamName", "tKey", "Remarks")
            w <- t(t(as.numeric((edf[jj,3:6]))))
            compsharpe(w, rr, cv)
      })  #compute sharpe ratio for valid entry of each team
      cashrewardn <- sapply(sharpe.entry.v, FUN = function(j){ 
                  maxs <- maxsharpe(rr, cv)
                  dev <- abs((maxs - j)/maxs)
                  threshold <- c(0.01, 0.05, 0.10, 0.20, 0.50)
                  cashr <- c(1e7, 8e6, 5e6, 2e6, 1e6, 0)
                  tr <- dev > threshold
                  cashr[sum(tr)+1]
      }) #loop through each valid entry #cash
      scoren <- sapply(sharpe.entry.v, FUN = function(j){ #loop through team name
                  maxs <- maxsharpe(rr, cv)
                  dev <- abs((maxs - j)/maxs)
                  threshold <- c(0.01, 0.05, 0.10, 0.20, 0.50)
                  score <- c(20, 18, 15, 10, 5, 0)/20*15
                  tr <- dev > threshold
                  score[sum(tr)+1]
      }) #score 
      
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
      N <- nrow(edf)
      
      TrackNo <- rep(NA, N)   
      classf <- rep("EXTRA", N)
      FIC <- rep(NA, N) 
      TeamName <- edf[,"TeamName"]
      cParty <- rep("Harry M.", N)
            cType <- rep("Cash", N) 
            Underlying <- rep(NA, N) 
            Currency <- rep("MYR", N)
            kPrice <- sharpe.entry.v      #sharpe ratio
      pos1 <- rep("Receive", N)
      cff <- sapply(X = 1:N, FUN = function(j){  cff.f(cType[j], pos1[j])  })
      Units <- cashrewardn
            tDate <- rep(4,N)
            mDate <- rep(5,N)
            tKey <- substr(edf[,"tKey"], 4, 7)
      Remarks <- edf[,"Remarks"]
      VL <-  rep(1,N)
      VLRemarks <- scoren 

#output fulldf
data.frame(TrackNo, classf, FIC, TeamName, cParty, 
           cType, Underlying, Currency, kPrice, 
           pos1, cff, Units, 
           tDate, mDate, tKey, 
           Remarks, VL, VLRemarks, stringsAsFactors = F)
}
}


