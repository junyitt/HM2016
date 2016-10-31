
#####OTC FUNCTIONS
#import OTC-EXC-TRAN
importrawOTC.f <- function(raw_OTC.dir, yy){
      setwd(raw_OTC.dir)
      OTCrawdf <- as.data.frame(read_excel(paste0("OTC_", yy, ".xlsx")))
      OTCrawdf <- cutoff.f(OTCrawdf)   
      OTCrawdf
}

##########################################################
#NUMBER ONE PRIORITY, DUPLICATE FIRST and FIX POSITION
###########################################################

##DUPLICATE: Main: DUPf
duplicate.f <- function(rawdf){
      colnames(rawdf) <- c("cType", "Team1", "Team2", "posTeam1",
                           "Underlying", "kPrice", "Units", "tDate",
                           "mDate", "Remarks")
      rawdf2 <- rawdf
      rawdf2[,"Team1"] <- rawdf[, "Team2"]  #flip team 1 and team 2 
      rawdf2[,"Team2"] <- rawdf[, "Team1"]
      rawdf2[,"posTeam1"] <- sapply(rawdf[,"posTeam1"], FUN = function(j){flippos.f(j)})
      rawdf0 <- rbind(rawdf, rawdf2)
            #FIX
            rawdf0[, "Units"] <- round(rawdf0[, "Units"],0) #round
            rawdf0[,"posTeam1"] <- sapply(1:nrow(rawdf0), FUN = function(j){posfix.f(rawdf0[j,"cType"], rawdf0[j, "posTeam1"])}) #fix position
            rawdf0[,"Underlying"] <- sapply(1:nrow(rawdf0), FUN = function(j){loanNAfix.f(rawdf0[j,"cType"], rawdf0[j,"Underlying"])}) #fix underlying NA for loan
                  #output, duplicated and fixed df
                  rawdf0
}

#DUPf: flip Long to Short, Lend to Borrow and vice versa
      flippos.f <- function(pos1){
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
      posfix.f <- function(cType, pos){
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
      loanNAfix.f <- function(cType, Underlying){
      if(cType %in% "Loan"){
            NA
      }else{
            Underlying
      }
}
      
#############################################################################
#CONVf: add currrency
      addcurr.f <- function(Underlying.c){
            if(Underlying.c %in% "GOL"){
                  "USD"     
            }else{
                  "MYR"
            }
      }
      
#CONVf: VL      
      VLandVLR.OTC.v2.f <- function(cType.c, Underlying.c, kPrice.c, Units.c, tDate.c, mDate.c, yy, meta.und.price.df){
            ss0 <- und.price.f(Underlying.c, yy, meta.und.price.df)
            if(Underlying.c %in% "GOL"){exss0 <- und.price.f("USD", yy, meta.und.price.df)}else{exss0 <- 1}
            
            #1) no underlying for Forward - ignore for loan
            if(cType.c %in% "Forward" & !Underlying.c %in% c("GOL", "CRU", "PAL", "USD", "EUR")){ 
                  c(0, "No Underlying specified for Forward")
                  print("WARNING: NA for Forward Underlying")
                  
            #2) Loan min/max interest rate
            }else if(cType.c %in% "Loan" & !(kPrice.c >= 0.01 & kPrice.c <= 0.15)){
                  c(0, "Interest rate not in [1%, 15%] interval")

            #3) Negative units  
            }else if(Units.c < 0){
                  c(0, "Negative number of units")
                  print("WARNING: Negative number of units")
                  
            #4) negative kPrice  
            }else if(Units.c < 0){
                  c(0, "Negative kPrice")
                  print("WARNING: Negative kPrice")
                  
            #5) Notional Value boundary        
            }else if(cType.c %in% "Forward" & Underlying.c %in% c("GOL", "CRU", "PAL", "USD", "EUR") & Units.c*ss0*exss0 > 1e7){
                  c(0, "Notional value (FWD) exceed RM 10 million")
                  
            }else if(cType.c %in% "Loan" & Units.c > 1e7){
                  c(0, "Notional value (Loan) exceed RM 10 million")
                  
            #6) tDate, mDate boundary
            }else if(    tDate.c >= mDate.c | !(tDate.c >= 0 & tDate.c <= 4) | !(mDate.c >= 1 & mDate.c <= 5) | !(tDate.c == yy)    ){
                  c(0, "tDate and mDate is wrong")
                  print("WARNING: tDate and mDate is wrong")
                  
            }else{
                  c(1, "OTC: OK")
            }
      
      }

##CONV-TO-FULL-TRAN-DF: Main: CONVf
OTCfullconv.f <- function(dupdf, meta.und.price.df){
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
            N <- nrow(dupdf)
            colnames(dupdf) <- c("cType", "Team1", "Team2", "posTeam1",
                                "Underlying", "kPrice", "Units", "tDate",
                                "mDate", "Remarks")
            
      #EACH VARIABLES - treatment
      TrackNo <- rep(NA, N)
      classf <- rep("OTC", N)
      FIC <- rep(NA, N)
      TeamName <- dupdf[,"Team1"]
      cParty <- dupdf[,"Team2"]
            cType <- dupdf[,"cType"]
            Underlying <- dupdf[,"Underlying"]
            Currency <- sapply(X = Underlying, FUN = function(Underlying.c){   addcurr.f(Underlying.c)    })   
            kPrice <- dupdf[,"kPrice"]
      pos1 <- dupdf[, "posTeam1"]
      cff <- sapply(X = 1:N, FUN = function(j){ cff.f(cType.c = cType[j], pos1.c = pos1[j])   })   
      Units <- round(dupdf[, "Units"],0)
            tDate <- dupdf[, "tDate"]
            mDate <- dupdf[, "mDate"]
            tKey <- rep(NA, N)
      Remarks <- dupdf[,"Remarks"]
      VL <-  sapply(X=1:N, function(j){   v2 <-  VLandVLR.OTC.v2.f(cType[j], Underlying[j], kPrice[j], Units[j], tDate[j], mDate[j], yy, meta.und.price.df); v2[1]     }) 
      VLRemarks <- sapply(X=1:N, function(j){   v2 <-  VLandVLR.OTC.v2.f(cType[j], Underlying[j], kPrice[j], Units[j], tDate[j], mDate[j], yy, meta.und.price.df); v2[2]     })  

      #output fulldf
      data.frame(TrackNo, classf, FIC, TeamName, cParty, 
            cType, Underlying, Currency, kPrice, 
            pos1, cff, Units, 
            tDate, mDate, tKey, 
            Remarks, VL, VLRemarks, stringsAsFactors = F)
}
