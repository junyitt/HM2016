#00_createmeta_f.R
writexlsx.f <- function(df, fname, metadir = meta.dir){
      setwd(metadir)
      write.xlsx(x = df, file = fname, row.names = F)
}

createbs01.df.f <- function(){
      teamname12.v <- teamname.f()
      PPE <- c(rep(20e6,6),rep(20e6,6))
      FA <- c(rep(0,6), rep(0,6))
      Cash <- c(rep(10e6,6), rep(10e6,6))
      ShareCap <- c(rep(25e6,6), rep(25e6,6))
      RE <- c(rep(5e6, 6), rep(5e6, 6))
      Loan <- c(rep(0,6), rep(0,6))
      NAV <- c(rep(30e6,6), rep(30e6,6))
      df0 <- data.frame(teamname12.v, PPE, FA, Cash, ShareCap, RE, Loan, NAV)
      colnames(df0) <- c("TeamName", "PPE", "Financial Assets", "Cash", "Share Capital", "Retained Earnings", "Loan", "NAV")
      df0
} #vary2  #done


create.core34.df.f <- function(team = "Alpha", kG = 3500, kC = 225, kP = 1700, uG = 1500, uC = 20000, uP = 3000, yr = 5){
      N <- yr*6
      TrackNo <- cff <- tKey <- Remarks <- VL <- VLRemarks <- rep(" ", N)
      classf <- rep("CORE", N)
      FIC <- cParty <- rep("NA", N)
      TeamName <- rep(team, N)
      cType <- c(rep("Production", yr*3), rep("AFUT", yr*3))
     
      Currency <- c(rep("MYR", yr*3), rep("USD", yr), rep("MYR", yr*2))
      kPrice <- c(rep(kG, yr), rep(kC, yr), rep(kP,yr), rep(NA, yr*3))
      if(team %in% "Alpha"){
            Underlying <- rep(c(rep("GOL", yr), rep("CRU", yr), rep("PAL", yr)),2)
            pos1 <- c(rep("Cost", yr*3), rep("Sell", yr*3))
      }else if(team %in% "Beta"){
            Underlying <- c(rep("GOL2", yr), rep("CRU2", yr), rep("PAL2", yr), rep("GOL", yr), rep("CRU", yr), rep("PAL", yr))
            pos1 <- c(rep("Revenue", yr*3), rep("Buy", yr*3))
      }
      Units <- rep(c(rep(uG, yr), rep(uC, yr), rep(uP,yr)),2)
      tDate <- rep(0:4, 6)
      mDate <- rep(1:5, 6)
      
      df <- data.frame(TrackNo, classf, FIC, TeamName, cParty, cType, Underlying, Currency, kPrice, pos1, cff, Units, tDate, mDate, tKey, Remarks, VL, VLRemarks)
      df
} #done set

create.employ5.df.f <- function(){
      N <- 12
      TrackNo <- cff <- tKey <- Remarks <- VL <- VLRemarks <- rep(" ", N)
      classf <- rep("EXTRA", N)
      FIC <- paste0("Service ", c("A1", "A2", "A3", "AA1", "AA2", "AA3", "B1", "B2", "B3", "BB1", "BB2", "BB3"))
      TeamName <- c(rep("Alpha", 6), rep("Beta", 6))
            cParty <- rep("ProTech", N)
            cType <- rep("Employ Service", N)
      Underlying <- c(rep(c("GOL", "CRU", "PAL"), 2), rep(c("GOL2", "CRU2", "PAL2"), 2))
      Currency <- rep("MYR", N)
      kPrice <- rep(NA, N)
      pos1 <- rep("Pay", N)
      Units <- rep(0, N)
      tDate <- rep(c(rep(0,3), rep(1,3)),2)
      mDate <- rep(c(rep(1,3), rep(2,3)),2) 
            SpotPrice <- ExRate <- rep(NA, N)
            Tfee <- rep(0, N)
            Proceed <- c(930000, 800000, 810000, #A1, A2, A3
                         1800000, 1800000, 1500000, #AA1, AA2, AA3
                         930000, 800000, 810000, #B1, B2, B3
                         1800000, 1800000, 1500000)*-1 #BB1, BB2, BB3
            NetProceed <- Proceed
      
      df <- data.frame(TrackNo, classf, FIC, TeamName, cParty, cType, Underlying, Currency, kPrice, pos1, cff, Units, 
                       tDate, mDate, tKey, Remarks, VL, VLRemarks,
                       SpotPrice, ExRate, Proceed, Tfee, NetProceed)
      df 
} #vary #done set

create.extrascore06.df.f <- function(){
      Service <- paste0("Service ", c("A1", "A2", "A3", "AA1", "AA2", "AA3", 
                                      "B1", "B2", "B3", "BB1", "BB2", "BB3") )
      Service <- c("None", Service)
      Score <- c(0, 10, 5, 15, #none, A1, A2, A3
                 10, 15, 5, #AA1
                 10, 5, 15, #B1
                 10, 15, 5) #BB1
      df <- data.frame(Service, Score)
      df           
} #vary #done set

create.extraharry7.listdf2.f <- function(){
      n <- 4
      k <- 20000
      a <- c(2:(k+1))
      
      set.seed(12)
      b <- sample(a)
      set.seed(20112)
      c <- log(sample(b)*a)*sample(rnorm(length(b), mean = 0.01, sd = 0.3))
      
      m <- matrix(c, as.integer(k/n),n)
      
      cv <- cov(m); cv <- round(cv, 4)
      rr <- matrix(colMeans(m), n,1); rr <- round(rr, 4)
            Asset <- c("A", "B", "C", "D")
            ExcessReturn <- rr
            dfrr <- data.frame(Asset, ExcessReturn)
            dfcv <- data.frame(cv); colnames(dfcv) <- c("A", "B", "C", "D")
      list(dfrr, dfcv)
}

create.extra8A.df.f <- function(team){
      N <- 27
            TrackNo <- cff <- tKey <- Remarks <- VL <- VLRemarks <- rep(" ", N)
            classf <- rep("EXTRA", N)
            cParty <- rep("ProTech", N)
            Currency <- rep("MYR", N)
            
            tDate <- c(rep(0:4, 3), rep(1:4, 3))
            mDate <- c(rep(1:5, 3), rep(2:5, 3)) 
      
      if(team %in% "Alpha"){            
            FIC <- c( paste0("Service ", c(rep("A1",5), rep("A2", 5), rep("A3", 5))),
                      paste0("Service ", c(rep("AA1", 4), rep("AA2", 4), rep("AA3", 4))) )
            TeamName <- rep("Alpha", N)
            cType <- rep("Cost Reduction", N)
            pos1 <- rep("Savings", N)
            Underlying <- c(rep("GOL", 5), rep("CRU",5), rep("PAL",5), rep("GOL", 4), rep("CRU",4), rep("PAL",4))
                  
            kPrice <- c(200,200,300,300,300, #A1 #GOL
                        10,10,20,20,20,      #A2 #CRU
                        110,110,120,120,120, #A3 #PAL
                        500,510,520,530,     ##AA1
                        40,42,44,46,         ##AA2
                        180,185,190,195 )    ##AA3
                  
            Units <- c( rep(1500, 5), rep(20000,5), rep(3000,5),
                        rep(1500, 4), rep(20000,4), rep(3000,4) )
            
      }else if(team %in% "Beta"){
            FIC <- c( paste0("Service ", c(rep("B1",5), rep("B2", 5), rep("B3", 5))),
                      paste0("Service ", c(rep("BB1", 4), rep("BB2", 4), rep("BB3", 4))) )
            TeamName <- rep("Beta", N)
            cType <- rep("Revenue Increment", N)
            pos1 <- rep("Revenue", N)  
            Underlying <- c(rep("GOL2", 5), rep("CRU2",5), rep("PAL2",5), rep("GOL2", 4), rep("CRU2",4), rep("PAL2",4))
            
            kPrice <- c(200,200,300,300,300, #B1 #GOL2
                        10,10,20,20,20,      #B2 #CRU2
                        110,110,120,120,120, #B3 #PAL2
                        500,510,520,530,     ##AA1
                        40,42,44,46,         ##AA2
                        180,185,190,195 )    ##AA3
            
            Units <- c( rep(1500, 5), rep(20000,5), rep(3000,5),
                        rep(1500, 4), rep(20000,4), rep(3000,4) )
            
      }
            
            
      df <- data.frame(TrackNo, classf, FIC, TeamName, cParty, cType, Underlying, Currency, kPrice, pos1, cff, Units, 
                       tDate, mDate, tKey, Remarks, VL, VLRemarks)
      df 
} #vary

# create.scenario.10.df.f <- function(){
#       TrackNo <- cff <- tKey <- Remarks <- VL <- VLRemarks <- "" 
#       classf <- "SCENARIO"
#       FIC <- "Set"
#       TeamName <- c(rep("Alpha", 14), rep("Beta", 14))
#       
#       cParty <- "Set"
#       cType <- "Set"
#       Underlying <- "Set"
#       Currency <- "Set"
#       pos1 <- "Set"
#       Units <- "Set"
#       tDate <- "Set"
#       mDate <- "Set"
#       
#       
# } #NO TIME

createscorebr11.df.f <- function(){
      teamname12.v <- teamname.f()
      h <- rep(0, length(teamname12.v))
      data.frame(TeamName = teamname12.v, 
                 Hedging = h,
                 CashFlow = h,
                 ExtraEvent = h,
                 NAV = h,
                 Year = h)
} #meta

createtkey12.df.f <- function(seed = rnorm(1)){
      teamname.v <- teamname.f()
            set.seed(seed)
            key1 <- round(runif(12,10000,99999))
      sectorclass <- c(rep(1,6),rep(2,6))
      teamclass <- c(1:6, 1:6)
      tkey <-paste0(key1, sectorclass, teamclass)
      data.frame(TeamName = teamname.v, tKey = tkey)
} #meta

createund13.df.f <- function(seed = 201){
      price0 <- c(1000, 250, 2000, 3.8, 4.2)
      v <- list()
      for(k in 1:5){
            if(k != 5){
                  set.seed(seed + k)
                  g <- runif(5, min = 0.8, max = 1.25)
                  p <- vector()
                  p[1] <- price0[k]
                        for(j in 2:6){
                              p[j] <- p[j-1]*g[j-1]
                        }
                  v[[k]] <- p
            }else{
                  set.seed(seed+k)
                  g <- runif(6, min = 1.00, max = 1.07)
                  v[[k]] <- v[[k-1]]*g
                  
            } 
            if(k <= 3){
                  v[[k]] <- round(v[[k]],0)
            }else{
                  v[[k]] <- round(v[[k]],4)
            }
      }
      df0 <- do.call(cbind, v)
      rateMYR <- c(0.0525, 0.0450, 0.0400, 0.0375, 0.0350, 0.0350)
      rateUSD <- c(0.0325, 0.0300, 0.0275, 0.0250, 0.0225, 0.0200)           
      rateEUR <- c(0.0225, 0.0200, 0.0175, 0.0150, 0.0125, 0.0100)           
      df1 <- data.frame(rateMYR, rateUSD, rateEUR)
      df <- data.frame(Year = 0:5, df0, df1)
      colnames(df) <- c("Year", "GOL", "CRU", "PAL", "USD", "EUR", "rateMYR", "rateUSD", "rateEUR")
      df
} #meta
