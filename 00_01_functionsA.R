

##SECTION A
      source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")
      
#load the meta data into df
      wdA <- getwd()
            wd_metaunderlyingprice <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"
            setwd(wd_metaunderlyingprice)
                  dfmetaunderprice <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
                  dfmetafic <- read.csv("meta-FIC-A.csv")
                  dfmetabonde <- read.csv("meta-bond-e-B.csv")
      setwd(wdA)

#SECTION A: FUNCTIONS to calculate S0, exS0, Pro, Tfee, Netpro
      cff_f <- function(cType, pos1){
            sell_buy <- c("AFUT")
            lend_borrow <- c("Loan")
            long_short <- c("Forward", "Forward-S", "Bond", "Call Option", "Put Option", "Bond-E", "Call Option-E", "Put Option-E", "Call Option-S", "Put Option-S")
            na_cost <- c("Production")
            pay_receive <- c("Employ Service", "Cash")
            revenue_na <- c("Revenue Increment")
            savings_na <- c("Cost Reduction")
      
            if(cType %in% buy_sell){
                  if(pos1 == "Sell"){1}else{-1}
            }else if(cType %in% lend_borrow){
                  if(pos1 == "Lend"){1}else{-1}
            }else if(cType %in% long_short){
                  if(pos1 == "Long"){1}else{-1}
            }else if(cType %in% na_cost){
                  if(pos1 == "Cost"){-1}else{1}
            }else if(cType %in% pay_receive){
                  if(pos1 == "Pay"){1}else{-1}
            }else if(cType %in% revenue_na){
                  if(pos1 == "Revenue"){1}else{-1}
            }else if(cType %in% savings_na){
                  if(pos1 == "Savings"){1}else{-1}
            }else{NA}
            
                  
            
      }
      
      S0f <- function(Underlying, tDate){
                 if(Underlying %in% colnames(dfmetaunderprice)){
                        s0 <- dfmetaunderprice[tDate+1, Underlying]
                 }else{
                       s0 <- NA
                 }  
                  s0
      } #dfmetaunderprice
      
      exS0f <- function(Currency, tDate){
            if(Underlying %in% colnames(dfmetaunderprice)){
                  exs0 <- dfmetaunderprice[tDate+1, Underlying]
            }else{
                  exs0 <- 1
            }  
            exs0
      } #dfmetunderprice)
      
      Prof <- function(FIC, cType, cff, Units, S0, exS0){
            zeropro <- c("Revenue Increment", "Production", "AFUT", "Forward", "Forward-S", "Cost Reduction", "Cash")
            #Method 1: -1*cff*Units*S0*exS0 
            m1 <- c("Call Option", "Put Option", "Bond")
            ##Method 2: -1*cff*Units*exS0 
            m2 <- c("Employ Service", "Loan")
            ##Method 3: meta
                  #m3 <- c("Call Option-S", "Put Option-S", "Bond-E", "Call Option-E", "Put Option-E")
                  #or FIC not NA
            if(!is.na(FIC)){
                  #meta procedure, m3
                  cc <- dfmetafic[,"FIC"] == FIC
                  -1*cff*Units*dfmetafic[cc, "Proceed"]
            }else if(cType %in% zeropro){0}
            else if(cType %in% m1){-1*cff*Units*S0*exS0}
            else if(cType %in% m2){-1*cff*Units*exS0}
            else{NA}
      } #dfmetafic
      
      Tfeef <- function(FIC, cType, Units, Pro, S0, exS0){
            #m1: 2% of notional value, units*S0*exS0
                  m2 <- c("Forward")
            #m3: meta
                  #FIC not NA
            #m4: else 0
                  if(!is.na(FIC)){
                        abs(Units*dfmetafic[,"Tfee"])
                  }else if(cType %in% m2){
                        abs(Units*S0*exS0*0.02)
                  }else{
                        0
                  }
      } #dfmetafic
            
      Netprof <- function(Pro, Tfee){
                  Pro - abs(Tfee)
            }

      
#SECTION B
      STf <- function(Underlying, td){
            if(Underlying %in% colnames(dfmetaunderprice)){
                  ST <- dfmetaunderprice[td+1, Underlying]
            }else{
                  ST <- NA
            }  
            ST
      } #dfmetaunderprice
      
      exSTf <- function(Currency, td){
            if(Underlying %in% colnames(dfmetaunderprice)){
                  exST <- dfmetaunderprice[td+1, Underlying]
            }else{
                  exST <- 1
            }  
            exST  
      } #dfmetaunderprice
      
      ProTf <- function(FIC, cType, cff, Units, Pro, kPrice, tDate, mDate, ST, exST, yy){
            m1 <- c("Employ Service") #zero proT because count on pro0
            m2 <- c("Cash") #opposite of employ service, count on end of year proT
            m3 <- c("AFUT")
            m4 <- c("Forward", "Forward-S")
            m5 <- c("Revenue Increment", "Cost Reduction", "Production", "Bond")
            m6 <- c("Put Option", "Put Option-S", "Put Option-E")
            m7 <- c("Call Option", "Call Option-S", "Call Option-E")
            m8 <- c("Loan")
            m9 <- c("Bond-E")
                  mm <- mDate == yy #criteria
            
            if(cType %in% m1){
                  0
            }else if(cType %in% m2){
                  if(mm){-1*cff*Units}else{0}
            }else if(cType %in% m3){
                  if(mm){cff*Units*exST*ST}else{0}
            }else if(cType %in% m4){
                  if(mm){cff*Units*exST*(ST-kPrice)}else{0}
            }else if(cType %in% m5){
                  if(mm){cff*Units*exST*kPrice}else{0}
            }else if(cType %in% m6){
                  if(mm){cff*Units*exST*max(kPrice-ST, 0)}else{0}
            }else if(cType %in% m7){
                  if(mm){cff*Units*exST*max(ST-kPrice,0)}else{0}
            }else if(cType %in% m8){
                  #loan
                  r1 <- kPrice
                  n1 <- mDate - tDate
                  loanamt <- abs(Units)
                  pay1 <- loanamt*r1/(1-(1+r1)^(-1*n1)) #payment per year in foreign currency
                  if(tDate < yy & mDate <= yy){
                        cff*pay1*exST
                  }else{
                        0
                  }
            }else if(cType %in% m9){
                  #meta
                  uu <- dfmetabonde[,"FIC"] == FIC
                  if(yy == 3){
                        0
                  }else if(yy == 4){
                        cff*units*dfmetabonde[uu, "Pro4"]*exST
                  }else if(yy == 5){exST
                        cff*units*dfmetabonde[uu, "Pro5"]*exST
                  }else{exST
                        0
                  }
            }
      } #dfmetabonde
      
      MVTf <- function(FIC, cType, cff, Units, Pro, kPrice, tDate, mDate, ST, exST, yy){
            zeromv <- c("Revenue Increment", "AFUT", "Cost Reduction", "Forward", "Forward-S","Production", "Cash", "Employ Service")
            tdatemv <- c("Call Option", "Put Option", "Call Option-S", "Put Option-S", "Call Option-E", "Put Option-E", "Bond")
            loanmv <- c("Loan")
            bondmeta <- c("Bond-E")
            
            if(cType %in% zeromv){
                  0
            }else if(cType %in% tdatemv){
                  if(tDate == yy){
                        cff*abs(Pro)
                  }else{0}
            }else if(cType %in% loanmv){
                  r1 <- kPrice
                  n1 <- mDate - tDate
                  loanamt <- abs(Units)
                  pay1 <- loanamt*r1/(1-(1+r1)^(-1*n1)) #payment per year in foreign currency
                        n2 <- yy-tDate
                        if(n2 >= 0){
                              mvex <- pay1*(1-(1+r1)^(-1*n2))/(r1) #mv in foreign currency
                              cff*exST*mvex #pro >(-cff) is opposite of mv -> cff
                        }else{
                              0
                        }
            }else if(cType %in% bondmeta){
                  uu <- dfmetabonde[,"FIC"] == FIC
                  if(yy == 3){
                        cff*units*dfmetabonde[uu, "Proceed"]*exST
                  }else if(yy == 4){
                        cff*units*dfmetabonde[uu, "MV4"]*exST
                  }else if(yy == 5){exST
                        cff*units*dfmetabonde[uu, "MV5"]*exST
                  }else{exST
                        0
                  }
            }else{
                  NA
            }
            
            
      } #dfmetabonde
            
      