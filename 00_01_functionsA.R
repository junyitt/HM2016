
#FUNCTIONS to calculate S0, exS0, Pro, Tfee, Netpro
##SECTION A
      source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")
      
      wd_metaunderlyingprice <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"
      
      cff_f <- function(cType, pos1){
            buy_sell <- c("AFUT")
            lend_borrow <- c("Loan")
            long_short <- c("Forward", "Forward-S", "Bond", "Call Option", "Put Option", "Bond-E", "Call Option-E", "Put Option-E", "Call Option-S", "Put Option-S")
            na_cost <- c("Production")
            pay_receive <- c("Employ Service", "Cash")
            revenue_na <- c("Revenue Increment")
            savings_na <- c("Cost Reduction")
      
            if(cType %in% buy_sell){
                  if(pos1 == "Buy"){1}else{-1}
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
            A <- getwd()
            #setwd to the location of meta-underlyingprice
            setwd(wd_metaunderlyingprice)
                 pricedf <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
                 if(Underlying %in% colnames(pricedf)){
                        s0 <- pricedf[tDate+1, Underlying]
                 }else{
                       s0 <- NA
                 }  
            setwd(A)
            s0
      } #require meta
      
      exS0f <- function(Currency, tDate){
            A <- getwd()
            #setwd to the location of meta-underlyingprice
            setwd(wd_metaunderlyingprice)
            pricedf <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
            if(Underlying %in% colnames(pricedf)){
                  exs0 <- pricedf[tDate+1, Underlying]
            }else{
                  exs0 <- 1
            }  
            setwd(A)
            exs0
      } #require meta
      
      #idea - include meta, inside prof, if(is.na(FIC){nonFIC procedure, elseif(FIC){meta FIC procedure})
      
      Prof <- function(cType, cff, Units, S0, exS0){
            zeropro <- c("Production", "AFUT", "Forward", "Forward-S", "Cost Reduction")
            #Method 1: -1*cff*Units*S0*exS0 
            m1 <- c("Call Option", "Put Option", "Bond")
            ##Method 2: -1*cff*Units*exS0 
            m2 <- c("Cash", "Employ Service", "Loan")
            if(cType %in% zeropro){0}
            else if(cType %in% m1){-1*cff*Units*S0*exS0}
            else if(cType %in% m2){-1*cff*Units*exS0}
            else{NA}
      }
      
      Tfeef <- function(cType, Units, Pro, S0, exS0){
            #m1: 2% of proceed, units*pro*exS0
                  m1 <- c("Call Option", "Put Option")
            #m2: 2% of notional value, units*S0*exS0
                  m2 <- c("Forward")
            #m3: else 0
                  if(cType %in% m1){} #Units*Pro*exS0*0.03 (meta)
                  else if(cType %in% m2){Units*S0*exS0*0.02}
                  else{0}
      }
            
      Netprof <- function(Pro, Tfee){
                  Pro - abs(Tfee)
            }

      
#SECTION B
      STf <- function(Underlying, td){
            A <- getwd()
            #setwd to the location of meta-underlyingprice
            setwd(wd_metaunderlyingprice)
            pricedf <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
            if(Underlying %in% colnames(pricedf)){
                  ST <- pricedf[td+1, Underlying]
            }else{
                  ST <- NA
            }  
            setwd(A)
            ST
      } #meta
      
      exSTf <- function(Currency, td){
            A <- getwd()
            #setwd to the location of meta-underlyingprice
            setwd(wd_metaunderlyingprice)
            pricedf <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
            if(Underlying %in% colnames(pricedf)){
                  exST <- pricedf[td+1, Underlying]
            }else{
                  exST <- 1
            }  
            setwd(A)
            exST  
      } #meta
      
      ProTf <- function(FIC, cType, cff, Units, ST, exST){
            
      }
      
      MVTf <- function(FIC, cType, cff, Units, ST, exST){
            
      }
            
      