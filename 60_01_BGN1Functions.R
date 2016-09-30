#60_01_BGN1Functions.R


##SECTION A

#ADD BGN columns to input df: fdf_y
bgn1df_f <- function(df){
      
      #assign values to the variable name, e.g. Underlying <- df[,"Underlying"]
      colName <- colnames(df)
      for(i in 1:length(colName)){assign(colName[i], value = df[, colName[i]])}
      
      #create empty vector for the target variable to be calculated
      newvar <- c("S0", "exS0", "Pro", "Tfee", "NetPro")
      for(i in 1:length(newvar)){assign(newvar[i], value = vector())}
      
      #S0, exS0, Pro, Tfee, NetPro
      N <- nrow(df); cff <- vector()
      
      # tryCatch({
            for(i in 1:N){
                  if(VL[i] == 1){
                        cff[i] <- cff_f(cType[i], pos1[i])
                        S0[i] <- S0f(Underlying[i], tDate[i])
                        exS0[i] <- exS0f(Currency[i], tDate[i])
                              kpp2 <<- kPrice[i]
                              Pro[i] <- Prof(FIC[i], cType[i], cff[i], Units[i], S0[i], exS0[i])
                        
                        Tfee[i] <- Tfeef(FIC[i], cType[i], cParty[i], Units[i], Pro[i], S0[i], exS0[i])
                        NetPro[i] <- Netprof(Pro[i], Tfee[i])
                  
                  }else{
                        S0[i] <- exS0[i] <- Tfee[i] <- NetPro[i] <-  NA
                  }
            }
            
      # }, error = function(e){
      #       print(i)
      # })

      #output df with added bgn columns
      data.frame(df, S0, exS0, Pro, Tfee, NetPro)
      
}
      


#SECTION A: FUNCTIONS to calculate S0, exS0, Pro, Tfee, Netpro
      cff_f <- function(cType, pos1){
            sell_buy <- c("AFUT")
            lend_borrow <- c("Loan")
            long_short <- c("Forward", "Forward-S", "Bond", "Call Option", "Put Option", "Bond-E", "Call Option-E", "Put Option-E", "Call Option-S", "Put Option-S")
            na_cost <- c("Production")
            pay_receive <- c("Employ Service", "Cash")
            revenue_na <- c("Revenue Increment")
            savings_na <- c("Cost Reduction")
      
            if(cType %in% sell_buy){
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
            if(Currency %in% colnames(dfmetaunderprice)){
                  exs0 <- dfmetaunderprice[tDate+1, Currency]
            }else{
                  exs0 <- 1
            }  
            exs0
      } #dfmetunderprice
      
      Prof <- function(FIC, cType, cff, Units, S0, exS0){ #need kprice parameter for bond-e
            zeropro <- c("Revenue Increment", "Production", "AFUT", "Forward", "Forward-S", "Cost Reduction", "Cash")
            #Method 1: -1*cff*Units*S0*exS0 
            m1 <- c("Call Option", "Put Option", "Bond")
            ##Method 2: -1*cff*Units*exS0 
            m2 <- c("Employ Service", "Loan")
            m3 <- c("Bond-E")
            ##Method 3: meta
                  #m3 <- c("Call Option-S", "Put Option-S", "Bond-E", "Call Option-E", "Put Option-E")
                  #or FIC not NA
            
            if(is.character(FIC)){
                  if(FIC %in% "NA"){
                        FIC <- NA     
                  }
            }#FIX: NA vs "NA" problem 
            
            if(cType %in% zeropro){0}
            else if(!is.na(FIC) & cType %in% m3){
                  -1*cff*Units*kpp2
            }else if(!is.na(FIC) & !cType %in% m2){
                  #meta procedure, m3
                  cc <- dfmetafic[,"FIC"] %in% FIC
                  -1*cff*Units*dfmetafic[cc, "Proceed"]
            }else if(cType %in% m1){-1*cff*Units*S0*exS0}
            else if(cType %in% m2){-1*cff*Units*exS0}
            else{NA}
      } #dfmetafic
      
      Tfeef <- function(FIC, cType, cParty, Units, Pro, S0, exS0){
            #m2: 2% of notional value, units*S0*exS0
                  m2 <- c("Forward")
            #m1: not "Employ Service" or cParty is OTC
                  c1 <- grepl("Service", FIC) | cParty %in% "OTC"
            #m3: meta
                  #FIC not NA
            #m4: else 0
                  
            if(is.character(FIC)){
                  if(FIC %in% "NA"){
                        FIC <- NA     
                  }
            }#FIX: NA vs "NA" problem 
                  
                  if(cType %in% m2){
                        abs(Units*S0*exS0*0.02)
                  }else if(!is.na(FIC) & !c1){
                        u <- dfmetafic[,"FIC"] == FIC
                        abs(Units*dfmetafic[u,"Tfee"])
                  }else{
                        0
                  }
      } #dfmetafic

      
      Netprof <- function(Pro, Tfee){
                  Pro - abs(Tfee)
            }
