#80_01_END1Functions.R

#main function: add end1 columns to bgndf
fdf8_f <- function(df, yy){
      {
            #assign values to the variable name, e.g. Underlying <- df[,"Underlying"]
            colName <- colnames(df)
            for(i in 1:length(colName)){assign(colName[i], value = df[, colName[i]])}
            
            #create empty vector for the target variable to be calculated
            newvar <- c("ST", "exST", "ProT", "MVT")
            for(i in 1:length(newvar)){assign(newvar[i], value = vector())}
                  cff <- vector()
            
            N <- nrow(df)
      } #assign vector, empty vector
      
      #SECTION B: yearly, ST, exST, ProT, MVT, T=1,2,3,4,5
      tryCatch({
            
            for(j in 1:N){
                  if(VL[j] == 1){
                        cff[j] <- cff_f(cType[j], pos1[j])
                        ST[j] <- STf(Underlying[j], yy+1)
                        exST[j] <- exSTf(Currency[j], yy+1)
                        ProT[j] <- ProTf(FIC[j], cType[j], cff[j], Units[j], Pro[j], kPrice[j], tDate[j], mDate[j], ST[j], exST[j], yy)
                        MVT[j] <- MVTf(FIC[j], cType[j], cff[j], Units[j], Pro[j], kPrice[j], tDate[j], mDate[j], ST[j], exST[j], yy)
                  }else{
                        ST[j] <- exST[j] <- ProT[j] <- MVT[j] <- NA
                  }
            }
            
      }, error = function(e){
            print(j)
      })
      
      #output final data frame
      data.frame(df, ST, exST, ProT, MVT)
}

#DUPLICATE from 60_BGN1f
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

#SECTION B
STf <- function(Underlying, td){
      if(Underlying %in% colnames(dfmetaunderprice)){
            ST <- dfmetaunderprice[td+1, Underlying]
      }else{
            ST <- NA
      }
      ST
} #dfmetaunderprice #td refers to yy2!

exSTf <- function(Currency, td){
      if(Currency %in% colnames(dfmetaunderprice)){
            exST <- dfmetaunderprice[td+1, Currency]
      }else{
            exST <- 1
      }
      exST
} #dfmetaunderprice #td refers to yy2!

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
      mm <- mDate == yy+1 #criteria
      
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
            if(tDate < yy+1 & mDate >= yy+1){ #BIG MISTAKES CORRECTED! <= to >=
                  cff*pay1*exST
            }else{
                  0
            }
      }else if(cType %in% m9){
            #meta
            uu <- dfmetabonde[,"FIC"] == FIC
            if(yy+1 == 3){
                  0
            }else if(yy+1 == 4){
                  cff*Units*dfmetabonde[uu, "Pro4"]*exST
            }else if(yy+1 == 5){exST
                  cff*Units*dfmetabonde[uu, "Pro5"]*exST
            }else{
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
                  # cff*abs(Pro)
                  0
            }else{0}
      }else if(cType %in% loanmv){
            r1 <- kPrice
            n1 <- mDate - tDate
            loanamt <- abs(Units)
            pay1 <- loanamt*r1/(1-(1+r1)^(-1*n1)) #payment per year in foreign currency
            n2 <- mDate - (yy+1)
            if(n2 > 0 & yy >= tDate){
                  mvex <- pay1*(1-(1+r1)^(-1*n2))/(r1) #mv in foreign currency
                  cff*exST*mvex #pro >(-cff) is opposite of mv -> cff
            }else{
                  0
            }
      }else if(cType %in% bondmeta){
            uu <- dfmetabonde[,"FIC"] == FIC
            if(yy == 3){
                  cff*Units*dfmetabonde[uu, "Proceed"]*exST #unnecessary
            }else if(yy == 4){
                  cff*Units*dfmetabonde[uu, "MV4"]*exST
            }else if(yy == 5){exST
                  cff*Units*dfmetabonde[uu, "MV5"]*exST
            }else{exST
                  0
            }
      }else{
            NA
      }
      
      
} #dfmetabonde
