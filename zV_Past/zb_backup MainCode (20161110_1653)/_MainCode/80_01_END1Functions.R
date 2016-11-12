#80_01_END1Functions.R

#end3 function: add end1 - 4 columns to bgn2.td.df
end3.td.df.f <- function(bgn2.td.df, meta.und.price.df, meta.bonde.df, yy){
      #assign values to the variable name, e.g. Underlying <- df[,"Underlying"]
      colName <- colnames(bgn2.td.df)
      for(i in 1:length(colName)){assign(colName[i], value = bgn2.td.df[, colName[i]])}
      
      #create empty vector for the target variable to be calculated
      newvar <- c("sT", "exsT", "proT", "mvT")
      for(i in 1:length(newvar)){assign(newvar[i], value = vector())}
      
      N <- nrow(bgn2.td.df)
      
      #SECTION B: yearly, ST, exST, ProT, MVT, T=1,2,3,4,5
      tryCatch({
            
            for(j in 1:N){
                  if(VL[j] == 1){
                        if(tDate[j] < yy + 1 & yy + 1 <= mDate[j]){
                              sT[j] <- und.price.f(Underlying[j], yy+1, meta.und.price.df)
                              exsT[j] <- und.price.f(Currency[j], yy+1, meta.und.price.df)  
      # for(i in 1:length(colName)){assign(  paste0(colName[i],".c") , value = bgn2.td.df[j, colName[i]])}
      # sT.c <- sT[j]; exsT.c <- exsT[j]
                                    proT[j] <- proT.f(FIC[j], cType[j], cff[j], Units[j], pro0[j], kPrice[j], tDate[j], mDate[j], sT[j], exsT[j], meta.bonde.df, yy)
                                    #proT[j] <- proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, sT.c, exsT.c, meta.bonde.df, yy)
                                    mvT[j] <-  mvT.f(FIC[j], cType[j], cff[j], Units[j], pro0[j], kPrice[j], tDate[j], mDate[j], sT[j], exsT[j], meta.bonde.df, yy)
                                    #mvT[j] <- mvT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, sT.c, exsT.c, meta.bonde.df, yy)
                        }else{
                              sT[j] <- exsT[j] <- proT[j] <- mvT[j] <- 0
                        }
                  }else{
                              sT[j] <- exsT[j] <- proT[j] <- mvT[j] <- NA
                  }
            }
            
      }, error = function(e){
            print(j)
      })
      
      #output final data frame
      data.frame(bgn2.td.df, sT, exsT, proT, mvT)
}

#SECTION B
# und.price.f - und.price.f(Underlying.c, yr, meta.und.price.df)

proT.f <- function(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, sT.c, exsT.c, meta.bonde.df, yy){
      mm <- mDate.c == yy+1 #criteria 1 - only at maturity date
      mm2 <- tDate.c < yy+1 & yy+1 <= mDate.c
            m1 <- cType.c %in% c("Employ Service") #zero proT because count on pro0
            m2 <- cType.c %in% c("Cash") & mm #opposite of employ service, count on end of year proT
            m3 <- cType.c %in% c("AFUT") & mm
                  m4 <- cType.c %in% c("Forward", "Forward-S") & mm
                  m5 <- cType.c %in% c("Revenue Increment", "Cost Reduction", "Production", "Bond") & mm
            m6 <- cType.c %in% c("Put Option", "Put Option-S", "Put Option-E") & mm
            m7 <- cType.c %in% c("Call Option", "Call Option-S", "Call Option-E") & mm
                  m8 <- cType.c %in% c("Loan") & mm2
                  m9 <- cType.c %in% c("Bond-E") & mm2 
      
      
      if(m1){
            0
      }else if(m2){
            -1*cff.c*Units.c
      }else if(m3){
            if(is.na(kPrice.c) | as.character(kPrice.c)%in%"NA"){
                  cff.c*Units.c*exsT.c*sT.c
            }else{
                  cff.c*Units.c*exsT.c*kPrice.c
            }
      }else if(m4){
            cff.c*Units.c*exsT.c*(sT.c-kPrice.c)
      }else if(m5){
            cff.c*Units.c*exsT.c*kPrice.c
      }else if(m6){
            cff.c*Units.c*exsT.c*max(kPrice.c-sT.c, 0)
      }else if(m7){
            cff.c*Units.c*exsT.c*max(sT.c - kPrice.c, 0)
      }else if(m8){
            #loan
            r1 <- kPrice.c
            n1 <- mDate.c - tDate.c
            loanamt <- abs(Units.c)
            pay1 <- loanamt*r1/(1-(1+r1)^(-1*n1)) #payment per year in foreign currency
            if(mm2){ 
                  cff.c*pay1*exsT.c
            }else{
                  0
            }
      }else if(m9){
            #meta
            uu <- meta.bonde.df[,"FIC"] %in% FIC.c
            if(yy+1 == 3){
                  0
            }else if(yy+1 == 4){
                  cff.c*Units.c*meta.bonde.df[uu, "Pro4"]*exsT.c
            }else if(yy+1 == 5){
                  cff.c*Units.c*meta.bonde.df[uu, "Pro5"]*exsT.c
            }else{
                  0
            }
      }else{
            0
      }
}

mvT.f <- function(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, sT.c, exsT.c, meta.bonde.df, yy){
      mm2 <- tDate.c < yy+1 & yy+1 <= mDate.c
      #zeromv 
      u0 <- cType.c %in% c("Revenue Increment", "AFUT", "Cost Reduction", "Forward", "Forward-S","Production", "Cash", "Employ Service", "Call Option", "Put Option", "Call Option-S", "Put Option-S", "Call Option-E", "Put Option-E", "Bond")
      #loanmv
      u1 <- cType.c %in% c("Loan") & mm2
      #bondmeta 
      u2 <- cType.c %in% c("Bond-E") & mm2
      
      if(u0){
            0
      }else if(u1){
            r1 <- kPrice.c
            n1 <- mDate.c - tDate.c
            loanamt <- abs(Units.c)
            pay1 <- loanamt*r1/(1-(1+r1)^(-1*n1)) #payment per year in foreign currency
            n2 <- mDate.c - (yy+1)
                  mvex <- pay1*(1-(1+r1)^(-1*n2))/(r1) #mv in foreign currency
                  cff.c*exsT.c*mvex #pro >(-cff) is opposite of mv -> cff
      }else if(u2){
            uu <- meta.bonde.df[,"FIC"] %in% FIC.c
            if(yy+1 == 3){
                  0 #unnecessary
            }else if(yy+1 == 4){
                  cff.c*Units.c*meta.bonde.df[uu, "MV4"]*exsT.c
            }else if(yy+1 == 5){
                  cff.c*Units.c*meta.bonde.df[uu, "MV5"]*exsT.c
            }else{
                  0
            }
      }else{
            0
      }
      
      
}
