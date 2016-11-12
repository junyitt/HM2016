#60_01_BGN1Functions.R

#ADD BGN columns to input df: f1.td.df
bgn0.f <- function(f1.td.df, meta.und.price.df, meta.fic.df, meta.employservice.df){
      
      #assign values to the variable name, e.g. Underlying <- df[,"Underlying"]
      colName <- colnames(f1.td.df) <- fullvar.f()
      for(i in 1:length(colName)){assign(colName[i], value = f1.td.df[, colName[i]])}
      
      
      #create empty vector for the 5 bgn variables to be calculated
      bgnvar <- bgnvar.f() 
      for(i in 1:length(bgnvar)){assign(bgnvar[i], value = vector())}
      
      #"ss0"     "exss0"   "pro0"    "tfee"    "netpro0"
      N <- nrow(f1.td.df)
      
      tryCatch({
            for(i in 1:N){
                  if(VL[i] %in% 1){
                        if(tDate[i] == yy){
      # for(jj in 1:length(colName)){assign(paste0(colName[jj],".c"), value = f1.td.df[i, colName[jj]])}
                        ss0[i] <- und.price.f(Underlying.c = Underlying[i], yr = tDate[i], meta.und.price.df)
                        exss0[i] <- und.price.f(Underlying.c = Currency[i], yr = tDate[i], meta.und.price.df)
      # ss0.c <- ss0[i]; exss0.c <- exss0[i]
                              pro0[i] <- pro0.f(FIC[i], classf[i], cType[i], cff[i], Units[i], ss0[i], exss0[i], kPrice[i], meta.fic.df, meta.employservice.df)
                              tfee[i] <- tfee.f(cType[i], classf[i], Units[i], pro0[i], ss0[i], exss0[i])
                              netpro0[i] <- pro0[i] - tfee[i]
                        }else{
                              ss0[i] <- exss0[i] <- pro0[i] <- tfee[i] <- netpro0[i] <-  0     
                        }
                  }else{
                        ss0[i] <- exss0[i] <- pro0[i] <- tfee[i] <- netpro0[i] <-  NA
                  }
            }
            
      }, error = function(e){
            print(i)
      })

      #output df with added bgn columns
            data.frame(f1.td.df, ss0, exss0, pro0, tfee, netpro0)

}
      


#SECTION A: FUNCTIONS to calculate S0, exS0, Pro, Tfee, Netpro

#und.price.f
      
pro0.f <- function(FIC.c, classf.c, cType.c, cff.c, Units.c, ss0.c, exss0.c, kPrice.c, meta.fic.df, meta.employservice.df){
      #CHECK TDATE!!!
      
      #zero pro0
      u1 <- cType.c %in% c("Revenue Increment", "Production", "AFUT", "Forward", "Forward-S", "Cost Reduction", "Cash")
      
      #meta-fic #-1*cff.c*Units.c*exss0.c*meta.fic.df[,"pro0"] 
      u2 <- FIC.c %in% meta.fic.df[,"FIC"] & !cType.c %in% "Bond-E"
      
      #Service & Employ Service #pro0 = meta.employservice.df[,"Proceed"]
      u3 <- FIC.c %in% meta.employservice.df[,"FIC"] & cType.c %in% "Employ Service"
      
      #Loan #-1*cff*Units*exss0 
      u4 <- cType.c %in% "Loan"
      
      #Special: Bond-E - use kPrice: -1*cff.c*Units.c*exss0.c*kPrice.c
      u5 <- cType.c %in% "Bond-E"
      
      
      if(u1){
            0
      }else if(u2){ 
            uu <- meta.fic.df[,"FIC"] %in% FIC.c
            -1*cff.c*Units.c*exss0.c*meta.fic.df[uu,"Proceed"] 
            
      }else if(u3){ 
            uu <- meta.employservice.df[,"FIC"] %in% FIC.c 
            meta.employservice.df[uu, "Proceed"]
            
      }else if(u4){
            -1*cff.c*Units.c*exss0.c 
            
      }else if(u5){
            -1*cff.c*Units.c*exss0.c*kPrice.c
      }else{
            0
      }
} 
      
tfee.f <- function(cType.c, classf.c, Units.c, pro0.c, ss0.c, exss0.c){
#m2: EXC Forward: 2% of notional value, units*S0*exS0
      u1 <- cType.c %in% c("Forward") & classf.c %in% "EXC"
#m1:  EXC CL & PT Options: 2% of pro0
      u2 <- cType.c %in% c("Call Option", "Put Option") & classf.c %in% "EXC"
#m3: Extra CL & PT
      u3 <- cType.c %in% c("Call Option-E", "Put Option-E") & classf.c %in% "EXTRA"
      
      if(u1){ #EXC FWD
            abs(Units.c*ss0.c*exss0.c*0.02)
      }else if(u2){ #EXC OPTIONS
            abs(pro0.c*0.02)
      }else if(u3){
            abs(pro0.c*0.02)
      }else{
            0
      }
} 
