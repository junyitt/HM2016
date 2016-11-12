#A7_01_HedgingEvaluationFunctions.R
      #required: proT.f

#vary underlying price; output: 61 values of simulated proceed at T for each transaction! OUTPUT: list(classH, sim61.v)
sim61.v.f <- function(tranj.df, yy, testasset.c, meta.bonde.df, meta.und.price.df){
      step <- -30:30; step <- step/100 + 1;
      
      colName <- colnames(tranj.df) 
      for(i in 1:length(colName)){   assign( paste0(colName[i],".c") , value = tranj.df[, colName[i]])   } #input df, then assign each variable to variablename.c
      
      if(classf.c %in% c("SCENARIO")){
            classH <- "Scenario"
      }else{
            classH <- "User"
      }
      
      if(testasset.c %in% "GOL"){
            
            #input cType[j], Underlying[j]...
            #output: 61.v
            #A: und != c("GOL", "USD") ; cur = "USD"
            ua <- !(Underlying.c %in% c("GOL","USD")) & Currency.c %in% "USD"; #ss0 = NA, exss0 = 4
            #B: und = GOL, cur = USD
            ub <- (Underlying.c %in% c("GOL")) & Currency.c %in% "USD"; #ss0 varies, exss0 = 4 
            #C: und = USD, cur = USD
            uc <- (Underlying.c %in% c("USD")) & Currency.c %in% "USD"; #ss0 = 4 , exss0 = 4
            #D: und = GOL, cur = MYR
            ud <- Underlying.c %in% c("GOL") & Currency.c %in% "MYR"; #ss0 varies, exss0 = 1
            #E: und = USD, cur  = MYR
            ue <- Underlying.c %in% c("USD") & Currency.c %in% "MYR"; #ss0 = 4, exss0 = 1
            
            if(ua){ #ss0 = NA, exss0 = 4
                  ss0 <- NA; exss0 <- und.price.f("USD", yy, meta.und.price.df)
                  pro1 <- proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0, meta.bonde.df, yy)
                  pro1.v <- rep(pro1, length(step))      
            }else if(ub){ #ss0 varies, exss0 = 4 
                  ss0 <- und.price.f("GOL", yy, meta.und.price.df) ; exss0 <- und.price.f("USD", yy, meta.und.price.df) 
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0*r, exss0, meta.bonde.df, yy)
                  })
                  pro1.v
            }else if(uc){ #ss0 = 4 , exss0 = 4
                  ss0 <- exss0 <- und.price.f("USD", yy, meta.und.price.df)
                  pro1 <- proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0, meta.bonde.df, yy)
                  pro1.v <- rep(pro1, length(step))      
            }else if(ud){ #ss0 varies, exss0 = 1
                  ss0 <- und.price.f("GOL", yy, meta.und.price.df) ; exss0 <- 1
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0*r, exss0, meta.bonde.df, yy)
                  })
                  pro1.v
            }else if(ue){ #ss0 = 4, exss0 = 1
                  ss0 <- und.price.f("USD", yy, meta.und.price.df) ; exss0 <- 1
                  pro1 <- proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0, meta.bonde.df, yy)
                  pro1.v <- rep(pro1, length(step))      
            }else{
                  pro1.v <- rep(0, length(step))  
            }
                  
                  
            
      }else if (testasset.c %in% "USD"){
            #A: und != c("GOL", "USD") ; cur = "USD"
            ua <- !(Underlying.c %in% c("GOL","USD")) & Currency.c %in% "USD"; #ss0 = NA, exss0 = varies usd
            #B: und = GOL, cur = USD
            ub <- (Underlying.c %in% c("GOL")) & Currency.c %in% "USD"; #ss0 = 1000, exss0 = varies usd
            #C: und = USD, cur = USD
            uc <- (Underlying.c %in% c("USD")) & Currency.c %in% "USD"; #ss0 = varies usd , exss0 = varies usd
            #D: und = GOL, cur = MYR
            ud <- Underlying.c %in% c("GOL") & Currency.c %in% "MYR"; #ss0 = 1000, exss0 = 1
            #E: und = USD, cur  = MYR
            ue <- Underlying.c %in% c("USD") & Currency.c %in% "MYR"; #ss0 = varies usd, exss0 = 1  
            
            
            if(ua){ #ss0 = NA, exss0 = varies usd
                  ss0 <- NA; exss0 <- und.price.f("USD", yy, meta.und.price.df)
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0*r, meta.bonde.df, yy)
                  })
                  pro1.v    
            }else if(ub){ #ss0 = 1000, exss0 = varies usd 
                  ss0 <- und.price.f("GOL", yy, meta.und.price.df) ; exss0 <- und.price.f("USD", yy, meta.und.price.df) 
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0*r, meta.bonde.df, yy)
                  })
                  pro1.v
            }else if(uc){ #ss0 = varies usd , exss0 = varies usd
                  ss0 <- exss0 <- und.price.f("USD", yy, meta.und.price.df)
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0*r, exss0*r, meta.bonde.df, yy)
                  })
                  pro1.v
            }else if(ud){ #ss0 = 1000, exss0 = 1
                  ss0 <- und.price.f("GOL", yy, meta.und.price.df) ; exss0 <- 1
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0, meta.bonde.df, yy)
                  })
                  pro1.v
            }else if(ue){ #ss0 = varies usd, exss0 = 1 
                  ss0 <- und.price.f("USD", yy, meta.und.price.df) ; exss0 <- 1
                  pro1.v <- sapply(step, FUN = function(r){ 
                        proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0*r, exss0, meta.bonde.df, yy)
                  })
                  pro1.v    
            }else{
                  pro1.v <- rep(0, length(step))  
            }
            
      }else if(testasset.c %in% c("CRU", "PAL", "EUR")){
            #varies ss0, exss0 constant
                  if(Underlying.c %in% testasset.c | Currency.c %in% testasset.c){
                        ss0 <- und.price.f(Underlying.c, yy, meta.und.price.df) ; exss0 <- und.price.f(Currency.c, yy, meta.und.price.df)
                        pro1.v <- sapply(step, FUN = function(r){ 
                              if(Currency.c %in% "EUR"){
                                    g <-  proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0, exss0*r, meta.bonde.df, yy)    
                              }else{
                                    g <-  proT.f(FIC.c, cType.c, cff.c, Units.c, pro0.c, kPrice.c, tDate.c, mDate.c, ss0*r, exss0, meta.bonde.df, yy)
                              }
                              if(is.numeric(g)){
                                    g
                              }else{
                                    0
                              }
                        })
                        pro1.v    
                  }else{
                        pro1.v <- rep(0, length(step))
                  }
                                      
      }
      
      list(classH, pro1.v)
} 

##############################A8_ScoreHedging.R#################

scoreH.df.f <- function(P.type.h, P.type.t, testasset.c, TeamName.c){ #P.type.h means scenario; P.type.t means aggregate?
      notran <- sd(P.type.h) == 0; nouser <- sd(P.type.t) ==0 
      s1 <- scoreH1.f(P.type.h, P.type.t)
      s2 <- scoreH2.f(P.type.h, P.type.t)
      s3 <- scoreH3.f(P.type.h, P.type.t)
            ss <- sum(s1,s2,s3)
      if(nouser & notran){
            ss <- ss*0.2
      }else if(notran){
            ss <- ss - 3     
      }else{
            ss <- ss     
      }
      data.frame(TeamName= TeamName.c, Underlying = testasset.c, s1 = s1, s2 = s2, s3 = s3, sum = ss)
      
}

#Criteria 1: Ma = kMu, k = [a,b]
scoreH1.f <- function(P.type.h, P.type.t){
      Ma <- abs(mean(P.type.t)); Mu <- abs(mean(P.type.h))
      if(sd(P.type.h) == 0 & sd(P.type.t)==0){
            1
      }else{
            if(Ma < 0.6*Mu){
                  0     
            }else if(Ma >= 0.6*Mu & Ma < 0.7*Mu){
                  0.25
            }else if(Ma >= 0.7*Mu & Ma < 0.8*Mu){
                  0.5
            }else if(Ma >= 0.8*Mu & Ma < 0.9*Mu){
                  0.75
            }else if(Ma >= 0.9*Mu ){
                  1
            }
      }
}
#Criteria 2: Sa = kSu, k = [a,b]
scoreH2.f <- function(P.type.h, P.type.t){
      Sa <- sd(P.type.t); Su <- sd(P.type.h)
      if(Sa == 0 & Su == 0){
            2
      }else{
            if(Sa >= 3.5*Su){
                  -3 
            }else if(Sa >= 2.8*Su & Sa < 3.5*Su){ 
                  -2.5
            }else if(Sa >= 2.1*Su & Sa < 2.8*Su){ 
                  -2
            }else if(Sa >= 1.5*Su & Sa < 2.1*Su){      
                  -1.5
            }else if(Sa >= 1.2*Su & Sa < 1.5*Su){      
                  -1
            }else if(Sa >= 1*Su & Sa < 1.2*Su){
                  0
            }else if(Sa >= 0.8*Su & Sa < 1.0*Su){
                  0.5
            }else if(Sa >= 0.5*Su & Sa < 0.8*Su){
                  1
            }else if(Sa >= 0.3*Su & Sa < 0.5*Su){
                  1.5
            }else if(Sa >= 0*Su & Sa < 0.3*Su){
                  2
            }
      }
}
#Criteria 3: Pm + k*M = Px, k = [a,b] a>0; a*M < Px - Pm <= b*M, where M = slope
scoreH3.f <- function(P.type.h, P.type.t){
      Pm <- (min(P.type.t)); Px <- (min(P.type.h))
      DP <- abs(Pm - Px)
      loc <- which(P.type.h == Px); if(sum(loc > 30) > 2){u <- length(loc)}else{u <- 1} #get the location of the worst case for unhedged
      d <- movingdiff.f(P.type.h); M <- abs(d[loc[u]]) #get the slope, M
      
      if(sd(P.type.h) == 0 & sd(P.type.t)==0){
            2
      }else{
            if(DP > 15*M){
                  2
            }else if(DP > 10*M & DP <= 15*M){
                  1.5
            }else if(DP > 5*M & DP <= 10*M){
                  1
            }else if(DP > 0*M & DP <= 5*M){
                  0.5
            }else if(DP <= 0*M){
                  0  
            }
      }
      
}

movingdiff.f <- function(v){
      n <- length(v); n2 <- (n-1)/2
      
      p1 <-v[1:n2]-v[2:(n2+1)]
      # m1 <- 0
      e1 <- v[(n2+1):n]-v[n2:(n-1)]
      
      # c(p1,m1,e1)
      c(p1,e1)
} #return vector with differenced value (from middle value difference down' from middle value difference up)

