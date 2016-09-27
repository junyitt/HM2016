
#A1 Report - 01 - Core  
core.f <- function(tName, df){
      u <- df[,"TeamName"] %in% tName
      u2 <- df[,"classf"] %in% "Core"
      u3 <- grepl(pattern = "^Service", df[, "FIC"])
      u5 <- grepl(pattern = "^AFUT", df[,"cType"])
      u6 <- grepl(pattern = "^Production", df[,"cType"]) #CORE
      under1 <- c("GOL", "CRU", "PAL")
      
      
      if(grepl("Alpha", tName)){
            {      
                  u4 <- grepl(pattern = "Reduction", df[,"cType"]) #EXTRA
                  #core - cost -  prod
                  coret <- df[u & u2 & u6, c("Underlying", "Units", "kPrice", "exST")]
                  #extra - cost reduction
                  ext <- df[u & u3 & u4, c("Underlying", "Units", "kPrice")]
                  ##core - revenue - AFUT
                  aft <- df[u & u2 & u5, c("Underlying", "Units", "ST", "exST")]
                  
                  coret[,"costred"] <-  sapply(under1, FUN = function(j){
                        if(nrow(ext) > 0){
                              if(ext[, "Underlying"] == j){
                                    ext[,"kPrice"]     
                              }else{
                                    0
                              }
                        }else{0}
                  })     #get cost reduction values
            } #subset necessary data
            
            #Revenue
            AssetSold <- aft[,"Underlying"]
            Units1 <- aft[,"Units"]
            Price <- aft[,"ST"]*aft[,"exST"]
            TotalRevenue <- Units1*Price
            #Cost
            RawAsset <- coret[,"Underlying"]
            Units2 <- coret[,"Units"]
            Cost <- coret[, "kPrice"] - coret[,"costred"]
            TotalCost <- Units2*Cost
            TotalProfit <- TotalRevenue - TotalCost
            
            data.frame(AssetSold, Units1, Price, TotalRevenue, RawAsset, Units2, Cost, TotalCost, TotalProfit)          
            
      }else{
            u4 <- grepl(pattern = "Increment", df[,"cType"]) #EXTRA
            
            revet <- df[u & u2 & u6, c("Underlying", "Units", "kPrice", "exST")] #core-production-gol2 (rev)
            ext <- df[u & u3 & u4, c("Underlying", "Units", "kPrice")]  #extra-service-increment      (rev)
            aft <- df[u & u2 & u5, c("Underlying", "Units", "ST", "exST")] #core-afut-buy (cost)
            revet[,"revinc"] <-  sapply(under1, FUN = function(j){
                  if(nrow(ext) > 0){
                        if(ext[, "Underlying"] == j){
                              ext[,"kPrice"]     
                        }else{
                              0
                        }
                  }else{0}
            }) 
            
            #Revenue
            AssetSold <- revet[,"Underlying"]
            Units1 <- revet[,"Units"]
            Price <- revet[,"kPrice"] + revet[,"revinc"]
            TotalRevenue <- Units1*Price
            #Cost
            RawAsset <- aft[,"Underlying"]
            Units2 <- aft[,"Units"]
            Cost <- aft[, "ST"]*aft[,"exST"]
            TotalCost <- Units2*Cost
            TotalProfit <- TotalRevenue - TotalCost
            
            data.frame(AssetSold, Units1, Price, TotalRevenue, RawAsset, Units2, Cost, TotalCost, TotalProfit)       
            
      }
      
      
      
} #function #return report df

#A1 Report - 02 - Scenario transactions
scenario.f <- function(tName, df, yy){
      
      #need to select columns that will be DISPLAYED
      dispvar <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
                   "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT")
      
      u <- df[,"TeamName"] %in% tName
      u1 <- df[, "classf"] %in% "Scenario"
      yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
      
      #outdf: 
      df[u & u1 & u2, dispvar]
} #function - return relevant scenario transactions, with subsetted variables


#A1 Report - 03 - Extra transactions

extraR.f <- function(tName, df, yy){
      
      #need to select columns that will be DISPLAYED
      dispvarE <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL")
      
      u <- df[,"TeamName"] %in% tName
      u1 <- df[, "classf"] %in% "Extra"
      yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
      
      #outdf: 
      df[u & u1 & u2, dispvarE]
} #function - return relevant scenario transactions, with subsetted variables  


#A1 Report - 05 - ALL Your transactions
userR.f <- function(tName, df){
      #need to select columns that will be DISPLAYED
      dispvarU <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL", "VLRemarks", "Remarks")
      
      u <- df[,"TeamName"] %in% tName
      u1 <- df[, "classf"] %in% c("OTC", "EXC")
      
      #outdf: 
      df[u & u1, dispvarU]
} #function - return relevant scenario transactions, with subsetted variables  


#A1 Report - 06 - Balance Sheet
accTypef <- function(cType, cff, mv_ctype){
      if(cType %in% mv_ctype){
            if(cff == 1){
                  "FinAsset"
            }else if(cff == -1){
                  "Loan"
            }
      }else{
            "N.A"
      }
} #return account type

old2newbs.f <- function(balsh_y, acc1, pro0, proT){
      balsh_y[,"Cash"] <- balsh_y[,"Cash"] + pro0 + proT
      balsh_y[,"FinAsset"] <- acc1[,"FinAsset"]
      balsh_y[,"Loan"] <- acc1[,"Loan"]
      balsh_y[,"RE"] <- balsh_y[,"PPE"] + balsh_y[,"FinAsset"] + balsh_y[,"Cash"] - balsh_y[,"ShareCap"] - balsh_y[,"Loan"]
      balsh_y[,"NAV"] <- balsh_y[,"RE"]+balsh_y[,"ShareCap"]
      balsh_y
} #return new balance sheet

newbs.df.f <- function(balsh_y, fdf8_td , teamname12, fdf_y7){
      #ALL cType 
      allctype <- c("Employ Service", "Cash", "Revenue Increment", "Cost Reduction", "Production",
                    "AFUT", "Forward", "Forward-S", "Bond", "Bond-E", "Loan",
                    "Put Option", "Put Option-S", "Put Option-E", 
                    "Call Option", "Call Option-S", "Call Option-E")
      # zeromv <- c("Revenue Increment", "AFUT", "Cost Reduction", "Forward", "Forward-S","Production", "Cash", "Employ Service")
      # tdatemv <- c("Call Option", "Put Option", "Call Option-S", "Put Option-S", "Call Option-E", "Put Option-E")
      mv_ctype <- c("Bond", "Bond-E", "Loan")
      
      m.acc <- c("FinAsset", "Loan")
      
      
      N <- nrow(fdf8_td); cff <- sapply(1:N, FUN = function(j){cff_f(fdf8_td[j, "cType"], fdf8_td[j, "pos1"])})
      
      accType <- sapply(1:N, FUN = function(j){
            accTypef(fdf8_td[j,"cType"], cff[j], mv_ctype)    
      })
      acclist <- lapply(teamname12, FUN = function(j){
            u <- fdf8_td[, "TeamName"] == j
            sapply(m.acc, FUN = function(k){
                  u2 <- accType == k
                  abs(sum(fdf8_td[u & u2, "MVT"], na.rm =T))
            })
            
            
      })
      acc1 <- do.call(rbind,acclist)
      #find sum pro, proT for each team
      {
            #must use fdf_y7, NOT fdf_y6
            pro0 <- sapply(teamname12, FUN = function(name){
                  u <- fdf_y7[, "TeamName"] == name
                  npro1 <- sum(fdf_y7[u, "NetPro"], na.rm = T)
                  npro1
            })
            
            #must use fdf8_td
            proT <- sapply(teamname12, FUN = function(name){
                  u <- fdf8_td[, "TeamName"] == name
                  npro2 <- sum(fdf8_td[u, "ProT"], na.rm = T)
                  npro2
            })
      } #pro0, proT
      
      balsh_new_yy2 <- old2newbs.f(balsh_y, acc1, pro0, proT) #return balance sheet for year yy2, each row representing each team
      
      
}
                       
                       
#A1 Report - 07 - Hedging Evaluation
subdfR.f <- function(df, v1, v2, v3, tName, yy){ #input df, criteria vector 1-v1 , criteria vector 2-v2, & v3 for classf output relevant df
      u1 <- match(df[,"Underlying"], v1, nomatch = 0) > 0 ##df[,"Underlying"] %in% v1
      u2 <- df[,"Underlying"] %in% v2
      u3 <- df[,"classf"] %in% v3 ; u4 <- df[,"TeamName"] %in% tName; u5 <- df[,"tDate"] < yy+1 &  df[,"mDate"] >= yy+1
      df[(u1 | u2) & (u3 & u4 & u5),]
} #return subdf

movingdiff.f <- function(v){
      n <- length(v); n2 <- (n-1)/2
      
      p1 <-v[1:n2]-v[2:(n2+1)]
      # m1 <- 0
      e1 <- v[(n2+1):n]-v[n2:(n-1)]
      
      # c(p1,m1,e1)
      c(p1,e1)
} #return vector with differenced value (from middle value difference down' from middle value difference up)


sumprov.vary.f <- function(df, step, yy, ss0, exss0, varyund = T){
      if(nrow(df)==0){
            rep(0, length(step))
      }else{
            sapply(step, FUN = function(r){
                  dfcalc <- df; cff <- sapply(1:nrow(dfcalc), FUN = function(j){cff_f(dfcalc[j,"cType"], dfcalc[j,"pos1"])})
                  
                  proTv1 <- sapply(1:nrow(dfcalc), FUN = function(i){ #return a vector containing proT of dfcalc, later sum it to get 'portfolio proT'
                                    { 
                                          FIC <- dfcalc[i,"FIC"]
                                          cType <- dfcalc[i,"cType"]
                                          cff1 <- cff[i]
                                          Units <- dfcalc[i,"Units"]
                                          Pro <- dfcalc[i,"Pro"]
                                          kPrice <- dfcalc[i,"kPrice"]
                                          tDate <-  dfcalc[i,"tDate"]; mDate <-  dfcalc[i,"mDate"]
                                    } #assign FIC, cType, cff1 etc.
                                    if(varyund){
                                          pros0 <- ProTf(FIC, cType, cff1, Units, Pro, kPrice, tDate, mDate, ss0*r, exss0, yy)
                                    }else{
                                          pros0 <- ProTf(FIC, cType, cff1, Units, Pro, kPrice, tDate, mDate, ss0, exss0*r, yy)
                                    }
                                    if(!is.numeric(pros0)){0}else{pros0}
                                    
                              })
                  portfolioproT <- sum(proTv1, na.rm = T); portfolioproT
            
            })
      }
} #vary underlying price #input a subset fulltran df, and step, output: sum of proceed for each step case

scoreH.f <- function(P.type.h, P.type.t, sumq = T){
            notran <- sd(P.type.h) == 0; nouser <- sd(P.type.t) ==0 
            s1 <- scoreH1.f(P.type.h, P.type.t)
            s2 <- scoreH2.f(P.type.h, P.type.t)
            s3 <- scoreH3.f(P.type.h, P.type.t)
                  if(sumq){
                        out <- sum(s1,s2,s3)
                  }else{
                        out <- c(s1,s2,s3)
                  }

            if(nouser){
                  out*0.5
            }else if(notran){
                  out - 3     
            }else{
                  out     
            }
         
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
            if(Sa >= 1*Su){
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
      Pm <- abs(min(P.type.t)); Px <- abs(min(P.type.h))
      loc <- grep(Px, P.type.h); if(sum(loc > 30) > 2){u <- length(loc)}else{u <- 1} #get the location of the worst case for unhedged
      d <- movingdiff.f(P.type.h); M <- abs(d[loc[u]]) #get the slope, M
      
      if(sd(P.type.h) == 0 & sd(P.type.t)==0){
            2
      }else{
            if(Px-Pm > 15*M){
                  2
            }else if(Px-Pm > 10*M & Px-Pm <= 15*M){
                  1.5
            }else if(Px-Pm > 5*M & Px-Pm <= 10*M){
                  1
            }else if(Px-Pm > 0*M & Px-Pm <= 5*M){
                  0.5
            }else if(Px-Pm <= 0*M){
                  0  
            }
      }
      
}


#A1 Report - 09 - Cash Evaluation
score.cash.f <- function(df, yy, tName){
      u1 <- df[,"Remarks"] %in% "ST loan due to negative cash"; u2 <- df[, "tDate"] == yy
      u3 <- df[,"TeamName"] %in% tName
      if(sum(u1 & u2 & u3) == 0){
            10
      }else{
            0
      }
}