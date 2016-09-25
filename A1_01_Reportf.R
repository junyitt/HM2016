
#A1 Report - 01 - Core  
core.f <- function(tName, df){
      u <- df[,"TeamName"] == tName
      u2 <- df[,"classf"] == "Core"
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
      
      u <- df[,"TeamName"] == tName
      u1 <- df[, "classf"] == "Scenario"
      yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
      
      #outdf: 
      df[u & u1 & u2, dispvar]
} #function - return relevant scenario transactions, with subsetted variables


#A1 Report - 03 - Extra transactions

extraR.f <- function(tName, df, yy){
      
      #need to select columns that will be DISPLAYED
      dispvarE <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL")
      
      u <- df[,"TeamName"] == tName
      u1 <- df[, "classf"] == "Extra"
      yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
      
      #outdf: 
      df[u & u1 & u2, dispvarE]
} #function - return relevant scenario transactions, with subsetted variables  


#A1 Report - 05 - ALL Your transactions
userR.f <- function(tName, df){
      #need to select columns that will be DISPLAYED
      dispvarU <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
                    "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL", "VLRemarks", "Remarks")
      
      u <- df[,"TeamName"] == tName
      u1 <- df[, "classf"] == c("OTC", "EXC")
      
      #outdf: 
      df[u & u1, dispvarU]
} #function - return relevant scenario transactions, with subsetted variables  


#A1 Report - 06 - Balance Sheet
accTypef <- function(cType, cff){
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
      balsh_y
} #return new balance sheet


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
      m1 <- 0
      e1 <- v[(n2+1):n]-v[n2:(n-1)]
      
      c(p1,m1,e1)
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
                                          pros0 <- ProTf(FIC, cType, cff1, Units, Pro, kPrice, tDate, mDate, ss0*r, exss0, yy+1)
                                    }else{
                                          pros0 <- ProTf(FIC, cType, cff1, Units, Pro, kPrice, tDate, mDate, ss0, exss0*r, yy+1)
                                    }
                                    if(!is.numeric(pros0)){0}else{pros0}
                                    
                              })
                  portfolioproT <- sum(proTv1, na.rm = T); portfolioproT
            
            })
      }
} #vary underlying price #input a subset fulltran df, and step, output: sum of proceed for each step case


#sumprov.varyex.f <- function(df, step, yy, ss0, exss0){
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
                  pros0 <- ProTf(FIC, cType, cff1, Units, Pro, kPrice, tDate, mDate, ss0, exss0*r, yy+1)
                  if(!is.numeric(pros0)){0}else{pros0}
                  
            })
            portfolioproT <- sum(proTv1, na.rm = T); portfolioproT
            
      })
      }
} #vary exchange rate #input a subset fulltran df, and step, output: sum of proceed for each step case