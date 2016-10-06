
#A1 Report - 01 - Core  
core.f <- function(tName, df, yy){
      u <- df[,"TeamName"] %in% tName
      u2 <- df[,"classf"] %in% "Core"
      u3 <- grepl(pattern = "^Service", df[, "FIC"])
      u5 <- grepl(pattern = "^AFUT", df[,"cType"])
      u6 <- grepl(pattern = "^Production", df[,"cType"]) #CORE
            u7 <- df[,"tDate"] %in% yy
      under1 <- c("GOL", "CRU", "PAL")
      
      
      if(grepl("Alpha", tName)){
            {      
                  u4 <- grepl(pattern = "Reduction", df[,"cType"]) #EXTRA
                  #core - cost -  prod
                  coret <- df[u & u2 & u6 & u7, c("Underlying", "Units", "kPrice", "exST")]
                  #extra - cost reduction
                  ext <- df[u & u3 & u4 & u7, c("Underlying", "Units", "kPrice")]
                  ##core - revenue - AFUT
                  aft <- df[u & u2 & u5 & u7, c("Underlying", "Units", "ST", "exST")]
                  
                  coret[,"costred"] <-  sapply(under1, FUN = function(j){
                        if(nrow(ext) > 0){
                              if(j %in% ext[, "Underlying"]){
                                    uu11 <- ext[, "Underlying"] %in% j
                                    ext[uu11,"kPrice"]     
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
            
            revet <- df[u & u2 & u6 & u7, c("Underlying", "Units", "kPrice", "exST")] #core-production-gol2 (rev)
            ext <- df[u & u3 & u4 & u7, c("Underlying", "Units", "kPrice")]  #extra-service-increment      (rev)
            aft <- df[u & u2 & u5 & u7, c("Underlying", "Units", "ST", "exST")] #core-afut-buy (cost)
            revet[,"revinc"] <-  sapply(under1, FUN = function(j){
                  if(nrow(ext) > 0){
                        if(j %in% ext[, "Underlying"]){
                              uu11 <- ext[, "Underlying"] %in% j
                              ext[uu11,"kPrice"]        
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
      u1 <- df[, "classf"] %in% "Extra" & df[, "cType"] %in% c("Employ Service", "Bond-E")
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



                       
#A1 Report - 07 - Hedging Evaluation
subdfR.f <- function(df, v1, v2, v3, tName, yy){ #input df, criteria vector 1-v1 , criteria vector 2-v2, & v3 for classf output relevant df
      u1 <- match(df[,"Underlying"], v1, nomatch = 0) > 0 ##df[,"Underlying"] %in% v1
      u2 <- df[,"Currency"] %in% v2
      u3 <- df[,"classf"] %in% v3 ; u4 <- df[,"TeamName"] %in% tName; u5 <- df[,"tDate"] < yy+1 &  df[,"mDate"] >= yy+1
      df[(u1 | u2) & (u3 & u4 & u5),]
} #return subdf






