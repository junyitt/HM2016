#B1_PreReportFunctions.R


#B1_PreReport_01 - Core  
core.f <- function(tName, df, yy){
      u <- df[,"TeamName"] %in% tName
      u2 <- df[,"classf"] %in% "CORE"
      u3 <- grepl(pattern = "^Service", df[, "FIC"])
      u5 <- grepl(pattern = "^AFUT", df[,"cType"])
      u6 <- grepl(pattern = "^Production", df[,"cType"]) #CORE
      u7 <- df[,"tDate"] %in% yy
      under1 <- c("GOL", "CRU", "PAL")
      under2 <- c("GOL2", "CRU2", "PAL2")
      
      if(grepl("Alpha", tName)){
            {      
                  u4 <- grepl(pattern = "Reduction", df[,"cType"]) #EXTRA
                  #core - cost -  prod
                  coret <- df[u & u2 & u6 & u7, c("Underlying", "Units", "kPrice", "exsT")]
                  #extra - cost reduction
                  ext <- df[u & u3 & u4 & u7, c("Underlying", "Units", "kPrice")]
                  ##core - revenue - AFUT
                  aft <- df[u & u2 & u5 & u7, c("Underlying", "Units", "sT", "exsT")]
                  
                  coret[,"costred"] <-  sapply(under1, FUN = function(j){
                        if(nrow(ext) > 0){
                              if(j %in% ext[, "Underlying"]){
                                    uu11 <- ext[, "Underlying"] %in% j
                                    sum(ext[uu11,"kPrice"])
                              }else{
                                    0
                              }
                        }else{0}
                  })     #get cost reduction values
            } #subset necessary data
            
            #Revenue
            AssetSold <- aft[,"Underlying"]
            Units1 <- aft[,"Units"]
            Price <- aft[,"sT"]*aft[,"exsT"]
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
            
            revet <- df[u & u2 & u6 & u7, c("Underlying", "Units", "kPrice", "exsT")] #core-production-gol2 (rev)
            ext <- df[u & u3 & u4 & u7, c("Underlying", "Units", "kPrice")]  #extra-service-increment      (rev)
            aft <- df[u & u2 & u5 & u7, c("Underlying", "Units", "sT", "exsT")] #core-afut-buy (cost)
            revet[,"revinc"] <-  sapply(under2, FUN = function(j){
                  if(nrow(ext) > 0){
                        if(j %in% ext[, "Underlying"]){
                              uu11 <- ext[, "Underlying"] %in% j
                              sum(ext[uu11,"kPrice"])        
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
            Cost <- aft[, "sT"]*aft[,"exsT"]
            TotalCost <- Units2*Cost
            TotalProfit <- TotalRevenue - TotalCost
            
            data.frame(AssetSold, Units1, Price, TotalRevenue, RawAsset, Units2, Cost, TotalCost, TotalProfit)       
            
      }
      
      
      
} #function #return report df

#B1_PreReport_02 - Relevant Transactions
relevant.f <- function(TeamName.c, end3.td.df, yy, rel = T, serv = F){
      
      if(rel){Q <- 1}else{Q <- 0}
      if(serv){S <- 1}else{S <- 0}
      #need to select columns that will be DISPLAYED
      dispvar <- c("TrackNo", "classf", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice",
                   "pos1", "Units", "tDate", "mDate", "netpro0", "proT", "mvT", "Remarks", "VLRemarks")
      
      u <- end3.td.df[,"TeamName"] %in% TeamName.c
      u1 <- end3.td.df[, "classf"] %in% toupper(c("SCENARIO", "EXTRA", "OTC", "EXC"))
      mm2 <- end3.td.df[,"tDate"] < yy+1 & yy+1 <= end3.td.df[,"mDate"] 
      valid1 <- end3.td.df[,"VL"] %in% Q
      if(serv){
            serv.u1 <- end3.td.df[,"cType"] %in% c("Revenue Increment", "Cost Reduction")
      }else{
            serv.u1 <- !end3.td.df[,"cType"] %in% c("Revenue Increment", "Cost Reduction")
      }
      #outdf:
      dispdf <- end3.td.df[u & u1 & mm2 & valid1 & serv.u1, dispvar]     
      colnames(dispdf) <- c("TrackNo", "TransactionClass", "FIC", "CounterParty", "ContractType", "Underlying", "Currency", "kPrice",
                           "Position", "Units", "TransactionDate", "MaturityDate", "NetProceed_0", "Payoff_T", "MarketValue_T", "Remarks", "Status")
      dispdf
}



      