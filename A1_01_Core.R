#A1 Report - 01 - Core       #A1_01_Core.R
{
#generate team names
a <- paste0("Alpha ", 1:6); b <- paste0("Beta ", 1:6); teamname12 <- c(a,b)

#function #return report df
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
      
      
      
}

#output c_list
cReport_list <- lapply(teamname12, FUN = function(x){
      core.f(x, fdf8_td)
      
}) #return list of core report, where list[[1]] is the core report df for team Alpha 1 and so on
} #cReport_list
      
#A1 Report - 02 - Scenario transactions
{
#need to select columns that will be DISPLAYED
dispvar <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
             "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT")
    
#function - return relevant scenario transactions, with subsetted variables  
scenario.f <- function(tName, df, yy){
      u <- df[,"TeamName"] == tName
      u1 <- df[, "classf"] == "Scenario"
      yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
      
      #outdf: 
            df[u & u1 & u2, dispvar]
}

#output s_list      
sReport_list <- lapply(teamname12, FUN = function(x){
                              scenario.f(x, fdf8_td, yy)
      
                        })
} #sReport_list

#A1 Report - 03 - Extra transactions
{
#need to select columns that will be DISPLAYED
dispvarE <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
             "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL")

#function - return relevant scenario transactions, with subsetted variables  
extraR.f <- function(tName, df, yy){
      u <- df[,"TeamName"] == tName
      u1 <- df[, "classf"] == "Extra"
      yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
      
      #outdf: 
      df[u & u1 & u2, dispvarE]
}

#output s_list      
eReport_list <- lapply(teamname12, FUN = function(x){
      extraR.f(x, fdf8_td, yy)
      
})
} #eReport_list

#A1 Report - 04 - Other Money Transactions? Hints/Tips
{
# 
# #need to select columns that will be DISPLAYED
# dispvarE <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
#               "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL")
# 
# #function - return relevant scenario transactions, with subsetted variables  
# extraR.f <- function(tName, df, yy){
#       u <- df[,"TeamName"] == tName
#       u1 <- df[, "classf"] == "Extra"
#       yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
#       
#       #outdf: 
#       df[u & u1 & u2, dispvarE]
# }
# 
# #output s_list      
# eReport_list <- lapply(teamname12, FUN = function(x){
#       extraR.f(x, fdf8_td, yy)
#       
# })
}

#A1 Report - 05 - ALL Your transactions
{
#need to select columns that will be DISPLAYED
dispvarU <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
              "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL", "VLRemarks", "Remarks")

#function - return relevant scenario transactions, with subsetted variables  
userR.f <- function(tName, df){
      u <- df[,"TeamName"] == tName
      u1 <- df[, "classf"] == c("OTC", "EXC")
      
      #outdf: 
      df[u & u1, dispvarU]
}

#output s_list      
uReport_list <- lapply(teamname12, FUN = function(x){
      userR.f(x, fdf8_td)
      
})
}

#A1 Report - 06 - Balance Sheet

#A1 Report - 07 - Hedging Evaluation
#A1 Report - 08 - Extra Event Evaluation - Score
#A1 Report - 09 - Cash Evaluation

#A1 Report - 10 - Total Score Breakdown
