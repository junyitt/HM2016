


#A1 Report - 02 - Scenario transactions
scenario.f <- function(tName, df, yy){

      #need to select columns that will be DISPLAYED
      dispvar <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice",
                   "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT")

      u <- df[,"TeamName"] %in% tName
      u1 <- df[, "classf"] %in% "SCENARIO"
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
      u1 <- df[, "classf"] %in% "EXTRA" & df[, "cType"] %in% c("Employ Service", "Bond-E")
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



                     






