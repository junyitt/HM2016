#70_01_BGN2Functions.R

addloan_f <- function(ncash, tName, yy){
      if(ncash < 0){
            TrackNo <- 3
            classf <- "OTC"
            FIC <- rClass <- sClass <- NA
            TeamName <- tName
            cParty <- "3rd Party Loan"
            cType <- "Loan"
            Underlying <- NA
            Currency <- "MYR"
            kPrice <- 0.12
            pos1 <- "Borrow"
            Units <- 1e6 - ncash
            tDate <- yy
            mDate <- yy+1
            tKey <- NA
            Remarks <- "ST loan due to negative cash"
            VL <- 1
            VLRemarks <- 1
            data.frame(TrackNo, classf, FIC, rClass, sClass, TeamName, cParty, 
                       cType, Underlying, Currency, kPrice, 
                       pos1, Units, tDate, mDate, tKey, Remarks,
                       VL, VLRemarks)    
            
      }else{}
}