#subset 
convoptionctype.f <- function(FIC.c){
      if(grepl("CL0", FIC.c)){
            "Call Option"
      }else if(grepl("PT0", FIC.c)){
            "Put Option"
      }
}
      

convcurr.f <- function(Underlying.c){
      if(Underlying.c %in% "GOL"){
            "USD"
      }else{
            "MYR"
      }
}
setwd("C:/Users/User/Google Drive/z_ALLHM/6.0_meta/5.9_tempmeta_generator/V5.0_9_next - OTCEXC all instruments")
source("createcontracts2.R")

##OPTIONS##
      optdf <- read.csv("option.csv")
      
      sub <- c("CRU", "GOL", "PAL", "CRU", "PAL")
      
      temp <- list(); temp2 <- list()
      #subset required raw asset at the particular year
      for(j in 1:5){
            u <- optdf[, "Underlying"] == sub[j]
            yy <- optdf[, "Yr"] == (j-1)
            temp[[j]] <- optdf[u & yy, ]
      }
            temp2 <- do.call(rbind, temp)
      
      #renaming the contract to format: #CL0203CRU500
      styr <- paste0(0, temp2[,4])
      edyr <- paste0(0, temp2[,4]+1)
      renameopt <- function(x){
            if(x == "Call"){"CL"}else if(x == "Put"){"PT"}
      }
      op <- sapply(X = temp2[,1], FUN = function(j){renameopt(j)})
            temp2[,1] <- paste0(op, styr, edyr, temp2[, "Underlying"], temp2[,"K"])
      
            FIC <- temp2[,"OPTION"]
            cType <- sapply(FIC, convoptionctype.f) 
            Underlying <- temp2[,"Underlying"] 
            Currency <- sapply(Underlying, convcurr.f)
            kPrice <- temp2[,"K"]
            S0 <- Proceed <- temp2[,"PREMIUM"]
            Tfee <- S0*0.02
            tDate <- as.numeric(substr(FIC, 3,4))
            mDate <- as.numeric(substr(FIC, 5,6))
                  opt.df <- data.frame(FIC, cType, Underlying, Currency, kPrice, S0, Proceed, Tfee, tDate, mDate)
            
      #writecsv
            write.csv(opt.df, "finalopt.csv", row.names = F)
            
            
##FORWARD##
fwdf <- read.csv("forward.csv")
styr <- paste0(0, fwdf[,3])
edyr <- paste0(0, fwdf[,3]+1)
      fwdf[,1] <- paste0("FW", styr, edyr, fwdf[, "Underlying"])
            FIC <- fwdf[,"CONTRACT"]; N <- length(FIC)
            cType <- rep("Forward", N)
            Underlying <- fwdf[,"Underlying"] 
            Currency <- rep("MYR", N)
            kPrice <- fwdf[,"FWP"]
            S0 <- Proceed <- Tfee <- 0
            tDate <- as.numeric(substr(FIC, 3,4))
            mDate <- as.numeric(substr(FIC, 5,6))
            fw.df <- data.frame(FIC, cType, Underlying, Currency, kPrice, S0, Proceed, Tfee, tDate, mDate)
            #write.csv
                  write.csv(fw.df, "finalfwd.csv", row.names = F)

##BOND##
bdf <- read.csv("bond.csv")
      styr <- paste0(0, bdf[,3])
      edyr <- paste0(0, bdf[,3]+1)
      bdf[,1] <- paste0("BD", styr, edyr)
            FIC <- bdf[,"BOND"]; N <- length(FIC)
            cType <- rep("Bond", N)
            Underlying <- rep(NA, N)
            Currency <- rep("MYR", N)
            kPrice <- rep(1000, N)
            S0 <- Proceed <- bdf[, "Price"]
            Tfee <- 0
            tDate <- as.numeric(substr(FIC, 3,4))
            mDate <- as.numeric(substr(FIC, 5,6))
                  bd.df <- data.frame(FIC, cType, Underlying, Currency, kPrice, S0, Proceed, Tfee, tDate, mDate)
      #write.csv
            write.csv(bd.df, "finalbond.csv", row.names = F)
            

            FICEXC <- rbind(opt.df, fw.df, bd.df)
            write.csv(FICEXC, "_FIC_EXC_V1.csv", row.names = F)
            