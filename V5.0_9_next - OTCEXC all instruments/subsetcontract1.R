#subset 

setwd("C:/Users/User/Google Drive/z_ALLHM/V5.0_9_next - OTCEXC all instruments")
source("createcontracts2.R")

##OPTIONS##
      optdf <- read.csv("option.csv")
      
      sub <- c("CRU", "PAL", "PAL", "CRU", "PAL")
      
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
      
      #writecsv
            write.csv(temp2, "finalopt3.csv", row.names = F)
            
            
##FORWARD##
fwdf <- read.csv("forward.csv")
styr <- paste0(0, fwdf[,3])
edyr <- paste0(0, fwdf[,3]+1)
      fwdf[,1] <- paste0("FW", styr, edyr, fwdf[, "Underlying"])
      
      #write.csv
            write.csv(fwdf, "finalfwd.csv", row.names = F)

##BOND##
bdf <- read.csv("bond.csv")
      styr <- paste0(0, bdf[,3])
      edyr <- paste0(0, bdf[,3]+1)
      bdf[,1] <- paste0("BD", styr, edyr)
            
      #write.csv
            write.csv(bdf, "finalbond.csv", row.names = F)
            