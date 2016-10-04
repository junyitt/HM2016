source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")

setwd("C:/Users/User/Google Drive/z_ALLHM/V5.0_9_next - OTCEXC all instruments")

source("optionscalc.R")

df <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))

createKloop <- function(nn){
      if(nn%%2 == 0){ 
            rp1 <- 0.10*(1:(nn/2)); rp2 <- -0.10*((nn/2):1)
            rp <- c(rp2, rp1)
      }else{
            nn2 <- nn-1
            rp1 <- 0.10*(1:(nn2/2)); rp2 <- -0.10*((nn2/2):1)
            rp <- c(rp2, 0, rp1)
      }
      rp <- rp+1
}

instrument <- c("GOL", "CRU", "PAL", "USD", "EUR")
vol <- sapply(instrument, FUN = function(i){
      v <- df[,i]; n <- length(v)
      p <- sapply(1:(n-1), FUN = function(j){
            set.seed(123+j)
            abs(v[j+1]/v[j]-1)*runif(1,0.8,1.2)
      })
})

##For Options#
      temp <- list();temp2 <- list();temp3 <- list()
      rawL <- c("GOL", "CRU", "PAL")
      rateL <- c("rateUSD", "rateMYR", "rateMYR")
      
      #create loop factor for K
      rp <- createKloop(2)
      
      #loop j for raw asset
      for(j in 1:3){
            #loop i for year
            for(i in 1:nrow(vol)){
                  s01 <- df[i,rawL[j]] 
                  r1 <- df[i,rateL[j]]
                  s1 <- vol[i,rawL[j]]
                       
                  #loop across K loop factor
                  temp <- lapply(rp, FUN = function(aa){
                        kk <- round(s01*(aa),-1)
                        cdf <- bsf(s0 = s01, K = kk, r = r1, d = 0, s = s1, t = 1)
                        cname <- c("Call", "Put")
                        cbind(OPTION = cname, PREMIUM = cdf, K = kk, Yr = i-1, Underlying = rawL[j])
                        })
                  temp2[[i]] <- do.call(rbind, temp)
            }
      
            temp3[[j]] <- do.call(rbind, temp2)
      }
      
      #fulldf on all possible options
      temp4 <- do.call(rbind, temp3)
      
      write.csv(temp4, "option.csv", row.names = F)


##for forward##
      temp <- list();temp2 <- list();temp3 <- list()
      exL <- c("USD", "EUR")
      rL <- rep(("rateMYR"), 2); dL <- c("rateUSD", "rateEUR")
      
      #loop j for exrate
      for(j in 1:2){
            #loop i for year
            for(i in 1:nrow(vol)){
                  s01 <- df[i,exL[j]] 
                  r1 <- df[i, rL[j]]
                  d1 <- df[i, dL[j]]
                        cdf <- fwf(s0 = s01, r = r1, d = d1, t = 1)
                        temp[[i]] <- cbind(CONTRACT = "FORWARD", FWP = cdf, Yr = i-1, Underlying = exL[j])
            }
            temp2[[j]] <- do.call(rbind, temp)
      }
      
      temp3 <- do.call(rbind, temp2)

      write.csv(temp3, "forward.csv", row.names = F)

##BOND##
      bondp <- round(1000/(1 + df[,"rateMYR"]),2)
      tempbond <- cbind(BOND = rep("BD",5), Price = bondp[1:5], Yr = 0:4)
            write.csv(tempbond, "bond.csv", row.names = F)
            