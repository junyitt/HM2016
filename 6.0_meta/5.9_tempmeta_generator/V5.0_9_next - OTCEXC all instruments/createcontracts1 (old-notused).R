source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")

setwd("C:/Users/User/Google Drive/z_ALLHM/6.0_meta/5.9_tempmeta_generator/V5.0_9_next - OTCEXC all instruments")

df <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))

# vol <- df

instrument <- c("GOL", "CRU", "PAL", "USD", "EUR")
vol <- sapply(instrument, FUN = function(i){
      v <- df[,i]; n <- length(v)
      p <- sapply(1:(n-1), FUN = function(j){
            set.seed(123+j)
            abs(v[j+1]/v[j]-1)*runif(1,0.8,1.2)
      })
}) #volatility


createins <- c("CRU", "GOL", "PAL", "CRU", "PAL")

      nn <- 2
      

      if(nn%%2 == 0){ 
            rp1 <- 0.10*(1:(nn/2)); rp2 <- -0.10*((nn/2):1)
            rp <- c(rp2, rp1)
      }else{
            nn2 <- nn-1
            rp1 <- 0.10*(1:(nn2/2)); rp2 <- -0.10*((nn2/2):1)
            rp <- c(rp2, 0, rp1)
      }
      rp <- rp+1

Kdf  <- sapply(X = 1:length(createins), function(I){
            s0v <- df[I,createins[I]]
             
                  sapply(rp, function(j){
                        round(s0v*j,-1)
                  })
            
      
})

# premdf <- apply(X = 1:5, MARGIN = 2, FUN = function(x){
#       sapply(x, FUN = function(y){
#             bsf(s0 = y, K = y, 
#       })
#           
# })
# N


#problem, no classification of year
      
      
      
      
