

library(reshape2)
###INPUT
source("~/003HedgeMaster/0f_pricing.R")
source("~/003HedgeMaster/0meta_pricing.R")

#########################
##Price of underlying - 8
#########################

#########

# assetn <- names(mu)
seed <- c(10,3,7,5,6,8,91,11)
S0 <- c(50,50,50,3.3,4,1000,80,2000)

assetlist <- list()
# for(i in 1:length(seedn)){
#         assetlist[[i]] <- simprice(mu = mu[,assetn[i]], sigma = sigma[,assetn[i]], S0 = S0n[i], seed = seedn[i])
# }
# 


assetlist <- lapply(X = 1:8, function(q){
        simprice(mu = mu[,q], sigma = sigma[,q], S0 = S0[q], seed = seed[q])
})
        
subpricelist <- lapply(X = assetlist, FUN = subprice)        
        underdf <- do.call(cbind, subpricelist)
        underdf[,c(1:3,6:8)] <- round(underdf[,c(1:3,6:8)],2)
        underdf[,4:5] <- round(underdf[,4:5],4)
        
        colnames(underdf) <- c("AAA", "BBB", "CCC", "USD", "EUR", "GOL", "CRU", "PAL")
        underdf <- t(underdf)

##################
##BOND PRICE - 4
##################
        n <- c(1,3,5,10)
        bondprice <- list()
        for(j in 1:length(n)){
                ii <- bondi[,j]
                z <- n[j]
                bondprice[[j]] <- bondcal(i = ii, n = z)
        }
        bonddf <- do.call(cbind, bondprice)
        bonddf <- round(bonddf, 2)
        colnames(bonddf) <- c("BO-ONE", "BO-THR", "BO-FIV", "BO-TEN")
        bonddf <- t(bonddf)
        
########################################
##FORWARD THEORETICAL PRICE - TOO MANY! 8 underlying* each year has n-k prices (k is the current period) - about 8*50 = 400
########################################
        
##############################        
##OPTIONS - 8*30 + 8*30 = 480
##############################
        volalist <- lapply(X = assetlist, FUN = vola2)
        
assetindex <- seq(1,8,1)
tcp <- list()
yearprice <- list()
# Y = Yth year Y = 1 to 11
for(Y in 1:11){
        #Y <- 1
tempdf <- data.frame()        
        tcp[[Y]] <- lapply(assetindex, FUN = function(z){
                S <- subpricelist[[z]][Y]
                K <- strike[[z]]
                #                 r <- interest[[z]][Y]
                #                 d <- dividend[[z]][Y]
                r <- interest[[z]][Y]
                d <- dividend[[z]][Y]
                v <- volalist[[z]][Y]
                T <- 1
                if(z %in% c(1:3,6:8)){
                        round(bscal(S = S, K = K, T=1, r = r , d = d, v = v),2)
                }else{
                        round(bscal(S = S, K = K, T=1, r = r , d = d, v = v),6)
                }
        })
##############################
        tempdf <- do.call(rbind, tcp[[Y]])
        yearprice[[Y]] <- c(tempdf[,1],tempdf[,2])
}

optiondf <- do.call(cbind, yearprice)
rownames(optiondf) <- instrument_v[13:492]

#########=- OUTPUT -=################
finaldf <- rbind(underdf, bonddf, optiondf)
finaldf <- cbind(rownames(finaldf), finaldf)
colnames(finaldf) <- c("TICKER", 0:10)
write.csv(finaldf, file = "~/003HedgeMaster/PriceMatrix_4.csv", row.names = F)
