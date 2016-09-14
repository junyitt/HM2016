
source(file = "~/003HedgeMaster/0f_pricing.R")
# VOLATILITY 1Master/0f_pricing.R")
source(file = "~/003HedgeMaster/0meta_pricing.R")

#assetlist - prices 2001x8
#volalist - volatility 11x8
#subpricelist - prices 11x8
#strike - df 31x8
#interest - df 11x7 - FED, ECB, BNM, MYR1, MYR3, MYR5, MYR10

###PRICING OPTIONS 
#LIST OF INPUT
# Annual risk free rate (MYR)
# Price 1
# Strike Price meta
# exchange rate: rfr for USD and EUR 



assetindex <- seq(1,8,1)
tcp <- list()
cplist <- list()
# Y = Yth year Y = 1 to 11
for(Y in 1:11){
        # Y <- 1
        tcp[[Y]] <- lapply(assetindex, FUN = function(z){
                S <- subpricelist[[z]][Y]
                        K <- strike[[z]]
#                 r <- interest[[z]][Y]
#                 d <- dividend[[z]][Y]
                        r <- interest[[z]][Y]
                        d <- dividend[[z]][Y]
                v <- volalist[[z]][Y]
                T <- 1
                        bscal(S = S, K = K, T=1, r = r , d = d, v = v)
        })
        
        cplist[[Y]] <- do.call(cbind, tcp[[Y]])
        colnames(cplist[[Y]]) <- c("A-Call", "A-Put", "B-Call", "B-Put","C-Call", "C-Put",
                                  "USDMYR-Call", "USDMYR-Put",
                                  "EURMYR-Call", "EURMYR-Put",
                                  "GOLD-Call", "GOLD-Put",
                                  "CRUDE-Call", "CRUDE-Put",
                                  "PALM-Call", "PALM-Put")
        cplist[[Y]] <- cbind(cplist[[Y]], strikemod)
        cplist[[Y]] <- cplist[[Y]][,c(17,1:2,18,3:4,19,5:6,20,7:8,21,9:10,22,11:12,23,13:14,24,15:16)]
        cplist[[Y]][,c(2:3,5:6,8:9,17:18,20:21,23:24)] <- round(cplist[[Y]][,c(2:3,5:6,8:9,17:18,20:21,23:24)],2)
        cplist[[Y]][,c(11:12,14:15)] <- round(cplist[[Y]][,c(11:12,14:15)],6)
}


for(i in 1:11){
        write.csv(cplist[[i]], file = paste0("~/003HedgeMaster/outputcp/option_T", i,".csv"), row.names = F)
}