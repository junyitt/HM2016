

library(dplyr)
###INPUT
source("~/003HedgeMaster/0f_pricing.R")
source("~/003HedgeMaster/0meta_pricing.R")

#########################
##Price of underlying - 8
#########################

#########

# assetn <- names(mu)
seed <- c(5,6,8,91,11)
S0 <- c(3.3,4,1000,240,2000)

assetlist <- list()
# for(i in 1:length(seedn)){
#         assetlist[[i]] <- simprice(mu = mu[,assetn[i]], sigma = sigma[,assetn[i]], S0 = S0n[i], seed = seedn[i])
# }
# 


assetlist <- lapply(X = 1:5, function(q){
        simprice(mu = mu[,q], sigma = sigma[,q], S0 = S0[q], seed = seed[q])
})
        
subpricelist <- lapply(X = assetlist, FUN = subprice)        
        underdf <- do.call(cbind, subpricelist)
        underdf[,c(3:5)] <- round(underdf[,c(3:5)],2)
        underdf[,1:2] <- round(underdf[,1:2],4)
        
        colnames(underdf) <- c("USD", "EUR", "GOL", "CRU", "PAL")
        underdf <- t(underdf)
        colnames(underdf) <- 0:10
            write.csv(underdf, "~/003HedgeMaster/UNDERLYINGPRICE00.csv")
##################
##BOND PRICE - 4
##################
        #read all the bond code - META procedure
        BondCode_v <- as.character(read.table("~/003HedgeMaster/meta3_AllBondCode.txt", header = F)[,1])
        #substr out the necessary information
        startdate_v <- as.numeric(sapply(X = BondCode_v, FUN = function(ss){
            substr(x = ss, start = 3, stop = 4)
        }))
        enddate_v <- as.numeric(sapply(X = BondCode_v, FUN = function(ss){
            substr(x = ss, start = 5, stop = 6)
        }))
        #create vector for interest
            loopindex <- 1:length(startdate_v)
            underlying_v <- rep("MYR", length(startdate_v))
            int_v <- sapply(X = loopindex, FUN = function(y){
                intfind_f_1(start_1 = startdate_v[y],end_1 = enddate_v[y], under_1 = underlying_v[y])
            })
            int_v <- exp(int_v)-1
            
        #finally create vector for bond price
            bondprice_v <- sapply(X = loopindex, FUN = function(y){
                bondcal_f_1(i = int_v[y], n = enddate_v[y]-startdate_v[y], Fr = 5, C = 100)
            })
        #round off 
            bondprice_v <- round(bondprice_v,2)
        #cbind
            bondprice_final_df <- cbind(code = BondCode_v, price = bondprice_v)
        #output
            #write.csv(bondprice_final_df, "~/003HedgeMaster/BONDPRICE_02.csv", row.names = F)
            
        
########################################
##FORWARD THEORETICAL PRICE - TOO MANY! 8 underlying* each year has n-k prices (k is the current period) - about 8*50 = 400
########################################
        #read all the forward code - META procedure
        FwdCode_v <- read.table("~/003HedgeMaster/meta3_AllForwardCode.txt", header = F)
  
            #substr out the necessary information
            startdate_v <- as.numeric(sapply(X = FwdCode_v, FUN = function(ss){
                substr(x = ss, start = 3, stop = 4)
            }))
            enddate_v <- as.numeric(sapply(X = FwdCode_v, FUN = function(ss){
                substr(x = ss, start = 5, stop = 6)
            }))
            underlying_v <- sapply(X = FwdCode_v, FUN = function(ss){
                substr(x = ss, start = 7, stop = 9)
            })
       
        loopindex <- 1:length(underlying_v)
        #create the relevant vector for dividend and interest
        div_v <- sapply(X = loopindex, FUN = function(y){
            divfind_f_1(start_1 = startdate_v[y],end_1 = enddate_v[y], under_1 = underlying_v[y])
        })
        int_v <- sapply(X = loopindex, FUN = function(y){
            intfind_f_1(start_1 = startdate_v[y],end_1 = enddate_v[y], under_1 = underlying_v[y])
        })
        s0_v <- sapply(X = loopindex, FUN = function(y){
            s0find_f_1(start_1 = startdate_v[y], under_1 = underlying_v[y])
        })
        #finally, create the relevant vector for forward price
        fwdprice_v <- sapply(X = loopindex, FUN = function(y){
            fwdprice_f_1(s0_1 = s0_v[y], int_1 = int_v[y], div_1 = div_v[y], startdate_v[y], enddate_v[y])
        })
        #ROUND UP
            fwdprice_v[underlying_v %in% c("EUR", "USD")] <- round(fwdprice_v[underlying_v %in% c("EUR", "USD")],4)
            fwdprice_v[underlying_v %in% c("GOL","CRU","PAL")] <- round(fwdprice_v[underlying_v %in% c("GOL","CRU","PAL")],2)
        #cbind    
        fwdprice_final_df <- cbind(code = FwdCode_v, price = fwdprice_v)
        colnames(fwdprice_final_df) <- c("code", "price")
        fwdprice_final_df <- as.matrix(fwdprice_final_df)
        #output
        #write.csv(fwdprice_final_df, file = "~/003HedgeMaster/FWDPRICE_02.csv", row.names = F)
        
    ##############################        
    ##NEW OPTIONS PRICING CODE- 8*30 + 8*30 = 480
    ##############################      
        #read all the option code - META procedure
        OptionCode_v <- as.character(read.table("~/003HedgeMaster/meta3_CallOptionCode.txt", header = F)[,1])
        PutOptionCode_v <- as.character(read.table("~/003HedgeMaster/meta3_PutOptionCode.txt", header = F)[,1])
        #substr out the necessary information
        startdate_v <- as.numeric(sapply(X = OptionCode_v, FUN = function(ss){
            substr(x = ss, start = 3, stop = 4)
        }))
        enddate_v <- as.numeric(sapply(X = OptionCode_v, FUN = function(ss){
            substr(x = ss, start = 5, stop = 6)
        }))
        underlying_v <- sapply(X = OptionCode_v, FUN = function(ss){
            substr(x = ss, start = 7, stop = 9)
        })
        strike_v <- as.numeric(sapply(X = OptionCode_v, FUN = function(ss){
            substr(x = ss, start = 10, stop = nchar(ss))
        }))
        loopindex <- 1:length(underlying_v)
        
        #create the relevant vector for dividend and interest and s0 and >>>"VOLATILITY"
        div_v <- sapply(X = loopindex, FUN = function(y){
            divfind_f_1(start_1 = startdate_v[y],end_1 = enddate_v[y], under_1 = underlying_v[y])
        })
        int_v <- sapply(X = loopindex, FUN = function(y){
            intfind_f_1(start_1 = startdate_v[y],end_1 = enddate_v[y], under_1 = underlying_v[y])
        })
        s0_v <- sapply(X = loopindex, FUN = function(y){
            s0find_f_1(start_1 = startdate_v[y], under_1 = underlying_v[y])
        })
            #**volatility
            volalist <- lapply(X = assetlist, FUN = vola2)
                #create the relevant vector for volatility
                vola_v <- sapply(X = loopindex, FUN = function(y){
                    volafind_f_1(start_1 = startdate_v[y], under_1 = underlying_v[y])
                })
                
        #finally, create a vector for the call premium and put premium
        callprem_v <- sapply(X = loopindex, FUN = function(y){
            bscall_f_1(S = s0_v[y], K = strike_v[y], T = 1, r = int_v[y], d = div_v[y], v = vola_v[y])
        })
        putprem_v <- sapply(X = loopindex, FUN = function(y){
            bsput_f_1(S = s0_v[y], K = strike_v[y], T = 1, r = int_v[y], d = div_v[y], v = vola_v[y])
        })
        
        #ROUND UP
        callprem_v[underlying_v %in% c("EUR", "USD")] <- round(callprem_v[underlying_v %in% c("EUR", "USD")],4)
        callprem_v[underlying_v %in% c("GOL","CRU","PAL")] <- round(callprem_v[underlying_v %in% c("GOL","CRU","PAL")],2)
        #ROUND UP
        putprem_v[underlying_v %in% c("EUR", "USD")] <- round(putprem_v[underlying_v %in% c("EUR", "USD")],4)
        putprem_v[underlying_v %in% c("GOL","CRU","PAL")] <- round(putprem_v[underlying_v %in% c("GOL","CRU","PAL")],2)
        #cbind    
        optionprice_final_df <- cbind(code = c(OptionCode_v, PutOptionCode_v), price = c(callprem_v, putprem_v))
        #output
        #write.csv(optionprice_final_df, file = "~/003HedgeMaster/OPTIONPRICE_02.csv", row.names = F)
        
        allprice_final_df <- rbind(bondprice_final_df,fwdprice_final_df, optionprice_final_df)
        #output
        write.csv(allprice_final_df, "~/003HedgeMaster/ALLPRICE_01.csv", row.names = F)
