#Problem price at time n+1 is dependent on time n, hence price should not be regenerated from scratch, and must be dependent
#test1 - 1000 points use unif
#test2 - 1000 points use normal
#take column of price_1 * K, where K = runif(n=1000, -0.5,0.5)
#test2 - K = 1 + rnorm(n=1000,0,0.5)

unif <- runif(n=1000, -0.5,0.5)+rnorm(n=1000,0,0.35)+1
norm <- rnorm(n=1000,0,0.5)+1
rand <- c(unif, norm)

df <- as.data.frame(read.csv("~/003HedgeMaster/PriceMatrix_4.csv", header = T))[1:8,]

df_list <- list()
for(i in 1:10){
        plist <- list()
        plist <- lapply(1:2000, FUN = function(x){
                df[,i+1]*rand[x]       
        })
        df_list[[i]] <- cbind(rep(i,8),as.data.frame(do.call(cbind, plist)))
}
fdf <- do.call(rbind, df_list)

write.csv(fdf,"~/003HedgeMaster/PriceStressTest_1a.csv", row.names = F)