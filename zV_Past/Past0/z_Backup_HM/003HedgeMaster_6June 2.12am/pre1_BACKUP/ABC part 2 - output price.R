###FUNCTION

timeline <- seq(0,10,1/200)
simprice <- function(mu, sigma, S0 = 1, seed = runif(1,0,100), T = 10, N = 200){
        set.seed(seed)
        n <- T*N
        h <- T/n
                A <- f <- rep(0,n+1)
                A[1] <- S0
                        
                for(j in 1:T){
                        for(i in 2:201){
                                x <- 200*(j-1)+i
                                s <- 200*(j-1)+1
                                f[i] <- f[i-1]+sqrt(h)*rnorm(1)
                                A[x] <- A[s]*exp((mu[j]-(sigma[j]^2)/2)*(i-1)*h + sigma[j]*f[i])
                }
                }
                        #out
                        A
}

###INPUT

mu <- read.table("~/003HedgeMaster/mu.txt", header = T)
sigma <- read.table("~/003HedgeMaster/sigma.txt", header = T)

A <- simprice(mu = mu$A, sigma = sigma$A, S0 = 50, seed = 10)
B <- simprice(mu = mu$B, sigma = sigma$B, S0 = 50, seed = 3)
C <- simprice(mu = mu$C, sigma = sigma$C, S0 = 50, seed = 7)
USDMYR <- simprice(mu = mu$USDMYR, sigma = sigma$USDMYR, S0 = 3.3000, seed = 5)
EURMYR <- simprice(mu = mu$EURMYR, sigma = sigma$EURMYR, S0 = 4.0000, seed = 6)
GOLD <- simprice(mu = mu$GOLD, sigma = sigma$GOLD, S0 = 1000, seed = 8)
CRUDE <- simprice(mu = mu$CRUDE, sigma = sigma$CRUDE, S0 = 80, seed = 91)
PALM <- simprice(mu = mu$PALM, sigma = sigma$PALM, S0 = 2000, seed = 11)

s <- seq(1, 2001, 200)
# outdf <- cbind(A, B, C, USDMYR, EURMYR, GOLD, CRUDE, PALM)
outdf <- cbind(A[s], B[s], C[s], USDMYR[s], EURMYR[s], GOLD[s], CRUDE[s], PALM[s])
colnames(outdf) <- colnames(mu)
write.csv(outdf, file = "~/003HedgeMaster/price_1a.csv", row.names = F)
# o_range<- range(A,B,C)
# o_range <- range(A,B,C, USDMYR, EURMYR, GOLD, CRUDE, PALM)
#         plot(timeline, GOLD, ylim=o_range, type="l", col=1)
#         plot(timeline, A, ylim=o_range, type="l", col=1)
#         lines(timeline, B, col = 2)
#         lines(timeline, C, col = 3)
#         
#         lines(timeline, USDMYR, col = 4)
#         lines(timeline, EURMYR, col = 5)
#         lines(timeline, GOLD, col = 6)
#         lines(timeline, CRUDE, col = 7)
#         lines(timeline, PALM, col = 8)
#         
#         legend(0, o_range[2], names, col = seq(from = 1,8), cex = 0.8, pch = 1, lty = 1)
#         rm(list=names)
