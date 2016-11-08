###FUNCTION 1: simulate prices 
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

###Function 2: Select 10 year prices from 2001 prices

subprice <- function(price){
        j <- seq(from = 1, to = 2001, by = 200)
        price[j]
}

###FUNCTION 3: calculate historical volatility (mean of past year and forward one year)

vola2 <- function(p){
        u <- vector()
        for(j in 1:10){
                i <- (j-1)*(200)+1
                k <- (j)*200+1
                u[j] <- log(p[k]/p[i])
        }
        u[11] <- u[10]
        
        w <- vector()
        w[1] <- abs(u[1])
        for(i in 2:11){
                w[i] <- abs((u[i-1]+u[i])/2)
        }
        w
}


###FUNCTION 4: Black Scholes Pricing 
bscal <- function(S, K, T=1, r, d, v){
        C <- P <- vector()
        d1 <- (log(S/K) + (r-d+(v^2/2))*(T))/(v*sqrt(T))
        d2 <- d1 - v*sqrt(T)
        C <- S*exp(-1*d*T)*pnorm(d1) - K*exp(-1*r*T)*pnorm(d2)
        P <- -1*S*exp(-1*d*T)*pnorm(-1*d1) + K*exp(-1*r*T)*pnorm(-1*d2)
        #RHS <- S*exp(-1*d*T) - K*exp(-1*r*T)
        #cbind(C,P, C-P, RHS)
        cbind(C,P)
}

###FUNCTION 5: Bond pricing
bondcal <- function(i = ii, n, Fr = 5, C=100){
        (Fr)*(1-(1+i)^(-1*n))/i + C*(1+i)^(-1*n)
        
}
