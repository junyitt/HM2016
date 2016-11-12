

bsf <- function(s0, K, r, d, s, t){
      if(s0 < 10){dec <- 6}else{dec <- 2}
      d1 <- (log(s0/K) + (r-d+0.5*s^2)*t)/(s*sqrt(t))
      d2 <- d1 - s*sqrt(t)
      C <- s0*exp(-1*d*t)*pnorm(d1) - K*exp(-1*r*t)*pnorm(d2)
      P <- -1*s0*exp(-1*d*t)*pnorm(-1*d1) + K*exp(-1*r*t)*pnorm(-1*d2)
      round(c(call = C,put = P), dec)
      
}

fwf <- function(s0, r, d, t){
      if(s0 < 10){dec <- 2}else{dec <- 0}
      fwd <- s0*exp((r-d)*t)
      round(fwd, dec)
}
# 
# s0 <- 4.088
# K <- 4
# r <- 0.0325
# d <- 0
# s <- 0.1
# t <- 1
# 
# x <- bsf(s0,K,r,d,s,t);x
# y <- fwf(s0,r,d,t); y
# 
# x[1]-x[2]; s0*exp(-1*d*t) - K*exp(-1*r*t)
