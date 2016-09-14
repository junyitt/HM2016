' Equivalent to the worksheet function RATE(nper,pmt,pv,fv,type,guess)'

pmtf <- function(){
    
    }
    

ratef <- function(nper, pmt, pv, fv, pend = 0, guess = 0.03){
    R <- 1 + guess
    a <- (pmt* (1- pend) - pv)/ (pv+ pmt*pend)
    b <- (fv - pmt * pend) / (pv + pmt * pend)
    c = (-pmt * (1 - pend) - fv) / (pv + pmt * pend)
    
    for(i in 1:20){
        RTmp <- R - (R ^ (nper + 1) + a * R ^ nper + b * R + c) / ((nper + 1) * R ^ nper + a * nper * R ^ (nper - 1) + b)
        if(abs(RTmp - R) < 0.0000001){ 
            p <- i
            break
        }
        R <- RTmp
    }
    
    if(p <= 20){ 
        RTmp - 1
    }else{
        NA
    }
}

pvf <- function(rate, nper, pmt, fv){
    -1*pmt*(1-(1+rate)^(-1*nper))/rate + -1*fv*(1+rate)^(-1*nper)
}

