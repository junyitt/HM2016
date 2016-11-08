#functions...      

#find spot price
findst_f <- function(underfind_1, yr = Y, under = under_df){
    if(underfind_1 == 0){
        0 
    }else{
        under[Y+1, underfind_1]   
    }
}

findcrst_f <- function(crfind_1, yr = Y, under = under_df){
    if(crfind_1 == "MYR"){
        1
    }else{
        under[Y+1, crfind_1]   
    }
}


#find RCF

findfwdrcf <- function(pos, units, fk, pdate, mdate, st, crst, yr = Y){
    if(mdate == Y){
        (st - fk)*pos*units*crst
    }else{
        0
    }
}
findclrcf <- function(pos, units, fk, pdate, mdate, st, crst, yr = Y){
    if(mdate == Y){
        max((st - fk),0)*pos*units*crst
    }else{
        0
    }
}

findptrcf <- function(pos, units, fk, pdate, mdate, st, crst, yr = Y){
    if(mdate == Y){
        max((fk - st),0)*pos*units*crst
    }else{
        0
    }
}

findbondrcf <- function(pos, units, fk, pdate, mdate, st, crst, yr = Y){
    if(mdate == Y){
        105*pos*units*crst
    }else if(mdate > Y & Y > pdate){
        5*pos*units*crst
    }else{
        0
    }
}
    
    
    findrcf <- function(ccode, pos, units, fk, pdate, mdate, st, crst, yr = Y){
        if (substr(ccode,1,2) == "CL"){
            findclrcf(pos, units, fk, pdate, mdate, st, crst, yr)
            
        }else if(substr(ccode,1,2) == "PT"){
            findptrcf(pos, units, fk, pdate, mdate, st, crst, yr)
            
        }else if(substr(ccode,1,2) == "BD"){
            findbondrcf(pos, units, fk, pdate, mdate, st, crst, yr)
            
        }else if(substr(ccode,1,2) == "FW"){
            findfwdrcf(pos, units, fk, pdate, mdate, st, crst, yr)
            
        }else{
            NA
            
        }
    }
    
    findbondmv <- function(ccode, pos, units, price, pdate, mdate, yr = Y, crst){
        if(substr(ccode,1,2) == "BD"){
            if(mdate == yr){
                0
            }else if(pdate == yr){
                price*pos*units*crst
            }else if(pdate < yr & yr < mdate){
                r <- ratef(nper = mdate - pdate, pmt = -5, pv = price, fv = -100)   
                pvf(rate = r, nper = mdate - yr, pmt = -5, fv = -100)*pos*units*crst
            }else{
                0
            }
        }else{
            0
        }
    }
    