# set the directory for the csv file of OutputPRE2

#Y as input year
Y <- readline("number?")

###**VERY IMPORTANT, have to subset mdate less than Y transactions only, else willl include future MVs?

    # meaning if want compute year 2 results, must subset pdate < 2 transactions, cannot include pdate =2


#import csv
#Y as the new end of the year to compute (not pdate, but pdate+1)
Y <- 1


### 1 -- Clean Import ###

setwd("C:/Users/User/Google Drive/_OneDrive/_Current Work/csvOutputPRE2")
csvin <- read.csv("Pre2CSV_0.csv", stringsAsFactors = F)
    #subset required transactions only - pdate = 6
        csvin <- csvin[csvin[,6] < Y ,]
        
#source functions
source("C:/Users/User/Google Drive/_OneDrive/_Current Work/004HedgeMaster/function_stcsv.R")
source("C:/Users/User/Google Drive/_OneDrive/_Current Work/004HedgeMaster/rate_function.R")

#import and clean underlying prices
undert <- t(read.csv("C:/Users/User/Google Drive/_OneDrive/_Current Work/004HedgeMaster/meta_import/UNDERLYINGPRICE00.csv"))
under_df <- undert[2:12,]
    under_df <- do.call(cbind, lapply(X=1:5, FUN = function(x){as.numeric(under_df[,x])}))
    colnames(under_df) <- undert[1,]
    
### 2 -- Find St ###
    
#identify St
    #column 2 = underlying
    st_v <- sapply(csvin[,2], FUN = function(g){
        findst_f(g)
    })
    
#identify currency st
    #column 10 = currency denomination
    crst_v <- sapply(csvin[,10], FUN = function(g){
        findcrst_f(g)
    })
 
    
### 3 -- Compute ###    
#compute realized cash flow
    #ccode = 1
    #pos = 3, units = 4, fk = 5, pdate = 6 , mdate = 7, st = st_v, crst = csrt_v, yr = Y
    qq <- length(crst_v)
    rcf_v <- sapply(X = (1:qq), FUN = function(g){
        findrcf(ccode = csvin[g,1], pos = csvin[g,3], units = csvin[g,4], fk = csvin[g,5], pdate = csvin[g,6], 
                mdate = csvin[g,7], st = st_v[g], crst = crst_v[g], yr = Y)
    })

#compute market value (only for bonds), other instruments not marked to market
    #price = 15
    #ccode = 1
    #pos = 3, units = 4, fk = 5, pdate = 6 , mdate = 7, st = st_v, crst = csrt_v, yr = Y
    qq <- length(crst_v)
    mv_v <- sapply(X = (1:qq), FUN = function(g){
        findbondmv(ccode = csvin[g,1], pos = csvin[g,3], units = csvin[g,4], price = csvin[g,15], 
                   pdate = csvin[g,6], mdate = csvin[g,7], yr = Y, crst = crst_v[g])
    })
        
    