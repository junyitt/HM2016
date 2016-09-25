#A1_01_Core.R

#DIR-f
A1f.dir <- "C:/Users/User/Google Drive/z_ALLHM"

#source functions
setwd(A1f.dir); source("A1_01_Reportf.R")

#generate team names
a <- paste0("Alpha ", 1:6); b <- paste0("Beta ", 1:6); teamname12 <- c(a,b)

#A1 Report - 01 - Core  
{

#output c_list
cReport_list <- lapply(teamname12, FUN = function(x){ #return list of core report, where list[[1]] is the core report df for team Alpha 1 and so on
      core.f(x, fdf8_td)
}) 
      
} #cReport_list
      
#A1 Report - 02 - Scenario transactions
{

#output s_list      
sReport_list <- lapply(teamname12, FUN = function(x){
                              scenario.f(x, fdf8_td, yy)
                        })
} #sReport_list

#A1 Report - 03 - Extra transactions
{
   
eReport_list <- lapply(teamname12, FUN = function(x){
      extraR.f(x, fdf8_td, yy)
}) #output s_list  
      
} #eReport_list

#A1 Report - 04 - Other Money Transactions? Hints/Tips
{
# 
# #need to select columns that will be DISPLAYED
# dispvarE <- c("TrackNo", "FIC", "cParty", "cType", "Underlying", "Currency", "kPrice", 
#               "pos1", "Units", "tDate", "mDate", "NetPro", "ProT", "MVT", "VL")
# 
# #function - return relevant scenario transactions, with subsetted variables  
# extraR.f <- function(tName, df, yy){
#       u <- df[,"TeamName"] == tName
#       u1 <- df[, "classf"] == "Extra"
#       yy2 <- yy+1; u2 <- df[, "tDate"] < yy2 & yy2 <= df[,"mDate"]
#       
#       #outdf: 
#       df[u & u1 & u2, dispvarE]
# }
# 
# #output s_list      
# eReport_list <- lapply(teamname12, FUN = function(x){
#       extraR.f(x, fdf8_td, yy)
#       
# })
}

#A1 Report - 05 - ALL Your transactions
{

#output s_list      
uReport_list <- lapply(teamname12, FUN = function(x){
      uR.df <- userR.f(x, fdf8_td)
      
})
} #uR.df

#A1 Report - 06 - Balance Sheet #clean code later
{
      #need bgn6 function - cff_f: DIR
      bgn6f.dir <- "C:/Users/User/Google Drive/z_ALLHM/"
      setwd(bgn6f.dir); source("60_01_BGN1Functions.R")
      
      #DIR-balance sheet
      balsh0.dir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta"
      balshy.dir <- "C:/Users/User/OneDrive/yy_YearlyBalanceSheet"
      
      #read balance sheet
      {
            if(yy == 0){
                  setwd(balsh0.dir)
                  balsh_y <- as.data.frame(read_excel("meta-balancesheet-0.xlsx"))
            }else{
                  setwd(balshy.dir)
                  balsh_y <- read.csv(paste0("meta-balancesheet-", yy, ".csv"))
            }
            colnames(balsh_y) <- c("TeamName", "PPE", "FinAsset", "Cash", "ShareCap", "RE", "Loan", "NAV")
      } #balsh_y
      
      #ALL cType 
            allctype <- c("Employ Service", "Cash", "Revenue Increment", "Cost Reduction", "Production",
                          "AFUT", "Forward", "Forward-S", "Bond", "Bond-E", "Loan",
                          "Put Option", "Put Option-S", "Put Option-E", 
                          "Call Option", "Call Option-S", "Call Option-E")
            # zeromv <- c("Revenue Increment", "AFUT", "Cost Reduction", "Forward", "Forward-S","Production", "Cash", "Employ Service")
            # tdatemv <- c("Call Option", "Put Option", "Call Option-S", "Put Option-S", "Call Option-E", "Put Option-E")
            mv_ctype <- c("Bond", "Bond-E", "Loan")
      
            m.acc <- c("FinAsset", "Loan")
                  
      
      N <- nrow(fdf8_td); cff <- sapply(1:N, FUN = function(j){cff_f(fdf8_td[j, "cType"], fdf8_td[j, "pos1"])})
      
      accType <- sapply(1:N, FUN = function(j){
            accTypef(fdf8_td[j,"cType"], cff[j])    
      })
      acclist <- lapply(teamname12, FUN = function(j){
                  u <- fdf8_td[, "TeamName"] == j
                  sapply(m.acc, FUN = function(k){
                        u2 <- accType == k
                        abs(sum(fdf8_td[u & u2, "MVT"], na.rm =T))
                  })
            
            
            })
      acc1 <- do.call(rbind,acclist)
      #find sum pro, proT for each team
      {
            #must use fdf_y7, NOT fdf_y6
            pro0 <- sapply(teamname12, FUN = function(name){
                  u <- fdf_y7[, "TeamName"] == name
                  npro1 <- sum(fdf_y7[u, "NetPro"], na.rm = T)
                  npro1
            })
            
            #must use fdf8_td
            proT <- sapply(teamname12, FUN = function(name){
                  u <- fdf8_td[, "TeamName"] == name
                  npro2 <- sum(fdf8_td[u, "ProT"], na.rm = T)
                  npro2
            })
      } #pro0, proT
     
      balsh_new_yy2 <- old2newbs.f(balsh_y, acc1, pro0, proT) #return balance sheet for year yy2, each row representing each team
      
}  #balsh_new_yy2

#A1 Report - 07 - Hedging Evaluation

#DIR-80_01_END1Functions.R 
e80f.dir <- "C:/Users/User/Google Drive/z_ALLHM"
setwd(e80f.dir); source("80_01_END1Functions.R") #cff_f and ProTf, STf, exSTf
#STf <- function(Underlying, td)
#exSTf <- function(Currency, td)
#ProTf <- function(FIC, cType, cff, Units, Pro, kPrice, tDate, mDate, ST, exST, yy)
#cff_f <- function(cType, pos1)

      
{

{
      #teamnamelist > commlist > teamd      

      hlist <- lapply(teamname12, FUN = function(k){
                 lapply(commClass, FUN = function(j){
                        u1 <- fdf8_td[,"Underlying"] %in% j; u2 <- fdf8_td[,"Currency"] %in% j; u <- u1 | u2
                        df <- fdf8_td[u,]
                        #subset extra and scenario tran with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate,  teamname
                        hdf <- subdfR.f(df, v1, v2, v3hedge, tName = k, yy)

            })
      })
      
      ulist <- lapply(teamname12, FUN = function(k){
            lapply(commClass, FUN = function(j){
                  u1 <- fdf8_td[,"Underlying"] %in% j; u2 <- fdf8_td[,"Currency"] %in% j; u <- u1 | u2
                  df <- fdf8_td[u,]
                  #subset OTC, EXC with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate, teamname
                  udf <- subdfR.f(df = fdf8_td, v1, v2, v3user, tName = k, yy)
                  
            })
      })
      
}   #optional: show what transactions are involved
      
      lapply(teamname12, FUN = function(j){
            {
                  v1 <-  c("GOL", "CRU", "PAL"); v2 <- c("USD", "EUR"); v3user <- c("OTC", "EXC"); v3hedge <- c("Scenario", "Extra")
                  
                  commClass <- list("CRU", "PAL", "EUR", c("GOL", "USD"))
                  
                  #subset extra and scenario tran with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate,  teamname
                  hdf <- subdfR.f(df = fdf8_td, v1, v2, v3hedge, tName = j, yy)
                  
                  #subset OTC, EXC with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate, teamname
                  udf <- subdfR.f(df = fdf8_td, v1, v2, v3user, tName = j, yy)
            } #subset all commClass, by team: hdf, udf
            
            #return vector of scores BY 4class of commodities
            sapply(commClass, FUN = function(h){
                  
                  {
                        u1 <- hdf[,"Underlying"] %in% h; u2 <- hdf[,"Currency"] %in% h; u <- u1 | u2
                        u3 <- udf[,"Underlying"] %in% h; u4 <- udf[,"Currency"] %in% h; uu <- u3 | u4
                        gclass <- 1
                        
                        h2df <<- hdf[u,]; u2df <- udf[uu,]; t2df <- rbind(h2df, u2df)
                  }#subset: h2df, u2df, t2df #use h2df as base, to evaluate t2df 
                  {        
                        if("GOL" %in% h){
                              ss0 <- STf(Underlying = "GOL", yy); exss0 <- exSTf(Currency = "USD", yy)
                              gclass <- 1
                        }else if("EUR" %in% h){
                              exss0 <- exSTf(Currency = h[1], yy); ss0 <- 1
                              gclass <- 2
                        
                        }else{
                              ss0 <- STf(Underlying = h[1], yy); exss0 <- 1
                              gclass <- 3      
                        }
                  }#ss0, exss0 #gClass 1 for golusd, 2 for eur, 3 for others
                              
                  step <- -30:30; step <- step/100 + 1
                  {      
                        
                        
                        sumpro <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = T)
                        
                            ##refix:^^^^^^^
                  }
                  
            })
            
      })
      
      
      
      
}

lapply(cReport_list, function(j){
      j[1,]
})
#A1 Report - 08 - Extra Event Evaluation - Score
#A1 Report - 09 - Cash Evaluation

#A1 Report - 10 - Total Score Breakdown
