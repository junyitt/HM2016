#90_01_BalshFunctions.R

#A1 Report - 06 - Balance Sheet
acctype.v.f <- function(cType.c, cff.c){
      mv_ctype <- c("Bond", "Bond-E", "Loan")
      
      if(cType.c %in% mv_ctype){
            if(cff.c == 1){
                  "FinAsset"
            }else if(cff.c == -1){
                  "Loan"
            }
      }else{
            "N.A"
      }
} #return account type

old2newbs.f <- function(balsh_y, acc1, pro0, proT){
      balsh_y[,"Cash"] <- balsh_y[,"Cash"] + pro0 + proT
      balsh_y[,"FinAsset"] <- acc1[,"FinAsset"]
      balsh_y[,"Loan"] <- acc1[,"Loan"]
      balsh_y[,"RE"] <- balsh_y[,"PPE"] + balsh_y[,"FinAsset"] + balsh_y[,"Cash"] - balsh_y[,"ShareCap"] - balsh_y[,"Loan"]
      balsh_y[,"NAV"] <- balsh_y[,"RE"]+balsh_y[,"ShareCap"]
      balsh_y
} #return new balance sheet

newbs.df.f <- function(balsh_y.df, end3.td.df , teamname12){
      #ALL cType 
      allctype <- c("Employ Service", "Cash", "Revenue Increment", "Cost Reduction", "Production",
                    "AFUT", "Forward", "Forward-S", "Bond", "Bond-E", "Loan",
                    "Put Option", "Put Option-S", "Put Option-E", 
                    "Call Option", "Call Option-S", "Call Option-E")
      # zeromv <- c("Revenue Increment", "AFUT", "Cost Reduction", "Forward", "Forward-S","Production", "Cash", "Employ Service")
      # tdatemv <- c("Call Option", "Put Option", "Call Option-S", "Put Option-S", "Call Option-E", "Put Option-E")
      
      
      m.acc <- c("FinAsset", "Loan")
      
      N <- nrow(end3.td.df)
      
      acctype.v <- sapply(1:N, FUN = function(j){
            acctype.v.f(end3.td.df[j,"cType"], end3.td.df[j,"cff"])    
      })
      acc.list <- lapply(teamname12, FUN = function(j){
            u <- end3.td.df[, "TeamName"] %in% j
            sapply(m.acc, FUN = function(k){
                  u2 <- acctype.v %in% k
                  abs(sum(end3.td.df[u & u2, "mvT"], na.rm =T))
            })
      })
      acc1 <- do.call(rbind,acc.list)
      #find sum pro, proT for each team
      {
            #must use fdf_y7, NOT fdf_y6
            pro0 <- sapply(teamname12, FUN = function(name){
                  u <- end3.td.df[, "TeamName"] %in% name
                  npro1 <- sum(end3.td.df[u, "netpro0"], na.rm = T)
                  npro1
            })
            
            #must use fdf8_td
            proT <- sapply(teamname12, FUN = function(name){
                  u <- end3.td.df[, "TeamName"] %in% name
                  npro2 <- sum(end3.td.df[u, "proT"], na.rm = T)
                  npro2
            })
      } #pro0, proT
      
      balsh_new_yy2 <- old2newbs.f(balsh_y.df, acc1, pro0, proT) #return balance sheet for year yy2, each row representing each team
      balsh_new_yy2
      
}
