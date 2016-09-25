#DIR-f
BGN2f.dir <- "C:/Users/User/Google Drive/z_ALLHM/"

#source functions
setwd(BGN2f.dir); source("70_01_BGN2Functions.R")


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
}

#find netcash for each team
{
      team12v <- balsh_y[,"TeamName"]
      
      netpro12 <- sapply(team12v, FUN = function(name){
            u <- fdf_y6[, "TeamName"] == name
            npro1 <- sum(fdf_y6[u, "NetPro"], na.rm = T)
      })
      
      netcash12 <- netpro12 + balsh_y[,"Cash"] 
}

#short term loan full transactions  
{
      stloanfulltran <- lapply(X = 1:length(team12v), FUN = function(j){
            addloan_f(netcash12[j], team12v[j], yy)
      })
      
      stloanfdf <- do.call(rbind, stloanfulltran)
} #stloanfdf

#preoutput: fdf_y7 (no bgn)
fdf_y7pre <- rbind(fdf_y, stloanfdf)

#output: fdf_y7 (with bgn)
      fdf_y7 <- bgn1df_f(fdf_y7pre) 

