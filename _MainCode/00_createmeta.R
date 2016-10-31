#00_createmeta.R

MAINCODEDIR <- "C:/Users/User/Google Drive/z_ALLHM/_MainCode"
setwd(MAINCODEDIR)
source("_HM_alldirectory.R"); source("_HM_allfunction.R")
source("00_createmeta_f.R")

##01## meta-balancesheet-0.xlsx
writexlsx.f(createbs01.df.f(), "meta-balancesheet-0.xlsx")

      ##02## meta-bond-e-B.xlsx
            #need set

##03## meta-core-fulltran-Alpha.xlsx
writexlsx.f(create.core34.df.f(team = "Alpha", kG = 3500, kC = 225, kP = 1700, uG = 1500, uC = 20000, uP = 3000), "meta-core-fulltran-Alpha.xlsx")
      
##04## meta-core-fulltran-Beta.xlsx
writexlsx.f(create.core34.df.f(team = "Beta", kG = 4500, kC = 260, kP = 1900, uG = 1500, uC = 20000, uP = 3000), "meta-core-fulltran-Beta.xlsx")
     
##05## meta-employservice.xlsx
writexlsx.f(create.employ5.df.f(), "meta-employservice.xlsx")
                  
##06## meta-extra0-score.xlsx
writexlsx.f(create.extrascore06.df.f(), "meta-extra0-score.xlsx")

##07## meta-extra-4-harrym.xlsx
setwd(meta.dir)
write.xlsx(create.extraharry7.listdf2.f()[[1]], file="meta-extra-4-harrym.xlsx", sheetName="R", row.names = F)
write.xlsx(create.extraharry7.listdf2.f()[[2]], file="meta-extra-4-harrym.xlsx", sheetName="COV", append=TRUE, row.names = F)
      
##08## meta-extra-fulltran.xlsx
setwd(meta.dir)
write.xlsx(create.extra8A.df.f("Alpha"), file="meta-extra-fulltran.xlsx", sheetName="Sheet1", row.names = F)
write.xlsx(create.extra8A.df.f("Beta"), file="meta-extra-fulltran.xlsx", sheetName="Sheet2", append=TRUE, row.names = F)
      
      ##09## meta-FIC.xlsx
      #need set
      
      ##10## meta-scenario-fulltran.xlsx
      #need set

       
##11## meta-scorebreakdown-td-0.xlsx
writexlsx.f(createscorebr11.df.f(), "meta-scorebreakdown-td-0.xlsx")

##12## meta-tradingkey.xlsx
writexlsx.f(createtkey12.df.f(200), "meta-tradingkey.xlsx")

##13## meta-underlyingprice.xlsx
writexlsx.f(createund13.df.f(201), "meta-underlyingprice.xlsx")


