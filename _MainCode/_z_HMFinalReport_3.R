#_HMFinalReport_3.R

#Generate final report for judges

scorebr.td.df <- importscorebr.f(meta.dir, fulltran.dir, yy = 5)

teamname12 <- teamname.f()

#temp final score csv
finalscore.list <- lapply(teamname12, FUN = function(TeamName.c){
                        u <- scorebr.td.df[,"TeamName"] %in% TeamName.c; u2 <- scorebr.td.df[,"Year"] %in% 0:5
                        df <- scorebr.td.df[u & u2,]
                        u3 <- df[,"Year"] == 0 
                              df[u3,"Hedging"] <- mean(df[!u3, "Hedging"], na.rm = T)
                              df[u3,"CashFlow"] <- mean(df[!u3, "CashFlow"], na.rm = T)
                              df[u3,"ExtraEvent"] <- mean(df[!u3, "ExtraEvent"], na.rm = T)
                              df[u3,"NAV"] <- mean(df[!u3, "NAV"], na.rm = T)
                              df[u3,"Year"] <- sum(df[u3,2:4])
                        setwd(finalreport.dir); write.csv(df, paste0(TeamName.c, "_Score.csv"), row.names = F) 
                  })


