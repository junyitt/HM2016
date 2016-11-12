#_HMProcessFlow_Report_2.R

#########################
#A7_HedgingEvaluation.R
#########################
      setwd(maincode.dir); source("A7_HedgingEvaluation.R")
#OUTPUT: team.asset.payoffdf.list

#########################
#A8_ScoreHedging
#########################
      setwd(maincode.dir); source("A8_ScoreHedging.R")  
            setwd(fulltran.dir); write.csv(hedgescore.df, paste0("a_hedgescore_", yy+1, ".csv"), row.names = F)
###OUTPUT: fullsim.df, hedgescore.df, score.hedge             $$fullsim.df for graphing
## a_hedgescore_yy2.csv
      
#########################
#A9_ScoreExtra.R
#########################
      setwd(maincode.dir); source("A9_ScoreExtra.R") 
###OUTPUT: score.extra
      
#########################
#A9B_ScoreCash_NAV.R
#########################
      setwd(maincode.dir); source("A9B_ScoreCash_NAV.R") 
###OUTPUT: score.cash, score.NAV
      
      
#########################
#B0_FinalScoreBreakdown.R
########################
      setwd(maincode.dir); source("B0_FinalScoreBreakdown.R") 
      setwd(fulltran.dir); write.csv(scorebr.td.df, paste0("aa-scorebreakdown-td-", yy+1, ".csv"), row.names = F)
#OUTPUT: scorebr.td.df
#"aa-scorebreakdown-td-yy2.csv"
      
      
      