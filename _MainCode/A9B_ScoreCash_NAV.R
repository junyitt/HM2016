#A9B_ScoreCash_NAV.R

#INPUT: end3.td.df, balsh_y2.df
#OUTPUT: score.cash, score.NAV

#INTERNAL FUNCTION
setwd(maincode.dir); source("A9B_ScoreCash_NAV_functions.R")

###########################################
#A1 Report - 09 - Cash Evaluation
###########################################
{
      score.cash <- sapply(teamname12, FUN = function(TeamName.c){
            score.cash.f(end3.td.df, yy, TeamName.c)  
      })
} #score.cash

###########################################
#A1 Report - 09b - NAV 
###########################################
{
      if(yy == 4){
            nav_v <- balsh_y2.df[,"NAV"]
            score.NAV1 <- round(rank(nav_v[1:6])/6*5,2) #not reproducible with different team numbers
            score.NAV2 <- round(rank(nav_v[7:12])/6*5,2)
            score.NAV <- c(score.NAV1, score.NAV2)
      }else{
            score.NAV <- rep(NA, 12)     
      }
} #score.NAV
