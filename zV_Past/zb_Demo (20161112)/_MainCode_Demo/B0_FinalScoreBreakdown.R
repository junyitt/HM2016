#B0_FinalScoreBreakdown.R

#DIR 
#meta.dir, fulltran.dir

#INPUT: score.hedge, score.extra, score.cash, score.NAV
#OUTPUT: scorebr.td.df

###############################################################

scorebr.td.df_pre <- importscorebr.f(meta.dir, fulltran.dir, yy)

      scorebr.td.df_new <- scorebr.td.df_pre[1:12,]       #teams; not reproducible
      #rbind with the previous scorebr.td.df
      scorebr.td.df_new[,"Hedging"] <- score.hedge
      scorebr.td.df_new[,"CashFlow"] <- score.cash
      scorebr.td.df_new[,"ExtraEvent"] <- score.extra
      scorebr.td.df_new[,"NAV"] <- score.NAV
      scorebr.td.df_new[,"Year"] <- yy+1

scorebr.td.df <- rbind(scorebr.td.df_pre, scorebr.td.df_new)

#output: scorebr.td.df