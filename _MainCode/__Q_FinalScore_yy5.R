
setwd(fulltran.dir)
scorebr.df <- read.csv("aa-scorebreakdown-td-5.csv", stringsAsFactors = F)

teamname.v <- teamname.f()

score1.list <<- lapply(teamname.v, FUN = function(teamname.c){
      u2 <- !(scorebr.df[, "Year"] %in% 0)
      u1 <- scorebr.df[, "TeamName"] %in% teamname.c 
      df1 <- scorebr.df[u1 & u2,c(1,6,2,3,4,5)]
      
      
      h0 <- round(mean(df1[,"Hedging"], na.rm = T),3)
      c0 <- round(mean(df1[,"CashFlow"], na.rm = T),3)
      e0 <- round(mean(df1[,"ExtraEvent"], na.rm = T),3)
      n0 <- round(mean(df1[,"NAV"], na.rm = T),3)
      df1[6, "TeamName"] <- "Average"
      df1[6, "Year"] <- NA
      df1[6, "Hedging"] <- h0
      df1[6, "CashFlow"] <- c0
      df1[6, "ExtraEvent"] <- e0
      df1[6, "NAV"] <- n0
      
            ss <- h0 + c0 + e0 + n0; ssp <- round(ss/50*60,3)
      msg0 <- paste0("Final Score is ", ss, " points.", "Convert to 60%, the Final Score is ", ssp, "%.")
      df1 <- data.frame(df1, row.names = NULL) 
      
      list(df1, msg0)
})

score2.list <- lapply(teamname.v, FUN = function(teamname.c){
      u2 <- !(scorebr.df[, "Year"] %in% 0)
      u1 <- scorebr.df[, "TeamName"] %in% teamname.c 
      df1 <- scorebr.df[u1 & u2,c(1,6,2,3,4,5)]
      
      
      h0 <- round(mean(df1[,"Hedging"], na.rm = T),3)
      c0 <- round(mean(df1[,"CashFlow"], na.rm = T),3)
      e0 <- round(mean(df1[,"ExtraEvent"], na.rm = T),3)
      n0 <- round(mean(df1[,"NAV"], na.rm = T),3)
      df1[6, "TeamName"] <- "Average"
      df1[6, "Year"] <- NA
      df1[6, "Hedging"] <- h0
      df1[6, "CashFlow"] <- c0
      df1[6, "ExtraEvent"] <- e0
      df1[6, "NAV"] <- n0
      
      ss <- h0 + c0 + e0 + n0; ssp <- round(ss/50*60,3)
            TeamName <- teamname.c
            Hedging <- h0
            CashFlow <- c0
            ExtraEvent <- e0
            NAV <- n0
            FinalPoints <- ss
            FinalPercentage <- ssp
      data.frame(TeamName, Hedging, CashFlow, ExtraEvent, NAV, FinalPoints, FinalPercentage)
})

finalscore.df <- do.call(rbind, score2.list)

#output csv final score (no breakdown in years)
setwd(finalreport.dir); write.csv(finalscore.df, "FinalScore_df.csv", row.names = F)

#output pdf (all score breakdown)
setwd(maincode.dir)
rmarkdown::render(input = "__rmd_FinalScoreReport_pdf.rmd", output_format = "pdf_document", output_file = paste0("FinalScore_R1_pdf", ".pdf"), output_dir = finalreport.dir)


