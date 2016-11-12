#B1_PreReport.R

#DIR #maincode.dir

      #INPUT: end3.td.df
#OUTPUT: 
#cReport_list, cComment.v
##rReport.df.list, rComment0.v, rCommentT.v
#fReport.df.list, fComment.v
## baly.accelement.list, baly2.accelement.list
##ggplot.team.asset.list ;;; NEXT: scorebr for hedging
##cash6Comment.v
#extra7Comment.v
#scorebr8.yy2.df.list

#INTERNAL functions ##
setwd(maincode.dir); source("B1_PreReportFunctions.R")

################################################################################################################

#generate team names
teamname12 <- teamname.f()

########################
##B1 PreReport - 01 - Core  
########################
{
      cReport_list_numeric <- lapply(teamname12, FUN = function(x){ 
                                core.f(x, end3.td.df, yy)
                              }) 
      cReport.df.list <- lapply(teamname12, FUN = function(x){ 
                  df <- core.f(x, end3.td.df, yy)
                  numcol <- c("Units1", "Price", "TotalRevenue", "Units2", "Cost", "TotalCost", "TotalProfit")
                  for(col in numcol){
                        df[,col] <- formatC(df[,col], digits = 2, big.mark = ",", format="f") 
                  }
                  df
      }) 
      cSumprofit.v <- sapply(cReport_list_numeric, FUN = function(df){
                              formatC(sum(df[,"TotalProfit"]), digits = 2, big.mark = ",", format="f") 
                  })
      cComment.v <- paste0("Total profit from running the usual business is MYR ", cSumprofit.v)
      cExtra.msg.v <- sapply(teamname12, FUN = function(x){ 
            extradf <- relevant.f(x, end3.td.df, yy, rel = T, serv = T)
            if(nrow(extradf) == 0){
                  paste0("No extra service employed to improve cost or revenue.")
            }else if(nrow(extradf) == 1){
                  FIC1 <- extradf[1,"FIC"]
                  cType1 <- extradf[1,"ContractType"]
                  Und1 <- extradf[1,"Underlying"]
                  kPrice1 <- extradf[1,"kPrice"]
                  paste0(FIC1, " was employed. There will be a ", cType1, " of MYR ", kPrice1, " per unit for ", Und1, " this year. ")
                  
            }else if(nrow(extradf) == 2){
                  FIC1 <- extradf[1,"FIC"]
                  cType1 <- extradf[1,"ContractType"]
                  Und1 <- extradf[1,"Underlying"]
                  kPrice1 <- extradf[1,"kPrice"]
                        FIC2 <- extradf[2,"FIC"]
                        cType2 <- extradf[2,"ContractType"]
                        Und2 <- extradf[2,"Underlying"]
                        kPrice2 <- extradf[2,"kPrice"]
                  paste0(FIC1, " was employed. There will be a ", cType1, " of MYR ", kPrice1, " per unit for ", Und1, " this year. ", FIC2, " was employed. There will be a ", cType2, " of MYR ", kPrice2, " per unit for ", Und2, " this year. ")
            }
      }) 
      
} #cSumprofit.v;; cReport.df.list (df), cComment.v, cExtra.msg.v

########################
##B1 PreReport - 02 - Relevant
########################
{
            rReport_list_numeric <- lapply(teamname12, FUN = function(x){ 
                                          relevant.f(x, end3.td.df, yy, rel = T, serv = F)
                                    }) 
      rReport.df.list <- lapply(teamname12, FUN = function(x){
                              df <- relevant.f(x, end3.td.df, yy, rel = T, serv = F)
                              numcol <- c("kPrice", "Units", "NetProceed_0", "Payoff_T", "MarketValue_T")
                              for(col in numcol){
                                    df[,col] <- formatC(df[,col], digits = 2, big.mark = ",", format="f") 
                              }
                              df
                        }) 
            rSumcash0.numeric.v <- sapply(rReport_list_numeric, FUN = function(df){
                  u <- !(df[,"ContractType"] %in% c("Revenue Increment", "Cost Reduction"))
                  sum(df[u,"NetProceed_0"])
            })
      rSumcash0.v <- sapply(rReport_list_numeric, FUN = function(df){
                        u <- !(df[,"ContractType"] %in% c("Revenue Increment", "Cost Reduction"))
                        formatC(sum(df[u,"NetProceed_0"]), digits = 2, big.mark = ",", format="f") 
                  })
      rSumcashT.v <- sapply(rReport_list_numeric, FUN = function(df){
                        u <- !(df[,"ContractType"] %in% c("Revenue Increment", "Cost Reduction"))
                        formatC(sum(df[u,"Payoff_T"]), digits = 2, big.mark = ",", format="f") 
                  })
      rComment0.v <- paste0("Cash Flow for new transactions at the End of Year ", yy, " is MYR ", rSumcash0.v)
      rCommentT.v <- paste0("Total Payoff at the End of Year ", yy+1, " is MYR ", rSumcashT.v)
      
} #rReport_list_numeric, rSumcash0.v, rSumcashT.v, rSumcash0.numeric.v  ##rReport.df.list, rComment0.v, rCommentT.v

########################
##B1 PreReport - 03 - Failed Transactions
########################
{
      fReport.df.list <- lapply(teamname12, FUN = function(x){ 
            df <- relevant.f(x, end3.td.df, yy, rel = F)
            df
      }) 
      fComment.v <- sapply(fReport.df.list, FUN = function(df){
                        if(nrow(df) == 0){
                              Comment <- "No failed transaction."
                        }else{
                              Comment <- "There are failed transactions. Refer to 'Status'."
                        }
                              return(Comment)
                  })
} #fReport.df.list, fComment.v

########################
##B1 PreReport - 04 - Statement of Financial Position #input: balsh_y.df, balsh_y2.df
########################
{#lapply wrt team when generate report, get vector >> assign to accelement, then accelement[1] = "Cash" etc.
      name.accelement <- c("PPE", "FA", "Cash", "TotalAsset", 
                           "ShareCap", "RE", "TotalEq", "Loan", 
                           "TotalLiabilities", 
                           "TotalEqandL", "NAV")
                           
      baly.accelement.list <- lapply(1:length(teamname12), FUN = function(j){
                        g <- vector()
                        g[1] <- balsh_y.df[j,"PPE"] 
                        g[2] <- balsh_y.df[j,"FinAsset"] 
                        g[3] <- balsh_y.df[j,"Cash"] 
                        g[4] <- g[1] + g[2] + g[3] #TotalAsset
                              g[5] <- balsh_y.df[j,"ShareCap"] 
                              g[6] <- balsh_y.df[j,"RE"]   
                              g[7] <- g[5] + g[6] #Total Equity
                              g[8] <- balsh_y.df[j,"Loan"] 
                        g[9] <- g[8] #Total Liabilities
                              g[10] <- g[7] + g[9]
                              g[11] <- balsh_y.df[j,"NAV"] 
                              formatC(g, digits = 2, big.mark = ",", format="f") 
      })
      baly2.accelement.list <- lapply(1:length(teamname12), FUN = function(j){
            g <- vector()
            g[1] <- balsh_y2.df[j,"PPE"] 
            g[2] <- balsh_y2.df[j,"FinAsset"] 
            g[3] <- balsh_y2.df[j,"Cash"] 
            g[4] <- g[1] + g[2] + g[3] #TotalAsset
            g[5] <- balsh_y2.df[j,"ShareCap"] 
            g[6] <- balsh_y2.df[j,"RE"]   
            g[7] <- g[5] + g[6] #Total Equity
            g[8] <- balsh_y2.df[j,"Loan"] 
            g[9] <- g[8] #Total Liabilities
            g[10] <- g[7] + g[9]
            g[11] <- balsh_y2.df[j,"NAV"] 
            
            formatC(g, digits = 2, big.mark = ",", format="f") 
      })
} ## baly.accelement.list, baly2.accelement.list

######################## 
##B1 PreReport - 05 - Hedging Graphs and scores for each asset  #listbyteam >> listbyplots
######################## #input: fullsim.df 
{
asset.v <- c("GOL", "CRU", "PAL", "USD", "EUR")

ggplot.team.asset.list <- lapply(teamname12, FUN = function(TeamName.c){
      lapply(asset.v, FUN = function(asset.c){
            u1 <- fullsim.df[,"TeamName"] %in% TeamName.c; u2 <- fullsim.df[,"Underlying"] %in% asset.c; 
            plot.df <- fullsim.df[u1 & u2,]
                  m1 <- plot.df[,"classH"] %in% "User"; m2 <- plot.df[,"classH"] %in% "Scenario"
            jtr <- sd(fullsim.df[u1 & u2,"Payoff"])*0.0175
            if(jtr == 0){jtr <- 0.00001}
            plot.df[m1,"Payoff"] <- plot.df[m1,"Payoff"]+jtr; plot.df[m2,"Payoff"] <- plot.df[m2,"Payoff"]-jtr
            ss0 <- plot.df[31, "ST"]; pmin <- min(plot.df[,"Payoff"]); pmax <- max(plot.df[,"Payoff"])
            if(pmax-pmin > 10){}else{pmax <- pmax*100; pmin <- pmin*100}; if(pmin > 0){pmin <- mean(c(pmin,pmax))*-1}
                  colnames(plot.df) <- c("ST", "Payoff", "Portfolio", "Underlying", "TeamName")
                        
            p1 <- ggplot(plot.df, aes(x = ST, y = Payoff, color = Portfolio)) +
                  geom_line(alpha = 0.9, size = 1.5) +
                  # scale_linetype_manual(values=c("dotted", "dotdash", "dashed")) +
                  scale_linetype_manual(values=c("solid", "solid", "solid")) +
                  ylim(pmin, pmax) + 
                  xlab(label = "ST") +
                  ylab(label = "Payoff at time T") +
                  ggtitle(paste0("Payoff Graph (", asset.c, ")"))+
                  geom_hline(yintercept = 0, colour="black", linetype="solid", size = 0.6, alpha = 0.3) +       
                  geom_vline(xintercept=ss0, colour="orange", linetype="dashed", size = 1) + 
                  geom_text(x = ss0, y = 0, label = paste0("S0 = ",ss0), colour = "orange", size = 3.5, hjust = -0.1, vjust = -1.5) 
            
            p1
      })
})

hedgescore.team.df.list <- lapply(teamname12, FUN = function(TeamName.c){
                              u <- hedgescore.df[,"TeamName"] %in% TeamName.c
                              hedgedf <- hedgescore.df[u,c(2:6,6)]
                                    hedgedf[,5] <- rowSums (hedgedf[,2:4])
                                    colnames(hedgedf) <- c("Underlying", "C1", "C2", "C3", "Sum", "Revised Sum")
                                          hedgedf
                              })
hedge5Comment.v <- sapply(hedgescore.team.df.list, FUN = function(df){
                              ss <- sum(df[, ncol(df)])
                              paste0("Raw Hedging Score is ", ss, ". Converting to 20 points, the Hedging Score is ", round(ss/9*20,2), " points.")
}) 

} ##ggplot.team.asset.list, hedgescore.team.df.list, hedge5Comment.v

######################## 
##B1 PreReport - 06 - Cash balance right after trading - balsh_y.df[cash]
########################
{#input: balsh_y.df, rSumcash0.numeric.v
      excludeloan.cash0.v <- sapply(rReport_list_numeric, FUN = function(df){
            u <- !(df[,"ContractType"] %in% c("Revenue Increment", "Cost Reduction"))
            u2 <- !(df[,"ContractType"] %in% "Loan" & grepl(pattern = "^additional loan borrowed due to negative", df[,"Remarks"]))
            sum(df[u & u2,"NetProceed_0"])
      })
            stloan.v <- rSumcash0.numeric.v - excludeloan.cash0.v; stloan.v <- formatC(stloan.v, digits = 2, big.mark = ",", format="f") 
            cash0.v <- balsh_y.df[,"Cash"] + excludeloan.cash0.v; cash0.v <- formatC(cash0.v, digits = 2, big.mark = ",", format="f") 
            cash6Comment.v <- vector()
      u <- cash0.v > 0
      cash6Comment.v[u] <- paste0("Cash balance right after trading is MYR ", cash0.v[u], ". Score for Cash Flow is 10 points.")
      cash6Comment.v[!u] <- paste0("Cash balance right after trading is MYR ", cash0.v[!u], 
                                   ". Additional loan amounting to MYR ", stloan.v[!u], 
                                    " was borrowed from 3rd Party Institution. ", 
                                   "Score for Cash Flow is 0 point.")
} ##cash6Comment.v

######################## 
##B1 PreReport - 07 - Extra Event - score.extra? service chosen (yr0,1).. harrym.(yr4)
########################
{
      extra7Comment.v <- sapply(teamname12, FUN = function(TeamName.c){
            j <- grep(TeamName.c, teamname12)
            if(yy %in% c(0,1)){
                  setwd(fulltran.dir); uspec.df <- read.csv(paste0("uspec_", yy, ".csv"), stringsAsFactors = F)
                  u <- uspec.df[,"TeamName"] %in% TeamName.c; serv <- uspec.df[u, "Service"] #get the service chosen by the team
                  if(length(serv) == 0){
                        serv <- "No service"
                  }
                  paste0(serv, " was chosen. The score for Extra Event is ", score.extra[j], " points.")
            }else if(yy %in% c(2,3)){
                  "Not applicable, NA."
            }else if(yy %in% c(4)){
                  u <- end3.td.df[,"cParty"] %in% "Harry M."; u2 <- end3.td.df[, "TeamName"]  %in% TeamName.c
                        sharpe.entry.c <- end3.td.df[u & u2,"kPrice"]
                        if(length(sharpe.entry.c) == 0){sharpe.entry.c <- NA}
                  paste0("Maximum Sharpe ratio is ", round(maxsharpe.c,5), ". Your Sharpe ratio is ", round(sharpe.entry.c,5),
                         ". The score for Extra Event is ", score.extra[j], " points.")
            }else{
                  "Not applicable, NA. Error."
            }
            
      })
} #extra7Comment.v

########################  IN PROGRESS ##########
##B1 PreReport - 08 - Total Score Breakdown (scorebr.df.list) and comment
########################
{#INPUT: scorebr.td.df
      scorebr8.yy2.df.list <- lapply(teamname12, FUN = function(TeamName.c){
            u1 <- scorebr.td.df[, "TeamName"] %in% TeamName.c; u2 <- !(scorebr.td.df[, "Year"] %in% 0)
            newscorebr.td.df0 <- scorebr.td.df[u1 & u2,c(6,2,3,4,5)]
            newscorebr.td.df0[, "Year"] <- as.character(newscorebr.td.df0[,"Year"])
                  hscore <- mean(newscorebr.td.df0[,"Hedging"], na.rm = T); hscore <- round(hscore, 2)
                  cscore <- mean(newscorebr.td.df0[,"CashFlow"], na.rm = T); cscore <- round(cscore, 2)
                  escore <- mean(newscorebr.td.df0[,"ExtraEvent"], na.rm = T); escore <- round(escore, 2)
                  nscore <- mean(newscorebr.td.df0[,"NAV"], na.rm = T); nscore <- round(nscore, 2)
            Qdf <- data.frame("Average", hscore, cscore, escore, nscore)
            colnames(Qdf) <- colnames(newscorebr.td.df0)
            rbind(newscorebr.td.df0, Qdf)
      })
      
      scoreComment8.v <- sapply(scorebr8.yy2.df.list, FUN = function(df){
                        if(yy == 4){
                              ss <- sum(df[nrow(df), 2:5])
                              QQ <- round(ss/50*60,2)
                              if(QQ > 60){
                                    Qcap <- 60
                                    paste0("Your Final Score for Round 1 of HedgeMaster Challenge is ", ss, " points. Converting to 60%, your Final Score is ", QQ, "%. Since your Final Score exceeds 60%, it will be capped at 60%. In the event of tie, ", QQ, "% will be used to break the tie. All the best for Round 2 of HedgeMaster Challenge!.")
                              }else{
                                    paste0("Your Final Score for Round 1 of HedgeMaster Challenge is ", ss, " points. Converting to 60%, your Final Score is ", QQ, "%. All the best for Round 2 of HedgeMaster Challenge!.")
                              }
                        }else{
                              ss <- sum(df[nrow(df), 2:4])
                              paste0("Your current score (average) is ", ss, " points.")
                        }
                  })
} #scorebr8.yy2.df.list; scoreComment8.v

