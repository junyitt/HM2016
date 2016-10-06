#A1_01_Core.R

#DIR #maincode.dir


#INTERNAL functions ##
setwd(maincode.dir); source("A1_01_Reportf.R")
      
################################################################

#generate team names
teamname12 <- teamname.f()

########################
##A1 Report - 01 - Core  
########################
{
cReport_list <- lapply(teamname12, FUN = function(x){ #return list of core report, where list[[1]] is the core report df for team Alpha 1 and so on
      core.f(x, fdf8_td, yy)
}) 
} #cReport_list
      
#########################################
#A1 Report - 02 - Scenario transactions
#########################################
{
sReport_list <- lapply(teamname12, FUN = function(x){
                              scenario.f(x, fdf8_td, yy)
                        })
} #sReport_list

###########################################
#A1 Report - 03 - Extra transactions
###########################################
{
      eReport_list <- lapply(teamname12, FUN = function(x){
            extraR.f(x, fdf8_td, yy)
      }) 
    
}  #output eReport_list 

###########################################
#A1 Report - 04 - Other Money Transactions? Hints/Tips
###########################################
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
} #NULL for now

###########################################
#A1 Report - 05 - ALL Your transactions
###########################################
{
uReport_list <- lapply(teamname12, FUN = function(x){
      uR.df <- userR.f(x, fdf8_td)
      uR.df
})
} #uReport_list

###########################################
#A1 Report - 07 - Hedging Evaluation ######OLD
###########################################
{
      #DIR-80_01_END1Functions.R 
      e80f.dir <- "C:/Users/User/Google Drive/z_ALLHM"
      setwd(e80f.dir); source("80_01_END1Functions.R") #cff_f and ProTf, STf, exSTf
      
      #$$###click #optional
      #teamnamelist > commlist > teamd      
      v1 <-  c("GOL", "CRU", "PAL", "USD", "EUR"); v2 <- c("USD", "EUR"); v3user <- c("OTC", "EXC"); v3hedge <- c("Scenario", "Extra")
      
      commClass <- list("CRU", "PAL", "EUR", c("GOL", "USD"))
      
      {
            
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
      
      score_hedge_byteam <- sapply(teamname12, FUN = function(j){
            {
                  #subset extra and scenario tran with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate,  teamname
                  hdf <- subdfR.f(df = fdf8_td, v1, v2, v3hedge, tName = j, yy)
                  
                  #subset OTC, EXC with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate, teamname
                  udf <- subdfR.f(df = fdf8_td, v1, v2, v3user, tName = j, yy)
            } #subset all commClass, by team: hdf, udf
            
            #return vector of scores BY 4class of commodities
            scorebycomm_v <- sapply(commClass, FUN = function(h){
                  
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
                  }#ss0, exss0 #gclass 1 for golusd, 2 for eur, 3 for others
                  
                  step <- -30:30; step <- step/100 + 1
                  {      
                        
                        if(gclass == 3){ #CRU or #PAL
                              
                              P.und.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = T)
                              P.und.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = T)
                              score <- scoreH.f(P.und.h, P.und.t, sumq = T)
                              
                        }else if(gclass == 2){ #EUR
                              P.cur.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = F)
                              P.cur.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = F)
                              score <- scoreH.f(P.cur.h, P.cur.t, sumq = T)
                              
                        }else if(gclass == 1){ #GOL & USD
                              P.und.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = T)
                              P.und.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = T)
                              score.und <- scoreH.f(P.und.h, P.und.t)
                              P.cur.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = F)
                              P.cur.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = F)
                              score.cur <- scoreH.f(P.cur.h, P.cur.t)
                              score <- mean(score.und, score.cur, sumq = T)
                        }
                        ##refix:^^^^^^^
                  } #return score
                  
                  score
            }) #return list of scores, list by commoditiesclass
            
            sum(scorebycomm_v)/12.5*20
      })
      
      
} #score_hedge_byteam (vector)

###########################################
#A1 Report - 08 - Extra Event Evaluation - Score
###########################################
{
      #DIR-spec0,1
      spec.dir <- "C:/Users/User/OneDrive/yy_YearlyFullTran" 
      score.meta.dir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta"
      
      if(yy %in% c(0,1)){
            setwd(spec.dir); 
            uspec <- read.csv(paste0("uspecfdf_", yy, ".csv"))
                  setwd(score.meta.dir)
                  metascore0_df <- as.data.frame(read_excel("meta-extra0-score.xlsx"))
            score.extra <- sapply(teamname12, FUN = function(j){
                              u <- uspec[,"tName"] == j; serv <- uspec[u, "Service"] #get the service chosen by the team
                              u2 <- metascore0_df[,"Service"] == serv
                              if(sum(u2)==0){
                                    0
                              }else{
                                    metascore0_df[u2, "Score"]*3 #return the respective score
                              }
                        })
      }else if(yy == 4){
            score.extra <- sapply(teamname12, FUN = function(j){
                  u <- fdf8_td[,"cParty"] %in% "Harry M."; u2 <- fdf8_td[, "TeamName"]  %in% j
                  if(sum(u & u2) == 0){
                        0
                  }else{
                        as.numeric(fdf8_td[u & u2, "VLRemarks"])
                  }
            })
      }else{
            score.extra <- sapply(teamname12, FUN = function(j){
                  0
            })
      }
      
} #score.extra
      
###########################################
#A1 Report - 09 - Cash Evaluation
###########################################
{
      score.cash <- sapply(teamname12, FUN = function(j){
            score.cash.f(fdf8_td, yy, j)  
      })
} #score.cash

###########################################
#A1 Report - 09b - NAV 
###########################################
{
      if(yy == 4){
            nav_v <- balsh_new_yy2[,"NAV"]
            score.NAV1 <- rank(nav_v[1:6])/6*25 #not reproducible with different team numbers
            score.NAV2 <- rank(nav_v[7:12])/6*25
            score.NAV <- c(score.NAV1, score.NAV2)
      }else{
            score.NAV <- rep(0, 12)     
      }
}#score.NAV

###########################################
#A1 Report - 10 - Total Score Breakdown
###########################################
{
      
      #DIR-scorebreakdown
      scoreb0.meta.dir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_meta"
      scoreb.dir <- "C:/Users/User/OneDrive/yy_Scorebreakdown" 
      
      { 
            setwd(scoreb0.meta.dir); score.break <- as.data.frame(read_excel("meta-scorebreakdown-0.xlsx"))
            L <- list(score_hedge_byteam, score.cash, score.extra, score.NAV)
            for(x in 2:5){
                  score.break[,x] <- L[[x-1]]
            }
            score.break[,"TotalScore"] <- sapply(1:nrow(score.break), FUN = function(k){
                                                sum(score.break[k, 2:5])   
                                          })
            score.break[, "Year"] <- rep(yy+1, nrow(score.break))
            
            setwd(scoreb.dir)
            write.csv(score.break, paste0("scorebreakdown_", yy+1, ".csv"), row.names = F)
            
      } #write latest scorebreakdown csv -> "scorebreakdown_", yy+1, ".csv"
      
            #read all scorebreakdown csv up to date
                  score_byyr_list <- lapply(1:(yy+1), FUN = function(i){
                                          setwd(scoreb.dir); read.csv(paste0("scorebreakdown_", i, ".csv"))
                                    })
                  score.tddf <- do.call(rbind, score_byyr_list)
            #return a list, by team, the score df to be displayed     
            scoredf_byteam_list <- lapply(teamname12, FUN = function(j){
                                          u <- score.tddf[,"TeamName"] == j; sections <- colnames(score.tddf)[2:5]
                                          N <- sum(u)
                                          df <- data.frame("Section" = sections)
                                          for(yr in 1:(yy+1)){
                                                # stringyr <- as.character(yr)
                                                u2 <- score.tddf[, "Year"] %in% yr
                                                yr_c <- as.character(yr)
                                                for(kk in 1:4){
                                                      ss <- sections[kk]
                                                      df[kk, yr_c] <- score.tddf[u & u2, ss]
                                                }
                                                # df[,stringyr] <- sapply(sections, FUN = function(x){
                                                #       score.tddf[u & u2, x]  #out: score for team j, year = yr, sections = x                       
                                                # })
                                          }
                                          
                                          df[, "Average"] <- sapply(X=1:nrow(df), FUN = function(m){
                                                                  mean(as.numeric(df[m, 2:(yy+2)]))
                                                            })
                                          
                                          df #output: displayed form scoredf
                        
                                    }) 
            
}#scoredf_byteam_list
            
      
#A1 Report_input all 10 parameters, output 12 csv report
      
      