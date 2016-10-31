#A1 Report - 07 - Hedging Evaluation
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
                  
                  
                  setwd(A1f.dir); source("A1_01_Reportf.R")
                  
                  
                  {
                        u1 <- hdf[,"Underlying"] %in% h; u2 <- hdf[,"Currency"] %in% h; u <- u1 | u2
                        u3 <- udf[,"Underlying"] %in% h; u4 <- udf[,"Currency"] %in% h; uu <- u3 | u4
                        gclass <- 1
                        
                        h2df <<- hdf[u,]; u2df <- udf[uu,]; t2df <- rbind(h2df, u2df)
                  }#subset: h2df, u2df, t2df #use h2df as base, to evaluate t2df 
                  # {        
                  #       if("GOL" %in% h){
                  #             ss0 <- STf(Underlying = "GOL", yy); exss0 <- exSTf(Currency = "USD", yy)
                  #             gclass <- 1
                  #       }else if("EUR" %in% h){
                  #             exss0 <- 1; ss0 <- exSTf(Currency = "EUR", yy)
                  #             gclass <- 2
                  #             
                  #       }else{
                  #             ss0 <- STf(Underlying = h[1], yy); exss0 <- 1
                  #             gclass <- 3      
                  #       }
                  # }#ss0, exss0 #gclass 1 for golusd, 2 for eur, 3 for others
                  
                  step <- -30:30; step <- step/100 + 1
                  {      
                        
                        if(gclass == 3){ #CRU or #PAL
                              
                              P.und.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = T)
                              P.und.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = T)
                              score <- scoreH.f(P.und.h, P.und.t, sumq = T)
                              
                        }else if(gclass == 2){ #EUR
                              P.cur.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = T)
                              P.cur.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = T)
                                    P.cur.u <- sumprov.vary.f(u2df, step, yy, ss0, exss0, varyund = T)
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