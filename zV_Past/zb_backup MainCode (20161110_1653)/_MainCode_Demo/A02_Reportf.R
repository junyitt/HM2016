

prolist.f <- function(fdf8_td, tname, yy){
{
      
      v1 <-  c("GOL", "CRU", "PAL"); v2 <- c("USD", "EUR"); v3user <- c("OTC", "EXC"); v3hedge <- c("Scenario", "Extra")
      
      commClass <- list("CRU", "PAL", "EUR", c("GOL", "USD"))
      
      j <- tname
      
      {
            #subset extra and scenario tran with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate,  teamname
            hdf <- subdfR.f(df = fdf8_td, v1, v2, v3hedge, tName = j, yy)
            
            #subset OTC, EXC with underlying = c("GOL", "CRU", "PAL") or Currency = c("USD", "EUR"), tDate < yy2 <= mDate, teamname
            udf <- subdfR.f(df = fdf8_td, v1, v2, v3user, tName = j, yy)
      } #subset all commClass, by team: hdf, udf
      
      #return vector of scores BY 4class of commodities
      prosimTdf_list <- lapply(commClass, FUN = function(h){
            
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
                  xU <- step*ss0; xC <- step*exss0
                  P.und.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = T)
                  P.und.u <- sumprov.vary.f(u2df, step, yy, ss0, exss0, varyund = T)
                  P.und.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = T)
                  P.cur.h <- sumprov.vary.f(h2df, step, yy, ss0, exss0, varyund = F)
                  P.cur.u <- sumprov.vary.f(u2df, step, yy, ss0, exss0, varyund = F)
                  P.cur.t <- sumprov.vary.f(t2df, step, yy, ss0, exss0, varyund = F)
                  dfpl <- data.frame("NoHedge.C" = P.cur.h, "User.C" = P.cur.u, "Aggregate.C" = P.cur.t, 
                                     "NoHedge.U" = P.und.h, "User.U" = P.und.u, "Aggregate.U" = P.und.t, 
                                     "xU" = xU, "xC" = xC) 
            } #return score
            
            dfpl
      }) #return list of scores, list by commoditiesclass
} #prosimTdf_list    
      
}

