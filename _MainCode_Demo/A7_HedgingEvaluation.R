#A7_HedgingEvaluation.R

#INPUT: tranj.df, yy, testasset.c, meta.bonde.df, meta.und.price.df
#OUTPUT: team.asset.payoffdf.list

#Internal functions - sim61.v.f
setwd(maincode.dir); source("A7_01_HedgingEvaluationFunctions.R")


#######################################################################
#subset team, 
#subset testasset
#subset User vs scenario
#output: sum.user.v  ;  sum.scenario.v

teamname12 <- teamname.f()
      testasset.v <- c("GOL", "CRU", "PAL", "USD", "EUR")

tryCatch({  #catch TEAM NAME    
#########LOOP THROUGH TEAM NAME##########
team.asset.payoffdf.list <- lapply(teamname12, FUN = function(TeamName.c){ 
      TeamName.c <<- TeamName.c
      m0 <- end3.td.df[,"classf"] %in% toupper(c("SCENARIO", "EXTRA", "EXC", "OTC"))
      m1 <-  end3.td.df[,"TeamName"] %in% TeamName.c

      tryCatch({  #catch Assets    
      ########LOOP THROUGH ASSETS##########
      lapply(1:length(testasset.v), FUN = function(k){
      
            testasset.c <- testasset.v[k]     
      
            
            #####################LOOP THROUGH EACH TRANSACTIONS###########################
            nocore4.td.df <- end3.td.df[m0 & m1,] #m0 - classf, m1 - team
            
            N <- nrow(nocore4.td.df)
            
            sim61.list <- lapply(1:N, FUN = function(j){
                  tryCatch({
                        tranj.df <- nocore4.td.df[j,]
                        sim61.v.f(tranj.df, yy, testasset.c, meta.bonde.df, meta.und.price.df) #output: list(classH, 61.v) #output: list("User/Scenario", sumproT.61v)
                  }, warning = function(w){
                        print(j)
                  })
                        
            })
            ######################
            sim61.user.list <- lapply(sim61.list, FUN = function(j){
                  if(j[[1]] %in% "User"){
                        j[[2]]
                  }
                        
            })
            
            sim61.scenario.list <- lapply(sim61.list, FUN = function(j){
                  if(j[[1]] %in% "Scenario"){
                        j[[2]]
                  }
                  
            })
            
            
            sim61.user.matrix <- do.call(rbind, sim61.user.list)
            if(is.null(sim61.user.matrix)){
                  sim61.user.matrix <- matrix(0, 1, 61)
            }
            sim61.scenario.matrix <- do.call(rbind, sim61.scenario.list)
            if(is.null(sim61.scenario.matrix)){
                  sim61.scenario.matrix <- matrix(0, 1, 61)
            }
            
            step <- -30:30; step <- step/100 + 1; ss0.v <- und.price.f(testasset.c, yy, meta.und.price.df)*step; ss0.v <- rep(ss0.v, 3) #col1: sT.61
            sum.user.v <- colSums(sim61.user.matrix); sum.scenario.v <- colSums(sim61.scenario.matrix); sum.agg.v <- sum.user.v + sum.scenario.v;  payoff61.v <- c(sum.user.v, sum.scenario.v, sum.agg.v) #payoff61.v
            classH.v <- c(rep("User", 61), rep("Scenario", 61), rep("Aggregate", 61))
            underlying.v <- rep(testasset.c, 61*3)
            tname.v <- rep(TeamName.c, 61*3)               
            assetj.payoff.df <- data.frame(ST = ss0.v, Payoff = payoff61.v, classH = classH.v, Underlying = underlying.v, TeamName = tname.v, stringsAsFactors = F)
      
            assetj.payoff.df #Output
            
      })
            
            
      }, error = function(e){
            print(testasset.c)     
      })
})
      
      
}, error = function(e){
      print(  paste0("A7_HedgingEvaluation.R - team problem at ", TeamName.c)  )     
}) #team.asset.payoffdf.list