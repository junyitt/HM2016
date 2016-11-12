#A8_ScoreHedging

#Internal functions - scoring
setwd(maincode.dir); source("A7_01_HedgingEvaluationFunctions.R")

#INPUT: team.asset.payoffdf.list
#OUTPUT: fullsim.df, hedgescore.df, score.hedge

################################################################
teamname12 <- teamname.f()
testasset.v <- c("GOL", "CRU", "PAL", "USD", "EUR")

#loop through team, loop through asset; subset scenario >> P.type.h;; subset aggregate >> P.type.t
list2 <- lapply(1:length(teamname12), FUN = function(a){
      list1 <- team.asset.payoffdf.list[[a]]
      und.df.list <- lapply(1:length(testasset.v), FUN = function(b){
                  list1[[b]]
      })
      df1 <- do.call(rbind, und.df.list)
      df1  
})
fullsim.df <- do.call(rbind, list2) 
######################################################  #fullsim.df
# tryCatch({
listB <- lapply(teamname12, FUN = function(TeamName.c){
      TeamName.c <<- TeamName.c
      m1 <- fullsim.df[,"TeamName"] %in% TeamName.c
            listA <- lapply(testasset.v, FUN = function(testasset.c){
                  testasset.c <<-testasset.c
                        m2 <- fullsim.df[,"Underlying"] %in% testasset.c
                        m3s <- fullsim.df[,"classH"] %in% "Scenario"
                        m3a <- fullsim.df[,"classH"] %in% "Aggregate"
                              P.type.h <- fullsim.df[m1 & m2 & m3s, "Payoff"]
                              P.type.t <- fullsim.df[m1 & m2 & m3a, "Payoff"]
                              scoreH.df.f(P.type.h, P.type.t, testasset.c, TeamName.c)
            })
            dfA <- do.call(rbind, listA)
})
# },error = function(e){print(paste0(TeamName.ca, testasset.ca))})

hedgescore.df <- do.call(rbind, listB)
hedgescore.df <- data.frame(hedgescore.df, Year = yy+1)

######################################################  #hedgescore.df  
#Final score by each team - return vector
score.hedge <- sapply(teamname12, FUN = function(t){
      u <- hedgescore.df[,"TeamName"] %in% t
      round(sum(hedgescore.df[u, "sum"])/9*20,2)
})
     
      