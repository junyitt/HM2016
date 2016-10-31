#A9_ScoreExtra.R
#A9: Extra Event Evaluation - Score

#DIR: fulltran.dir, meta.dir

#INPUT: #end3.td.df
#OUTPUT: #score.extra

########################################################
teamname12 <- teamname.f()

if(yy %in% c(0,1)){
      setwd(fulltran.dir); uspec.df <- read.csv(paste0("uspec_", yy, ".csv"))
      metascore0_df <- importmeta.f(meta.dir, "meta-extra0-score.xlsx")

      score.extra <- sapply(teamname12, FUN = function(j){
            u <- uspec.df[,"TeamName"] %in% j; serv <- uspec.df[u, "Service"] #get the service chosen by the team
            u2 <- metascore0_df[,"Service"] %in% serv
            if(sum(u2)==0){
                  0
            }else{
                  metascore0_df[u2, "Score"] #return the respective score
            }
      })
}else if(yy == 4){
      score.extra <- sapply(teamname12, FUN = function(j){
            u <- end3.td.df[,"cParty"] %in% "Harry M."; u2 <- end3.td.df[, "TeamName"]  %in% j
            if(sum(u & u2) == 0){
                  0
            }else{
                  as.numeric(end3.td.df[u & u2, "VLRemarks"])
            }
      })
}else{
      score.extra <- sapply(teamname12, FUN = function(j){
            NA
      })
}
      
#score.extra