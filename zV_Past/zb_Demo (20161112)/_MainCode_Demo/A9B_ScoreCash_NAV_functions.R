#A9B_ScoreCash_NAV_functions.R

#A1 Report - 09 - Cash Evaluation
score.cash.f <- function(end3.td.df, yy, TeamName.c){
      u1 <- grepl(pattern = "^additional loan", end3.td.df[,"Remarks"])  ; u2 <- end3.td.df[, "tDate"] %in% yy
      u3 <- end3.td.df[,"TeamName"] %in% TeamName.c
      if(sum(u1 & u2 & u3) == 0){
            10
      }else{
            0
      }
}