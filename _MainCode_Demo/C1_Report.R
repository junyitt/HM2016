#C1_Report.R

#DIR #distreport.dir

#INPUT: 
#cReport_list, cComment.v
##rReport.df.list, rComment0.v, rCommentT.v
#fReport.df.list, fComment.v
## baly.accelement.list, baly2.accelement.list
##ggplot.team.asset.list, hedgescore.team.df.list, hedge5Comment.v
##cash6Comment.v
#extra7Comment.v
#scorebr8.yy2.df.list, scoreComment8.v


#######################################
teamname12 <- teamname.f()
# i=6
for(i in 1:length(teamname12)){
      c1Report.df <- cReport.df.list[[i]]; cComment.c <- cComment.v[i]
      r2Report.df <- rReport.df.list[[i]]; r2Comment0.c <- rComment0.v[i]; r2CommentT.c <- rCommentT.v[i]
      f2Report.df <- fReport.df.list[[i]]; f2Comment.c <- fComment.v[i]
      accelementy.v <- baly.accelement.list[[i]]; accelementy2.v <- baly2.accelement.list[[i]]
      ggplot.asset.list <-  ggplot.team.asset.list[[i]]; hedgescore.team.df <- hedgescore.team.df.list[[i]]; hedge5Comment.c <- hedge5Comment.v[i]
      cash6Comment.c <- cash6Comment.v[i]
      extra7Comment.c <- extra7Comment.v[i]
      scorebr8.yy2.df <- scorebr8.yy2.df.list[[i]]; scoreComment8.c <- scoreComment8.v[i]
      
TeamName.c <<- teamname12[i]; fname <<- paste0(TeamName.c, "_Report_", yy+1)
setwd(maincode.dir)
rmarkdown::render(input = "C_Report.Rmd", output_format = "pdf_document", output_file = paste0(fname, ".pdf"), output_dir = distreport.dir)

} #@ distreport.dir;; Alpha K_Report_yy.pdf

# cComment.v
