
#Create from scratch code
#team names?
teamname12 <- c(paste0("Alpha ", rep(1:6)), paste0("Beta ", rep(1:6)))

#how many years?
yyend <- 5

#where to start?
main00.dir <- "C:/Users/User/OneDrive/"

{

      
      #start by creating "0_Team" folder
      setwd(main00.dir)
      dir.create("0_TEAM")
      
      team0fold.dir <- "C:/Users/User/OneDrive/0_TEAM/"
      for(tname in teamname12){
            #create team folder
            setwd(team0fold.dir)
                  dir.create(tname)
            #go inside the team folder, and create yearly folders
                  foldt.dir <- paste0(team0fold.dir, tname, "/")
            setwd(foldt.dir)
            for(yr in 0:yyend){
                  foldyrname <- paste0("Year ", yr)
                  dir.create(foldyrname)
            }
            
      } 
      
      #create 3 "forms" folders
      form.fold <- c("1_Form_EXC", "1_Form_OTC", "1_Form_EXTRA")
      for(fr in form.fold){
            setwd(main00.dir)
            dir.create(fr)
      }
      
      #create 4 "yy_fulltran" folders
      yy_fold <- c("yy_b_YearlyFullTran", "yy_Scorebreakdown", "yy_YearlyBalanceSheet", "yy_YearlyFullTran")
      for(yyf in yy_fold){
            setwd(main00.dir)
            dir.create(yyf)
      }

} #Create folders
