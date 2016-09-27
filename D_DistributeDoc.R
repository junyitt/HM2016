#Main parameter
yy2 <- yy+1
teamname12 <- c(paste0("Alpha ", rep(1:6)), paste0("Beta ", rep(1:6)))

#Main - 0_TEAM - DIR
team0.dir <- "C:/Users/User/OneDrive/0_TEAM/"

#distribute doc

#REPORT - distribute end of year report
report.dir <- "C:/Users/User/OneDrive/D_Distribute/R_report/"

temp <- lapply(teamname12, FUN = function(tname){
      teamfold.dir <- paste0(team0.dir, tname, "/Year ", yy2)
      # identify the folders
      from.dir <- report.dir
      to.dir <- teamfold.dir
      
      # find the files that you want #Alpha 1_Report_1.html
      filename <- paste0(tname, "_Report_", yy2)
      filetocopy <- list.files(from.dir, filename)
      
      # copy the files to the new folders
      setwd(from.dir)
      file.copy(from = filetocopy, to.dir)
}) #Alpha 1_Report_1.html

#EXTRA, SCENARIO, INSTRUMENTS
extra.dir <- "C:/Users/User/OneDrive/D_Distribute/E_Extra"
scenario.dir <- "C:/Users/User/OneDrive/D_Distribute/S_Scenario"
instrument.dir <- "C:/Users/User/OneDrive/D_Distribute/F_FIC"

temp2 <- lapply(teamname12, FUN = function(tname){
      teamfold.dir <- paste0(team0.dir, tname, "/Year ", yy2) #team folder dir
      
      #sector 
      if(substr(tname,1,1) == "A"){
            sector <- "Alpha"
      }else if(substr(tname,1,1) == "A"){
            sector <- "Beta"
      }
      
      #to.dir - FIXED
      to.dir <- teamfold.dir
      
      #Alpha_Extra_0 #Beta_Extra_0
      from.dir <- extra.dir
      setwd(from.dir)
            doc <- "Extra"
            filezname <- paste0(sector, "_", doc, "_", yy2)
                  filetocopy <- list.files(from.dir, pattern = filezname)
                  file.copy(from = filetocopy, to.dir)
                  
      #Alpha_Scenario_0 #Beta_Scenario_0
      from.dir <- scenario.dir
      setwd(from.dir)
            doc <- "Scenario"
            filezname <- paste0(sector, "_", doc, "_", yy2)
                  filetocopy <- list.files(from.dir, pattern = filezname)
                  file.copy(from = filetocopy, to.dir)
      
      #Prices_Financial Instruments List_0
      from.dir <- instrument.dir
      setwd(from.dir)
            doc <- "Scenario"
            filezname <- paste0("Prices_Financial Instruments List_", yy2)
                  filetocopy <- list.files(from.dir, pattern = filezname)
                  file.copy(from = filetocopy, to.dir)


})


print(paste0("Success. Now we are in END OF YEAR ", yy2))
