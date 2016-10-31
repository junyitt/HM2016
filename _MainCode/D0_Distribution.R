# yy == 0 distribution

teamname12 <- c(paste0("Alpha ", rep(1:6)), paste0("Beta ", rep(1:6)))

#Main - 0_TEAM - DIR
team0.dir <- "C:/Users/User/OneDrive/0_TEAM/"


#distribute doc
#Initial0.dir
initial0.dir <- "C:/Users/User/OneDrive/D_Distribute/0_Initial/"

#general instructions
temp0 <- lapply(teamname12, FUN = function(tname){
      teamfold.dir <- paste0(team0.dir, tname, "/Year ", yy) #team folder dir
      
      #sector 
      if(substr(tname,1,1) == "A"){
            sector <- "Alpha"
      }else if(substr(tname,1,1) == "B"){
            sector <- "Beta"
      }
      
      #to.dir - FIXED
      to.dir <- teamfold.dir
      
      #Alpha_Extra_0 #Beta_Extra_0
      from.dir <- initial0.dir
      setwd(from.dir)
      doc <- "Extra"
      filezname <- paste0(sector, "_General Instructions and Rules")
      filetocopy <- list.files(from.dir, pattern = filezname)
      file.copy(from = filetocopy, to.dir, overwrite = T)
})

#EXTRA, SCENARIO, INSTRUMENTS
extra.dir <- "C:/Users/User/OneDrive/D_Distribute/E_Extra"
scenario.dir <- "C:/Users/User/OneDrive/D_Distribute/S_Scenario"
instrument.dir <- "C:/Users/User/OneDrive/D_Distribute/F_FIC"

temp2 <- lapply(teamname12, FUN = function(tname){
      teamfold.dir <- paste0(team0.dir, tname, "/Year ", yy) #team folder dir
      
      #sector 
      if(substr(tname,1,1) == "A"){
            sector <- "Alpha"
      }else if(substr(tname,1,1) == "B"){
            sector <- "Beta"
      }
      
      #to.dir - FIXED
      to.dir <- teamfold.dir
      
      #Alpha_Extra_0 #Beta_Extra_0
      from.dir <- extra.dir
      setwd(from.dir)
      doc <- "Extra"
      filezname <- paste0(sector, "_", doc, "_", yy)
      filetocopy <- list.files(from.dir, pattern = filezname)
      file.copy(from = filetocopy, to.dir, overwrite = T)
      
      #Alpha_Scenario_0 #Beta_Scenario_0
      from.dir <- scenario.dir
      setwd(from.dir)
      doc <- "Scenario"
      filezname <- paste0(sector, "_", doc, "_", yy)
      filetocopy <- list.files(from.dir, pattern = filezname)
      file.copy(from = filetocopy, to.dir, overwrite = T)
      
      #Prices_Financial Instruments List_0
      from.dir <- instrument.dir
      setwd(from.dir)
      doc <- "Scenario"
      filezname <- paste0("Prices_Financial_Instruments_List_", yy)
      filetocopy <- list.files(from.dir, pattern = filezname)
      file.copy(from = filetocopy, to.dir, overwrite = T)
      
      
})
