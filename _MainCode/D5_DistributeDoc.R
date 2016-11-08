#Main parameter
yy2 <- yy+1
teamname12 <- c(paste0("Alpha ", rep(1:6)), paste0("Beta ", rep(1:6)))


#distribute doc

#REPORT - distribute end of year report
report.dir <- distreport.dir ##"C:/Users/User/OneDrive/D_Distribute/R_report/"

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
      file.copy(from = filetocopy, to.dir, overwrite = T)
}) #Alpha 1_Report_1.html

temp2 <- lapply(teamname12, FUN = function(tname){
      teamfold.dir <- paste0(team0.dir, tname, "/Year ", yy2) #team folder dir
      
      #sector 
      if(substr(tname,1,1) == "A"){
            sector <- "Alpha"
      }else if(substr(tname,1,1) == "B"){
            sector <- "Beta"
      }
      
      #to.dir - FIXED
      to.dir <- teamfold.dir
      
      #Prices_Financial Instruments List_0
      from.dir <- instrument.dir
      setwd(from.dir)
      doc <- "Scenario"
      filezname <- paste0("Prices_Financial_Instruments_List_", yy2)
      filetocopy <- list.files(from.dir, pattern = filezname)
      file.copy(from = filetocopy, to.dir, overwrite = T)
      
      
})


print(paste0("Success. Now we are in END OF YEAR ", yy2))
