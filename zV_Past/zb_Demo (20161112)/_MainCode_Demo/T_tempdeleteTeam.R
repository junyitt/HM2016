if(length(list.files("C:/Users/User/OneDrive/0_TEAM_Demo/Alpha 1/Year 0")) > 0){file.tf <- T}else{file.tf <- F}
print(paste0("File exist? ", file.tf))

rr0 <- readline(prompt="Restart? True = 1, False = 0 \n 0 or 1?")
if(as.numeric(rr0) %in% 1){
      rr1 <- readline(prompt="CONFIRM: Restart? True = 1, False = 0 \n 0 or 1?")
      if(as.numeric(rr1) %in% 1){
               
            mainteam.dir <- team0.dir
            
            teamname.v <- teamname.f()
            yr.v <- paste0("Year ", 0:5)
            
            for(t in teamname.v){
                  for(yrr in yr.v){
                        dirori <- paste0(mainteam.dir, t, "/", yrr)
                        todelete <- dir(dirori, full.names = TRUE)
                        unlink(todelete)
                  }
            }
            
            todelete <- dir(fulltran.dir, full.names = TRUE)
            unlink(todelete)
            
            # todelete <- dir(finalreport.dir, full.names = TRUE)
            # unlink(todelete)
            
            todelete <- dir(distreport.dir, full.names = TRUE)
            unlink(todelete)
            
            print("Restart COMPLETE!")   
            
      }else{
            print("Restart ABORTED...")    
      }
}else{
      print("Restart ABORTED...")  
}





# 
# origindir <- c("c:/origindir")
# targetdir <- c("c/targetdir")
# filestocopy <- c("myfile.doc", "myfile.rda", "myfile.xls", 
#                  "myfile.txt", "myfile.pdf", "myfile.R")
# 
# file.copy(from=filestocopy, to=targetdir, 
#           overwrite = recursive, recursive = FALSE, 
#           copy.mode = TRUE)
# file.remove(filestocopy)

# hi <- "C:/Users/User/Desktop/New folder (2)/New folder"
# todelete <- dir(hi, full.names = TRUE)
# 
# unlink(todelete)
