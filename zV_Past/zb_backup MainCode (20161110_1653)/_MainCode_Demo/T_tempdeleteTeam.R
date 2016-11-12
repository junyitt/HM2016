# MAINCODEDIR <- "C:/Users/User/Google Drive/z_ALLHM/_MainCode"
# setwd(MAINCODEDIR)
# source("_HM_alldirectory.R"); source("_HM_allfunction.R")
# source("00_createmeta_f.R")

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
