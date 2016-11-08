
#RESTART CODE - redo (may cause sync problem)
{
      main00.dir <- "C:/Users/User/OneDrive/"
      back.dir <- "C:/Users/User/OneDrive/Z_PastTran/"
      
      form.fold <- c("1_Form_EXC", "1_Form_OTC", "1_Form_EXTRA")
      yy_fold <- c("yy_b_YearlyFullTran", "yy_Scorebreakdown", "yy_YearlyBalanceSheet", "yy_YearlyFullTran")
      team0_fold <- c("0_TEAM")
      
      
      all.pastfold <- c(form.fold, yy_fold, team0_fold)
            all.pastfold2 <- paste0(main00.dir, all.pastfold)
      
      unifold <- 
      {x<-as.character(Sys.time())
      x <- gsub(pattern = "[[:punct:]]", replacement = "", x)
      x <- gsub(pattern = " ", replacement = "_", x)
      x
      }; setwd(back.dir); dir.create(unifold)
      
      
      #move folders to backup directory
      v <- vector()
      for(i in 1:length(all.pastfold2)){
            
      v[i] <- file.rename(from = all.pastfold2[i], to = paste0(back.dir, unifold, "/",  all.pastfold[i]))
      
      }
} 

##PROBLEM NOT YET FIXED: For forms, it is linked to the survey, hence cannot move the whole file. consider deleting the content inside?
#may need sophisticated xl package

#Clear transactions, without deleting form xlsx

main00.dir <- "C:/Users/User/OneDrive/"; setwd(main00.dir)

install.packages("openxlsx"); install.packages("devtools")
library(openxlsx)

openXL("testt123.xlsx")

wb <- loadWorkbook(file = "Survey1.xlsx")


deleteData(wb, sheet = 1, cols = 1:100, rows = 2:2000, gridExpand = TRUE)
saveWorkbook(wb, "Survey1.xlsx", overwrite = TRUE)


# #########
# ## Create a new workbook and add a worksheet
# wb <- createWorkbook("Creator of workbook")
# addWorksheet(wb, sheetName = "My first worksheet")
# ## Save workbook to working directory
# saveWorkbook(wb, file = "saveWorkbookExample.xlsx", overwrite = TRUE)

###test
file.rename("C:/Users/User/OneDrive/z_testsurvey/Survey1.xlsx", "C:/Users/User/OneDrive/Z_PastTran/Survey1.xlsx")
