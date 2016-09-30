source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")
library(reshape2); library(knitr) #install.packages(rmarkdown);
library(ggplot2); library(gridExtra); require(cowplot)

#set year to "collect" data from, i.e. EXC_0 >> yy <- 0
# yy <- 0

#Section 4 to 8 (fdf) 
{
      
############################
# 4_ConvUser
############################
{
EXCraw2full.R_dir <- "C:/Users/User/Google Drive/z_ALLHM"
OTCraw2full.R_dir <- "C:/Users/User/Google Drive/z_ALLHM"
EXTRAraw2full.R_dir <- "C:/Users/User/Google Drive/z_ALLHM"

##EXCHANGE
setwd(EXCraw2full.R_dir); source("01_EXC_raw_full.R")
      #output: 
      head(EXCfulltrandf)

##OTC
setwd(OTCraw2full.R_dir); source("02_OTC_raw_full.R")
      #output: 
      head(OTCfulltrandf)
      
##EXTRA -> SPECIAL links to META-FULL
setwd(EXTRAraw2full.R_dir); source("03_EXTRA_raw_full.R")
      #output:
      head(spectrandf)  #yr0, yr1 PV
      head(extrafulltrandf) #yr2 arbitrage, yr3 bond, yr4 harryM - cash
      
###COMBINED########33333333333333
      convclass.f1 <- function(df){
            N <- ncol(df)
            if(length(N) == 0){
            }else{
                  for(i in 1:N){
                        df[,i] <- as.character(df[,i])
                  }
                  g <- c("kPrice", "Units", "tDate", "mDate", "VL")
                  for(h in g){
                        df[,h] <- as.numeric(df[,h])
                  }
                  df
            }
      }
      EXCfulltrandf <- convclass.f1(EXCfulltrandf)
      OTCfulltrandf <- convclass.f1(OTCfulltrandf)
      extrafulltrandf <- convclass.f1(extrafulltrandf)
      ufullfdf <- rbind(EXCfulltrandf, OTCfulltrandf, extrafulltrandf)
      
      
      uspecfdf <- spectrandf
} #output: ufullfdf, uspecfdf

      ############################
      # 4_ConvUser -- backup files, move files
      ############################
      {
            fulldf_csvdir <- "C:/Users/User/OneDrive/yy_YearlyFullTran"; b_fulldf_csvdir <- "C:/Users/User/OneDrive/yy_b_YearlyFullTran"
            setwd(fulldf_csvdir); write.csv(ufullfdf, paste0("ufullfdf_", yy, ".csv"), row.names = F); write.csv(uspecfdf, paste0("uspecfdf_", yy, ".csv"), row.names = F)
            setwd(b_fulldf_csvdir); write.csv(ufullfdf, paste0("b_ufullfdf_", yy, ".csv"), row.names = F); write.csv(uspecfdf, paste0("b_uspecfdf_", yy, ".csv"), row.names = F)
      } 
      
############################
# 5_Routine
############################
{
      routine5.dir <- ("C:/Users/User/Google Drive/z_ALLHM")
      # yy <- 0    
      #Routine add - core, scenario, and extra yr 0/1 full transactions
      setwd(routine5.dir); source("50_RoutineAdd.R") #output: sfdf, cfdf, e0fdf
      rfdf <- rbind(sfdf, cfdf, e0fdf)
} #Output: Rfdf
      
      ############################
      # 5_Routine -- backup files, move files
      ############################
      {
            rfdf_csvdir <- "C:/Users/User/OneDrive/yy_YearlyFullTran"; b_rfdf_csvdir <- "C:/Users/User/OneDrive/yy_b_YearlyFullTran"
            setwd(rfdf_csvdir); write.csv(rfdf, paste0("rfullfdf_", yy, ".csv"), row.names = F) 
            setwd(b_rfdf_csvdir); write.csv(rfdf, paste0("b_rfullfdf_", yy, ".csv"), row.names = F) #backup   
      }

      ##############################################
            #5A: fdf_y - fulltrandf of the year yy
      ##############################################
      {
            ufullfdf.c <- ufullfdf; rfdf.c <- rfdf
                  ufullfdf.c[] <- lapply(ufullfdf, as.character)
                  rfdf.c[] <- lapply(rfdf, as.character)
                        fdf_y <- rbind(ufullfdf.c, rfdf.c)
            
            suppressWarnings({
                  col <- c("kPrice", "Units", "tDate", "mDate")
                  fdf_y[,col] <- sapply(col, FUN = function(j){as.numeric(fdf_y[,j])})
                  
            }) #as.numeric numeric columns
      } #output: fdf_y

############################
# 6_1BGNcalc
###########################
{
      #DIR
      bgn6.dir <- "C:/Users/User/Google Drive/z_ALLHM"
      
      setwd(bgn6.dir)
      source("60_BGN1.R") #output fdf with bgn columns - fdf_y6
} #output: fdf_y6

############################
# 7_BGNstloan & 8_2BGNcalc
###########################
{
      #DIR
      bgn7.dir <- "C:/Users/User/Google Drive/z_ALLHM"
      
      setwd(bgn7.dir)
      source("70_BGNstloan.R") #output fdf_y7
} #output: fdf_y7

      ############################
      # 7_BGN2 -- writecsv, backup files, move files
      ############################
      {
            fdf_csvdir <- "C:/Users/User/OneDrive/yy_YearlyFullTran"; b_fdf_csvdir <- "C:/Users/User/OneDrive/yy_b_YearlyFullTran"
            setwd(fdf_csvdir); write.csv(fdf_y7, paste0("zfdf_",yy,".csv"), row.names = F)
            setwd(b_fdf_csvdir); write.csv(fdf_y7, paste0("b_zfdf_",yy,".csv"), row.names = F)
      }     #"z_fdf_yy.csv"

############################
# 8_ENDcalc
###########################
{
      #DIR
      end8.dir <- "C:/Users/User/Google Drive/z_ALLHM"
      
      setwd(end8.dir)
      source("80_END1.R") #output fdf8_td
      fdf8_td[,"TrackNo"] <- trackno_f(fdf8_td)
} #output: fdf8_td
      
      ############################
      # 8_ENDcalc -- writecsv, backup files, move files
      ############################
      {
            fdftd_csvdir <- "C:/Users/User/OneDrive/yy_YearlyFullTran"; b_fdftd_csvdir <- "C:/Users/User/OneDrive/yy_b_YearlyFullTran"
            setwd(fdftd_csvdir); write.csv(fdf8_td, paste0("fdf8_td_",yy,".csv"), row.names = F)
            setwd(b_fdftd_csvdir); write.csv(fdf8_td, paste0("b_fdf8_td_",yy,".csv"), row.names = F)
      }     #"fdf8_td_",yy,".csv"
      
      
} #fdf8_td



{
############################
# A1_Report_prep_list
###########################

#DIR-A1 
A1.dir <- "C:/Users/User/Google Drive/z_ALLHM"


setwd(A1.dir); source("A1_01_Core.R") #get 10 list for report? - aggregate them! >write.csv //rmarkdown      

# tname <- "Alpha 1"
# temp <- lapply(1:1, FUN = function(i){
#       i <<- i
#       tname <- teamname12[i]; fname <- paste0(tname, "_Report_", yy+1)
#       tname <- tname #main parameter to Report.Rmd
#       # options(scipen=12)
#       rmarkdown::render(input = "Report.Rmd", output_format = "html_document", output_file = paste0(fname, ".html"), output_dir = out.report.dir)
#       
# })

} #Report prep



{
############################
# A2_Report_output
###########################

#output-report-DIR
out.report.dir <- "C:/Users/User/OneDrive/D_Distribute/R_report"

setwd(A1.dir) #for Report.Rmd
# temp <- lapply(3, FUN = function(i){
# temp <- lapply(1:length(teamname12), FUN = function(i){
for(i in 8){
      i <<- i
      tname <<- teamname12[i]; fname <<- paste0(tname, "_Report_", yy+1)
      tname <<- tname #main parameter to Report.Rmd
      
      rmarkdown::render(input = "Report.Rmd", output_format = "html_document", output_file = paste0(fname, ".html"), output_dir = out.report.dir)

# })
}
      
} #REPORT: output 12 html report to R_Report, waiting to be distributed

