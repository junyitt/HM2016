


############################
# A1_Report_prep_list
###########################
{
#DIR-A1 
A1.dir <- "C:/Users/User/Google Drive/z_ALLHM"


setwd(A1.dir); source("A1_01_Core.R") #get 10 list for report? - aggregate them! >write.csv //rmarkdown      
      
      
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
      
      
      loop <- 1:12
      for(i in loop){
            i <<- i
            tname <<- teamname12[i]; fname <<- paste0(tname, "_Report_", yy+1)
            tname <<- tname #main parameter to Report.Rmd
            
            rmarkdown::render(input = "Report.Rmd", output_format = "html_document", output_file = paste0(fname, ".html"), output_dir = out.report.dir)
            
            # })
      }
      
} #REPORT: output 12 html report to R_Report, waiting to be distributed

