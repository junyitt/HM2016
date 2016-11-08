#_HMProcessFlow.R
##dependencies
#01_EXC_raw_full.R, 02_OTC_raw_full.R, 03_EXTRA_raw_full.R, 50_RoutineAdd.R
#60_BGN1.R, 70_BGNstloan.R, 80_END1.R, 90_Balsh.R

#LINE 9  #LINE 45  #LINE 84  #LINE 95  #LINE 116
#Section 4 to 9 (fdf)   

      
############################
# 4_ConvUser
############################
{

##EXCHANGE
setwd(maincode.dir); 
source("01_EXC_raw_full.R") #EXCraw.df, ficmeta.df, tkey.df  #EXCfulltran.df
      
##OTC
setwd(maincode.dir); 
source("02_OTC_raw_full.R") #preOTCraw.df, OTCraw.df, meta.und.price.df, #OTCfulltran.df
      
##EXTRA -> SPECIAL links to META-FULL
setwd(maincode.dir); 
source("03_EXTRA_raw_full.R")  #spectran.df, extrafulltran.df

###COMBINE into full-user-df###
EXCfulltran.df <- convclass.f1(EXCfulltran.df)
OTCfulltran.df <- convclass.f1(OTCfulltran.df)
extrafulltran.df <- convclass.f1(extrafulltran.df)
      ufull.yy.df <- rbind(EXCfulltran.df, OTCfulltran.df, extrafulltran.df)
      uspec.yy.df <- spectran.df


      ############################
      # 4_ConvUser -- backup files, move files
      ############################
            setwd(fulltran.dir); 
            write.csv(ufull.yy.df, paste0("ufull_", yy, ".csv"), row.names = F); 
            write.csv(uspec.yy.df, paste0("uspec_", yy, ".csv"), row.names = F)
            
} 
#ufull_yy.csv #"uspec_yy.csv"
#output: ufull.yy.df, uspec.yy.df    #maxsharpe.c

############################
# 5_Routine
############################
{
      #Routine add - core, scenario, and extra yr 0/1 full transactions
      setwd(maincode.dir); source("50_RoutineAdd.R") #output: s.yy.df, c.yy.df, e.yy.df
      r.yy.df <- rbind(s.yy.df, c.yy.df, e.yy.df) #r.yy.df

      ############################
      # 5_Routine -- backup files
      ############################
      setwd(fulltran.dir); write.csv(r.yy.df, paste0("rfull_", yy, ".csv"), row.names = F) 

} #s.yy.df, c.yy.df, e.yy.df
# "rfull_yy.csv"
#output: #r.yy.df
##############################################
# 5A: f0.yy.df - fulltran.df of the year yy
##############################################
{
      ufull.yy.df[] <- lapply(ufull.yy.df, as.character)
      r.yy.df[] <- lapply(r.yy.df, as.character)
      
      f0.yy.df <- rbind(ufull.yy.df, r.yy.df)
      suppressWarnings({
            f0.yy.df <- convclass.f1(f0.yy.df) #as.numeric numeric columns
      }) 
      
      #write.csv ###
      setwd(fulltran.dir);write.csv(f0.yy.df, paste0("urfull_", yy, ".csv"), row.names = F)
      
} #output: f0.yy.df
#output: urfull_yy.csv
      
##############################################
# 5B: f0.td.df - fulltran.df from 0 to yy
##############################################
{
# f1.td.df <- f0.yy.df ####!!!!!TEMP!!!

#read f0.yy.df (up till yy) assign to f1.td.df
setwd(fulltran.dir); f0df.csv.files <- list.files(pattern = '^urfull_')
f0df.csv.files <- subfiles.td.f(f0df.csv.files, yy) #subset 0:yy  #subset up to yy (td)(.files)
      urfull.td.list <- lapply(f0df.csv.files, FUN = function(f){
            read.csv(f, stringsAsFactors = F)
      })
      f1.td.df <- do.call(rbind, urfull.td.list) 
      f1.td.df <- convclass.f1(f1.td.df) #convert columns to char then numerics
#####

}#out: f1.td.df

############################
# 6_1BGNcalc   #INPUT f1.td.df (gonna use =tdate, to calculate pro0, and <= mdate to calculate proT) else 0 method
###########################
{
      setwd(maincode.dir)
      source("60_BGN1.R") #output fdf with bgn columns - bgn1.td.df
} #output: bgn1.td.df

############################
# 7_BGNstloan & ADD LOAN
###########################
{
      setwd(maincode.dir)
      source("70_BGNstloan.R") #output fdf_y7
      
} #output: bgn2.td.df

############################
# 8_ENDcalc
###########################
{
      setwd(maincode.dir)
      source("80_END1.R")
      end3.td.df[,"TrackNo"] <- trackno.f(end3.td.df)
      end3.td.df[,"classf"] <- toupper(end3.td.df[,"classf"])
            #BACKUP:
            setwd(fulltran.dir); write.csv(end3.td.df, paste0("b_end3_td_", yy, ".csv"), row.names = F)
} 
#output: end3.td.df
#"b_end3_td_yy.csv"

      # View(end3.td.df)
      
############################
# 9_balsheet_yy+1, write.csv(balsh_y)
###########################
{
setwd(maincode.dir); source("90_Balsh.R") #output: balsh_y2.df
setwd(fulltran.dir); write.csv(balsh_y2.df, paste0("meta-balancesheet-", yy+1, ".csv"), row.names = F)
} #balsh_y,  balsh_y2.df  
#meta-balancesheet-yy2.csv"

      # View(balsh_y2.df)


