#50_RoutineAdd.R

#DIR
routinef_dir <- "C:/Users/User/Google Drive/z_ALLHM"

#source function
setwd(routinef_dir); source("50_01_RoutineFunctions.R")


###########################
      #SCENARIO
###########################

#DIR
metascenario_dir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_FullTran_Core_Extra_Scenario"

#META-Scenario
      setwd(metascenario_dir)
      meta.scenario <- as.data.frame(read_excel("meta-scenario-fulltran.xlsx"))[,1:15]
      
#yy <- 1
      sfdf <- addsc_ftran_f(meta.scenario, yy)
      
###########################
      #CORE
###########################
      
#DIR
metacore_dir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_FullTran_Core_Extra_Scenario"

#META-Core
setwd(metacore_dir)
metaA.core <- as.data.frame(read_excel("meta-core-fulltran-Alpha.xlsx"))[,1:15]
metaB.core <- as.data.frame(read_excel("meta-core-fulltran-Beta.xlsx"))[,1:15]
      meta.core <- rbind(metaA.core, metaB.core)
      
#yy <- 1
      cfdf <- addsc_ftran_f(meta.core, yy)


###########################
      #EXTRA
###########################

#DIR
metaextra_dir <- "C:/Users/User/Google Drive/z_ALLHM/v5M_FullTran_Core_Extra_Scenario"
      metaspec <- "C:/Users/User/OneDrive/yy_YearlyFullTran"
      
#META-EXTRA01
setwd(metaextra_dir)
meta.extra1 <- as.data.frame(read_excel("meta-extra-fulltran.xlsx", sheet = 1))[,1:15]
meta.extra2 <- as.data.frame(read_excel("meta-extra-fulltran.xlsx", sheet = 2))[,1:15]
      meta.extra <- rbind(meta.extra1, meta.extra2)

#SPEC-user (up till yy)
      setwd(metaspec); specxlsxfiles <- list.files(pattern = 'specfdf')
      {
            speclist <- lapply(specxlsxfiles, FUN = function(f){
                  as.data.frame(read.csv(f))
            })
            specfdf2 <- do.call(rbind, speclist)
      } #out: specfdf2

#yy <- 1
      e0fdf <- adde_ftran_f(specfdf2, meta.extra, yy)

      