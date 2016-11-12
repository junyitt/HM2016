#50_RoutineAdd.R

#DIR
#maincode.dir, meta.dir, fulltran.dir

#General Functions?
#importmeta.f

#Internal Function
setwd(maincode.dir); source("50_01_RoutineFunctions.R")
#addsc_ftran.f
#subfiles.td.f, adde_ftran.f

      #ENVIRONMENT OUTPUT:
      #s.yy.df, c.yy.df, e.yy.df

###########################
      #SCENARIO
###########################

#META-Scenario
      setwd(meta.dir)
      meta.scenario.df <- importmeta.f(meta.dir, "meta-scenario-fulltran.xlsx")

      s.yy.df <- addsc_ftran.f(meta.scenario.df, yy)
      
###########################
      #CORE
###########################
#META-Core
setwd(meta.dir)
metaA.core <- importmeta.f(meta.dir, "meta-core-fulltran-Alpha.xlsx")   
metaB.core <- importmeta.f(meta.dir, "meta-core-fulltran-Beta.xlsx")   
      meta.core.df <- rbind(metaA.core, metaB.core)
      
      c.yy.df <- addsc_ftran.f(meta.core.df, yy)

###########################
      #EXTRA
###########################

#DIR #fulltran.dir
      
#META-EXTRA01
setwd(meta.dir)
meta.extra1 <- as.data.frame(read_excel("meta-extra-fulltran.xlsx", sheet = 1))[,1:18]
meta.extra2 <- as.data.frame(read_excel("meta-extra-fulltran.xlsx", sheet = 2))[,1:18]
      meta.extra01.df <- rbind(meta.extra1, meta.extra2)

#SPEC-user (up till yy)
#Read spec-user-"raw"df    
setwd(fulltran.dir); specxlsx.files <- list.files(pattern = '^uspec_')   #!!!!!!!NOT XLSX, BUT CSV - leave variable name unchanged for now!
subspecxlsx.files <- subfiles.td.f(specxlsx.files, yy) #subset 0:yy
      spec.list <- lapply(subspecxlsx.files, FUN = function(f){
            read.csv(f, stringsAsFactors = F)
      })
      spec.td.df <- do.call(rbind, spec.list) #out: spec.td.df

#Routine add extra transactions (only for yy = 0 and yy=1)      
      e.yy.df <- adde_ftran.f(spec.td.df, meta.extra01.df, yy)

      