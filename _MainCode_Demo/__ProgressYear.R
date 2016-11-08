lock.tf <- F

if(checkcutoff.f(yy, raw_EXC.dir, raw_OTC.dir, raw_EXTRA.dir)){
      if(preyy == 0){
            yy <- 0
            lock.tf <- T
      }else if(preyy %in% DONE){
            print("REPEAT YEAR: stop()")
            lock.tf <- F
      }else if(preyy > max(DONE) + 1){
            print("SKIP YEAR: stop()")
            lock.tf <- F
      }else{
            yy <- yy + 1
            lock.tf <- T
      }
}else{
      checkcutoff.f(yy, raw_EXC.dir, raw_OTC.dir, raw_EXTRA.dir)
      print("CUTOFF ERROR: stop()")     
}

if(lock.tf){   
      tryCatch({
            setwd(maincode.dir); source("_HMProcessFlow.R")
            setwd(maincode.dir); source("_HMProcessFlow_Report_2.R")
            setwd(maincode.dir); source("B1_PreReport.R")
            setwd(maincode.dir); source("C1_Report.R")
      
            setwd(maincode.dir); source("D_DistributeDoc.R")
            DONE <- c(DONE, yy)
            
      }, error = function(e){
            print(paste0("Error Message:  ", geterrmessage()))
            
            d0 <- readline(prompt="An error occur. Proceed to distribute 3 docs? True = 1, False = 0 \n 0 or 1?")
            if(as.numeric(d0) %in% 1){
                  d1 <- readline(prompt="CONFIRMATION: Proceed to distribute 3 docs? True = 1, False = 0 \n 0 or 1?")
                        if(as.numeric(d1) %in% 1){
                              setwd(maincode.dir); source("D_DistributeDoc.R") #distribute
                              print("3 Docs DISTRIBUTED")      
                        }else{
                              print("3 Docs NOT DISTRIBUTED")    
                        }
            }else{
                  print("3 Docs NOT DISTRIBUTED")  
            }
            
      })
}

      