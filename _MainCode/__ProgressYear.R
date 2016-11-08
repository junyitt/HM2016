lock.tf <- F

if(checkcutoff.f(yy, raw_EXC.dir, raw_OTC.dir, raw_EXTRA.dir)){
      lock.tf <- T
}else{
      lock.tf <- F
      print("CUTOFF ERROR: stop()")     
}

u1 <- u2 <- 0
if(lock.tf){   
      tryCatch({
            u1 <- readline(prompt=paste0("         Now yy = ", yy, "\nLast Success.yy = ", success.yy, "\n\nContinue run? True = 1, False = 0 \n0 or 1?"))
            if(u1 %in% "1"){   
                  u2 <- readline(prompt=paste0("CONFIRM?", "\n\n         Now yy = ", yy, "\nLast Success.yy = ", success.yy, "\n\nCONFIRM: continue run? True = 1, False = 0 \n 0 or 1?"))
                  if(u2 %in% "1"){
                        setwd(maincode.dir); source("_HMProcessFlow.R")
                        setwd(maincode.dir); source("_HMProcessFlow_Report_2.R")
                        setwd(maincode.dir); source("B1_PreReport.R")
                        setwd(maincode.dir); source("C1_Report.R")
                        if(!yy %in% 4){
                              setwd(maincode.dir); source("D_DistributeDoc.R")
                        }else{
                              setwd(maincode.dir); source("D5_DistributeDoc.R")
                        }
                        success.yy <<- yy  
                  }else{print(paste0("Code not run, yy = ", yy))}
            }else{print(paste0("Code not run, yy = ", yy))}
      }, error = function(e){
            traceback()
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

      