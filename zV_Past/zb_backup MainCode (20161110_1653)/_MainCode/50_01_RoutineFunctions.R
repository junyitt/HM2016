#50_01_RoutineFunctions.R

###for scenario and core
addsc_ftran.f <- function(meta.scenario.df, yy){
            #FULL-TRAN required variables list
            fullvar1 <- fullvar.f()
            colnames(meta.scenario.df) <- fullvar1
                  
            u <- meta.scenario.df[, "tDate"] %in% yy; add.sc.df <- meta.scenario.df[u,] #select only the particular year scenario transactions
            
if(!any(u)){
      add.sc.df
}else{
      df12_list <- lapply(1:6, FUN = function(i){
            dft1 <- add.sc.df
            dft1[,"TeamName"] <- paste0(add.sc.df[,"TeamName"], " ", i)
            dft1
      }) #duplicate the transactions for Alpha 1 to 6 and Beta 1 to 6
      add2df <- do.call(rbind, df12_list); N <- nrow(add2df) 
      add2df[,"Remarks"] <- add2df[,"tKey"] <- rep(NA,N) ; add2df[,"VL"] <- add2df[,"VLRemarks"] <- rep(1, N)   #add remaining columns
            #add cff
            add2df[,"cff"] <- sapply(X = 1:N, FUN = function(j){ cff.f(cType.c = add2df[j,"cType"], pos1.c = add2df[j,"pos1"])   }) 
      #output fullscenariotran.df 
      add2df
}      
}

######subset specxlsx from 0 up to yy
##chg to GENERAL FUNCTION:
##subfiles.td.f

###for extra yr 0 and 1; spectran
adde_ftran.f <- function(spec.td.df, meta.extra01.df, yy){
      #FULL-TRAN required variables list
      fullvar1 <- fullvar.f()
      varspec1 <- c("ExtraName", "tDate", "Service", "TeamName", "tKey", "Remarks")
            colnames(meta.extra01.df) <- fullvar1; colnames(spec.td.df) <- varspec1

      u <- meta.extra01.df[, "tDate"] %in% yy; 
      meta.e1.df <- meta.extra01.df[u,] #select only the particular year extra (0,1) transactions
      
      u2 <- spec.td.df[, "tDate"] <= yy; u3 <- !(spec.td.df[, "Service"] %in% "None") ; 
      spec.e1.df <- spec.td.df[u2 & u3,] #select specdf, for year 00 up to year yy, chosen service
            
      N <- nrow(spec.e1.df)
      
      eadd_list <- lapply(1:N, FUN = function(k){
            sname <- spec.e1.df[k, "Service"]; srv <- meta.e1.df[, "FIC"]
            u1 <- grepl(paste0("^",sname,"$"), srv)
                  df1 <- meta.e1.df[u1, ]; n <- nrow(df1)
                  df1[,"TeamName"] <- rep(spec.e1.df[k, "TeamName"], n)
                        df1
      }) #output a list containing fulltrandf for each spectran
      
      eadd1.df <- do.call(rbind, eadd_list); N <- nrow(eadd1.df)
      if(N > 0){
            eadd1.df[,"tKey"] <- eadd1.df[,"Remarks"] <- rep(NA, N) ; eadd1.df[,"VL"] <- eadd1.df[,"VLRemarks"] <- rep(1, N)   #add remaining columns
            eadd1.df[,"cff"]  <- sapply(1:nrow(eadd1.df), FUN = function(j){  cff.f(eadd1.df[j, "cType"], eadd1.df[j, "pos1"])     })
      }
      eadd1.df
}
