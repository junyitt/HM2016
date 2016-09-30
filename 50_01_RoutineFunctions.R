#50_01_RoutineFunctions.R

###for scenario and core
addsc_ftran_f <- function(metadf, yy){
      {      
            #FULL-TRAN required variables list
            fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                          "cType", "Underlying", "Currency", "kPrice", 
                          "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                          "VL", "VLRemarks")
            
            varadd1 <- c("TrackNo", "classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                         "cType", "Underlying", "Currency", "kPrice", 
                         "pos1", "Units", "tDate", "mDate")
      }#fullvar1, varadd1
      colnames(metadf) <- varadd1
      u <- metadf[, "tDate"] == yy; add1df <- metadf[u,] #select only the particular year scenario transactions
if(sum(u) == 0){
}else{
      df12_list <- lapply(1:6, FUN = function(i){
            dft1 <- add1df
            dft1[,"TeamName"] <- paste0(add1df[,"TeamName"], " ", i)
            dft1
      }) #duplicate the transactions for Alpha 1 to 6 and Beta 1 to 6
      add2df <- do.call(rbind, df12_list); N <- nrow(add2df) 
      tKey <- Remarks <- rep(NA,N) ; VL <- VLRemarks <- rep(1, N)   #add remaining columns
      
      #output fullscenariotrandf 
      data.frame(add2df, tKey, Remarks, VL, VLRemarks)      
}      
}


###for extra yr 0 and 1; spectran
adde_ftran_f <- function(specdf, metadf, yy){
      {      
            #FULL-TRAN required variables list
            fullvar1 <- c("TrackNo","classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                          "cType", "Underlying", "Currency", "kPrice", 
                          "pos1", "Units", "tDate", "mDate", "tKey", "Remarks",
                          "VL", "VLRemarks")
            
            varadd1 <- c("TrackNo", "classf", "FIC", "rClass", "sClass", "TeamName", "cParty", 
                         "cType", "Underlying", "Currency", "kPrice", 
                         "pos1", "Units", "tDate", "mDate")
            varspec1 <- c("ExtraName", "tDate", "Service", "tName", "tKey", "Remarks")
      
      }#fullvar1, varadd1
      colnames(metadf) <- varadd1; colnames(specdf) <- varspec1
      u <- metadf[, "tDate"] == yy; meta1df <- metadf[u,] #select only the particular year scenario transactions
      u2 <- specdf[, "tDate"] <= yy; u3 <- !(specdf[, "Service"] == "None") ; spec1df <- specdf[u2 & u3,] #select specdf, for year 00 up to year yy
            N <- nrow(spec1df)
      
      eadd_list <- lapply(1:N, FUN = function(k){
            sname <- spec1df[k, "Service"]; srv <- meta1df[, "FIC"]
            u1 <- grepl(paste0("^",sname,"$"), srv); u2 <- meta1df[,"tDate"] == yy
                  df1 <- meta1df[u1 & u2, ]; n <- nrow(df1)
                  df1[,"TeamName"] <- rep(spec1df[k, "tName"], n)
                        df1
      })
      
      eadd1df <- do.call(rbind, eadd_list); N <- nrow(eadd1df)
      tKey <- Remarks <- rep(NA, N) ; VL <- VLRemarks <- rep(1, N)   #add remaining columns
      
      #output fullscenariotrandf 
      data.frame(eadd1df, tKey, Remarks, VL, VLRemarks)      
      
}
