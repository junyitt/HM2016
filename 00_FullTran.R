#Done: Section A, as of 20160913
#Next: Section B: evaluate settlement at time T 
#DIR
      fulltrandir <- "C:/Users/User/Google Drive/z_ALLHM"

#step 0: source functions
source("C:/Users/User/Google Drive/z_ALLHM/00_01_functionsA.R")

#first assign the full transactions.csv to df
      setwd(fulltrandir)
      df <- as.data.frame(read_excel("fulltransaction.csv"))

#assign values to the variable name, e.g. Underlying <- df[,"Underlying"]
colName <- colnames(df)
for(i in 1:length(colName)){assign(colName[i], value = df[, colName[i]])}

#create empty vector for the target variable to be calculated
      newvar <- c("S0", "exS0", "Pro", "Tfee", "NetPro")
      for(i in 1:length(newvar)){assign(newvar[i], value = vector())}

#SECTION A: S0, exS0, Pro, Tfee, NetPro
N <- nrow(df)
cff <- vector()
      for(i in 1:N){
            cff[i] <- cff_f(cType[i], pos1[i])
            S0[i] <- S0f(Underlying[i], tDate[i])
            exS0[i] <- exS0f(Currency[i], tDate[i])
            Pro[i] <- Prof(FIC[i], cType[i], cff[i], Units[i], S0[i], exS0[i])
            Tfee[i] <- Tfeef(FIC[i], cType[i], Units[i], Pro[i], S0[i], exS0[i])
            NetPro[i] <- Netprof(Pro[i], Tfee[i])
            #2: df2 <- merge(x = df1, y = dfmeta, by = "FIC", all.x = TRUE)
      }
      
      #final s0 df:
      df0 <- cbind(df, S0, exS0, Pro, Tfee, NetPro)
      View(df0)
      
#create empty vector for the target variable to be calculated
      newvar <- c("ST", "exST", "ProT", "MVT")
      for(i in 1:length(newvar)){assign(newvar[i], value = vector())}
      
#SECTION B: yearly, ST, exST, ProT, MVT, T=1,2,3,4,5      
      for(j in 1:N){
                  ST[j] <- STf(Underlying[j], yy)
                  exST[j] <- exSTf(Currency[j], yy)
                  ProT[j] <- ProTf(FIC[j], cType[j], cff[j], Units[j], Pro[j], kPrice[j], tDate[j], mDate[j], ST[j], exST[j], yy)
                  MVT[j] <- MVTf(FIC[j], cType[j], cff[j], Units[j], Pro[j], kPrice[j], tDate[j], mDate[j], ST[j], exST[j], yy)
            }
      
      #final ST df
      dfT <- cbind(df0, ST, exST, ProT, MVT)
      View(dfT)
      
      
      
      

