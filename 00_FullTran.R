#final transactions calculation method?

# FIC <- df[,"FIC"]
# 

#assign values to the variable name, e.g. Underlying <- df[,"Underlying"]
colName <- colnames(df)
for(i in 1:length(colName)){assign(colName[i], value = df[, colName[i]])}

#loop through each row, test cType
N <- nrow(df)

newvar <- c("S0", "exS0", "Pro", "Tfee", "NetPro")
for(i in 1:length(newvar)){assign(newvar[i], value = vector())}

#SECTION A: S0, exS0, Pro, Tfee, NetPro
for(i in 1:N){
#1st - input FIC, 
#if NA, proceed to other method to calculate spot, exrate, proceed, tfee and net pro
      cff <- vector()
      if(is.na(FIC[i])){
            cff[i] <- cff_f(cType[i], pos1[i])
            S0[i] <- S0f(Underlying[i], tDate[i])
            exS0[i] <- exS0f(Currency[i], tDate[i])
            Pro[i] <- Prof(cType[i], cff[i], Units[i], S0[i], exS0[i])
            Tfee[i] <- Tfeef(cType[i], Units[i], Pro[i], S0[i], exS0[i])
            NetPro[i] <- Netprof(Pro[i], Tfee[i])
      }else{
            #refer to meta of that FIC
            #1: dfmeta <- read.csv("meta.csv")  
            cc <- dfmeta[,"FIC"] == FIC[i]
            cff[i] <- cff_f(cType[i], pos1[i])
            S0[i] <- dfmeta[cc,"S0"]
            exS0[i] <- exS0f(Currency[i], tDate[i])
            Pro[i] <- -1*cff[i]*Units[i]*dfmeta[cc, "Proceed"]
            Tfee[i] <- abs(Units[i]*dfmeta[,"Tfee"])
            NetPro[i] <- Netprof(Pro[i], Tfee[i])
      }
            #2: df2 <- merge(x = df1, y = dfmeta, by = "FIC", all.x = TRUE)
}

#SECTION B: yearly, ST, exST, ProT, NetProT, T=1,2,3,4,5
            


#1st - function: input cType, return function to calculate spot, exrate, proceed, tfee, and netpro
#2nd - then apply that function on that rows input: underlying, currency, kprice, position, units, tdate, mdate
      #caveat on banker 1 meta-scenario put option

