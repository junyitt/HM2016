#70_01_BGN2Functions.R

#compute netcash right after transactions of the 12 teams - return vector
netcash.v.f  <- function(teamname12, bgn1.td.df, balsh_y.df){
      netpro12 <- sapply(teamname12, FUN = function(name){
            u1 <- bgn1.td.df[, "TeamName"] %in% name; u2 <- balsh_y.df[, "TeamName"] %in% name
            npro1 <- sum(bgn1.td.df[u1, "pro0"], na.rm = T); cash1 <- balsh_y.df[u2, "Cash"]
            npro1 + cash1
      })
      
      netpro12
}

#add loan if ncash < 0 
addloan.f <- function(ncash, tName, loanlength = 1, minbalance = 1e6, yy){
      if(ncash < 0){
            TrackNo <- NA
            classf <- "OTC"
            FIC <- NA
            TeamName <- tName
            cParty <- "3rd Party Institution"
                  cType <- "Loan"
                  Underlying <- NA
                  Currency <- "MYR"
                  kPrice <- 0.12
            pos1 <- "Borrow"
            cff <- -1
            Units <- minbalance - ncash
                  tDate <- yy
                  mDate <- yy+loanlength
                  tKey <- NA
            Remarks <- "additional loan borrowed due to negative cash"
            VL <- 1
            VLRemarks <- 1
                  ss0 <- NA
                  exss0 <- 1
                  pro0 <- abs(Units)
                  tfee <- 0
                  netpro0 <- abs(Units)
            data.frame(TrackNo, classf, FIC, TeamName, cParty, 
                       cType, Underlying, Currency, kPrice, 
                       pos1, cff, Units, 
                       tDate, mDate, tKey, 
                       Remarks, VL, VLRemarks,
                       ss0, exss0, pro0, tfee, netpro0)    
            
      }else{
            
      }
}

#out: bgn2.td.df.f (with loan added, still at bgn stage; 23 columns) 
#function: input netcash.v, teamname12, bgn1.td.df, length of loan, min balance, yy ;;    output short term loan bgn2.td.df
bgn2.td.df.f <- function(netcash.v, teamname12, bgn1.td.df, loanlength = 1, minbalance = 1e6, yy){
      addloan.list <- lapply(1:length(netcash.v), FUN = function(j){
            addloan.f(netcash.v[j], teamname12[j], loanlength, minbalance, yy)
      })
      addloan.df <- do.call(rbind, addloan.list)
      rbind(bgn1.td.df, addloan.df)
}