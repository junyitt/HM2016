#_HM_allfunction.R

library0 <- function(y){
      g <- sapply(X = y, FUN = function(x){
            tryCatch(
                  expr = {library(x, character.only = T)},
                  warning = function(w){},
                  error = function(e){install.packages(x, dependencies = T); library(x, character.only = T)},
                  finally = {}
            )
            
      })
      rm(g)
      print("Packages loaded!")
      rbind(y)
}

packages <- c("rmarkdown", "xlsx", "gridExtra", "knitr", "reshape2", "readxl", "dplyr", "lubridate", "ggplot2", "TTR", "devtools", "pander", "knitr", "xtable")
library0(packages)


library(reshape2); library(knitr) #install.packages(rmarkdown);
library(ggplot2); library(gridExtra); require(cowplot)
library(xlsx)

#CONVERT FACTOR CLASS TO CHARACTER CLASS
convclass.f1 <- function(df){
      N <- ncol(df)
      if(length(N) == 0){
            print("convclass.f1 - problem converting class of df")
      }else{
            for(i in 1:N){
                  df[,i] <- as.character(df[,i])
            }
            g <- c("cff", "kPrice", "Units", "tDate", "mDate", "VL")
            for(h in g){
                  df[,h] <- as.numeric(df[,h])
            }
            df
      }
}

#return the subset of rawdf
cutoff.f <- function(rawdf){
      tryCatch({
            N <- nrow(rawdf)
            startcode <- "startjan"; endcode <- "cutoffapril"
                  a <- grep(pattern = startcode, x = rawdf[,"Remarks"]); a <- max(a)
                  b <- grep(pattern = endcode, x = rawdf[,"Remarks"]); b <- max(b)
                  if(b-a < 2){
                        rawdf[rep(F,N),]
                  }else{
                        rawdf[(a+1):(b-1),]
                  }
            
      }, error = function(e){
            rawdf <<- rawdf
            print("cutoff.f - start/cutoff transactions problem!")
      })
      
           
}

#return true or false if cutoff transactions exist
cutoff.TF.f <- function(rawdf){
            N <- nrow(rawdf)
            startcode <- "startjan"; endcode <- "cutoffapril"
            a <- grep(pattern = startcode, x = rawdf[,"Remarks"]); a2 <- max(a)
            b <- grep(pattern = endcode, x = rawdf[,"Remarks"]); b2 <- max(b)
            a.last <- max(a); b.last <- max(b)
            a.last2 <- a[length(a)-1]; b.last2 <- b[length(b)-1]
            
            c1 <- a.last > b.last2
            c2 <- a.last < b.last
            c1 & c2
}

checkcutoff.f <- function(yy, raw_EXC.dir, raw_OTC.dir, raw_EXTRA.dir){
      
      setwd(raw_EXC.dir); EXCfile <- list.files(); EXCfile2 <- paste0(raw_EXC.dir, "/", EXCfile[grepl(yy, EXCfile)])
      setwd(raw_OTC.dir); OTCfile <- list.files(); OTCfile2 <- paste0(raw_OTC.dir, "/", OTCfile[grepl(yy, OTCfile)])
      setwd(raw_EXTRA.dir); EXTRAfile <- list.files(); EXTRAfile2 <- paste0(raw_EXTRA.dir, "/", EXTRAfile[grepl(yy, EXTRAfile)])
      
      rfile <- c(EXCfile2, OTCfile2, EXTRAfile2)
      
      tf <- sapply(rfile, function(file){
            rawdf <- as.data.frame(read_excel(file))
            cutoff.TF.f(rawdf)
      })
      
      
      if(all(tf)){
            return(TRUE)
      }else{
            errorfilename <- rfile[!tf]; print(errorfilename)
            return(FALSE)
      }
      
}


#return the value of the desired variable, input: FIC, metadf, variablename  -- EXC and EXTRA
returnmetaval.f <- function(FIC.c, meta.df, vname.c){
      uu <- meta.df[,"FIC"] %in% FIC.c
      #vname can take: Underlying, Currency, kPrice, tDate, mDate
      tryCatch({
            meta.df[uu, vname.c]
      }, error = function(e){
            NA
            print("returnmetaval.f - error returning value of desired variable from meta.df")
      })
}

#Import any meta
importmeta.f <- function(meta.dir, fname){
      tryCatch({
            setwd(meta.dir)
            as.data.frame(read_excel(fname))
      }, error = function(e){
            meta.dir <<- meta.dir
            fname <<- fname
            print("importmeta.f - problem importing meta-? file")
            print(meta.dir)
            print(fname)
            
      })
}

#import balance sheet
importbalsh.f <- function(meta.dir, fulltran.dir, yy){
      #read balance sheet
      {
            if(yy == 0){
                  setwd(meta.dir)
                  balsh_y <- as.data.frame(read_excel("meta-balancesheet-0.xlsx"))
            }else{
                  setwd(fulltran.dir)
                  balsh_y <- read.csv(paste0("meta-balancesheet-", yy, ".csv"))
            }
            colnames(balsh_y) <- c("TeamName", "PPE", "FinAsset", "Cash", "ShareCap", "RE", "Loan", "NAV")
      }
      balsh_y
}

#import final score breakdown
importscorebr.f <- function(meta.dir, fulltran.dir, yy){
      #read balance sheet
      {
            if(yy == 0){
                  setwd(meta.dir)
                  scorebr.td.df <- as.data.frame(read_excel("meta-scorebreakdown-td-0.xlsx"))
            }else{
                  setwd(fulltran.dir)
                  scorebr.td.df <- read.csv(  paste0("aa-scorebreakdown-td-", yy, ".csv")   )
            }
            colnames(scorebr.td.df) <- c("TeamName", "Hedging", "CashFlow", "ExtraEvent", "NAV", "Year")
      }
      scorebr.td.df
}

#fullvar.f 
fullvar.f <- function(){
      
      fullvar <- c("TrackNo","classf", "FIC", "TeamName", "cParty", 
                   "cType", "Underlying", "Currency", "kPrice", 
                   "pos1", "cff", "Units", 
                   "tDate", "mDate", "tKey", 
                   "Remarks", "VL", "VLRemarks")
            x <- c(rep("A", 5), rep("B", 4), rep("C", 3), rep("D", 3), rep("E", 3))
            # print(data.frame(x, fullvar))
            return(fullvar)
}

bgnvar.f <- function(){
      
      newvar <- c("ss0", "exss0", "pro0", "tfee", "netpro0")
      x <- c(rep("A", 5))
      # print(data.frame(x, newvar))
      return(newvar)
}

#teamname12 generate
teamname.f <- function(){
      a <- paste0("Alpha ", 1:6); b <- paste0("Beta ", 1:6); teamname12 <- c(a,b)     
      teamname12
}

#cff.f     
cff.f <- function(cType.c, pos1.c){
      sell_buy <- c("AFUT")
      lend_borrow <- c("Loan")
      long_short <- c("Forward", "Forward-S", "Bond", "Call Option", "Put Option", "Bond-E", "Call Option-E", "Put Option-E", "Call Option-S", "Put Option-S")
      na_cost <- c("Production")
      pay_receive <- c("Employ Service", "Cash")
      revenue_na <- c("Revenue Increment")
      savings_na <- c("Cost Reduction")
      
      if(cType.c %in% sell_buy){
            if(pos1.c %in% "Sell"){1}else{-1}
      }else if(cType.c %in% lend_borrow){
            if(pos1.c %in% "Lend"){1}else{-1}
      }else if(cType.c %in% long_short){
            if(pos1.c %in% "Long"){1}else{-1}
      }else if(cType.c %in% na_cost){
            if(pos1.c %in% "Cost"){-1}else{1}
      }else if(cType.c %in% pay_receive){
            if(pos1.c %in% "Pay"){1}else{-1}
      }else if(cType.c %in% revenue_na){
            if(pos1.c %in% "Revenue"){1}else{-1}
      }else if(cType.c %in% savings_na){
            if(pos1.c %in% "Savings"){1}else{-1}
      }else{NA}
}

#und.price.f
und.price.f <- function(Underlying.c, yr, meta.und.price.df){
      if(Underlying.c %in% colnames(meta.und.price.df)){
            undprice <- meta.und.price.df[yr+1, Underlying.c]
      }else if(Underlying.c %in% "MYR"){
            undprice <- 1
      }else{
            undprice <- NA
      }  
      undprice
}

######subset csv from 0 up to yy
subfiles.td.f <- function(list2.files, yy){
      list1 <- strsplit(list2.files, split = "_")
      yr.v <- sapply(list1, FUN = function(x){
            substr(x[2], 1,1)
      })
      td <- 0:yy
      u <- yr.v %in% td
      list2.files[u]            
      
}
#trackno.f

#general: TRACKNO function
trackno.f <- function(fulltrandf){
      trackv <- rep(1, nrow(fulltrandf))
      yearuni <- as.integer(unique(fulltrandf[,"tDate"]))
      classfuni <- unique(fulltrandf[,"classf"])
      for(yrr in yearuni){
            for(clf in classfuni){
                  Y <- fulltrandf[,"tDate"] == yrr; Z <- fulltrandf[,"classf"] == clf
                  nt <- length(trackv[Y & Z])
                  start <- yrr+100; mid <- substr(clf, 1,3)
                  trackv[Y & Z] <- sapply(1:nt, FUN = function(j){
                        paste0(start, mid, (1000+j))      
                  })
            }
      }
      #output: track number vector
      trackv
      
}