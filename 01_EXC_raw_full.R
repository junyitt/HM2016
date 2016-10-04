#DIR required
#raw_EXC.dir, meta.dir

#FUNCTIONs (general) required
#importrawEXC.f, importmetafic.f

#ENVIRONMENT OUTPUT:
      #EXCraw.df, ficmeta.df, tkey.df
      #EXCfulltran.df

#FUNCTION
      setwd(maincode.dir)
      source("01_01_EXC_functions.R")
      
#####SECTION EXC-to-FULL-1#####

##Import RAW-EXC-TRAN
EXCraw.df <- importrawEXC.f(raw_EXC.dir, yy)

##Import META
ficmeta.df <<- importmeta.f(meta.dir, "meta-FIC.xlsx")         #get ficmeta.df
tkey.df <<- importmeta.f(meta.dir, "meta-tradingkey.xlsx")     #get tkey.df


#write a function that convert EXCrawdf to full tran
EXCfulltran.df <- EXCfullconv.f(EXCraw.df, ficmeta.df, tkey.df)



     