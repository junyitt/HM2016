#60_BGN1.R

#DIR required
#maincode.dir, meta.dir

#FUNCTIONs (general) required
#importmeta.f

      #ENVIRONMENT OUTPUT:
      #bgn1.td.df

#Internal FUNCTION #bgn0.f, pro0.f, tfee.f

setwd(maincode.dir)
source("60_01_BGN1Functions.R")

####### 60_BGN Calc 1 #########

#META-underlyingprice, metafic, meta-employservice
      meta.undprice.df <- importmeta.f(meta.dir, fname = "meta-underlyingprice.xlsx")
      meta.fic.df <- importmeta.f(meta.dir, fname = "meta-FIC.xlsx")
      meta.employservice.df <- importmeta.f(meta.dir, fname = "meta-employservice.xlsx")
      
#output: bgn1.td.df
      bgn1.td.df <- bgn0.f(f1.td.df, meta.undprice.df, meta.fic.df, meta.employservice.df) #input: f1.td.df
      