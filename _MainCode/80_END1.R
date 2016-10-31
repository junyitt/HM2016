#DIR required
#maincode.dir, meta.dir

#FUNCTIONs (general) required
#importmeta.f

      #MAIN INPUT:
      #bgn2.td.df

      #ENVIRONMENT OUTPUT:
      #

#Internal FUNCTION #bgn0.f, pro0.f, tfee.f
setwd(maincode.dir); source("80_01_END1Functions.R")

###############################################################################

#META-underlyingprice, metaficA, metabond-e
meta.undprice.df <- importmeta.f(meta.dir, fname = "meta-underlyingprice.xlsx")
meta.fic.df <- importmeta.f(meta.dir, fname = "meta-FIC.xlsx")
meta.bonde.df <- importmeta.f(meta.dir, fname = "meta-bond-e-B.xlsx")

#add final 4 columns >> total 27 columns
end3.td.df <- end3.td.df.f(bgn2.td.df, meta.undprice.df, meta.bonde.df, yy) #use yy, function inside 'chg' to yy + 1 or cater for yy
#output: end3.td.df

