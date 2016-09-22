#60_BGN1.R

#source("C:/Users/User/Google Drive/r_Rfunction/_myCode.R")

#DIR
bgn6.f.dir <- "C:/Users/User/Google Drive/z_ALLHM"

#source functions
setwd(bgn6.f.dir); source("C:/Users/User/Google Drive/z_ALLHM/60_01_BGN1Functions.R")


#META_DIR
wd_metaunderlyingprice <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"

#META-underlyingprice, metaficA, metabond-e
setwd(wd_metaunderlyingprice)
      dfmetaunderprice <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
      dfmetafic <- as.data.frame(read_excel("meta-FIC-A.xlsx"))
      dfmetabonde <- as.data.frame(read_excel("meta-bond-e-B.xlsx"))  
      
#output: fdf_y6
      fdf_y6 <- bgn1df_f(fdf_y) #input: fdf_y
      