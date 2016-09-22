#DIR-f
end8f.dir <- "C:/Users/User/Google Drive/z_ALLHM"

#source functions
setwd(end8f.dir); source("80_01_END1Functions.R")


#META_DIR
wd_metaunderlyingprice <- "C:/Users/User/Google Drive/z_ALLHM/v5.0_7_Instruments"

#META-underlyingprice, metaficA, metabond-e
setwd(wd_metaunderlyingprice)
      dfmetaunderprice <- as.data.frame(read_excel("meta-underlyingprice.xlsx"))
      dfmetafic <- as.data.frame(read_excel("meta-FIC-A.xlsx"))
      dfmetabonde <- as.data.frame(read_excel("meta-bond-e-B.xlsx"))  

{
      #DIR-zfdf
      fdf_csvdir <- "C:/Users/User/OneDrive/yy_YearlyFullTran"
      setwd(fdf_csvdir); zfdf.files <- list.files(pattern = "^zfdf")
      
      #list.files and import all fdf -> fdf_td (td = to date).
      zfdf.list <- lapply(zfdf.files, FUN = function(x){
            read.csv(x)
      })
      fdf_td <- do.call(rbind, zfdf.list)
      
      #fix fdf_td (variable class)
      {
            fdf_td[] <- lapply(fdf_td, as.character)
            suppressWarnings({
                  col <- c("kPrice", "Units", "tDate", "mDate", "VL", "S0", "exS0", "Pro", "Tfee", "NetPro")
                  fdf_td[,col] <- sapply(col, FUN = function(j){as.numeric(fdf_td[,j])})
                  
            }) #as.numeric numeric columns
      } 
      
}#output: fdf_td

#final ST df
fdf8_td <- fdf8_f(fdf_td, yy+1)


