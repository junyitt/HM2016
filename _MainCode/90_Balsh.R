#90_Balsh.R


#DIR #maincode.dir

#GENERAL FUNCTION #NA

      #INPUT: balsh_y.df, end3.td.df, teamname12
      #OUTPUT: balsh_y2.df
      
#INTERNAL FUNCTION #acctype.v.f, old2newbs.f, newbs.df.f
setwd(maincode.dir); source("90_01_BalshFunctions.R") 


#####################################################################
teamname12 <- teamname.f()

balsh_y2.df <- newbs.df.f(balsh_y.df, end3.td.df , teamname12) 
      

