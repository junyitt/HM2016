#GENERAL FUNCTION: #importbalsh.f, teamname.f

#source functions 
#netcash.v.f, addloan.f, bgn2.td.df.f
setwd(maincode.dir); source("70_01_BGN2Functions.R")

      #ENVIRONMENT OUTPUT:
      #balsh_y.df, teamname12, netcash.v
            #bgn2.td.df

###############################################################
#MAIN INPUT: bgn1.td.df
###############################################################

#Import year y balance sheet 
balsh_y.df <- importbalsh.f(meta.dir, fulltran.dir, yy)


#find netcash for each team
teamname12 <- teamname.f()
netcash.v <- netcash.v.f(teamname12, bgn1.td.df, balsh_y.df)

#bgn2.td.df
bgn2.td.df <- bgn2.td.df.f(netcash.v, teamname12, bgn1.td.df, loanlength = 1, minbalance = 1e6, yy)

