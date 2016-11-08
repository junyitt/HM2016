###INPUT

mu <- read.table("~/003HedgeMaster/meta-mu.txt", header = T)
sigma <- read.table("~/003HedgeMaster/meta-sigma.txt", header = T)
instrument_v <- read.table("~/003HedgeMaster/meta2-instrumentlist.txt", header = F, stringsAsFactors = F)[,1]
        bondi <- read.table(file = "~/003HedgeMaster/meta1-bondinterest.txt", header = T)

#options strike price meta
strike <- read.table("~/003HedgeMaster/meta-strike_K1.txt", header = T)
interest <- read.table("~/003HedgeMaster/meta-interest_r1.txt", header = T)
        interest <- log(1+interest)
dividend <- read.table("~/003HedgeMaster/meta-dividend_d1.txt", header = T)
        dividend <- log(1+dividend)

        
        
        #read all the intrest rate - META procedure
        YieldUSD_df <- log(1+read.table("~/003HedgeMaster/meta1-10YearYieldCurve_USD.txt", header = T))
        YieldEUR_df <- log(1+read.table("~/003HedgeMaster/meta1-10YearYieldCurve_EUR.txt", header = T))
        YieldMYR_df <- log(1+read.table("~/003HedgeMaster/meta1-10YearYieldCurve_MYR.txt", header = T))
        ##list-- to filter usd,eur,myr
        Yield3_list_df <- list(YieldUSD_df, YieldEUR_df, YieldMYR_df)


                