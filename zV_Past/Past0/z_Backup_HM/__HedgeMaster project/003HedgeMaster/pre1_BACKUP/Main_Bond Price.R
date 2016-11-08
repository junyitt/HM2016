source(file = "~/003HedgeMaster/0f_pricing.R")
source(file = "~/003HedgeMaster/0meta_pricing.R")


bondi <- read.table(file = "~/003HedgeMaster/meta1-bondinterest.txt", header = T)

n <- c(1,3,5,10)
bondprice <- list()
for(j in 1:length(n)){
        ii <- bondi[,j]
        z <- n[j]
        bondprice[[j]] <- bondcal(i = ii, n = z)
        
}

        
bonddf <- do.call(cbind, bondprice)
colnames(bonddf) <- c("BOND1", "BOND3", "BOND5", "BOND10")

write.csv(bonddf, file = paste0("~/003HedgeMaster/0_bondprice/bondprice2.csv"), row.names = F)
