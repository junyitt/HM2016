#01A_startcutoff_f.R

startcutoff.f <- function(rawdf){
      u1 <- grep("startjan", rawdf[,"Remarks"])
      u2 <- grep("cutoffapril", rawdf[,"Remarks"])
      
      if(length(u1) == 0 & length(u2) == 0){
                  rawdf
      }else if(length(u1) == 0 & length(u2) > 0){
                  ua1 <- 1
                  ua2 <- max(u2) 
                  rawdf[(ua1:ua2),]
      }else if(length(u1) > 0 & length(u2) == 0){
                  ua1 <- max(u1)+1
                  ua2 <- nrow(rawdf)
                  if(ua2 < ua1){
                        rawdf[F,]          
                  }else{
                        rawdf[(ua1:ua2),]
                  }
      }else{
                  ua1 <- max(u1)+1
                  ua2 <- max(u2) 
                  rawdf[(ua1:ua2),]
      }
            
            
}