MSE_list_to_df <- function(ls){
  simyears <- length(ls[[1]]$Catch[,1])-(length(1966:2017))+1
  
  yr <- 1966:(2017+simyears-1)
  
  
  df.time <- data.frame(yr = rep(yr,2), 
                       catch = c(ls[[1]]$Catch[,1],ls[[1]]$Catch[,2]), 
                       country = rep(c('CAN','USA'), each = length(yr)))
  
  for(i in 2:length(yr)){
    
    df.tmp <- data.frame(yr = rep(yr,2), 
                            catch = c(ls[[i]]$Catch[,1],ls[[i]]$Catch[,2]), 
                            country = rep(c('CAN','USA'), each = length(yr)))
    
    df.time <- rbind(df.time,df.tmp)
    
  }
  
  return(df.time)
}