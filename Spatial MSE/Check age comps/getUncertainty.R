getUncertainty <- function(name,data){
  
  df <- data.frame(name = sdrep[rep.values == name,1])
  df$SE <- sdrep[rep.values == name,2]
  df$min <- df$name-2*df$SE
  df$max <- df$name+2*df$SE
  
  if(dim(df)[1] == data$nyear){
    df$year <- data$year
  }
  
  return(df)  
}

