AAV <- function(df, idx = NA) {
  df <- as.data.frame(df)

  yrs <- unique(df$year)
  nyear <- length(yrs)

  df$AAV <- as.numeric(NA)
  df[df$year %in% yrs[2:nyear],]$AAV <- (abs(df[,idx][df$year %in% yrs[2:nyear]]-df[,idx][df$year %in% yrs[1:(nyear-1)]])/
    df[,idx][df$year %in% yrs[2:nyear]])

  AAV <- df
  return(AAV)
}
