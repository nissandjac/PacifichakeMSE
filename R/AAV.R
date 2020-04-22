AAV <- function(df) {

  yrs <- unique(df$year)
  nyear <- length(yrs)

  df$AAV <- as.numeric(NA)
  df[df$year %in% yrs[2:nyear],]$AAV <- as.numeric(abs(df$value[df$year %in% yrs[2:nyear]]-df$value[df$year %in% yrs[1:(nyear-1)]])/
    df$value[df$year %in% yrs[2:nyear]])

  AAV <- df
  return(AAV)
}
