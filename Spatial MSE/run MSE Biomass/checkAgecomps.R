checkAgecomps <- function(age_obs, age_est, df){
  
  age_obs[age_obs < 0] <- NA
  # Turn the vector into a matrix 
  A <- df$age_maxage
  
  df.am <- data.frame(year = rep(df$years,each = A), age = rep(1:A,df$nyear),am = NA, min = NA, max = NA)
  
  df.am$am <- age_est$name
  df.am$min <- age_est$min
  df.am$max <- age_est$max
  
 
  # Calculate the average age 
  df.mean.est <- data.frame(year = df$years, am = NA, min = NA, max = NA)
  df.mean.obs <- data.frame(year = df$years, am = NA)
  
  
  for(i in 1:df$nyear){
    df.tmp <- df.am[df.am$year == df$years[i],]
    
    df.mean.est$am[i] <- sum(df.tmp$am*df.tmp$age)
    df.mean.est$min[i] <- mean(df.tmp$min, na.rm = T)
    df.mean.est$max[i] <- mean(df.tmp$max, na.rm = T)
  
    df.mean.obs$am[i] <- sum(age_obs[,i]*df$age[2:16])
    }
  
  
  p <- ggplot(df.mean.est, aes(x = year, y = am))+geom_line(size = 2)+geom_point(data = df.mean.obs, col = 'red', size = 2)+theme_classic()+
    geom_line(data = df.mean.obs, col = 'red', size = 1)
   
  print(p)
  
}