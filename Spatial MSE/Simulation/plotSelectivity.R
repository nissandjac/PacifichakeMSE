### Plot selectivity for years 

plotSelectivity <- function(df.new, parameters){
  
  yrs <- df.new$years[df.new$selYear]:df.new$years[length(df.new$years)]
  nyrs <- length(yrs)
  
  
  PSEL <- parameters[which(names(parameters) == 'PSEL')]
  
  psel_fish <- parameters[which(names(parameters) == 'psel_fish')]
  yr.idx <- seq(1,nyrs*length(psel_fish), by = length(psel_fish))
  
  par(mfrow = c(ceiling(sqrt(nyrs)), floor(sqrt(nyrs))), mar = c(1,1,1,1))
  for(i in 1:(nyrs-1)){
    
    psel <- PSEL[yr.idx[i]:(yr.idx[i+1]-1)]
    
    tmpsel <- getSelec(df.new$age, psel = psel, Smin = df.new$Smin, Smax =df.new$Smax)
    
    plot(df.new$age,tmpsel, type = 'l')
    
  }
  
  
  
}
