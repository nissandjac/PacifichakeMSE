getParameters_OM <- function(trueparms = TRUE,df){
  
  
  
  if (trueparms == TRUE){
    #assessment <- read.csv('data/asssessment_MLE.csv')
    
    #F0 <- assessment$F0[assessment$year > 1965 & assessment$year < 2018]
    
    
    parms <- df$parms
    parms$Rin <- parms$Rin[1:(length(parms$Rin)-1)]
    
    
    
  }else{
    
    PSEL <- matrix(0,5, length(1991:df$years[length(df$years)]))
    initN <- rep(0,df$nage-1)
    F0 <- rep(0.01, df$tEnd)
    Rdev <- rep(0, df$tEnd-1)
    #Rdev <- read.csv('Rdev_MLE.csv')[,1]
    
    
    parms <- list( # Just start all the simluations with the same initial conditions 
      logRinit = 16,
      logh = log(0.7),
      logMinit = log(0.3),
      logSDsurv = log(0.3),
      # logSDR = log(1.4),
      logphi_catch = log(0.8276),
      # logphi_survey = log(11.33),
      # logSDF = log(0.1),
      # Selectivity parameters 
      psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
      psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
      initN = initN,
      Rin = Rdev,
      F0 = F0,
      PSEL = PSEL
    )
    
    
  }
  return(parms) 
}