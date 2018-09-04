
runAssessment <- function(df.new){
  
  years <- df.new$years
  
  
  #U[2,] <- 0.01
  parms <- getParameters(TRUE,df.new)
  
  
  compile("runHakeassessment2.cpp")
  dyn.load(dynlib("runHakeassessment2"))
  
  obj <-MakeADFun(df.new,parms,DLL="runHakeassessment2")#, )
  
  # Test the input things 
  reps <- obj$report()
  
  plot(reps$SSB)
  lines(assessment$SSB)
  
  
  return(reps)
}