
runAssessment <- function(df.new){
  
  years <- df.new$years
  
  
  #U[2,] <- 0.01
  parms <- getParameters_old(TRUE)
  
  
  compile("runHakeassessment.cpp")
  dyn.load(dynlib("runHakeassessment"))
  
  obj <-MakeADFun(df.new,parms,DLL="runHakeassessment", checkParameterOrder = FALSE)#, )
  
  # Test the input things 
  reps <- obj$report()
  
  # plot(reps$SSB)
  # lines(assessment$SSB)
  # 
  # 
  return(reps)
}