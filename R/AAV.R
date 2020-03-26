AAV <- function(catches, nyear = NA) {
  
  if(is.na(nyear)){
    nyear <- length(catches)
  }
  
  AAV  <-  abs(catches[2:nyear]-catches[1:(nyear-1)])/catches[1:(nyear-1)] 
  
  
  return(AAV)
}